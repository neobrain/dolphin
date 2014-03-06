// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.


// http://developer.nvidia.com/object/General_FAQ.html#t6 !!!!!


#include "Common/Atomic.h"
#include "Common/ChunkFile.h"
#include "Common/Common.h"
#include "Core/ConfigManager.h"
#include "Core/CoreTiming.h"
#include "Core/State.h"
#include "Core/HW/MMIO.h"
#include "Core/HW/ProcessorInterface.h"
#include "VideoCommon/CommandProcessor.h"
#include "VideoCommon/PixelEngine.h"
#include "VideoCommon/RenderBase.h"
#include "VideoCommon/VideoCommon.h"

namespace PixelEngine
{

union UPEZConfReg
{
	u16 Hex;
	struct
	{
		u16 ZCompEnable : 1; // Z Comparator Enable
		u16 Function    : 3;
		u16 ZUpdEnable  : 1;
		u16             : 11;
	};
};

union UPEAlphaConfReg
{
	u16 Hex;
	struct
	{
		u16 BMMath         : 1; // GX_BM_BLEND || GX_BM_SUBSTRACT
		u16 BMLogic        : 1; // GX_BM_LOGIC
		u16 Dither         : 1;
		u16 ColorUpdEnable : 1;
		u16 AlphaUpdEnable : 1;
		u16 DstFactor      : 3;
		u16 SrcFactor      : 3;
		u16 Substract      : 1; // Additive mode by default
		u16 BlendOperator  : 4;
	};
};

union UPEDstAlphaConfReg
{
	u16 Hex;
	struct
	{
		u16 DstAlpha : 8;
		u16 Enable   : 1;
		u16          : 7;
	};
};

union UPEAlphaModeConfReg
{
	u16 Hex;
	struct
	{
		u16 Threshold   : 8;
		u16 CompareMode : 8;
	};
};

// fifo Control Register
union UPECtrlReg
{
	struct
	{
		u16 PETokenEnable  : 1;
		u16 PEFinishEnable : 1;
		u16 PEToken        : 1; // write only
		u16 PEFinish       : 1; // write only
		u16                : 12;
	};
	u16 Hex;
	UPECtrlReg() {Hex = 0; }
	UPECtrlReg(u16 _hex) {Hex = _hex; }
};

// STATE_TO_SAVE
static UPEZConfReg         m_ZConf;
static UPEAlphaConfReg     m_AlphaConf;
static UPEDstAlphaConfReg  m_DstAlphaConf;
static UPEAlphaModeConfReg m_AlphaModeConf;
static UPEAlphaReadReg     m_AlphaRead;
static UPECtrlReg          m_Control;
//static u16                 m_Token; // token value most recently encountered

volatile u32 g_bSignalTokenInterrupt;
volatile u32 g_bSignalFinishInterrupt;

static int et_SetTokenOnMainThread;
static int et_SetFinishOnMainThread;

volatile u32 interruptSetToken = 0;
volatile u32 interruptSetFinish = 0;

u16 bbox[4];
bool bbox_active;

enum
{
	INT_CAUSE_PE_TOKEN  = 0x200, // GP Token
	INT_CAUSE_PE_FINISH = 0x400, // GP Finished
};

void DoState(PointerWrap &p)
{
	p.Do(m_ZConf);
	p.Do(m_AlphaConf);
	p.Do(m_DstAlphaConf);
	p.Do(m_AlphaModeConf);
	p.Do(m_AlphaRead);
	p.DoPOD(m_Control);

	p.Do(g_bSignalTokenInterrupt);
	p.Do(g_bSignalFinishInterrupt);
	p.Do(interruptSetToken);
	p.Do(interruptSetFinish);

	p.Do(bbox);
	p.Do(bbox_active);
}

void UpdateInterrupts();
void UpdateTokenInterrupt(bool active);
void UpdateFinishInterrupt(bool active);
void SetToken_OnMainThread(u64 userdata, int cyclesLate);
void SetFinish_OnMainThread(u64 userdata, int cyclesLate);

void Init()
{
	m_Control.Hex = 0;
	m_ZConf.Hex = 0;
	m_AlphaConf.Hex = 0;
	m_DstAlphaConf.Hex = 0;
	m_AlphaModeConf.Hex = 0;
	m_AlphaRead.Hex = 0;

	g_bSignalTokenInterrupt = 0;
	g_bSignalFinishInterrupt = 0;
	interruptSetToken = 0;
	interruptSetFinish = 0;

	et_SetTokenOnMainThread = CoreTiming::RegisterEvent("SetToken", SetToken_OnMainThread);
	et_SetFinishOnMainThread = CoreTiming::RegisterEvent("SetFinish", SetFinish_OnMainThread);

	bbox[0] = 0x80;
	bbox[1] = 0xA0;
	bbox[2] = 0x80;
	bbox[3] = 0xA0;

	bbox_active = false;
}

void RegisterMMIO(MMIO::Mapping* mmio, u32 base)
{
	// Directly mapped registers.
	struct {
		u32 addr;
		u16* ptr;
	} directly_mapped_vars[] = {
		{ PE_ZCONF, &m_ZConf.Hex },
		{ PE_ALPHACONF, &m_AlphaConf.Hex },
		{ PE_DSTALPHACONF, &m_DstAlphaConf.Hex },
		{ PE_ALPHAMODE, &m_AlphaModeConf.Hex },
		{ PE_ALPHAREAD, &m_AlphaRead.Hex },
	};
	for (auto& mapped_var : directly_mapped_vars)
	{
		mmio->Register(base | mapped_var.addr,
			MMIO::DirectRead<u16>(mapped_var.ptr),
			MMIO::DirectWrite<u16>(mapped_var.ptr)
		);
	}

	// Performance queries registers: read only, need to call the video backend
	// to get the results.
	struct {
		u32 addr;
		PerfQueryType pqtype;
	} pq_regs[] = {
		{ PE_PERF_ZCOMP_INPUT_ZCOMPLOC_L, PQ_ZCOMP_INPUT_ZCOMPLOC },
		{ PE_PERF_ZCOMP_OUTPUT_ZCOMPLOC_L, PQ_ZCOMP_OUTPUT_ZCOMPLOC },
		{ PE_PERF_ZCOMP_INPUT_L, PQ_ZCOMP_INPUT },
		{ PE_PERF_ZCOMP_OUTPUT_L, PQ_ZCOMP_OUTPUT },
		{ PE_PERF_BLEND_INPUT_L, PQ_BLEND_INPUT },
		{ PE_PERF_EFB_COPY_CLOCKS_L, PQ_EFB_COPY_CLOCKS },
	};
	for (auto& pq_reg : pq_regs)
	{
		mmio->Register(base | pq_reg.addr,
			MMIO::ComplexRead<u16>([pq_reg](u32) {
				return g_video_backend->Video_GetQueryResult(pq_reg.pqtype) & 0xFFFF;
			}),
			MMIO::InvalidWrite<u16>()
		);
		mmio->Register(base | (pq_reg.addr + 2),
			MMIO::ComplexRead<u16>([pq_reg](u32) {
				return g_video_backend->Video_GetQueryResult(pq_reg.pqtype) >> 16;
			}),
			MMIO::InvalidWrite<u16>()
		);
	}

	// Control register
	mmio->Register(base | PE_CTRL_REGISTER,
		MMIO::DirectRead<u16>(&m_Control.Hex),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			UPECtrlReg tmpCtrl(val);

			if (tmpCtrl.PEToken)  g_bSignalTokenInterrupt = 0;
			if (tmpCtrl.PEFinish) g_bSignalFinishInterrupt = 0;

			m_Control.PETokenEnable  = tmpCtrl.PETokenEnable;
			m_Control.PEFinishEnable = tmpCtrl.PEFinishEnable;
			m_Control.PEToken = 0;  // this flag is write only
			m_Control.PEFinish = 0; // this flag is write only

			DEBUG_LOG(PIXELENGINE, "(w16) CTRL_REGISTER: 0x%04x", val);
			UpdateInterrupts();
		})
	);

	// Token register, readonly.
	mmio->Register(base | PE_TOKEN_REG,
		MMIO::DirectRead<u16>(&CommandProcessor::fifo.PEToken),
		MMIO::InvalidWrite<u16>()
	);

	// BBOX registers, readonly and need to update a flag.
	for (int i = 0; i < 4; ++i)
	{
		mmio->Register(base | (PE_BBOX_LEFT + 2 * i),
			MMIO::ComplexRead<u16>([i](u32) {
				bbox_active = false;
				return bbox[i];
			}),
			MMIO::InvalidWrite<u16>()
		);
	}
}

bool AllowIdleSkipping()
{
	return !SConfig::GetInstance().m_LocalCoreStartupParameter.bCPUThread || (!m_Control.PETokenEnable && !m_Control.PEFinishEnable);
}

void UpdateInterrupts()
{
	// check if there is a token-interrupt
	UpdateTokenInterrupt((g_bSignalTokenInterrupt & m_Control.PETokenEnable));

	// check if there is a finish-interrupt
	UpdateFinishInterrupt((g_bSignalFinishInterrupt & m_Control.PEFinishEnable));
}

void UpdateTokenInterrupt(bool active)
{
	ProcessorInterface::SetInterrupt(INT_CAUSE_PE_TOKEN, active);
	Common::AtomicStore(interruptSetToken, active ? 1 : 0);
}

void UpdateFinishInterrupt(bool active)
{
	ProcessorInterface::SetInterrupt(INT_CAUSE_PE_FINISH, active);
	Common::AtomicStore(interruptSetFinish, active ? 1 : 0);
}

// TODO(mb2): Refactor SetTokenINT_OnMainThread(u64 userdata, int cyclesLate).
//            Think about the right order between tokenVal and tokenINT... one day maybe.
//            Cleanup++

// Called only if BPMEM_PE_TOKEN_INT_ID is ack by GP
void SetToken_OnMainThread(u64 userdata, int cyclesLate)
{
	// XXX: No 16-bit atomic store available, so cheat and use 32-bit.
	// That's what we've always done. We're counting on fifo.PEToken to be
	// 4-byte padded.
	Common::AtomicStore(*(volatile u32*)&CommandProcessor::fifo.PEToken, userdata & 0xffff);
	INFO_LOG(PIXELENGINE, "VIDEO Backend raises INT_CAUSE_PE_TOKEN (btw, token: %04x)", CommandProcessor::fifo.PEToken);
	if (userdata >> 16)
	{
		Common::AtomicStore(*(volatile u32*)&g_bSignalTokenInterrupt, 1);
		UpdateInterrupts();
	}
	CommandProcessor::interruptTokenWaiting = false;
}

void SetFinish_OnMainThread(u64 userdata, int cyclesLate)
{
	Common::AtomicStore(*(volatile u32*)&g_bSignalFinishInterrupt, 1);
	UpdateInterrupts();
	CommandProcessor::interruptFinishWaiting = false;
	CommandProcessor::isPossibleWaitingSetDrawDone = false;
}

// SetToken
// THIS IS EXECUTED FROM VIDEO THREAD
void SetToken(const u16 _token, const int _bSetTokenAcknowledge)
{
	if (_bSetTokenAcknowledge) // set token INT
	{
		Common::AtomicStore(*(volatile u32*)&g_bSignalTokenInterrupt, 1);
	}

	CommandProcessor::interruptTokenWaiting = true;
	CoreTiming::ScheduleEvent_Threadsafe(0, et_SetTokenOnMainThread, _token | (_bSetTokenAcknowledge << 16));
}

// SetFinish
// THIS IS EXECUTED FROM VIDEO THREAD (BPStructs.cpp) when a new frame has been drawn
void SetFinish()
{
	CommandProcessor::interruptFinishWaiting = true;
	CoreTiming::ScheduleEvent_Threadsafe(0, et_SetFinishOnMainThread, 0);
	INFO_LOG(PIXELENGINE, "VIDEO Set Finish");
}

//This function is used in CommandProcessor when write CTRL_REGISTER and the new fifo is attached.
void ResetSetFinish()
{
	//if SetFinish happened but PE_CTRL_REGISTER not, I reset the interrupt else
	//remove event from the queue
	if (g_bSignalFinishInterrupt)
	{
		UpdateFinishInterrupt(false);
		g_bSignalFinishInterrupt = false;
	}
	else
	{
		CoreTiming::RemoveEvent(et_SetFinishOnMainThread);
	}
	CommandProcessor::interruptFinishWaiting = false;
}

void ResetSetToken()
{
	if (g_bSignalTokenInterrupt)
	{
		UpdateTokenInterrupt(false);
		g_bSignalTokenInterrupt = 0;
	}
	else
	{
		CoreTiming::RemoveEvent(et_SetTokenOnMainThread);
	}
	CommandProcessor::interruptTokenWaiting = false;
}

UPEAlphaReadReg GetAlphaReadMode()
{
	return m_AlphaRead;
}

} // end of namespace PixelEngine
