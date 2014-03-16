// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include "Common/ChunkFile.h"
#include "Common/Common.h"
#include "Common/StringUtil.h"

#include "Core/Core.h"
#include "Core/CoreTiming.h"
#include "Core/State.h"
#include "Core/HW/Memmap.h"
#include "Core/HW/MMIO.h"
#include "Core/HW/ProcessorInterface.h"
#include "Core/HW/SystemTimers.h"
#include "Core/HW/VideoInterface.h"
#include "Core/PowerPC/PowerPC.h"

#include "VideoCommon/VideoBackendBase.h"

namespace VideoInterface
{

// STATE_TO_SAVE
// Registers listed in order:
static UVIVerticalTimingRegister m_VerticalTimingRegister;
static UVIDisplayControlRegister m_DisplayControlRegister;
static UVIHorizontalTiming0      m_HTiming0;
static UVIHorizontalTiming1      m_HTiming1;
static UVIVBlankTimingRegister   m_VBlankTimingOdd;
static UVIVBlankTimingRegister   m_VBlankTimingEven;
static UVIBurstBlankingRegister  m_BurstBlankingOdd;
static UVIBurstBlankingRegister  m_BurstBlankingEven;
static UVIFBInfoRegister         m_XFBInfoTop;
static UVIFBInfoRegister         m_XFBInfoBottom;
static UVIFBInfoRegister         m_3DFBInfoTop;     // Start making your stereoscopic demos! :p
static UVIFBInfoRegister         m_3DFBInfoBottom;
static u16                       m_VBeamPos = 0;    // 0: Inactive
static u16                       m_HBeamPos = 0;    // 0: Inactive
static UVIInterruptRegister      m_InterruptRegister[4];
static UVILatchRegister          m_LatchRegister[2];
static UVIHorizontalStepping     m_HorizontalStepping;
static UVIHorizontalScaling      m_HorizontalScaling;
static SVIFilterCoefTables       m_FilterCoefTables;
static u32                       m_UnkAARegister = 0;// ??? 0x00FF0000
static u16                       m_Clock = 0;       // 0: 27MHz, 1: 54MHz
static UVIDTVStatus              m_DTVStatus;
static u16                       m_FBWidth = 0;     // Only correct when scaling is enabled?
static UVIBorderBlankRegister    m_BorderHBlank;
// 0xcc002076 - 0xcc00207f is full of 0x00FF: unknown
// 0xcc002080 - 0xcc002100 even more unknown

u32 TargetRefreshRate = 0;

static u32 TicksPerFrame = 0;
static u32 s_lineCount = 0;
static u32 s_upperFieldBegin = 0;
static u32 s_lowerFieldBegin = 0;
static int fields = 1;

void DoState(PointerWrap &p)
{
	p.DoPOD(m_VerticalTimingRegister);
	p.DoPOD(m_DisplayControlRegister);
	p.Do(m_HTiming0);
	p.Do(m_HTiming1);
	p.Do(m_VBlankTimingOdd);
	p.Do(m_VBlankTimingEven);
	p.Do(m_BurstBlankingOdd);
	p.Do(m_BurstBlankingEven);
	p.Do(m_XFBInfoTop);
	p.Do(m_XFBInfoBottom);
	p.Do(m_3DFBInfoTop);
	p.Do(m_3DFBInfoBottom);
	p.Do(m_VBeamPos);
	p.Do(m_HBeamPos);
	p.DoArray(m_InterruptRegister, 4);
	p.DoArray(m_LatchRegister, 2);
	p.Do(m_HorizontalStepping);
	p.DoPOD(m_HorizontalScaling);
	p.Do(m_FilterCoefTables);
	p.Do(m_UnkAARegister);
	p.Do(m_Clock);
	p.Do(m_DTVStatus);
	p.Do(m_FBWidth);
	p.Do(m_BorderHBlank);
	p.Do(TargetRefreshRate);
	p.Do(TicksPerFrame);
	p.Do(s_lineCount);
	p.Do(s_upperFieldBegin);
	p.Do(s_lowerFieldBegin);
}

// Executed after Init, before game boot
void Preset(bool _bNTSC)
{
	m_VerticalTimingRegister.EQU = 6;

	m_DisplayControlRegister.ENB = 1;
	m_DisplayControlRegister.FMT = _bNTSC ? 0 : 1;

	m_HTiming0.HLW = 429;
	m_HTiming0.HCE = 105;
	m_HTiming0.HCS = 71;
	m_HTiming1.HSY = 64;
	m_HTiming1.HBE640 = 162;
	m_HTiming1.HBS640 = 373;

	m_VBlankTimingOdd.PRB = 502;
	m_VBlankTimingOdd.PSB = 5;
	m_VBlankTimingEven.PRB = 503;
	m_VBlankTimingEven.PSB = 4;

	m_BurstBlankingOdd.BS0 = 12;
	m_BurstBlankingOdd.BE0 = 520;
	m_BurstBlankingOdd.BS2 = 12;
	m_BurstBlankingOdd.BE2 = 520;
	m_BurstBlankingEven.BS0 = 13;
	m_BurstBlankingEven.BE0 = 519;
	m_BurstBlankingEven.BS2 = 13;
	m_BurstBlankingEven.BE2 = 519;

	m_InterruptRegister[0].HCT = 430;
	m_InterruptRegister[0].VCT = 263;
	m_InterruptRegister[0].IR_MASK = 1;
	m_InterruptRegister[0].IR_INT = 0;
	m_InterruptRegister[1].HCT = 1;
	m_InterruptRegister[1].VCT = 1;
	m_InterruptRegister[1].IR_MASK = 1;
	m_InterruptRegister[1].IR_INT = 0;

	m_HorizontalStepping.FbSteps = 40;
	m_HorizontalStepping.FieldSteps = 40;

	m_HBeamPos = -1; // NTSC-U N64 VC games check for a non-zero HBeamPos
	m_VBeamPos = 0; // RG4JC0 checks for a zero VBeamPos

	// 54MHz, capable of progressive scan
	m_Clock = Core::g_CoreStartupParameter.bProgressive;

	// Say component cable is plugged
	m_DTVStatus.component_plugged = Core::g_CoreStartupParameter.bProgressive;

	UpdateParameters();
}

void Init()
{
	m_VerticalTimingRegister.Hex = 0;
	m_DisplayControlRegister.Hex = 0;
	m_HTiming0.Hex = 0;
	m_HTiming1.Hex = 0;
	m_VBlankTimingOdd.Hex = 0;
	m_VBlankTimingEven.Hex = 0;
	m_BurstBlankingOdd.Hex = 0;
	m_BurstBlankingEven.Hex = 0;
	m_XFBInfoTop.Hex = 0;
	m_XFBInfoBottom.Hex = 0;
	m_3DFBInfoTop.Hex = 0;
	m_3DFBInfoBottom.Hex = 0;
	m_VBeamPos = 0;
	m_HBeamPos = 0;
	m_HorizontalStepping.Hex = 0;
	m_HorizontalScaling.Hex = 0;
	m_UnkAARegister = 0;
	m_Clock = 0;
	m_DTVStatus.Hex = 0;
	m_FBWidth = 0;
	m_BorderHBlank.Hex = 0;
	memset(&m_FilterCoefTables, 0, sizeof(m_FilterCoefTables));

	fields = 1;

	m_DTVStatus.ntsc_j = Core::g_CoreStartupParameter.bForceNTSCJ;

	for (UVIInterruptRegister& reg : m_InterruptRegister)
	{
		reg.Hex = 0;
	}

	for (UVILatchRegister& reg : m_LatchRegister)
	{
		reg.Hex = 0;
	}

	m_DisplayControlRegister.Hex = 0;
	UpdateParameters();
}

void RegisterMMIO(MMIO::Mapping* mmio, u32 base)
{
	struct {
		u32 addr;
		u16* ptr;
	} directly_mapped_vars[] = {
		{ VI_VERTICAL_TIMING, &m_VerticalTimingRegister.Hex },
		{ VI_HORIZONTAL_TIMING_0_HI, &m_HTiming0.Hi },
		{ VI_HORIZONTAL_TIMING_0_LO, &m_HTiming0.Lo },
		{ VI_HORIZONTAL_TIMING_1_HI, &m_HTiming1.Hi },
		{ VI_HORIZONTAL_TIMING_1_LO, &m_HTiming1.Lo },
		{ VI_VBLANK_TIMING_ODD_HI, &m_VBlankTimingOdd.Hi },
		{ VI_VBLANK_TIMING_ODD_LO, &m_VBlankTimingOdd.Lo },
		{ VI_VBLANK_TIMING_EVEN_HI, &m_VBlankTimingEven.Hi },
		{ VI_VBLANK_TIMING_EVEN_LO, &m_VBlankTimingEven.Lo },
		{ VI_BURST_BLANKING_ODD_HI, &m_BurstBlankingOdd.Hi },
		{ VI_BURST_BLANKING_ODD_LO, &m_BurstBlankingOdd.Lo },
		{ VI_BURST_BLANKING_EVEN_HI, &m_BurstBlankingEven.Hi },
		{ VI_BURST_BLANKING_EVEN_LO, &m_BurstBlankingEven.Lo },
		{ VI_FB_LEFT_TOP_LO, &m_XFBInfoTop.Lo },
		{ VI_FB_RIGHT_TOP_LO, &m_3DFBInfoTop.Lo },
		{ VI_FB_LEFT_BOTTOM_LO, &m_XFBInfoBottom.Lo },
		{ VI_FB_RIGHT_BOTTOM_LO, &m_3DFBInfoBottom.Lo },
		{ VI_PRERETRACE_LO, &m_InterruptRegister[0].Lo },
		{ VI_POSTRETRACE_LO, &m_InterruptRegister[1].Lo },
		{ VI_DISPLAY_INTERRUPT_2_LO, &m_InterruptRegister[2].Lo },
		{ VI_DISPLAY_INTERRUPT_3_LO, &m_InterruptRegister[3].Lo },
		{ VI_DISPLAY_LATCH_0_HI, &m_LatchRegister[0].Hi },
		{ VI_DISPLAY_LATCH_0_LO, &m_LatchRegister[0].Lo },
		{ VI_DISPLAY_LATCH_1_HI, &m_LatchRegister[1].Hi },
		{ VI_DISPLAY_LATCH_1_LO, &m_LatchRegister[1].Lo },
		{ VI_HSCALEW, &m_HorizontalStepping.Hex },
		{ VI_HSCALER, &m_HorizontalScaling.Hex },
		{ VI_FILTER_COEF_0_HI, &m_FilterCoefTables.Tables02[0].Hi },
		{ VI_FILTER_COEF_0_LO, &m_FilterCoefTables.Tables02[0].Lo },
		{ VI_FILTER_COEF_1_HI, &m_FilterCoefTables.Tables02[1].Hi },
		{ VI_FILTER_COEF_1_LO, &m_FilterCoefTables.Tables02[1].Lo },
		{ VI_FILTER_COEF_2_HI, &m_FilterCoefTables.Tables02[2].Hi },
		{ VI_FILTER_COEF_2_LO, &m_FilterCoefTables.Tables02[2].Lo },
		{ VI_FILTER_COEF_3_HI, &m_FilterCoefTables.Tables36[0].Hi },
		{ VI_FILTER_COEF_3_LO, &m_FilterCoefTables.Tables36[0].Lo },
		{ VI_FILTER_COEF_4_HI, &m_FilterCoefTables.Tables36[1].Hi },
		{ VI_FILTER_COEF_4_LO, &m_FilterCoefTables.Tables36[1].Lo },
		{ VI_FILTER_COEF_5_HI, &m_FilterCoefTables.Tables36[2].Hi },
		{ VI_FILTER_COEF_5_LO, &m_FilterCoefTables.Tables36[2].Lo },
		{ VI_FILTER_COEF_6_HI, &m_FilterCoefTables.Tables36[3].Hi },
		{ VI_FILTER_COEF_6_LO, &m_FilterCoefTables.Tables36[3].Lo },
		{ VI_CLOCK, &m_Clock },
		{ VI_DTV_STATUS, &m_DTVStatus.Hex },
		{ VI_FBWIDTH, &m_FBWidth },
		{ VI_BORDER_BLANK_END, &m_BorderHBlank.Lo },
		{ VI_BORDER_BLANK_START, &m_BorderHBlank.Hi },
	};

	// Declare all the boilerplate direct MMIOs.
	for (auto& mapped_var : directly_mapped_vars)
	{
		mmio->Register(base | mapped_var.addr,
			MMIO::DirectRead<u16>(mapped_var.ptr),
			MMIO::DirectWrite<u16>(mapped_var.ptr)
		);
	}

	// XFB related MMIOs that require special handling on writes.
	mmio->Register(base | VI_FB_LEFT_TOP_HI,
		MMIO::DirectRead<u16>(&m_XFBInfoTop.Hi),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_XFBInfoTop.Hi = val;
			if (m_XFBInfoTop.CLRPOFF) m_XFBInfoTop.POFF = 0;
		})
	);
	mmio->Register(base | VI_FB_LEFT_BOTTOM_HI,
		MMIO::DirectRead<u16>(&m_XFBInfoBottom.Hi),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_XFBInfoBottom.Hi = val;
			if (m_XFBInfoBottom.CLRPOFF) m_XFBInfoBottom.POFF = 0;
		})
	);
	mmio->Register(base | VI_FB_RIGHT_TOP_HI,
		MMIO::DirectRead<u16>(&m_3DFBInfoTop.Hi),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_3DFBInfoTop.Hi = val;
			if (m_3DFBInfoTop.CLRPOFF) m_3DFBInfoTop.POFF = 0;
		})
	);
	mmio->Register(base | VI_FB_RIGHT_BOTTOM_HI,
		MMIO::DirectRead<u16>(&m_3DFBInfoBottom.Hi),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_3DFBInfoBottom.Hi = val;
			if (m_3DFBInfoBottom.CLRPOFF) m_3DFBInfoBottom.POFF = 0;
		})
	);

	// MMIOs with unimplemented writes that trigger warnings.
	mmio->Register(base | VI_VERTICAL_BEAM_POSITION,
		MMIO::DirectRead<u16>(&m_VBeamPos),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			WARN_LOG(VIDEOINTERFACE, "Changing vertical beam position to 0x%04x - not documented or implemented yet", val);
		})
	);
	mmio->Register(base | VI_HORIZONTAL_BEAM_POSITION,
		MMIO::DirectRead<u16>(&m_HBeamPos),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			WARN_LOG(VIDEOINTERFACE, "Changing horizontal beam position to 0x%04x - not documented or implemented yet", val);
		})
	);

	// The following MMIOs are interrupts related and update interrupt status
	// on writes.
	mmio->Register(base | VI_PRERETRACE_HI,
		MMIO::DirectRead<u16>(&m_InterruptRegister[0].Hi),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_InterruptRegister[0].Hi = val;
			UpdateInterrupts();
		})
	);
	mmio->Register(base | VI_POSTRETRACE_HI,
		MMIO::DirectRead<u16>(&m_InterruptRegister[1].Hi),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_InterruptRegister[1].Hi = val;
			UpdateInterrupts();
		})
	);
	mmio->Register(base | VI_DISPLAY_INTERRUPT_2_HI,
		MMIO::DirectRead<u16>(&m_InterruptRegister[2].Hi),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_InterruptRegister[2].Hi = val;
			UpdateInterrupts();
		})
	);
	mmio->Register(base | VI_DISPLAY_INTERRUPT_3_HI,
		MMIO::DirectRead<u16>(&m_InterruptRegister[3].Hi),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_InterruptRegister[3].Hi = val;
			UpdateInterrupts();
		})
	);

	// Unknown anti-aliasing related MMIO register: puts a warning on log and
	// needs to shift/mask when reading/writing.
	mmio->Register(base | VI_UNK_AA_REG_HI,
		MMIO::ComplexRead<u16>([](u32) {
			return m_UnkAARegister >> 16;
		}),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_UnkAARegister = (m_UnkAARegister & 0x0000FFFF) | ((u32)val << 16);
			WARN_LOG(VIDEOINTERFACE, "Writing to the unknown AA register (hi)");
		})
	);
	mmio->Register(base | VI_UNK_AA_REG_LO,
		MMIO::ComplexRead<u16>([](u32) {
			return m_UnkAARegister & 0xFFFF;
		}),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			m_UnkAARegister = (m_UnkAARegister & 0xFFFF0000) | val;
			WARN_LOG(VIDEOINTERFACE, "Writing to the unknown AA register (lo)");
		})
	);

	// Control register writes only updates some select bits, and additional
	// processing needs to be done if a reset is requested.
	mmio->Register(base | VI_CONTROL_REGISTER,
		MMIO::DirectRead<u16>(&m_DisplayControlRegister.Hex),
		MMIO::ComplexWrite<u16>([](u32, u16 val) {
			UVIDisplayControlRegister tmpConfig(val);
			m_DisplayControlRegister.ENB = tmpConfig.ENB;
			m_DisplayControlRegister.NIN = tmpConfig.NIN;
			m_DisplayControlRegister.DLR = tmpConfig.DLR;
			m_DisplayControlRegister.LE0 = tmpConfig.LE0;
			m_DisplayControlRegister.LE1 = tmpConfig.LE1;
			m_DisplayControlRegister.FMT = tmpConfig.FMT;

			if (tmpConfig.RST)
			{
				// shuffle2 clear all data, reset to default vals, and enter idle mode
				m_DisplayControlRegister.RST = 0;
				for (UVIInterruptRegister& reg : m_InterruptRegister)
				{
					reg.Hex = 0;
				}
				UpdateInterrupts();
			}

			UpdateParameters();
		})
	);

    // Map 8 bit reads (not writes) to 16 bit reads.
	for (int i = 0; i < 0x1000; i += 2)
	{
		mmio->Register(base | i,
			MMIO::ReadToLarger<u8>(mmio, base | i, 8),
			MMIO::InvalidWrite<u8>()
		);
		mmio->Register(base | (i + 1),
			MMIO::ReadToLarger<u8>(mmio, base | i, 0),
			MMIO::InvalidWrite<u8>()
		);
	}

    // Map 32 bit reads and writes to 16 bit reads and writes.
    for (int i = 0; i < 0x1000; i += 4)
    {
		mmio->Register(base | i,
			MMIO::ReadToSmaller<u32>(mmio, base | i, base | (i + 2)),
			MMIO::WriteToSmaller<u32>(mmio, base | i, base | (i + 2))
		);
    }
}

void SetRegionReg(char region)
{
	if (!Core::g_CoreStartupParameter.bForceNTSCJ)
		m_DTVStatus.ntsc_j = region == 'J';
}

void UpdateInterrupts()
{
	if ((m_InterruptRegister[0].IR_INT && m_InterruptRegister[0].IR_MASK) ||
		(m_InterruptRegister[1].IR_INT && m_InterruptRegister[1].IR_MASK) ||
		(m_InterruptRegister[2].IR_INT && m_InterruptRegister[2].IR_MASK) ||
		(m_InterruptRegister[3].IR_INT && m_InterruptRegister[3].IR_MASK))
	{
		ProcessorInterface::SetInterrupt(ProcessorInterface::INT_CAUSE_VI, true);
	}
	else
	{
		ProcessorInterface::SetInterrupt(ProcessorInterface::INT_CAUSE_VI, false);
	}
}

u32 GetXFBAddressTop()
{
	if (m_XFBInfoTop.POFF)
		return m_XFBInfoTop.FBB << 5;
	else
		return m_XFBInfoTop.FBB;
}

u32 GetXFBAddressBottom()
{
	// POFF for XFB bottom is connected to POFF for XFB top
	if (m_XFBInfoTop.POFF)
		return m_XFBInfoBottom.FBB << 5;
	else
		return m_XFBInfoBottom.FBB;
}

void UpdateParameters()
{
	fields = m_DisplayControlRegister.NIN ? 2 : 1;

	switch (m_DisplayControlRegister.FMT)
	{
	case 0: // NTSC
		TargetRefreshRate = NTSC_FIELD_RATE;
		TicksPerFrame = SystemTimers::GetTicksPerSecond() / NTSC_FIELD_RATE;
		s_lineCount = NTSC_LINE_COUNT;
		s_upperFieldBegin = NTSC_UPPER_BEGIN;
		s_lowerFieldBegin = NTSC_LOWER_BEGIN;
		break;

	case 2: // PAL-M
		TargetRefreshRate = NTSC_FIELD_RATE;
		TicksPerFrame = SystemTimers::GetTicksPerSecond() / NTSC_FIELD_RATE;
		s_lineCount = PAL_LINE_COUNT;
		s_upperFieldBegin = PAL_UPPER_BEGIN;
		s_lowerFieldBegin = PAL_LOWER_BEGIN;
		break;

	case 1: // PAL
		TargetRefreshRate = PAL_FIELD_RATE;
		TicksPerFrame = SystemTimers::GetTicksPerSecond() / PAL_FIELD_RATE;
		s_lineCount = PAL_LINE_COUNT;
		s_upperFieldBegin = PAL_UPPER_BEGIN;
		s_lowerFieldBegin = PAL_LOWER_BEGIN;
		break;

	case 3: // Debug
		PanicAlert("Debug video mode not implemented");
		break;

	default:
		PanicAlert("Unknown Video Format - CVideoInterface");
		break;
	}
}

int GetNumFields()
{
	if (Core::g_CoreStartupParameter.bVBeamSpeedHack)
		return (2 / fields);
	else
		return 1;
}

unsigned int GetTicksPerLine()
{
	if (s_lineCount == 0)
	{
		return 1;
	}
	else
	{
		if (Core::g_CoreStartupParameter.bVBeamSpeedHack)
			return TicksPerFrame / s_lineCount;
		else
			return TicksPerFrame / (s_lineCount / (2 / fields)) ;
	}
}

unsigned int GetTicksPerFrame()
{
	return TicksPerFrame;
}

static void BeginField(FieldType field)
{
	u32 fbWidth = m_HorizontalStepping.FieldSteps * 16;
	u32 fbHeight = (m_HorizontalStepping.FbSteps / m_HorizontalStepping.FieldSteps) * m_VerticalTimingRegister.ACV;
	u32 xfbAddr;

	// NTSC and PAL have opposite field orders.
	if (m_DisplayControlRegister.FMT == 1) // PAL
	{
		// But the PAL ports of some games are poorly programmed and don't use correct ordering.
		// Zelda: Wind Waker and Simpsons Hit & Run are exampes of this, there are probally more.
		// PAL Wind Waker also runs at 30fps instead of 25.
		if (field == FieldType::FIELD_PROGRESSIVE || GetXFBAddressBottom() != (GetXFBAddressTop() - 1280))
		{
			WARN_LOG(VIDEOINTERFACE, "PAL game is trying to use incorrect (NTSC) field ordering");
			// Lets kindly fix this for them.
			xfbAddr = GetXFBAddressTop();

			// TODO: PAL Simpsons Hit & Run now has a green line at the bottom when Real XFB is used.
			// Might be a bug later on in our code, or a bug in the actual game.
		}
		else
		{
			xfbAddr = GetXFBAddressBottom();
		}
	} else {
		xfbAddr = GetXFBAddressTop();
	}

	static const char* const fieldTypeNames[] = { "Progressive", "Upper", "Lower" };

	DEBUG_LOG(VIDEOINTERFACE,
			  "(VI->BeginField): Address: %.08X | FieldSteps %u | FbSteps %u | ACV %u | Field %s",
			  xfbAddr, m_HorizontalStepping.FieldSteps,m_HorizontalStepping.FbSteps,
			  m_VerticalTimingRegister.ACV, fieldTypeNames[field]);

	if (xfbAddr)
		g_video_backend->Video_BeginField(xfbAddr, fbWidth, fbHeight);
}

static void EndField()
{
	g_video_backend->Video_EndField();
	Core::VideoThrottle();
}

// Purpose: Send VI interrupt when triggered
// Run when: When a frame is scanned (progressive/interlace)
void Update()
{
	if (m_DisplayControlRegister.NIN)
	{
		// Progressive
		if (m_VBeamPos == 1)
			BeginField(FIELD_PROGRESSIVE);
	}
	else if (m_VBeamPos == s_upperFieldBegin)
	{
		// Interlace Upper
		BeginField(FIELD_UPPER);
	}
	else if (m_VBeamPos == s_lowerFieldBegin)
	{
		// Interlace Lower
		BeginField(FIELD_LOWER);
	}

	if (m_VBeamPos == s_upperFieldBegin + m_VerticalTimingRegister.ACV)
	{
		// Interlace Upper.
		EndField();
	}
	else if (m_VBeamPos == s_lowerFieldBegin + m_VerticalTimingRegister.ACV)
	{
		// Interlace Lower
		EndField();
	}

	if (++m_VBeamPos > s_lineCount * fields)
		m_VBeamPos = 1;

	for (UVIInterruptRegister& reg : m_InterruptRegister)
	{
		if (m_VBeamPos == reg.VCT)
		{
			reg.IR_INT = 1;
		}
	}
	UpdateInterrupts();
}

} // namespace
