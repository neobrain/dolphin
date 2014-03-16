// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include <algorithm>
#include <cinttypes>
#include <vector>

#include "Common/Common.h"
#include "Common/FileUtil.h"
#include "Common/StringUtil.h"

#include "Core/PowerPC/JitInterface.h"
#include "Core/PowerPC/PPCTables.h"
#include "Core/PowerPC/Interpreter/Interpreter.h"
#include "Core/PowerPC/Interpreter/Interpreter_Tables.h"

GekkoOPInfo *m_infoTable[64];
GekkoOPInfo *m_infoTable4[1024];
GekkoOPInfo *m_infoTable19[1024];
GekkoOPInfo *m_infoTable31[1024];
GekkoOPInfo *m_infoTable59[32];
GekkoOPInfo *m_infoTable63[1024];

GekkoOPInfo *m_allInstructions[512];
int m_numInstructions;

GekkoOPInfo *GetOpInfo(UGeckoInstruction _inst)
{
	GekkoOPInfo *info = m_infoTable[_inst.OPCD];
	if ((info->type & 0xFFFFFF) == OPTYPE_SUBTABLE)
	{
		int table = info->type>>24;
		switch (table)
		{
		case 4:  return m_infoTable4[_inst.SUBOP10];
		case 19: return m_infoTable19[_inst.SUBOP10];
		case 31: return m_infoTable31[_inst.SUBOP10];
		case 59: return m_infoTable59[_inst.SUBOP5];
		case 63: return m_infoTable63[_inst.SUBOP10];
		default:
			_assert_msg_(POWERPC,0,"GetOpInfo - invalid subtable op %08x @ %08x", _inst.hex, PC);
			return nullptr;
		}
	}
	else
	{
		if ((info->type & 0xFFFFFF) == OPTYPE_INVALID)
		{
			_assert_msg_(POWERPC,0,"GetOpInfo - invalid op %08x @ %08x", _inst.hex, PC);
			return nullptr;
		}
		return m_infoTable[_inst.OPCD];
	}
}

Interpreter::_interpreterInstruction GetInterpreterOp(UGeckoInstruction _inst)
{
	const GekkoOPInfo *info = m_infoTable[_inst.OPCD];
	if ((info->type & 0xFFFFFF) == OPTYPE_SUBTABLE)
	{
		int table = info->type>>24;
		switch (table)
		{
		case 4:  return Interpreter::m_opTable4[_inst.SUBOP10];
		case 19: return Interpreter::m_opTable19[_inst.SUBOP10];
		case 31: return Interpreter::m_opTable31[_inst.SUBOP10];
		case 59: return Interpreter::m_opTable59[_inst.SUBOP5];
		case 63: return Interpreter::m_opTable63[_inst.SUBOP10];
		default:
			_assert_msg_(POWERPC,0,"GetInterpreterOp - invalid subtable op %08x @ %08x", _inst.hex, PC);
			return nullptr;
		}
	}
	else
	{
		if ((info->type & 0xFFFFFF) == OPTYPE_INVALID)
		{
			_assert_msg_(POWERPC,0,"GetInterpreterOp - invalid op %08x @ %08x", _inst.hex, PC);
			return nullptr;
		}
		return Interpreter::m_opTable[_inst.OPCD];
	}
}
namespace PPCTables
{

bool UsesFPU(UGeckoInstruction _inst)
{
	switch (_inst.OPCD)
	{
	case 04: // PS
		return _inst.SUBOP10 != 1014;

	case 48: // lfs
	case 49: // lfsu
	case 50: // lfd
	case 51: // lfdu
	case 52: // stfs
	case 53: // stfsu
	case 54: // stfd
	case 55: // stfdu
	case 56: // psq_l
	case 57: // psq_lu

	case 59: // FPU-sgl
	case 60: // psq_st
	case 61: // psq_stu
	case 63: // FPU-dbl
		return true;

	case 31:
		switch (_inst.SUBOP10)
		{
		case 535:
		case 567:
		case 599:
		case 631:
		case 663:
		case 695:
		case 727:
		case 759:
		case 983:
			return true;
		default:
			return false;
		}
	default:
		return false;
	}
}

void InitTables(int cpu_core)
{
	// Interpreter ALWAYS needs to be initialized
	InterpreterTables::InitTables();
	switch (cpu_core)
	{
	case 0:
		{
			// Interpreter
			break;
		}
	default:
		{
			JitInterface::InitTables(cpu_core);
			break;
		}
	}
}

#define OPLOG
#define OP_TO_LOG "mtfsb0x"

#ifdef OPLOG
namespace {
	std::vector<u32> rsplocations;
}
#endif

const char *GetInstructionName(UGeckoInstruction _inst)
{
	const GekkoOPInfo *info = GetOpInfo(_inst);
	return info ? info->opname : nullptr;
}

bool IsValidInstruction(UGeckoInstruction _inst)
{
	const GekkoOPInfo *info = GetOpInfo(_inst);
	return info != nullptr;
}

void CountInstruction(UGeckoInstruction _inst)
{
	GekkoOPInfo *info = GetOpInfo(_inst);
	if (info)
	{
		info->runCount++;
	}
}

void PrintInstructionRunCounts()
{
	typedef std::pair<const char*, u64> OpInfo;
	std::vector<OpInfo> temp;
	temp.reserve(m_numInstructions);
	for (int i = 0; i < m_numInstructions; ++i)
	{
		GekkoOPInfo *pInst = m_allInstructions[i];
		temp.emplace_back(pInst->opname, pInst->runCount);
	}
	std::sort(temp.begin(), temp.end(), 
		[](const OpInfo &a, const OpInfo &b)
		{
			return a.second > b.second;
		});

	for (auto &inst : temp)
	{
		if (inst.second == 0)
			break;

		DEBUG_LOG(POWERPC, "%s : %" PRIu64, inst.first, inst.second);
		//PanicAlert("%s : %llu", inst.first, inst.second);
	}
}

void LogCompiledInstructions()
{
	static unsigned int time = 0;

	File::IOFile f(StringFromFormat("%sinst_log%i.txt", File::GetUserPath(D_LOGS_IDX).c_str(), time), "w");
	for (int i = 0; i < m_numInstructions; i++)
	{
		GekkoOPInfo *pInst = m_allInstructions[i];
		if (pInst->compileCount > 0)
		{
			fprintf(f.GetHandle(), "%s\t%i\t%" PRId64 "\t%08x\n", pInst->opname,
				pInst->compileCount, pInst->runCount, pInst->lastUse);
		}
	}

	f.Open(StringFromFormat("%sinst_not%i.txt", File::GetUserPath(D_LOGS_IDX).c_str(), time), "w");
	for (int i = 0; i < m_numInstructions; i++)
	{
		GekkoOPInfo *pInst = m_allInstructions[i];
		if (pInst->compileCount == 0)
		{
			fprintf(f.GetHandle(), "%s\t%i\t%" PRId64 "\n", pInst->opname,
				pInst->compileCount, pInst->runCount);
		}
	}

#ifdef OPLOG
	f.Open(StringFromFormat("%s" OP_TO_LOG "_at%i.txt", File::GetUserPath(D_LOGS_IDX).c_str(), time), "w");
	for (auto& rsplocation : rsplocations)
	{
		fprintf(f.GetHandle(), OP_TO_LOG ": %08x\n", rsplocation);
	}
#endif

	++time;
}

}  // namespace
