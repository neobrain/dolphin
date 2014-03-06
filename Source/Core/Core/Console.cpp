// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include <cstring>
#include <iostream>
#include <PowerPCDisasm.h> // Bochs

#include "Common/Common.h"
#include "Common/Thread.h"
#include "Core/Console.h"
#include "Core/Core.h"
#include "Core/CoreTiming.h"
#include "Core/HW/Memmap.h"
#include "Core/PowerPC/PPCAnalyst.h"
#include "Core/PowerPC/PPCSymbolDB.h"
#include "Core/PowerPC/PPCTables.h"
#include "Core/PowerPC/JitCommon/JitBase.h"

#define CASE1(x) if (!strcmp(cmd, (x)))
#define CASE(x) else if (!strcmp(cmd, (x)))

void Console_Submit(const char *cmd)
{
	CASE1("r")
	{
		Core::StartTrace(false);
		INFO_LOG(CONSOLE, "Read tracing started.");
	}
	CASE("w")
	{
		Core::StartTrace(true);
		INFO_LOG(CONSOLE, "Write tracing started.");
	}
	CASE("trans")
	{
		TCHAR temp[256];
		u32 addr;
		sscanf(cmd, "%s %08x", temp, &addr);

		if (addr)
		{
#if MAX_LOGLEVEL >= INFO_LEVEL
			u32 EA =
				Memory::TranslateAddress(addr, Memory::FLAG_NO_EXCEPTION);
			INFO_LOG(CONSOLE, "EA 0x%08x to 0x%08x", addr, EA);
#endif
		}
		else
		{
			DEBUG_LOG(CONSOLE, "Syntax: trans ADDR");
		}
	}
	CASE("call")
	{
		TCHAR temp[256];
		u32 addr;
		sscanf(cmd, "%s %08x", temp, &addr);
		if (addr)
		{
			g_symbolDB.PrintCalls(addr);
		}
		else
		{
			DEBUG_LOG(CONSOLE, "Syntax: call ADDR");
		}
	}
	CASE("llac")
	{
		TCHAR temp[256];
		u32 addr;
		sscanf(cmd, "%s %08x", temp, &addr);
		if (addr)
		{
			g_symbolDB.PrintCallers(addr);
		}
		else
		{
			DEBUG_LOG(CONSOLE, "Syntax: llac ADDR");
		}
	}
	CASE("pend")
	{
		CoreTiming::LogPendingEvents();
	}
	CASE("dump")
	{
		char temp[256];
		char filename[256];
		u32 start;
		u32 end;
		sscanf(cmd, "%s %08x %08x %s", temp, &start, &end, filename);

		File::IOFile f(filename, "wb");
		for (u32 i = start; i < end; i++)
		{
			u8 b = Memory::ReadUnchecked_U8(i);
			fputc(b, f.GetHandle());
		}
		INFO_LOG(CONSOLE, "Dumped from %08x to %08x to %s",start,end,filename);
	}
	CASE("disa")
	{
		u32 start;
		u32 end;
		TCHAR temp[256];
		sscanf(cmd, "%s %08x %08x", temp, &start, &end);
		char disasm[256];
		for (u32 addr = start; addr <= end; addr += 4)
		{
			u32 data = Memory::ReadUnchecked_U32(addr);
			DisassembleGekko(data, addr, disasm, 256);
			DEBUG_LOG(CONSOLE, "%08x: %08x: %s\n", addr, data, disasm);
		}
	}
	CASE("help")
	{
		ERROR_LOG(CONSOLE, "Dolphin Console Command List");
		ERROR_LOG(CONSOLE, "scan ADDR - will find functions that are called by this function");
		ERROR_LOG(CONSOLE, "call ADDR - will find functions that call this function");
		ERROR_LOG(CONSOLE, "dump START_A END_A FILENAME - will dump memory between START_A and END_A");
		ERROR_LOG(CONSOLE, "help - guess what this does :P");
		ERROR_LOG(CONSOLE, "lisd - list signature database");
		ERROR_LOG(CONSOLE, "lisf - list functions");
		ERROR_LOG(CONSOLE, "trans ADDR - translate address");
	}
	CASE("lisd")
	{
		// PPCAnalyst::ListDB();
	}
	CASE("ipro")
	{
		PPCTables::PrintInstructionRunCounts();
	}
	CASE("lisf")
	{
		g_symbolDB.List();
	}
	else
	{
		ERROR_LOG(CONSOLE, "Invalid command");
	}
}

#undef CASE1
#undef CASE
