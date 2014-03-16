// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

// The user of the DSPCore library must supply a few functions so that the
// emulation core can access the environment it runs in. If the emulation
// core isn't used, for example in an asm/disasm tool, then most of these
// can be stubbed out.

namespace DSPHost
{
u8 ReadHostMemory(u32 addr);
void WriteHostMemory(u8 value, u32 addr);
void OSD_AddMessage(const std::string& str, u32 ms);
bool OnThread();
bool IsWiiHost();
void InterruptRequest();
void CodeLoaded(const u8 *ptr, int size);
void UpdateDebugger();
}
