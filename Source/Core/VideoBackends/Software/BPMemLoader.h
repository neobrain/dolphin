// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once


#include "Common/Common.h"
#include "VideoCommon/BPMemory.h"

void InitBPMemory();
void SWBPWritten(int address, int newvalue);
void SWLoadBPReg(u32 value);
