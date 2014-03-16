// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "AudioCommon/AudioCommon.h"
#include "Common/Common.h"

// TODO: Get rid of this file.

#define PROFILE   0

#if PROFILE
	void ProfilerDump(u64 _count);
	void ProfilerInit();
	void ProfilerAddDelta(int _addr, int _delta);
	void ProfilerStart();
#endif
