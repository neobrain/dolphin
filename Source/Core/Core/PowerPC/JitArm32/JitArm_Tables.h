// Copyright 2014 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "Core/PowerPC/Gekko.h"
#include "Core/PowerPC/PPCTables.h"

namespace JitArmTables
{
	void CompileInstruction(PPCAnalyst::CodeOp & op);
	void InitTables();
}
