// Copyright 2014 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "Common/ArmEmitter.h"
#include "Core/PowerPC/JitCommon/JitAsmCommon.h"

using namespace ArmGen;
class JitArmILAsmRoutineManager : public CommonAsmRoutinesBase, public ARMXCodeBlock
{
private:
	void Generate();
	void GenerateCommon() {}

public:
	void Init() {
		AllocCodeSpace(8192);
		Generate();
		WriteProtect();
	}

	void Shutdown() {
		FreeCodeSpace();
	}
};

extern JitArmILAsmRoutineManager armil_asm_routines;
