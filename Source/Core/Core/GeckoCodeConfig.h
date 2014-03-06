// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "Common/IniFile.h"

#include "Core/GeckoCode.h"

namespace Gecko
{

void LoadCodes(const IniFile& globalIni, const IniFile& localIni, std::vector<GeckoCode>& gcodes);
void SaveCodes(IniFile& inifile, const std::vector<GeckoCode>& gcodes);

};
