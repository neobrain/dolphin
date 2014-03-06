// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "Common/Common.h"

bool SaveData(const char* filename, const char* pdata);
bool TextureToPng(u8* data, int row_stride, const std::string& filename, int width, int height, bool saveAlpha = true);
