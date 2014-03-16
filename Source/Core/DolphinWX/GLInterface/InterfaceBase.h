// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <string>

#include "Common/Common.h"

enum GLInterfaceMode {
	MODE_DETECT = 0,
	MODE_OPENGL,
	MODE_OPENGLES2,
	MODE_OPENGLES3,
};

class cInterfaceBase
{
protected:
	// Window dimensions.
	u32 s_backbuffer_width;
	u32 s_backbuffer_height;

	u32 s_opengl_mode;
public:
	virtual void Swap() {}
	virtual void UpdateFPSDisplay(const std::string& text) {}
	virtual void SetMode(u32 mode) { s_opengl_mode = GLInterfaceMode::MODE_OPENGL; }
	virtual u32 GetMode() { return s_opengl_mode; }
	virtual void* GetFuncAddress(std::string name) { return nullptr; }
	virtual bool Create(void *&window_handle) { return true; }
	virtual bool MakeCurrent() { return true; }
	virtual bool ClearCurrent() { return true; }
	virtual void Shutdown() {}

	virtual void SwapInterval(int Interval) { }
	virtual u32 GetBackBufferWidth() { return s_backbuffer_width; }
	virtual u32 GetBackBufferHeight() { return s_backbuffer_height; }
	virtual void SetBackBufferDimensions(u32 W, u32 H) {s_backbuffer_width = W; s_backbuffer_height = H; }
	virtual void Update() { }
	virtual bool PeekMessages() { return false; }
};
