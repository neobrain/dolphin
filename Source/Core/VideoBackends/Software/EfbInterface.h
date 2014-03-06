// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "VideoCommon/VideoCommon.h"

namespace EfbInterface
{
	const int DEPTH_BUFFER_START = EFB_WIDTH * EFB_HEIGHT * 3;

	// xfb color format - packed so the compiler doesn't mess with alignment
#pragma pack(push,1)
	typedef struct {
		u8 Y;
		u8 UV;
	} yuv422_packed;
#pragma pack(pop)

	// But this struct is only used internally, so we could optimise alignment
	typedef struct {
		u8 Y;
		s8 U;
		s8 V;
	} yuv444;

	enum { ALP_C, BLU_C, GRN_C, RED_C };

	// color order is ABGR in order to emulate RGBA on little-endian hardware

	// does full blending of an incoming pixel
	void BlendTev(u16 x, u16 y, u8 *color);

	// compare z at location x,y
	// writes it if it passes
	// returns result of compare.
	bool ZCompare(u16 x, u16 y, u32 z);

	// sets the color and alpha
	void SetColor(u16 x, u16 y, u8 *color);
	void SetDepth(u16 x, u16 y, u32 depth);

	void GetColor(u16 x, u16 y, u8 *color);
	void GetColorYUV(u16 x, u16 y, yuv444 *color);
	u32 GetDepth(u16 x, u16 y);

	u8* GetPixelPointer(u16 x, u16 y, bool depth);

	void CopyToXFB(yuv422_packed* xfb_in_ram, u32 fbWidth, u32 fbHeight, const EFBRectangle& sourceRc, float Gamma);
	void BypassXFB(u8* texture, u32 fbWidth, u32 fbHeight, const EFBRectangle& sourceRc, float Gamma);

	void DoState(PointerWrap &p);
}
