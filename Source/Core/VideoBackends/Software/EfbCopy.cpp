// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include "Core/Core.h"
#include "Core/HW/Memmap.h"
#include "VideoBackends/Software/BPMemLoader.h"
#include "VideoBackends/Software/DebugUtil.h"
#include "VideoBackends/Software/EfbCopy.h"
#include "VideoBackends/Software/EfbInterface.h"
#include "VideoBackends/Software/HwRasterizer.h"
#include "VideoBackends/Software/SWCommandProcessor.h"
#include "VideoBackends/Software/SWRenderer.h"
#include "VideoBackends/Software/SWStatistics.h"
#include "VideoBackends/Software/SWVideoConfig.h"
#include "VideoBackends/Software/TextureEncoder.h"

static const float s_gammaLUT[] =
{
	1.0f,
	1.7f,
	2.2f,
	1.0f
};

namespace EfbCopy
{
	void CopyToXfb(u32 xfbAddr, u32 fbWidth, u32 fbHeight, const EFBRectangle& sourceRc, float Gamma)
	{
		GLInterface->Update(); // update the render window position and the backbuffer size

		if (!g_SWVideoConfig.bHwRasterizer)
		{
			INFO_LOG(VIDEO, "xfbaddr: %x, fbwidth: %i, fbheight: %i, source: (%i, %i, %i, %i), Gamma %f",
					 xfbAddr, fbWidth, fbHeight, sourceRc.top, sourceRc.left, sourceRc.bottom, sourceRc.right, Gamma);

			if (!g_SWVideoConfig.bBypassXFB)
			{
				EfbInterface::yuv422_packed* xfb_in_ram = (EfbInterface::yuv422_packed *) Memory::GetPointer(xfbAddr);

				EfbInterface::CopyToXFB(xfb_in_ram, fbWidth, fbHeight, sourceRc, Gamma);
			}
			else
			{
				// Ask SWRenderer for the next color texture
				u8 *colorTexture = SWRenderer::getColorTexture();

				EfbInterface::BypassXFB(colorTexture, fbWidth, fbHeight, sourceRc, Gamma);

				// Tell SWRenderer we are now finished with it.
				SWRenderer::swapColorTexture();

				// FifoPlayer is broken and never calls BeginFrame/EndFrame.
				// Hence, we manually force a swap now. This emulates the behavior
				// of hardware backends with XFB emulation disabled.
				// TODO: Fix FifoPlayer by making proper use of VideoInterface!
				//       This requires careful synchronization since GPU commands
				//       are processed on a different thread than VI commands.
				SWRenderer::Swap(fbWidth, fbHeight);
			}
		}
	}

	void CopyToRam()
	{
		if (!g_SWVideoConfig.bHwRasterizer)
		{
			u8 *dest_ptr = Memory::GetPointer(bpmem.copyTexDest << 5);

			TextureEncoder::Encode(dest_ptr);
		}
	}

	void ClearEfb()
	{
		u32 clearColor = (bpmem.clearcolorAR & 0xff) << 24 | bpmem.clearcolorGB << 8 | (bpmem.clearcolorAR & 0xff00) >> 8;

		int left   = bpmem.copyTexSrcXY.x;
		int top    = bpmem.copyTexSrcXY.y;
		int right  = left + bpmem.copyTexSrcWH.x;
		int bottom = top + bpmem.copyTexSrcWH.y;

		for (u16 y = top; y <= bottom; y++)
		{
			for (u16 x = left; x <= right; x++)
			{
				EfbInterface::SetColor(x, y, (u8*)(&clearColor));
				EfbInterface::SetDepth(x, y, bpmem.clearZValue);
			}
		}
	}

	void CopyEfb()
	{
		EFBRectangle rc;
		rc.left = (int)bpmem.copyTexSrcXY.x;
		rc.top = (int)bpmem.copyTexSrcXY.y;

		// flipper represents the widths internally as last pixel minus starting pixel, so
		// these are zero indexed.
		rc.right = rc.left + (int)bpmem.copyTexSrcWH.x + 1;
		rc.bottom = rc.top + (int)bpmem.copyTexSrcWH.y + 1;

		if (!g_bSkipCurrentFrame)
		{
			if (bpmem.triggerEFBCopy.copy_to_xfb)
			{
				float yScale;
				if (bpmem.triggerEFBCopy.scale_invert)
					yScale = 256.0f / (float)bpmem.dispcopyyscale;
				else
					yScale = (float)bpmem.dispcopyyscale / 256.0f;

				float xfbLines = ((bpmem.copyTexSrcWH.y + 1.0f) * yScale);

				if (yScale != 1.0)
					WARN_LOG(VIDEO, "yScale of %f is currently unsupported", yScale);

				if ((u32)xfbLines > MAX_XFB_HEIGHT)
				{
					INFO_LOG(VIDEO, "Tried to scale EFB to too many XFB lines (%f)", xfbLines);
					xfbLines = MAX_XFB_HEIGHT;
				}

				CopyToXfb(bpmem.copyTexDest << 5,
						  bpmem.copyMipMapStrideChannels << 4,
						  (u32)xfbLines,
						  rc,
						  s_gammaLUT[bpmem.triggerEFBCopy.gamma]);
			}
			else
			{
				CopyToRam(); // FIXME: should use the rectangle we have already created above
			}

			if (bpmem.triggerEFBCopy.clear)
			{
				if (g_SWVideoConfig.bHwRasterizer)
					HwRasterizer::Clear();
				else
					ClearEfb();
			}
		}
	}
}
