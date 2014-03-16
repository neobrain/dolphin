// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#include "Common/Common.h"
#include "Common/FileUtil.h"
#include "Common/StringUtil.h"

#include "VideoBackends/Software/BPMemLoader.h"
#include "VideoBackends/Software/DebugUtil.h"
#include "VideoBackends/Software/EfbInterface.h"
#include "VideoBackends/Software/HwRasterizer.h"
#include "VideoBackends/Software/SWCommandProcessor.h"
#include "VideoBackends/Software/SWStatistics.h"
#include "VideoBackends/Software/SWVideoConfig.h"
#include "VideoBackends/Software/TextureSampler.h"

#include "VideoCommon/ImageWrite.h"


namespace DebugUtil
{

u32 skipFrames = 0;
bool drawingHwTriangles = false;

enum { NumObjectBuffers = 40};

u32 ObjectBuffer[NumObjectBuffers][EFB_WIDTH*EFB_HEIGHT];
u32 TempBuffer[NumObjectBuffers];

bool DrawnToBuffer[NumObjectBuffers];
const char* ObjectBufferName[NumObjectBuffers];
int BufferBase[NumObjectBuffers];

void Init()
{
	for (int i = 0; i < NumObjectBuffers; i++)
	{
		memset(ObjectBuffer[i], 0, sizeof(ObjectBuffer[i]));
		DrawnToBuffer[i] = false;
		ObjectBufferName[i] = nullptr;
		BufferBase[i] = 0;
	}
}

void SaveTexture(const std::string& filename, u32 texmap, s32 mip)
{
	FourTexUnits& texUnit = bpmem.tex[(texmap >> 2) & 1];
	u8 subTexmap = texmap & 3;

	TexImage0& ti0 = texUnit.texImage0[subTexmap];

	u32 width = ti0.width + 1;
	u32 height = ti0.height + 1;

	u8 *data = new u8[width * height * 4];

	GetTextureRGBA(data, texmap, mip, width, height);

	(void)TextureToPng(data, width*4, filename, width, height, true);
	delete[] data;

}

void GetTextureRGBA(u8 *dst, u32 texmap, s32 mip, u32 width, u32 height)
{
	for (u32 y = 0; y < height; y++)
	{
		for (u32 x = 0; x < width; x++)
		{
			TextureSampler::SampleMip(x << 7, y << 7, mip, false, texmap, dst);
			dst += 4;
		}
	}
}

s32 GetMaxTextureLod(u32 texmap)
{
	FourTexUnits& texUnit = bpmem.tex[(texmap >> 2) & 1];
	u8 subTexmap = texmap & 3;

	u8 maxLod = texUnit.texMode1[subTexmap].max_lod;
	u8 mip = maxLod >> 4;
	u8 fract = maxLod & 0xf;

	if (fract)
		++mip;

	return (s32)mip;
}

void DumpActiveTextures()
{
	for (unsigned int stageNum = 0; stageNum < bpmem.genMode.numindstages; stageNum++)
	{
		u32 texmap = bpmem.tevindref.getTexMap(stageNum);

		s32 maxLod = GetMaxTextureLod(texmap);
		for (s32 mip = 0; mip <= maxLod; ++mip)
		{
			SaveTexture(StringFromFormat("%star%i_ind%i_map%i_mip%i.png",
						File::GetUserPath(D_DUMPTEXTURES_IDX).c_str(),
						swstats.thisFrame.numDrawnObjects, stageNum, texmap, mip), texmap, mip);
		}
	}

	for (unsigned int stageNum = 0; stageNum <= bpmem.genMode.numtevstages; stageNum++)
	{
		int stageNum2 = stageNum >> 1;
		int stageOdd = stageNum&1;
		TwoTevStageOrders &order = bpmem.tevorders[stageNum2];

		int texmap = order.getTexMap(stageOdd);

		s32 maxLod = GetMaxTextureLod(texmap);
		for (s32 mip = 0; mip <= maxLod; ++mip)
		{
			SaveTexture(StringFromFormat("%star%i_stage%i_map%i_mip%i.png",
						File::GetUserPath(D_DUMPTEXTURES_IDX).c_str(),
						swstats.thisFrame.numDrawnObjects, stageNum, texmap, mip), texmap, mip);
		}
	}
}

void DumpEfb(const std::string& filename)
{
	u8 *data = new u8[EFB_WIDTH * EFB_HEIGHT * 4];
	u8 *writePtr = data;
	u8 sample[4];

	for (int y = 0; y < EFB_HEIGHT; y++)
	{
		for (int x = 0; x < EFB_WIDTH; x++)
		{
			EfbInterface::GetColor(x, y, sample);
			// ABGR to RGBA
			*(writePtr++) = sample[3];
			*(writePtr++) = sample[2];
			*(writePtr++) = sample[1];
			*(writePtr++) = sample[0];
		}
	}

	(void)TextureToPng(data, EFB_WIDTH * 4, filename, EFB_WIDTH, EFB_HEIGHT, true);
	delete[] data;
}

void DumpDepth(const std::string& filename)
{
	u8 *data = new u8[EFB_WIDTH * EFB_HEIGHT * 4];
	u8 *writePtr = data;

	for (int y = 0; y < EFB_HEIGHT; y++)
	{
		for (int x = 0; x < EFB_WIDTH; x++)
		{
			u32 depth = EfbInterface::GetDepth(x, y);
			// depth to rgba
			*(writePtr++) = depth & 0xff; 
			*(writePtr++) = (depth >> 8) & 0xff;
			*(writePtr++) = (depth >> 16) & 0xff;
			*(writePtr++) = 255;
		}
	}

	(void)TextureToPng(data, EFB_WIDTH * 4, filename, EFB_WIDTH, EFB_HEIGHT, true);
	delete[] data;
}

void DrawObjectBuffer(s16 x, s16 y, u8 *color, int bufferBase, int subBuffer, const char *name)
{
	int buffer = bufferBase + subBuffer;

	u32 offset = (x + y * EFB_WIDTH) * 4;
	u8 *dst = (u8*)&ObjectBuffer[buffer][offset];
	*(dst++) = color[2];
	*(dst++) = color[1];
	*(dst++) = color[0];
	*(dst++) = color[3];

	DrawnToBuffer[buffer] = true;
	ObjectBufferName[buffer] = name;
	BufferBase[buffer] = bufferBase;
}

void DrawTempBuffer(u8 *color, int buffer)
{
	u8 *dst = (u8*)&TempBuffer[buffer];
	*(dst++) = color[2];
	*(dst++) = color[1];
	*(dst++) = color[0];
	*(dst++) = color[3];
}

void CopyTempBuffer(s16 x, s16 y, int bufferBase, int subBuffer, const char *name)
{
	int buffer = bufferBase + subBuffer;

	u32 offset = (x + y * EFB_WIDTH);
	ObjectBuffer[buffer][offset] = TempBuffer[buffer];

	DrawnToBuffer[buffer] = true;
	ObjectBufferName[buffer] = name;
	BufferBase[buffer] = bufferBase;
}

void OnObjectBegin()
{
	if (!g_bSkipCurrentFrame)
	{
		if (g_SWVideoConfig.bDumpTextures && swstats.thisFrame.numDrawnObjects >= g_SWVideoConfig.drawStart && swstats.thisFrame.numDrawnObjects < g_SWVideoConfig.drawEnd)
			DumpActiveTextures();

		if (g_SWVideoConfig.bHwRasterizer)
		{
			HwRasterizer::BeginTriangles();
			drawingHwTriangles = true;
		}
	}
}

void OnObjectEnd()
{
	if (!g_bSkipCurrentFrame)
	{
		if (g_SWVideoConfig.bDumpObjects && swstats.thisFrame.numDrawnObjects >= g_SWVideoConfig.drawStart && swstats.thisFrame.numDrawnObjects < g_SWVideoConfig.drawEnd)
			DumpEfb(StringFromFormat("%sobject%i.png",
						File::GetUserPath(D_DUMPFRAMES_IDX).c_str(),
						swstats.thisFrame.numDrawnObjects));

		if (g_SWVideoConfig.bHwRasterizer || drawingHwTriangles)
		{
			HwRasterizer::EndTriangles();
			drawingHwTriangles = false;
		}

		for (int i = 0; i < NumObjectBuffers; i++)
		{
			if (DrawnToBuffer[i])
			{
				DrawnToBuffer[i] = false;
				std::string filename = StringFromFormat("%sobject%i_%s(%i).png",
					File::GetUserPath(D_DUMPFRAMES_IDX).c_str(),
					swstats.thisFrame.numDrawnObjects, ObjectBufferName[i], i - BufferBase[i]);

				(void)TextureToPng((u8*)ObjectBuffer[i], EFB_WIDTH * 4, filename, EFB_WIDTH, EFB_HEIGHT, true);
				memset(ObjectBuffer[i], 0, sizeof(ObjectBuffer[i]));

			}
		}

		swstats.thisFrame.numDrawnObjects++;
	}
}

void OnFrameEnd()
{
	if (!g_bSkipCurrentFrame)
	{
		if (g_SWVideoConfig.bDumpFrames)
		{
			DumpEfb(StringFromFormat("%sframe%i_color.png",
						File::GetUserPath(D_DUMPFRAMES_IDX).c_str(), swstats.frameCount));
			DumpDepth(StringFromFormat("%sframe%i_depth.png",
						File::GetUserPath(D_DUMPFRAMES_IDX).c_str(), swstats.frameCount));
		}
	}
}

}
