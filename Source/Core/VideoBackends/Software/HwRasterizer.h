// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <map>

#include "VideoBackends/OGL/GLUtil.h"
#include "VideoBackends/Software/BPMemLoader.h"

struct OutputVertexData;

namespace HwRasterizer
{
	void Init();
	void Shutdown();

	void Prepare();

	void BeginTriangles();
	void EndTriangles();

	void DrawTriangleFrontFace(OutputVertexData *v0, OutputVertexData *v1, OutputVertexData *v2);

	void Swap(const EFBRectangle& targetRc);

	void Clear();

	void CopyEfb();
}
