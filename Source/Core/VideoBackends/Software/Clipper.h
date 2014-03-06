// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "Common/ChunkFile.h"
#include "Common/Common.h"
#include "VideoBackends/Software/NativeVertexFormat.h"

namespace Clipper
{
	void Init();

	void SetViewOffset();

	void ProcessTriangle(OutputVertexData *v0, OutputVertexData *v1, OutputVertexData *v2);

	void ProcessLine(OutputVertexData *v0, OutputVertexData *v1);

	bool CullTest(OutputVertexData *v0, OutputVertexData *v1, OutputVertexData *v2, bool &backface);

	void PerspectiveDivide(OutputVertexData *vertex);

	void DoState(PointerWrap &p);
}
