// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <cstdlib>
#include "AudioCommon/SoundStream.h"

#define BUF_SIZE (48000 * 4 / 32)

class NullSound : public SoundStream
{
	// playback position
	short realtimeBuffer[BUF_SIZE / sizeof(short)];

public:
	NullSound(CMixer *mixer)
		: SoundStream(mixer)
	{}

	virtual ~NullSound() {}

	virtual bool Start();
	virtual void SoundLoop();
	virtual void SetVolume(int volume);
	virtual void Stop();
	virtual void Clear(bool mute);
	static bool isValid() { return true; }
	virtual bool usesMixer() const { return true; }
	virtual void Update();
};
