// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#if defined(HAVE_ALSA) && HAVE_ALSA
#include <alsa/asoundlib.h>
#endif

#include "AudioCommon/SoundStream.h"
#include "Common/Common.h"
#include "Common/Thread.h"

class AlsaSound : public SoundStream
{
#if defined(HAVE_ALSA) && HAVE_ALSA
public:
	AlsaSound(CMixer *mixer);
	virtual ~AlsaSound();

	virtual bool Start() override;
	virtual void SoundLoop() override;
	virtual void Stop() override;

	static bool isValid() {
		return true;
	}
	virtual bool usesMixer() const {
		return true;
	}

	virtual void Update() override;

private:
	bool AlsaInit();
	void AlsaShutdown();

	u8 *mix_buffer;
	std::thread thread;
	// 0 = continue
	// 1 = shutdown
	// 2 = done shutting down.
	volatile int thread_data;

	snd_pcm_t *handle;
	int frames_to_deliver;
#else
public:
	AlsaSound(CMixer *mixer) : SoundStream(mixer) {}
#endif
};
