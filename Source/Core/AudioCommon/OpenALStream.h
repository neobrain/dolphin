// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "AudioCommon/SoundStream.h"
#include "Common/Thread.h"
#include "Core/Core.h"
#include "Core/HW/AudioInterface.h"
#include "Core/HW/SystemTimers.h"

#if defined HAVE_OPENAL && HAVE_OPENAL
#ifdef _WIN32
#include <OpenAL/include/al.h>
#include <OpenAL/include/alc.h>
#include <OpenAL/include/alext.h>
#elif defined __APPLE__
#include <OpenAL/al.h>
#include <OpenAL/alc.h>
#else
#include <AL/al.h>
#include <AL/alc.h>
#include <AL/alext.h>
#endif

#include <soundtouch/SoundTouch.h>
#include <soundtouch/STTypes.h>

// 16 bit Stereo
#define SFX_MAX_SOURCE          1
#define OAL_MAX_BUFFERS         32
#define OAL_MAX_SAMPLES         256
#define STEREO_CHANNELS         2
#define SURROUND_CHANNELS       6   // number of channels in surround mode
#define SIZE_SHORT              2
#define SIZE_FLOAT              4   // size of a float in bytes
#define FRAME_STEREO_SHORT      STEREO_CHANNELS * SIZE_SHORT
#define FRAME_STEREO_FLOAT      STEREO_CHANNELS * SIZE_FLOAT
#define FRAME_SURROUND_FLOAT    SURROUND_CHANNELS * SIZE_FLOAT
#endif

class OpenALStream: public SoundStream
{
#if defined HAVE_OPENAL && HAVE_OPENAL
public:
	OpenALStream(CMixer *mixer, void *hWnd = nullptr)
		: SoundStream(mixer)
		, uiSource(0)
	{}

	virtual ~OpenALStream() {}

	virtual bool Start() override;
	virtual void SoundLoop() override;
	virtual void SetVolume(int volume) override;
	virtual void Stop() override;
	virtual void Clear(bool mute) override;
	static bool isValid() { return true; }
	virtual bool usesMixer() const { return true; }
	virtual void Update() override;

private:
	std::thread thread;
	Common::Event soundSyncEvent;

	short realtimeBuffer[OAL_MAX_SAMPLES * STEREO_CHANNELS];
	soundtouch::SAMPLETYPE sampleBuffer[OAL_MAX_SAMPLES * SURROUND_CHANNELS * OAL_MAX_BUFFERS];
	ALuint uiBuffers[OAL_MAX_BUFFERS];
	ALuint uiSource;
	ALfloat fVolume;

	u8 numBuffers;
#else
public:
	OpenALStream(CMixer *mixer)
		: SoundStream(mixer)
	{}
#endif // HAVE_OPENAL
};
