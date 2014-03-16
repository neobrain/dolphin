// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "AudioCommon/SoundStream.h"
#include "Common/Thread.h"

class OpenSLESStream : public SoundStream
{
#ifdef ANDROID
public:
	OpenSLESStream(CMixer *mixer, void *hWnd = nullptr)
		: SoundStream(mixer)
	{};

	virtual ~OpenSLESStream() {};

	virtual bool Start();
	virtual void Stop();
	static bool isValid() { return true; }
	virtual bool usesMixer() const { return true; }

private:
	std::thread thread;
	Common::Event soundSyncEvent;
#else
public:
	OpenSLESStream(CMixer *mixer, void *hWnd = nullptr): SoundStream(mixer) {}
#endif // HAVE_OPENSL
};
