// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

// This audio backend uses XAudio2 via XAudio2_7.dll
// This version of the library is included in the June 2010 DirectX SDK and
// works on all versions of Windows, however the SDK and/or redist must be
// seperately installed.
// Therefore this backend is available iff:
//  * SDK is available at compile-time
//  * runtime dll is available at runtime

#pragma once

#include <memory>
#include "AudioCommon/SoundStream.h"
#include "Common/Thread.h"

#ifdef _WIN32

#include <Windows.h>

struct StreamingVoiceContext2_7;
struct IXAudio2;
struct IXAudio2MasteringVoice;

#endif

class XAudio2_7 : public SoundStream
{
#ifdef _WIN32

private:
	static void ReleaseIXAudio2(IXAudio2 *ptr);

	class Releaser
	{
	public:
		template <typename R>
		void operator()(R *ptr)
		{
			ReleaseIXAudio2(ptr);
		}
	};

	std::unique_ptr<IXAudio2, Releaser> m_xaudio2;
	std::unique_ptr<StreamingVoiceContext2_7> m_voice_context;
	IXAudio2MasteringVoice *m_mastering_voice;

	Common::Event m_sound_sync_event;
	float m_volume;

	const bool m_cleanup_com;

	static HMODULE m_xaudio2_dll;

	static bool InitLibrary();

public:
	XAudio2_7(CMixer *mixer);
	virtual ~XAudio2_7();
 
	virtual bool Start();
	virtual void Stop();

	virtual void Update();
	virtual void Clear(bool mute);
	virtual void SetVolume(int volume);
	virtual bool usesMixer() const;

	static bool isValid() { return InitLibrary(); }

#else

public:
	XAudio2_7(CMixer *mixer)
		: SoundStream(mixer)
	{}

#endif
};
