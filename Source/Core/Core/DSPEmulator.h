// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "AudioCommon/SoundStream.h"
#include "Common/ChunkFile.h"

class DSPEmulator
{
public:
	virtual ~DSPEmulator() {}

	virtual bool IsLLE() = 0;

	virtual bool Initialize(void *hWnd, bool bWii, bool bDSPThread) = 0;
	virtual void Shutdown() = 0;

	virtual void DoState(PointerWrap &p) = 0;
	virtual void PauseAndLock(bool doLock, bool unpauseOnUnlock=true) = 0;

	virtual void DSP_WriteMailBoxHigh(bool _CPUMailbox, unsigned short) = 0;
	virtual void DSP_WriteMailBoxLow(bool _CPUMailbox, unsigned short) = 0;
	virtual unsigned short DSP_ReadMailBoxHigh(bool _CPUMailbox) = 0;
	virtual unsigned short DSP_ReadMailBoxLow(bool _CPUMailbox) = 0;
	virtual unsigned short DSP_ReadControlRegister() = 0;
	virtual unsigned short DSP_WriteControlRegister(unsigned short) = 0;
	virtual void DSP_SendAIBuffer(unsigned int address, unsigned int num_samples) = 0;
	virtual void DSP_Update(int cycles) = 0;
	virtual void DSP_StopSoundStream() = 0;
	virtual void DSP_ClearAudioBuffer(bool mute) = 0;
	virtual u32 DSP_UpdateRate() = 0;

protected:
	SoundStream *soundStream;
	void *m_hWnd;
};

DSPEmulator *CreateDSPEmulator(bool HLE);
