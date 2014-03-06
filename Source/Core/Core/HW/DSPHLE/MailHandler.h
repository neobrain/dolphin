// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <queue>

#include "Common/ChunkFile.h"
#include "Common/Common.h"

class CMailHandler
{
public:
	CMailHandler();
	~CMailHandler();

	void PushMail(u32 _Mail);
	void Clear();
	void Halt(bool _Halt);
	void DoState(PointerWrap &p);
	bool IsEmpty();

	u16 ReadDSPMailboxHigh();
	u16 ReadDSPMailboxLow();

	u32 GetNextMail()
	{
		if (!m_Mails.empty())
		{
			return m_Mails.front();
		}
		else
		{
			// WARN_LOG(DSPHLE, "GetNextMail: No mails");
			return 0;
		}
	}

private:
	// mail handler
	std::queue<u32> m_Mails;
};
