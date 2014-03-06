// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <cstdio>
#include <cstring>

// Don't include common.h here as it will break LogManager
#include "Common/CommonTypes.h"
#include "Common/StdConditionVariable.h"
#include "Common/StdMutex.h"
#include "Common/StdThread.h"

// This may not be defined outside _WIN32
#ifndef _WIN32
#ifndef INFINITE
#define INFINITE 0xffffffff
#endif

//for gettimeofday and struct time(spec|val)
#include <time.h>
#include <sys/time.h>
#endif

namespace Common
{

int CurrentThreadId();

void SetThreadAffinity(std::thread::native_handle_type thread, u32 mask);
void SetCurrentThreadAffinity(u32 mask);

class Event
{
public:
	Event()
		: is_set(false)
	{}

	void Set()
	{
		std::lock_guard<std::mutex> lk(m_mutex);
		if (!is_set)
		{
			is_set = true;
			m_condvar.notify_one();
		}
	}

	void Wait()
	{
		std::unique_lock<std::mutex> lk(m_mutex);
		m_condvar.wait(lk, [&]{ return is_set; });
		is_set = false;
	}

	void Reset()
	{
		std::unique_lock<std::mutex> lk(m_mutex);
		// no other action required, since wait loops on 
		// the predicate and any lingering signal will get 
		// cleared on the first iteration
		is_set = false;
	}

private:
	volatile bool is_set;
	std::condition_variable m_condvar;
	std::mutex m_mutex;
};

// TODO: doesn't work on windows with (count > 2)
class Barrier
{
public:
	Barrier(size_t count)
		: m_count(count), m_waiting(0)
	{}

	// block until "count" threads call Sync()
	bool Sync()
	{
		std::unique_lock<std::mutex> lk(m_mutex);

		// TODO: broken when next round of Sync()s
		// is entered before all waiting threads return from the notify_all

		if (m_count == ++m_waiting)
		{
			m_waiting = 0;
			m_condvar.notify_all();
			return true;
		}
		else
		{
			m_condvar.wait(lk, [&]{ return (0 == m_waiting); });
			return false;
		}
	}

private:
	std::condition_variable m_condvar;
	std::mutex m_mutex;
	const size_t m_count;
	volatile size_t m_waiting;
};

void SleepCurrentThread(int ms);
void SwitchCurrentThread(); // On Linux, this is equal to sleep 1ms

// Use this function during a spin-wait to make the current thread
// relax while another thread is working. This may be more efficient
// than using events because event functions use kernel calls.
inline void YieldCPU()
{
	std::this_thread::yield();
}

void SetCurrentThreadName(const char *name);

} // namespace Common
