// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include "Common/CPUDetect.h"
#include "Common/MathUtil.h"
#include "Core/PowerPC/Interpreter/Interpreter.h"

using namespace MathUtil;

// warning! very slow! This setting fixes NAN
//#define VERY_ACCURATE_FP

#define MIN_SINGLE 0xc7efffffe0000000ull
#define MAX_SINGLE 0x47efffffe0000000ull

// FPSCR exception flags
const u32 FPSCR_OX         = (u32)1 << (31 - 3);
const u32 FPSCR_UX         = (u32)1 << (31 - 4);
const u32 FPSCR_ZX         = (u32)1 << (31 - 5);
// ! XX shouldn't be accessed directly to set 1. Use SetFI() instead !
const u32 FPSCR_XX         = (u32)1 << (31 - 6);
const u32 FPSCR_VXSNAN     = (u32)1 << (31 - 7);
const u32 FPSCR_VXISI      = (u32)1 << (31 - 8);
const u32 FPSCR_VXIDI      = (u32)1 << (31 - 9);
const u32 FPSCR_VXZDZ      = (u32)1 << (31 - 10);
const u32 FPSCR_VXIMZ      = (u32)1 << (31 - 11);
const u32 FPSCR_VXVC       = (u32)1 << (31 - 12);
const u32 FPSCR_VXSOFT     = (u32)1 << (31 - 21);
const u32 FPSCR_VXSQRT     = (u32)1 << (31 - 22);
const u32 FPSCR_VXCVI      = (u32)1 << (31 - 23);

const u32 FPSCR_VX_ANY     = FPSCR_VXSNAN | FPSCR_VXISI | FPSCR_VXIDI | FPSCR_VXZDZ |
                             FPSCR_VXIMZ | FPSCR_VXVC | FPSCR_VXSOFT | FPSCR_VXSQRT | FPSCR_VXCVI;

const u32 FPSCR_ANY_X      = FPSCR_OX | FPSCR_UX | FPSCR_ZX | FPSCR_XX | FPSCR_VX_ANY;

const u64 PPC_NAN_U64      = 0x7ff8000000000000ull;
const double PPC_NAN       = *(double* const)&PPC_NAN_U64;

// the 4 less-significand bits in FPSCR[FPRF]
enum FPCC {
	FL = 8, // <
	FG = 4, // >
	FE = 2, // =
	FU = 1, // ?
};

inline void SetFPException(u32 mask)
{
	if ((FPSCR.Hex & mask) != mask)
	{
		FPSCR.FX = 1;
	}
	FPSCR.Hex |= mask;
}

inline void SetFI(int FI)
{
	if (FI)
	{
		SetFPException(FPSCR_XX);
	}
	FPSCR.FI = FI;
}

inline void UpdateFPSCR()
{
	FPSCR.VX = (FPSCR.Hex & FPSCR_VX_ANY) != 0;
	FPSCR.FEX = 0; // we assume that "?E" bits are always 0
}

inline double ForceSingle(double _x)
{
	// convert to float...
	float x = (float) _x;
	if (!cpu_info.bFlushToZero && FPSCR.NI)
	{
		x = FlushToZero(x);
	}
	// ...and back to double:
	return x;
}

inline double ForceDouble(double d)
{
	if (!cpu_info.bFlushToZero && FPSCR.NI)
	{
		d = FlushToZero(d);
	}
	return d;
}

// these functions allow globally modify operations behaviour
// also, these may be used to set flags like FR, FI, OX, UX

inline double NI_mul(const double a, const double b)
{
#ifdef VERY_ACCURATE_FP
	if (a != a) return a;
	if (b != b) return b;
	double t = a * b;
	if (t != t)
	{
		SetFPException(FPSCR_VXIMZ);
		return PPC_NAN;
	}
	return t;
#else
	return a * b;
#endif
}

inline double NI_add(const double a, const double b)
{
#ifdef VERY_ACCURATE_FP
	if (a != a) return a;
	if (b != b) return b;
	double t = a + b;
	if (t != t)
	{
		SetFPException(FPSCR_VXISI);
		return PPC_NAN;
	}
	return t;
#else
	return a + b;
#endif
}

inline double NI_sub(const double a, const double b)
{
#ifdef VERY_ACCURATE_FP
	if (a != a) return a;
	if (b != b) return b;
	double t = a - b;
	if (t != t)
	{
		SetFPException(FPSCR_VXISI);
		return PPC_NAN;
	}
	return t;
#else
	return a - b;
#endif
}

inline double NI_madd(const double a, const double b, const double c)
{
#ifdef VERY_ACCURATE_FP
	if (a != a) return a;
	if (c != c) return c;
	if (b != b) return b;
	double t = a * b;
	if (t != t)
	{
		SetFPException(FPSCR_VXIMZ);
		return PPC_NAN;
	}
	t = t + c;
	if (t != t)
	{
		SetFPException(FPSCR_VXISI);
		return PPC_NAN;
	}
	return t;
#else
	return NI_add(NI_mul(a, b), c);
#endif
}

inline double NI_msub(const double a, const double b, const double c)
{
//#ifdef VERY_ACCURATE_FP
//  This code does not produce accurate fp!  NAN's are not calculated correctly, nor negative zero.
//	The code is kept here for reference.
//
//	if (a != a) return a;
//	if (c != c) return c;
//	if (b != b) return b;
//	double t = a * b;
//	if (t != t)
//	{
//		SetFPException(FPSCR_VXIMZ);
//		return PPC_NAN;
//	}
//
//	t = t - c;
//	if (t != t)
//	{
//		SetFPException(FPSCR_VXISI);
//		return PPC_NAN;
//	}
//	return t;
//#else
//	This code does not calculate QNAN's correctly but calculates negative zero correctly.
	return NI_sub(NI_mul(a, b), c);
// #endif
}

// used by stfsXX instructions and ps_rsqrte
inline u32 ConvertToSingle(u64 x)
{
	u32 exp = (x >> 52) & 0x7ff;
	if (exp > 896 || (x & ~DOUBLE_SIGN) == 0)
	{
		return ((x >> 32) & 0xc0000000) | ((x >> 29) & 0x3fffffff);
	}
	else if (exp >= 874)
	{
		u32 t = (u32)(0x80000000 | ((x & DOUBLE_FRAC) >> 21));
		t = t >> (905 - exp);
		t |= (x >> 32) & 0x80000000;
		return t;
	}
	else
	{
		// This is said to be undefined.
		// The code is based on hardware tests.
		return ((x >> 32) & 0xc0000000) | ((x >> 29) & 0x3fffffff);
	}
}

// used by psq_stXX operations.
inline u32 ConvertToSingleFTZ(u64 x)
{
	u32 exp = (x >> 52) & 0x7ff;
	if (exp > 896 || (x & ~DOUBLE_SIGN) == 0)
	{
		return ((x >> 32) & 0xc0000000) | ((x >> 29) & 0x3fffffff);
	}
	else
	{
		return (x >> 32) & 0x80000000;
	}
}

inline u64 ConvertToDouble(u32 _x)
{
	// This is a little-endian re-implementation of the algorithm described in
	// the PowerPC Programming Environments Manual for loading single
	// precision floating point numbers.
	// See page 566 of http://www.freescale.com/files/product/doc/MPCFPE32B.pdf

	u64 x = _x;
	u64 exp = (x >> 23) & 0xff;
	u64 frac = x & 0x007fffff;

	if (exp > 0 && exp < 255) // Normal number
	{
		u64 y = !(exp >> 7);
		u64 z = y << 61 | y << 60 | y << 59;
		return ((x & 0xc0000000) << 32) | z | ((x & 0x3fffffff) << 29);
	}
	else if (exp == 0 && frac != 0) // Subnormal number
	{
		exp = 1023 - 126;
		do
		{
			frac <<= 1;
			exp -= 1;
		} while ((frac & 0x00800000) == 0);
		return ((x & 0x80000000) << 32) | (exp << 52) | ((frac & 0x007fffff) << 29);
	}
	else // QNaN, SNaN or Zero
	{
		u64 y = exp >> 7;
		u64 z = y << 61 | y << 60 | y << 59;
		return ((x & 0xc0000000) << 32) | z | ((x & 0x3fffffff) << 29);
	}
}
