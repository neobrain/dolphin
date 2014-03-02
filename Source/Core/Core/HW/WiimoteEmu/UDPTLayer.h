// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

// UDP Wiimote Translation Layer

#pragma once

#include "Core/HW/WiimoteEmu/WiimoteEmu.h"
#include "InputCommon/UDPWiimote.h"

namespace UDPTLayer
{
	void GetButtons(UDPWrapper* m, wm_buttons* butt)
	{
		if (!(m->inst))
			return;

		if (!(m->updButt))
			return;

		u32 mask = m->inst->getButtons();
		butt->a |= (mask & UDPWM_BA);
		butt->b |= (mask & UDPWM_BB);
		butt->one |= (mask & UDPWM_B1);
		butt->two |= (mask & UDPWM_B2);
		butt->plus |= (mask & UDPWM_BP);
		butt->minus |= (mask & UDPWM_BM);
		butt->home |= (mask & UDPWM_BH);
		butt->up |= (mask & UDPWM_BU);
		butt->down |= (mask & UDPWM_BD);
		butt->left |= (mask & UDPWM_BL);
		butt->right |= (mask & UDPWM_BR);
	}

	void GetAcceleration(UDPWrapper* m, WiimoteEmu::AccelData* const data)
	{
		if (!(m->inst))
			return;

		if (!(m->updAccel))
			return;

		float x, y, z;
		m->inst->getAccel(x, y, z);
		data->x = x;
		data->y = y;
		data->z = z;
	}

	void GetIR(UDPWrapper* m, float* x,  float* y,  float* z)
	{
		if (!(m->inst))
			return;

		if (!(m->updIR))
			return;

		// the received values are used ONLY when the normal pointer is offscreen
		if ((*x >= -0.999) && (*x <= 0.999) && (*y >= -0.999) && (*y <= 0.999))
			return;

		float _x, _y;
		m->inst->getIR(_x, _y);
		*x = _x * 2 - 1;
		*y = -(_y * 2 - 1);
		*z = 0;
	}
}
