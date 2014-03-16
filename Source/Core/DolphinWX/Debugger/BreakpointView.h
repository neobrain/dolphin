// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <wx/listctrl.h>
#include <wx/windowid.h>

class wxWindow;

class CBreakPointView : public wxListCtrl
{
public:
	CBreakPointView(wxWindow* parent, const wxWindowID id);

	void Update() override;
	void DeleteCurrentSelection();
};
