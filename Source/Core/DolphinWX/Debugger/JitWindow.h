// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <vector>
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/gdicmn.h>
#include <wx/listctrl.h>
#include <wx/panel.h>
#include <wx/string.h>
#include <wx/translation.h>
#include <wx/windowid.h>

#include "Common/CommonTypes.h"

class wxButton;
class wxListBox;
class wxTextCtrl;
class wxWindow;

class JitBlockList : public wxListCtrl
{
	std::vector<int> block_ranking;
public:
	JitBlockList(wxWindow* parent, const wxWindowID id, const wxPoint& pos, const wxSize& size, long style);
	void Init();
	void Update() override;
};

class CJitWindow : public wxPanel
{
public:
	CJitWindow(wxWindow* parent,
		wxWindowID id = wxID_ANY,
		const wxPoint& pos = wxDefaultPosition,
		const wxSize& size = wxDefaultSize,
		long style = wxTAB_TRAVERSAL | wxBORDER_NONE,
		const wxString& name = _("JIT block viewer"));

	void ViewAddr(u32 em_address);
	void Update() override;

private:
	void OnRefresh(wxCommandEvent& /*event*/);
	void Compare(u32 em_address);

	JitBlockList* block_list;
	wxButton* button_refresh;
	wxTextCtrl* ppc_box;
	wxTextCtrl* x86_box;
	wxListBox* top_instructions;

	DECLARE_EVENT_TABLE()

	void OnSymbolListChange(wxCommandEvent& event);
	void OnCallstackListChange(wxCommandEvent& event);
	void OnAddrBoxChange(wxCommandEvent& event);
	void OnHostMessage(wxCommandEvent& event);
};
