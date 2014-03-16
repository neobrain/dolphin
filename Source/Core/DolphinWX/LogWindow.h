// Copyright 2013 Dolphin Emulator Project
// Licensed under GPLv2
// Refer to the license.txt file included.

#pragma once

#include <mutex>
#include <queue>
#include <utility>
#include <vector>
#include <wx/defs.h>
#include <wx/event.h>
#include <wx/font.h>
#include <wx/gdicmn.h>
#include <wx/panel.h>
#include <wx/string.h>
#include <wx/translation.h>
#include <wx/windowid.h>

#include "Common/Common.h"
#include "Common/LogManager.h"

class CFrame;
class wxBoxSizer;
class wxCheckBox;
class wxChoice;
class wxTextCtrl;
class wxTimer;
class wxTimerEvent;

enum
{
	IDM_LOG,
	IDM_CLEARLOG,
	IDM_TOGGLEALL,
	IDM_WRAPLINE,
	IDTM_UPDATELOG,
	IDM_FONT,
	IDM_SUBMITCMD
};

// Uses multiple inheritance - only sane because LogListener is a pure virtual interface.
class CLogWindow : public wxPanel, LogListener
{
public:
	CLogWindow(CFrame *parent,
		wxWindowID id = wxID_ANY,
		const wxPoint& pos = wxDefaultPosition,
		const wxSize& size = wxDefaultSize,
		long style = wxTAB_TRAVERSAL,
		const wxString& name = _("Log")
		);
	~CLogWindow();

	void SaveSettings();
	void Log(LogTypes::LOG_LEVELS, const char *text) override;

	int x, y, winpos;

private:
	CFrame *Parent;
	wxFont DefaultFont, MonoSpaceFont;
	std::vector<wxFont> LogFont;
	wxTimer *m_LogTimer;
	bool m_ignoreLogTimer;
	LogManager *m_LogManager;
	std::queue<std::pair<u8, wxString> > msgQueue;
	bool m_writeFile, m_writeWindow, m_writeDebugger, m_LogAccess;

	// Controls
	wxBoxSizer *sBottom;
	wxTextCtrl *m_Log, *m_cmdline;
	wxChoice *m_FontChoice;
	wxCheckBox *m_WrapLine;

	std::mutex m_LogSection;

	DECLARE_EVENT_TABLE()

	wxTextCtrl * CreateTextCtrl(wxPanel* parent, wxWindowID id, long Style);
	void CreateGUIControls();
	void PopulateBottom();
	void UnPopulateBottom();
	void OnClose(wxCloseEvent& event);
	void OnFontChange(wxCommandEvent& event);
	void OnWrapLineCheck(wxCommandEvent& event);
	void OnClear(wxCommandEvent& event);
	void OnLogTimer(wxTimerEvent& WXUNUSED(event));
	void UpdateLog();

	// LogListener
	const char *getName() const { return "LogWindow"; }
};
