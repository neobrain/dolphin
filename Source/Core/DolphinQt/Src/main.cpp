#include <QApplication>
#include <QMessageBox>
#include <QMainWindow>
#include "MainWindow.h"

#include "Common/CommonPaths.h"
#include "Common/CPUDetect.h"
#include "Common/LogManager.h"

#include "Core/ConfigManager.h"
#include "Core/HW/Wiimote.h"

#include "VideoCommon/VideoBackendBase.h"

DMainWindow* mainWindow = NULL;

int main(int argc, char* argv[])
{
	// TODO: Add command line options:
	// help, debugger, logger, load file, exit on emulation stop, chose video backend, DSP LLE/HLE

	// TODO
/*#if defined _DEBUG && defined _WIN32
	int tmpflag = _CrtSetDbgFlag(_CRTDBG_REPORT_FLAG);
	tmpflag |= _CRTDBG_DELAY_FREE_MEM_DF;
	_CrtSetDbgFlag(tmpflag);
#endif*/

	// Register message box and translation handlers
//RegisterMsgAlertHandler( .. );
//RegisterStringTranslator( .. );

	// "ExtendedTrace" looks freakin dangerous!!!
/*#ifdef _WIN32
	EXTENDEDTRACEINITIALIZE(".");
	SetUnhandledExceptionFilter(&MyUnhandledExceptionFilter);
#elif wxUSE_ON_FATAL_EXCEPTION
	wxHandleFatalExceptions(true);
#endif*/


	// Copy initial Wii NAND data from Sys to User.
	File::CopyDir(File::GetSysDirectory() + WII_USER_DIR DIR_SEP,
				  File::GetUserPath(D_WIIUSER_IDX));

	File::CreateFullPath(File::GetUserPath(D_USER_IDX));
	File::CreateFullPath(File::GetUserPath(D_CACHE_IDX));
	File::CreateFullPath(File::GetUserPath(D_CONFIG_IDX));
	File::CreateFullPath(File::GetUserPath(D_DUMPDSP_IDX));
	File::CreateFullPath(File::GetUserPath(D_DUMPTEXTURES_IDX));
	File::CreateFullPath(File::GetUserPath(D_GAMESETTINGS_IDX));
	File::CreateFullPath(File::GetUserPath(D_GCUSER_IDX));
	File::CreateFullPath(File::GetUserPath(D_GCUSER_IDX) + USA_DIR DIR_SEP);
	File::CreateFullPath(File::GetUserPath(D_GCUSER_IDX) + EUR_DIR DIR_SEP);
	File::CreateFullPath(File::GetUserPath(D_GCUSER_IDX) + JAP_DIR DIR_SEP);
	File::CreateFullPath(File::GetUserPath(D_HIRESTEXTURES_IDX));
	File::CreateFullPath(File::GetUserPath(D_MAILLOGS_IDX));
	File::CreateFullPath(File::GetUserPath(D_MAPS_IDX));
	File::CreateFullPath(File::GetUserPath(D_SCREENSHOTS_IDX));
	File::CreateFullPath(File::GetUserPath(D_SHADERS_IDX));
	File::CreateFullPath(File::GetUserPath(D_STATESAVES_IDX));
	File::CreateFullPath(File::GetUserPath(D_THEMES_IDX));


	// TODO: Move this out of GUI code
	LogManager::Init();
	SConfig::Init();
	VideoBackend::PopulateList();
	VideoBackend::ActivateBackend(SConfig::GetInstance().m_LocalCoreStartupParameter.m_strVideoBackend);
	WiimoteReal::LoadSettings();

	SetEnableAlert(SConfig::GetInstance().m_LocalCoreStartupParameter.bUsePanicHandlers);

	// TODO?
/*#if defined HAVE_X11 && HAVE_X11
	XInitThreads();
#endif */

	QApplication app(argc, argv);
	if (!cpu_info.bSSE2)
	{
		QMessageBox::information(NULL,"Hardware does not support","Hi,\n\nDolphin requires that your CPU has support for SSE2 extensions.\n"
					"Unfortunately your CPU does not support them, so Dolphin will not run.\n\n"
					"Sayonara!\n");
		return 0;
	}

	mainWindow = new DMainWindow();
	// TODO: Title => svn_rev_str
	// TODO: UseLogger
	return app.exec();

	// TODO: On exit:
		// We'll do these in subclass of MainWindow/QMLViewer
	// WiimoteReal::Shutdown();
/*#ifdef _WIN32
	if (SConfig::GetInstance().m_WiiAutoUnpair)
		WiimoteReal::UnPair();
#endif*/
	// VideoBackend::ClearList();
	// SConfig::Shutdown();
	// LogManager::Shutdown();

}

void Host_SetStartupDebuggingParameters()
{
	SCoreStartupParameter& StartUp = SConfig::GetInstance().m_LocalCoreStartupParameter;
	StartUp.bEnableDebugging = false;
	StartUp.bBootToPause = false;
}

void* Host_GetInstance()
{
	return NULL;
}

void* Host_GetRenderHandle()
{
	return (void*)(mainWindow->GetRenderWindow()->winId());
}

bool Host_GetKeyState(int)
{
	return false;
}

void Host_RefreshDSPDebuggerWindow()
{

}

void Host_RequestRenderWindowSize(int w, int h)
{
	if (SConfig::GetInstance().m_LocalCoreStartupParameter.bRenderToMain)
	{
		// Make sure to resize the actual client area
		// TODO: Might not work properly, yet.
		QSize sizediff = mainWindow->size() - mainWindow->GetRenderWindow()->size();
		mainWindow->resize(w + sizediff.width(), h + sizediff.height());
	}
	else mainWindow->GetRenderWindow()->resize(w, h);
}

// TODO: Rename this to GetRenderClientSize
void Host_GetRenderWindowSize(int& x, int& y, int& width, int& height)
{
	// TODO: Make it more clear what this is supposed to return.. i.e. wxw always sets x=y=0
	x = 0;
	y = 0;
	width = mainWindow->GetRenderWindow()->width();
	height = mainWindow->GetRenderWindow()->height();
}

void Host_ConnectWiimote(int, bool)
{

}

void Host_Message(int Id)
{
	// TODO: Handle IDM_UPDATEGUI, IDM_UPDATESTATUSBAR, IDM_UPDATETITLE, WM_USER_CREATE, IDM_WINDOWSIZEREQUEST, IDM_PANIC(?), IDM_KEYSTATE(?), WM_USER_STOP
}

void Host_UpdateMainFrame()
{

}

void Host_UpdateTitle(const char* title)
{
	// TODO: Doesn't work perfectly with render to main, yet.
	if (SConfig::GetInstance().m_LocalCoreStartupParameter.bRenderToMain)
		mainWindow->setWindowTitle(QString(title));
	else
		mainWindow->GetRenderWindow()->setWindowTitle(QString(title));
}

void Host_UpdateDisasmDialog()
{
}

void Host_UpdateLogDisplay()
{
}

void Host_UpdateMemoryView()
{
}

void Host_NotifyMapLoaded()
{
}

void Host_UpdateBreakPointView()
{
}

void Host_ShowJitResults(unsigned int address)
{
}

void Host_SetDebugMode(bool enable)
{
}

bool Host_RendererHasFocus()
{
	// TODO: Sometimes with render to main this won't return the correct value
	if (mainWindow->GetRenderWindow())
		return mainWindow->GetRenderWindow()->hasFocus();
	else return false;
}


void Host_SetWaitCursor(bool enable)
{
}


void Host_UpdateStatusBar(const char* _pText, int Filed = 0)
{
}


void Host_SysMessage(const char *fmt, ...)
{
	// TODO
}

void Host_SetWiiMoteConnectionState(int _State)
{
}

