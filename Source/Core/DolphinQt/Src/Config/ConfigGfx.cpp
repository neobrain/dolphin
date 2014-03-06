#include <QBoxLayout>
#include <QCheckBox>
#include <QComboBox>
#include <QLabel>
#include <QTabBar>
#include "ConfigGfx.h"
#include "../Util/Util.h"

#include "Core/ConfigManager.h"

#include "VideoCommon/VideoBackendBase.h"
#include "VideoCommon/VideoConfig.h"

// TODO: Clean this up...
static std::string GetIniName(VideoBackend* backend)
{
	if (backend->GetName().compare("D3D") == 0) return std::string("gfx_dx11.ini");
	else if (backend->GetName().compare("OGL") == 0) return std::string("gfx_opengl.ini");
	else if (backend->GetName().compare("Software Renderer") == 0) return std::string("gfx_software.ini");
	else return std::string(); // TODO: This case causes our ini loading code to hang indefinitely....
}

QWidget* DConfigGfx::CreateGeneralTabWidget()
{
	// TODO: Should just make all backends use the same config file, that's easier to handle from a code perspective and more user friendly, too
	// TODO: As long as we don't do that, we should have an event handler for backend changes.
	g_Config.Load((File::GetUserPath(D_CONFIG_IDX) + GetIniName(g_video_backend)).c_str());

	QWidget* tab = new QWidget;

	// Widgets
	cbBackend = new QComboBox;
	for (std::vector<VideoBackend*>::iterator it = g_available_video_backends.begin(); it != g_available_video_backends.end(); ++it)
		cbBackend->addItem(QString::fromStdString((*it)->GetName()));

	chAspectRatio = new QComboBox;
	chAspectRatio->addItems(QStringList() << tr("Auto") << tr("Force 16:9") << tr("Force 4:3") << tr("Stretch to Window"));

	cbVsync = new QCheckBox(tr("V-Sync"));
	cbFullscreen = new QCheckBox(tr("Fullscreen"));

	cbFPS = new QCheckBox(tr("Show FPS"));
	cbAutoWindowSize = new QCheckBox(tr("Automatic Sizing"));
	cbHideCursor = new QCheckBox(tr("Hide Mouse Cursor"));
	cbRenderToMain = new QCheckBox(tr("Render to Main Window"));


	// Layouts
	QGroupBox* groupDisplay = new QGroupBox(tr("Display"));
	QGridLayout* layoutDisplay = new QGridLayout;
	layoutDisplay->addWidget(new QLabel(tr("Backend:")), 0, 0);
	layoutDisplay->addWidget(cbBackend, 0, 1);
	layoutDisplay->addWidget(new QLabel(tr("Aspect Ratio:")), 1, 0);
	layoutDisplay->addWidget(chAspectRatio, 1, 1);
	layoutDisplay->addWidget(cbVsync, 2, 0, 1, 2);
	layoutDisplay->addWidget(cbFullscreen, 3, 0, 1, 2);
	groupDisplay->setLayout(layoutDisplay);

	DGroupBoxV* groupEmuWindow = new DGroupBoxV(tr("Emulation Window"));
	groupEmuWindow->addWidget(cbFPS);
	groupEmuWindow->addWidget(cbAutoWindowSize);
	groupEmuWindow->addWidget(cbHideCursor);
	groupEmuWindow->addWidget(cbRenderToMain);

	QBoxLayout* mainLayout = new QVBoxLayout;
	mainLayout->addWidget(groupDisplay);
	mainLayout->addWidget(groupEmuWindow);
	mainLayout->addStretch();
	tab->setLayout(mainLayout);


	// Initial values
	// TODO: Load the actual config, don't use the current one (which might be "dirtied" by game inis.
	ctrlManager = new DControlStateManager(this);
	ctrlManager->RegisterControl(cbBackend, QString::fromStdString(g_video_backend->GetName()));
	ctrlManager->RegisterControl(chAspectRatio, g_Config.iAspectRatio);
	ctrlManager->RegisterControl(cbVsync, g_Config.bVSync);
	ctrlManager->RegisterControl(cbFullscreen, SConfig::GetInstance().m_LocalCoreStartupParameter.bFullscreen);
	ctrlManager->RegisterControl(cbFPS, g_Config.bShowFPS);
	ctrlManager->RegisterControl(cbAutoWindowSize, SConfig::GetInstance().m_LocalCoreStartupParameter.bRenderWindowAutoSize);
	ctrlManager->RegisterControl(cbHideCursor, SConfig::GetInstance().m_LocalCoreStartupParameter.bHideCursor);
	ctrlManager->RegisterControl(cbRenderToMain, SConfig::GetInstance().m_LocalCoreStartupParameter.bRenderToMain);

	return tab;
}

QWidget* DConfigGfx::CreateEnhancementsTabWidget()
{
	QWidget* tab = new QWidget;

	// Widgets
	chInternalResolution = new QComboBox;
	chInternalResolution->addItems(QStringList() << tr("Auto (Window Size)") << tr("Auto (Multiple of 640x528)")
									<< tr("1x Native (640x528)") << tr("1.5x Native (960x792)") << tr("2x Native (1280x1056)")
									<< tr("2.5x Native (1600x1320)") << tr("3x Native (1920x1584)") << tr("4x Native (2560x2112)"));

	//chAntiAliasing = new QComboBox;
	chAnisotropicFiltering = new QComboBox;
	chAnisotropicFiltering->addItems(QStringList() << tr("1x") << tr("2x") << tr("4x") << tr("8x") << tr("16x"));

	cbPerPixelLighting = new QCheckBox(tr("Enable per-Pixel Lighting"));

	// Layouts
	QGroupBox* groupEnhancements = new QGroupBox(tr("Enhancements"));
	QGridLayout* layoutEnhancements = new QGridLayout;
	layoutEnhancements->addWidget(new QLabel(tr("Internal Resolution:")), 0, 0);
	layoutEnhancements->addWidget(chInternalResolution, 0, 1);
	//layoutEnhancements->addWidget(new QLabel(tr("Anti-Aliasing:")), 1, 0);
	//layoutEnhancements->addWidget(chAntiAliasing, 1, 1);
	layoutEnhancements->addWidget(new QLabel(tr("Anisotropic Filtering:")), 1, 0);
	layoutEnhancements->addWidget(chAnisotropicFiltering, 1, 1);
	layoutEnhancements->addWidget(cbPerPixelLighting, 2, 0, 1, 2);
	groupEnhancements->setLayout(layoutEnhancements);

	QBoxLayout* mainLayout = new QVBoxLayout;
	mainLayout->addWidget(groupEnhancements);
	mainLayout->addStretch();
	tab->setLayout(mainLayout);


	// Initial values
	ctrlManager->RegisterControl(chInternalResolution, g_Config.iEFBScale);
	//ctrlManager->RegisterControl(chAntiAliasing, TODO);
	ctrlManager->RegisterControl(chAnisotropicFiltering, g_Config.iMaxAnisotropy);
	ctrlManager->RegisterControl(cbPerPixelLighting, g_Config.bEnablePixelLighting);

	return tab;
}

QWidget* DConfigGfx::CreateEmulationTabWidget()
{
	QWidget* tab = new QWidget;

	// Widgets
	chEfbCopyMethod = new QComboBox;
	chEfbCopyMethod->addItems(QStringList(tr("Disabled")) << tr("Fast") << tr("Fast + Pretty") << tr("Accurate"));

	// Layouts
	QGroupBox* groupEfbCopy = new QGroupBox(tr("EFB Copy Emulation"));
	QGridLayout* layoutEfbCopy = new QGridLayout;
	layoutEfbCopy->addWidget(new QLabel(tr("Copy Method")), 0, 0);
	layoutEfbCopy->addWidget(chEfbCopyMethod, 0, 1);
	groupEfbCopy->setLayout(layoutEfbCopy);

	QBoxLayout* mainLayout = new QVBoxLayout;
	mainLayout->addWidget(groupEfbCopy);
	mainLayout->addStretch();
	tab->setLayout(mainLayout);


	// Initial values
	SCoreStartupParameter& StartUp = SConfig::GetInstance().m_LocalCoreStartupParameter;
	int efb_copy_method;
	if (!g_Config.bEFBCopyEnable) efb_copy_method = 0;
	else if (!g_Config.bCopyEFBToTexture) efb_copy_method = 3;
	else if (!g_Config.bCopyEFBScaled) efb_copy_method = 1;
	else efb_copy_method = 2;

	ctrlManager->RegisterControl(chEfbCopyMethod, efb_copy_method);

	return tab;
}

QWidget* DConfigGfx::CreateAdvancedTabWidget()
{
	QWidget* tab = new QWidget;

	// Widgets


	// Layouts
	QBoxLayout* mainLayout = new QVBoxLayout;
	tab->setLayout(mainLayout);


	// Initial values

	return tab;
}

DConfigGfx::DConfigGfx(QWidget* parent) : QTabWidget(parent)
{
	addTab(CreateGeneralTabWidget(), tr("General"));
	addTab(CreateEnhancementsTabWidget(), tr("Enhancements"));
	addTab(CreateEmulationTabWidget(), tr("Emulation"));
//	addTab(CreateAdvancedTabWidget(), tr("Advanced"));

	tabBar()->setUsesScrollButtons(false);
}

void DConfigGfx::Reset()
{
	ctrlManager->OnReset();
}

void DConfigGfx::Apply()
{
	SCoreStartupParameter& StartUp = SConfig::GetInstance().m_LocalCoreStartupParameter;

	g_video_backend = g_available_video_backends[cbBackend->currentIndex()];
	StartUp.m_strVideoBackend = g_video_backend->GetName();
	g_Config.iAspectRatio = chAspectRatio->currentIndex();
	g_Config.bVSync = cbVsync->isChecked();
	StartUp.bFullscreen = cbFullscreen->isChecked();

	g_Config.bShowFPS = cbFPS->isChecked();
	StartUp.bRenderWindowAutoSize = cbAutoWindowSize->isChecked();
	StartUp.bHideCursor = cbHideCursor->isChecked(); // StartUp.AutoHideCursor??
	StartUp.bRenderToMain = cbRenderToMain->isChecked();

	g_Config.iEFBScale = chInternalResolution->currentIndex();
	// TODO: Antialiasing = chAntiAliasing->currentIndex()
	g_Config.iMaxAnisotropy = chAnisotropicFiltering->currentIndex();
	g_Config.bEnablePixelLighting = cbPerPixelLighting->isChecked();

	g_Config.bEFBCopyEnable = chEfbCopyMethod->currentIndex() != ECM_DISABLED;
	g_Config.bCopyEFBToTexture = chEfbCopyMethod->currentIndex() != ECM_ACCURATE && g_Config.bEFBCopyEnable;
	g_Config.bCopyEFBScaled = chEfbCopyMethod->currentIndex() != ECM_FAST && g_Config.bCopyEFBToTexture;

	g_Config.Save((File::GetUserPath(D_CONFIG_IDX) + GetIniName(g_video_backend)).c_str());
}
