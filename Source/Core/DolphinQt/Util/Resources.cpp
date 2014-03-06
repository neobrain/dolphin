#include <QtGui>
#include "Resources.h"
#include "../resources/Platform_Gamecube.c"
#include "../resources/Platform_Wad.c"
#include "../resources/Platform_Wii.c"
#include "../resources/Flag_Europe.c"
#include "../resources/Flag_France.c"
#include "../resources/Flag_Italy.c"
#include "../resources/Flag_Japan.c"
#include "../resources/Flag_Taiwan.c"
#include "../resources/Flag_Unknown.c"
#include "../resources/Flag_USA.c"
#include "../resources/rating0.c"
#include "../resources/rating1.c"
#include "../resources/rating2.c"
#include "../resources/rating3.c"
#include "../resources/rating4.c"
#include "../resources/rating5.c"
#include "../resources/Dolphin.c"
#include "../resources/toolbar_help.c"
#include "../resources/no_banner.cpp"

#include "DiscIO/Volume.h"
#include "Core/ConfigManager.h"

Resources* Resources::instance = NULL;

Resources::Resources()
{

}

Resources::~Resources()
{

}

void Resources::Init()
{
	instance = new Resources;

	QString dir = QString::fromStdString(File::GetThemeDir(SConfig::GetInstance().m_LocalCoreStartupParameter.theme_name));

	instance->regions.resize(DiscIO::IVolume::NUMBER_OF_COUNTRIES);
	instance->regions[DiscIO::IVolume::COUNTRY_EUROPE].loadFromData(flag_europe_png, sizeof(flag_europe_png));
	instance->regions[DiscIO::IVolume::COUNTRY_FRANCE].loadFromData(flag_france_png, sizeof(flag_france_png));
	instance->regions[DiscIO::IVolume::COUNTRY_RUSSIA].loadFromData(flag_unknown_png, sizeof(flag_unknown_png)); // TODO
	instance->regions[DiscIO::IVolume::COUNTRY_USA].loadFromData(flag_usa_png, sizeof(flag_usa_png));
	instance->regions[DiscIO::IVolume::COUNTRY_JAPAN].loadFromData(flag_japan_png, sizeof(flag_japan_png));
	instance->regions[DiscIO::IVolume::COUNTRY_KOREA].loadFromData(flag_unknown_png, sizeof(flag_unknown_png)); // TODO
	instance->regions[DiscIO::IVolume::COUNTRY_ITALY].loadFromData(flag_italy_png, sizeof(flag_italy_png));
	instance->regions[DiscIO::IVolume::COUNTRY_TAIWAN].loadFromData(flag_taiwan_png, sizeof(flag_taiwan_png));
	instance->regions[DiscIO::IVolume::COUNTRY_SDK].loadFromData(flag_unknown_png, sizeof(flag_unknown_png)); // TODO
	instance->regions[DiscIO::IVolume::COUNTRY_UNKNOWN].loadFromData(flag_unknown_png, sizeof(flag_unknown_png));

	instance->platforms.resize(3);
	instance->platforms[0].loadFromData(platform_gamecube_png, sizeof(platform_gamecube_png));
	instance->platforms[1].loadFromData(platform_wii_png, sizeof(platform_wii_png));
	instance->platforms[2].loadFromData(platform_wad_png, sizeof(platform_wad_png));

	instance->ratings.resize(6);
	instance->ratings[0].loadFromData(rating0_png, sizeof(rating0_png));
	instance->ratings[1].loadFromData(rating1_png, sizeof(rating1_png));
	instance->ratings[2].loadFromData(rating2_png, sizeof(rating2_png));
	instance->ratings[3].loadFromData(rating3_png, sizeof(rating3_png));
	instance->ratings[4].loadFromData(rating4_png, sizeof(rating4_png));
	instance->ratings[5].loadFromData(rating5_png, sizeof(rating5_png));

	instance->pixmaps.resize(NUM_ICONS);
	instance->pixmaps[TOOLBAR_OPEN].load(dir + "open.png");
	instance->pixmaps[TOOLBAR_REFRESH].load(dir + "refresh.png");
	instance->pixmaps[TOOLBAR_BROWSE].load(dir + "browse.png");
	instance->pixmaps[TOOLBAR_PLAY].load(dir + "play.png");
	instance->pixmaps[TOOLBAR_STOP].load(dir + "stop.png");
	instance->pixmaps[TOOLBAR_FULLSCREEN].load(dir + "fullscreen.png");
	instance->pixmaps[TOOLBAR_SCREENSHOT].load(dir + "screenshot.png");
	instance->pixmaps[TOOLBAR_CONFIGURE].load(dir + "config.png");
	instance->pixmaps[TOOLBAR_PLUGIN_GFX].load(dir + "graphics.png");
	instance->pixmaps[TOOLBAR_PLUGIN_DSP].load(dir + "dsp.png");
	instance->pixmaps[TOOLBAR_PLUGIN_GCPAD].load(dir + "gcpad.png");
	instance->pixmaps[TOOLBAR_PLUGIN_WIIMOTE].load(dir + "wiimote.png");
	instance->pixmaps[TOOLBAR_PAUSE].load(dir + "pause.png");
	instance->pixmaps[TOOLBAR_HELP].loadFromData(toolbar_help_png, sizeof(toolbar_help_png));
	// TODO: instance->toolbar[MEMCARD];
	// TODO: instance->toolbar[HOTKEYS];
	instance->pixmaps[DOLPHIN_LOGO].loadFromData(dolphin_ico32x32, sizeof(dolphin_ico32x32));
	instance->pixmaps[BANNER_MISSING].loadFromData(no_banner_png, sizeof(no_banner_png));
}

QPixmap& Resources::GetRegionPixmap(DiscIO::IVolume::ECountry region)
{
	return instance->regions[region];
}

QPixmap& Resources::GetPlatformPixmap(int console)
{
	if (console >= instance->platforms.size()) return instance->platforms[0];
	return instance->platforms[console];
}

QPixmap& Resources::GetRatingPixmap(int rating)
{
	if (rating >= instance->ratings.size()) return instance->ratings[0];
	return instance->ratings[rating];
}

QPixmap& Resources::GetPixmap(int id)
{
	if (id >= instance->pixmaps.size()) return instance->pixmaps[0];
	return instance->pixmaps[id];
}

QIcon Resources::GetIcon(int id)
{
	return QIcon(GetPixmap(id));
}
