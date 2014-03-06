set(QT_USE_QTNETWORK FALSE)
set(QT_USE_QTOPENGL FALSE)
set(QT_USE_QTSQL FALSE)
set(QT_USE_QTXML FALSE)
set(QT_USE_QTSVG FALSE)
set(QT_USE_QTTEST FALSE)
set(QT_USE_QTDBUS FALSE)
set(QT_USE_QTSCRIPT FALSE)
set(QT_USE_QTWEBKIT FALSE)
set(QT_USE_QTXMLPATTERNS FALSE)
set(QT_USE_PHONON FALSE)

set(SRCS	Config/ConfigAudio.cpp
			Config/ConfigGeneral.cpp
			Config/ConfigGfx.cpp
			Config/ConfigMain.cpp
			Config/ConfigPad.cpp
			Config/ConfigWiimote.cpp
			Util/Util.cpp
			Util/Resources.cpp
			GameList.cpp
			ISOFile.cpp
			IssueReporter.cpp
			LogWindow.cpp
			main.cpp
			MainWindow.cpp
			MainWindowEvents.cpp
			RenderWindow.cpp)

if(USE_EGL)
	set(SRCS ${SRCS} ../DolphinWX/GLInterface/Platform.cpp
		../DolphinWX/GLInterface/EGL.cpp)
	if(USE_WAYLAND)
		set(SRCS ${SRCS} ../DolphinWX/GLInterface/Wayland_Util.cpp)
	endif()
	if(USE_X11)
		set(SRCS ${SRCS} ../DolphinWX/GLInterface/X11_Util.cpp)
	endif()
else()
	if(WIN32)
		set(SRCS ${SRCS} ../DolphinWX/GLInterface/WGL.cpp)
	elseif(${CMAKE_SYSTEM_NAME} MATCHES "Darwin")
		set(SRCS ${SRCS} ../DolphinWX/GLInterface/AGL.cpp)
	else()
		set(SRCS ${SRCS} ../DolphinWX/GLInterface/GLX.cpp
			../DolphinWX/GLInterface/X11_Util.cpp)

	endif()
endif()

set(LIBS		core
			${LZO}
			discio
			bdisasm
			inputcommon
			common
			audiocommon
			z
			sfml-network
			rt
			${OPENGL_LIBRARIES}
			${X11_LIBRARIES}
			${XINPUT2_LIBRARIES}
			${XRANDR_LIBRARIES})

if(SDL2_FOUND)
	# Using shared SDL2
	set(LIBS	${LIBS} ${SDL2_LIBRARY})
else(SDL2_FOUND)
	if(SDL_FOUND)
		# Using shared SDL
		set(LIBS	${LIBS} ${SDL_LIBRARY})
	else(SDL_FOUND)
		# Using static SDL from Externals
		set(LIBS	${LIBS} SDL)
	endif()
endif()

set(EXEGUI dolphin-emu-qt4)

qt_wrap_cpp(${EXEGUI} MOC_SRCS	Config/ConfigAudio.h
				Config/ConfigGeneral.h
				Config/ConfigGfx.h
				Config/ConfigMain.h
				Config/ConfigPad.h
				Config/ConfigWiimote.h
				Util/Util.h
				IssueReporter.h
				GameList.h
				LogWindow.h
				MainWindow.h
				RenderWindow.h)
add_executable(${EXEGUI} ${SRCS} ${MOC_SRCS})
target_link_libraries(${EXEGUI} ${LIBS} ${QT_LIBRARIES})
install(TARGETS ${EXEGUI} RUNTIME DESTINATION ${bindir})
