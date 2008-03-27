.PHONY: all clean

PARAMS=-Pagpl -XGtkAda_Include_Core=Yes -XHungarian_Include_Test=Yes -XHungarian_Include_Base=Yes -XHungarian_Link=Static_Library -XAgpl_Include_Test=Yes -XAgpl_Include_Psql=Yes -XAgpl_Include_Http=No -XAgpl_Include_Gtk=Yes -XAgpl_Include_Concorde=Yes -XAgpl_Include_Boost=Yes -XAgpl_Link=Static_Library

all:
	# NOTE: you don't need to build the library first. Just "with" the project file in your project and choose the appropriate linking type.
	#
	# gprmake builds the C and C++ files
	#
	gprmake ${PARAMS}
	#
	# gnatmake builds all the Ada files and does the linking
	# I think gprmake should do this too, since it does when a main procedure is specified. Bug?
	#
	gnatmake ${PARAMS}

fix:
	# sometimes depending of previous builds, some C object files are not properly included. Use this to force recompilation of only C sources.
	rm -f obj/*.d

clean:
	rm -f obj/* libstatic/* libdynamic/*
	make -C hungarian clean
