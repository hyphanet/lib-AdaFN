.PHONY: all lib clean

PARAMS=-XHungarian_Include_Base=False -XAgpl_Include_PngIO=False -XAgpl_Include_Boost=False -XAgpl_Include_Concorde=False

PARAMSBIN=${PARAMS} -Padafn -XAdaFN_Link=Dependencies

PARAMSLIB=${PARAMS} -Padafn -XAdaFN_Link=Static_Library

all:
	gprbuild ${PARAMSBIN} adafn_getkey
	gprbuild ${PARAMSBIN} adafn_putfile
	gprbuild ${PARAMSBIN} adafn_putdir
	gprbuild ${PARAMSBIN} adafn_test

lib:
	# NOTE: you don't need to build the library. Just "with" the project file in your project and choose the appropriate linking type.
	# This makefile is provided only as a way to check the build and build the utils
	#
	# gprbuild builds the C and C++ files
	#
	gprbuild ${PARAMSLIB}
	#
	# gnatmake builds all the Ada files and does the linking
	# I think gprbuild should do this too, since it does when a main procedure is specified. Bug?
	#
	gnatmake ${PARAMSLIB}

clean:
	gnatclean -r -q ${PARAMSBIN}
	gnatclean -r -q ${PARAMSLIB}
