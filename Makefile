.PHONY: all lib clean

PARAMS=-XHungarian_Include_Base=No -XAgpl_Include_PngIO=No -XAgpl_Include_Boost=No -XAgpl_Include_Concorde=No

PARAMSBIN=${PARAMS} -Padafn -XAdaFN_Link=Dependencies

PARAMSLIB=${PARAMS} -Padafn -XAdaFN_Link=Static_Library

all:
	gprmake ${PARAMSBIN} adafn_getkey
	gprmake ${PARAMSBIN} adafn_putfile
	gprmake ${PARAMSBIN} adafn_putdir
	gprmake ${PARAMSBIN} adafn_test

lib:
	# NOTE: you don't need to build the library. Just "with" the project file in your project and choose the appropriate linking type.
	# This makefile is provided only as a way to check the build and build the utils
	#
	# gprmake builds the C and C++ files
	#
	gprmake ${PARAMSLIB}
	#
	# gnatmake builds all the Ada files and does the linking
	# I think gprmake should do this too, since it does when a main procedure is specified. Bug?
	#
	gnatmake ${PARAMSLIB}

clean:
	gnatclean -r -q ${PARAMSBIN}
	gnatclean -r -q ${PARAMSLIB}
