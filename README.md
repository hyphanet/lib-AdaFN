This is an Ada library implementing the [FCPv2](https://github.com/freenet/wiki/wiki/FCPv2) protocol for communication with a freenet node.

This library is developed with GNAT and this instructions are specific for that compiler.

Clone with the "--recursive" switch to pull in the agpl submodule:

    git clone --recursive https://github.com/freenet/lib-AdaFN

Building requires the GNU Scientific Library. For `apt` package manager this is:

    sudo apt-get install libgsl-dev

Usage: with the adafn.gpr project file in your own project file.

Alternatively: issue

    make 

to build the example executables (tests folder).

    make lib

to build a statically linkable library.

Binary executables are placed in the `obj` folder.

Tested works with gnat-gpl-2016. [Instructions for installing GNAT GPL](https://bluishcoder.co.nz/2017/04/27/installing-gnat-and-spark-gpl-editions.html). Tested does not work with gnat-4.1/4.2 (`Preelaborable_Initialization` pragma still unimplemented in gnat).
