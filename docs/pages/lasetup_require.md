X3dsetup Requirements
---------------------

To run lasetup, you will need:

-   the proper version of lasetup. Irix5.3, Irix6.x, Sunultracreator,
    and IBM versions are available.
-   a Unix workstation. You should have no problem with Silicon Graphics
    (SGI) machines. Sun Ultra Creator systems also work. IBM
    workstations have been problematical in the past. You must have the
    3D option. It may or may not work, depending on things that I don't
    understand. I haven't made a DEC version, but it should be possible.
    HP should work someday when HP starts providing OpenGL.
-   the OpenGL libraries and associated include files, to wit:
-   libGL.a, libGLU.a, libGLw.a -or- libGL.so, libGLU.so, libGLw.so
-   the Motif library libXm.a
-   the X Window System libraries, libX11.a and libXt.a

You will also need a 24-bit TrueColor visual, preferably double
buffered, available on your machine. You can find out what is available
by running xdpyinfo. Also the X server must be running the GLX
extension. Extensions are listed near the top of the output of xdpyinfo.

On some systems, you may need to modify your environmental variable
LD\_LIBRARY\_PATH so that all of the libraries can be found at runtime.

To 
*compile
* the code, you will need the above stuff plus lasetup
source.

You may have to change the paths in the makefile, depending on where
your libraries are located. Similarly, you may have to change the
makefiles in each of the lasetup subdirectories. You will also need the
include files:

-   gl.h, glu.h, glx,h, in /usr/include/GL
-   GLwMDrawA.h, in /usr/include/X11/GLw

(On Suns these may be in /usr/openwin/include/....).


*
*
*Or
*
*
*

If you don't have OpenGL and are the adventurous type, you can get and
compile [Brian Paul's Mesa
library](http://www.ssec.wisc.edu/~billh/bp/Mesa30.html), to which you
can link rather than to OpenGL.

Since Mesa doesn't support hardware even if it's on your machine, the
code will run slowly. But this does eliminate the OpenGL requirement.
There is a macro MESALIBS in the makefile that you will have to modify,
but it might help you get started. I have compiled lasetup with Mesa, so
I know it can be done. :)


