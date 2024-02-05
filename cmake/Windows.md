# Windows Helps and Hints

LaGriT V3.3.3 was not tested on Windows. 
Earlier versions of LaGriT worked on Windows using CYGWIN.

## LaGrit V3.3.3 Windows 10 with gcc/gfortran
From User cean wang ceanwang@gmail.com Dec 11 2023

Cean compiled LaGriT under WIndows 10 with gcc/gfortran.

Cean followed instructions at https://lanl.github.io/LaGriT/pages/docs/build.html
and copied the example driver function into adrivgen.f and had a successful compile.

Cean noticed an error message suggesting to add a '' at the end of a function name, that is: initlagrit to initlagrit. This is normal for C & Fortran mixed programing.


## LaGriT V3.2 with C++ main on Windows with Microsoft Visual Studio

GOLDER Project has a forked version of LaGriT V3.2 that builds on Windows and includes instructions for their work: See repo at https://github.com/daniellivingston/LaGriT-Windows

Daniel Livingston developed a branch working on windows that is an update to the Golder project. See https://github.com/lanl/LaGriT/tree/windows

The code on the windows branch uses C++ as main calling LaGriT fortran and c codes. The branch has not been merged with master because of compile failures on Linux and Mac systems.

## ERROR: Undefined references to srand48() and drand48()

Complaint using WIndows 10 with gcc/gfortran edit src/metis_lg.c
Functions srand48() and drand48() are Linux or some flavor of Windows with MinGW as compiler system.
Solution at https://stackoverflow.com/questions/74274179/i-cant-use-drand48-and-srand48-in-c

Note metis codes are no longer used and will be updated or removed.




*Check LaGriT Github Issues for help and hints for  Windows systems.*
