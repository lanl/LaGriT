c
c----------------------------------------------------------------
c lagrit.template.h
c This is a template for lagrit.h for the LaGriT program banner
c Substitute the TAG strings with Date and Linux, Darwin, SunOS, or IRIX
c Compile library with updated lagrit.h and writinit.f 
c This template is preserved in lagrit.template.h
c
c----------------------------------------------------------------
c
      integer         NCall, VMajor, VMinor
      save            NCall
      parameter      (VMajor=2)
      parameter      (VMinor=002)
c
      character*16    MyName
      parameter      (MyName='lagritgen')
c
      character*8    OSName
      parameter      (OSName='OSTAG')
c
      character*22    Compiled
      parameter      (Compiled='DATETAG')
c
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
