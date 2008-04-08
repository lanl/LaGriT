c
c----------------------------------------------------------------
c lagrit.h for Darwin
c This is a template for the lagrit program banner
c Substitute the TAG strings with Date and Linux, Darwin, SunOS, or IRIX
c Compile library with updated lagrit.h used in writinit()
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
      parameter      (OSName='Darwin')
c
      character*22    Compiled
      parameter      (Compiled='2008/04/08              ')
c
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
