c
c----------------------------------------------------------------
c lagrit.h for mac OS 12.x Mountain Lion  arch x86_64 
c
c This is a template for the lagrit program banner
c Substitute the TAG strings with Date and Linux, Darwin, SunOS, or IRIX
c Compile library with updated lagrit.h used in writinit()
c This template is preserved in lagrit.template.h
c
c----------------------------------------------------------------
c
      integer        v_major, v_minor
      parameter      (v_major=3)
      parameter      (v_minor=103)
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/

c     os_name is used to find and write OS related files
c     make sure it is a version recognized in Makefile
c     and writinit.f for forming header info
c     data os_name      /'Darwini '/
      data os_name      /'MacOS12 '/

c     This string is used for writing LaGriT header
c     data date_compile /'2012/07/26 gfort 4.6 '/
      data date_compile /'2015/03/15 x64 GCC4.8'/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
