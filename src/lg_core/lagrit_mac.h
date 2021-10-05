c
c----------------------------------------------------------------
c lagrit.h for mac OS 11.x Lion  arch x86_64 
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
      parameter      (v_minor=202)
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/

c     os_name is used to find and write OS related files
c     make sure it is a version recognized in Makefile
c     and writinit.f for forming header info
      data os_name      /'MacOS11 '/
c
      data date_compile /'2017/05/31 release    '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
