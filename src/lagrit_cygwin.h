c
c----------------------------------------------------------------
c lagrit.h for Cygwin 64 bit
C
c This is a template for the lagrit program banner
c Substitute the TAG strings with Date and Linux, Darwin, SunOS, IRIX or Cygwin
c Compile library with updated lagrit.h used in writinit()
c This template is preserved in lagrit.template.h
c
c----------------------------------------------------------------
c
      integer        v_major, v_minor
      parameter      (v_major=3)
      parameter      (v_minor=200)
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/

      data os_name      /'Cygwin64 '/
c     data os_name      /'Cygwin32 '/

      data date_compile /'2015/03/15 WIN7       '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c

