c
c----------------------------------------------------------------
c lagrit.h for Linux 64 bit
C
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

c     data os_name      /'Linux   '/
      data os_name      /'Linux64 '/
c     data os_name      /'Linux32 '/

c     data date_compile /'2011/09/01  Absoft 11 '/
c     data date_compile /'2015/03/15  RH gf4.5  '/
      data date_compile /'2015/03/15  RH gf4.5  '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
