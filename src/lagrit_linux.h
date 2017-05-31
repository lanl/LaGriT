c
c----------------------------------------------------------------
c lagrit.h for Linux 64 bit on Ubuntu
C
c This is a template for the lagrit program banner
c Substitute the TAG strings with Date and Linux, OSX, SunOS
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

      data os_name      /'Linux64 '/

      data date_compile /'2017/05/31 static '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
