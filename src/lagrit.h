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

c     data os_name      /'Linux32 '/
      data os_name      /'Linux64 '/

c     data date_compile /'2017/07/10 -g tam'/
c     data date_compile /'2017/07/17 -O tam'/
      data date_compile /'2017/07/17 -O mod'/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
