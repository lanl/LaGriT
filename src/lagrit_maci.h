c
c----------------------------------------------------------------
c lagrit.h for mac OSX intel 32 bit 
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
      parameter      (v_minor=100)
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/
c     data os_name      /'Darwini '/
c     data os_name      /'Maci64  '/
      data os_name      /'Maci32  '/
      data date_compile /'2013/05/20  OSX gf4.7 '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
