c
c----------------------------------------------------------------
c lagrit.h for mac OSX powerpc 32 bit 
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
      parameter      (v_minor=006)
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/
c     data os_name      /'Darwin  '/
c     data os_name      /'Mac64   '/
      data os_name      /'Mac32   '/
      data date_compile /'2012/07/20            '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
