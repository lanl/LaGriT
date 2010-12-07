c
c----------------------------------------------------------------
c lagrit.h for Linux 32 bit
C
C New util for 32 and 64 lib indicated by 32 or 64 at name end
C Otherwise the old util library is used
C 
c This is a template for the lagrit program banner
c Substitute the TAG strings with Date and Linux, Darwin, SunOS, or IRIX
c Compile library with updated lagrit.h used in writinit()
c This template is preserved in lagrit.template.h
c
c----------------------------------------------------------------
c
      integer        v_major, v_minor
      parameter      (v_major=2)
      parameter      (v_minor=201)
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/

c     data os_name      /'Linux   '/
c     data os_name      /'Linux64 '/
      data os_name      /'Linux32 '/

      data date_compile /'2010/12/03            '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
