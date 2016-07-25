c
c----------------------------------------------------------------
c lagrit.h for linux with 32 bit
c
c This is a template for the lagrit program banner
c Substitute the TAG strings with Date and Linux, Darwin etc
c Compile library with updated lagrit.h used in writinit()
c This template is preserved in lagrit.template.h
c
c----------------------------------------------------------------
c
      integer        v_major, v_minor
      parameter      (v_major=3)
      parameter      (v_minor=107)
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/
      data os_name      /'OSTAG   '/
      data date_compile /'DATETAG               '/
      data date_compile /'2015/03/15 RH gf4.5   '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
