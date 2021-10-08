c
c----------------------------------------------------------------
c lagrit.h.template 
c
c This is a template for the lagrit program banner
c Substitute the TAG strings with Date and Linux, Darwin etc
c Compile library with updated lagrit.h used in writinit()
c This template is preserved in lagrit.template.h
c Your own date and words can be used on the date_compile line
c
c----------------------------------------------------------------
c
      integer        v_major, v_minor
      parameter      (v_major=3)
      parameter      (v_minor=333)
c
      character*22   date_compile
      character*8    os_name
      character*16   my_name
c
      data my_name      /'lagritgen'/
      data os_name      /'OSTAG   '/
      data date_compile /'DATETAG               '/
      data date_compile /'2021/10/8  release    '/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
