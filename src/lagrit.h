c
c----------------------------------------------------------------
c Auto-generated LaGriT program banner
c
c Substitute the TAG strings with Date and Linux, Darwin, WIN, etc.
c Compile library with updated lagrit.h used in writinit()
c This template is preserved in lagrit.template.h
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
      data my_name      /'lagrit'/
c     os_name is used to find and write OS related files
c     make sure it is a version recognized in Makefile
c     and writinit.f for forming header info
      data os_name      /'Linux'/
c
      data date_compile /'2021/10/08 release'/
c
      integer         NCall
      save            NCall
      character*8     Version
      save            Version
c
c----------------------------------------------------------------
c
