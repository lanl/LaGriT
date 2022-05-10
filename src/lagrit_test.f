      subroutine lagrit_test(imsgin,xmsgin,cmsgin,msgtyp,nwds,ierror)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Simple test commands for after compile
C        This file can be copied to user_sub.f to help get started
C        writing user routine that will be parsed as a command.
C
C     INPUT ARGUMENTS -
C
C        imsgin - integer array of tokens returned by parser
C        xmsgin - real array of tokens returned by parser
C        cmsgin - character array of tokens returned by parser
C        msgtyp - integer array of token types returned by parser
C
C     OUTPUT ARGUMENTS -
C
C        ierror - 0 for successful completion - -1 otherwise
C
C
C #####################################################################
      implicit none
C
C Define arguments 
      character*32 cmsgin(nwds)
      integer imsgin(nwds),msgtyp(nwds)
      real*8  xmsgin(nwds)
      integer nwds,ierror

C Define variables for this subroutine
      integer nnodes, nelements, iout
      integer i, ilen, ityp, ierr, ierrw
      real*8 xreal

C cray style pointers for memory managed arrays
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)

      pointer (ipimt1, imt1)
      integer imt1(*)

      pointer (iprout,rout)
      real*8 rout(*)

C character buffers and variables
      character*32  cmoname, cattname, cout
      character*32  isubname 
      character*132 logmess
      character*8092 cbuf
      integer icharlnf
       
C EXAMPLE CODE ------------------------------------------------
C This example creates a mesh object 
C cmo_get_ commands are used to get information from a mesh object
C Insert or modify code here to handle user coded subroutines

      isubname="test_lagrit"
      ierr= 0

      if (nwds.ne.1) then
          write(logmess,'(a)')
     *   'Syntax: test [example with no arguments]' 
         call writloga('default',1,logmess,0,ierrw)
         go to 9999
      endif 

C     Create a mesh object to use in this example
      write(logmess,'(a)') 'Create mesh object using dotask calls.'
      call writloga('default',1,logmess,1,ierrw)

C     Check exist of cmo name before using it

      cmoname="test_hex"
      call cmo_exist(cmoname,ierr)
      if(ierr.eq.0) then
        write(logmess,'(a)')
     *  'TEST Warning: mesh object test_hex exists.'
        call writloga('default',0,logmess,1,ierrw)
        goto 9999 
      endif

      cbuf = 'cmo/create/test_hex/ / /hex ; finish '
      call dotaskx3d(cbuf,ierr)
      cbuf = 'createpts/brick/xyz/3,3,3 /1.,2.,3./' //
     *       '1.5,2.5,3.5/ ; finish '
      call dotaskx3d(cbuf,ierr)
      if(ierr .ne. 0) then
          call x3d_error(isubname, cmoname )
          ierr = -1
          goto 9999
      endif

C     Set default attributes and report mesh object status
      cbuf='cmo/setatt/test_hex/imt/1 ; resetpts/itp ; finish'
      call dotaskx3d(cbuf,ierr)
      cbuf='cmo/status/test_hex ; finish'
      call dotaskx3d(cbuf,ierr)

C ----------------------------------------------------------
C     Get mesh memory managed arrays and values
C     Use cmo_get_info    for standard mesh arrays
C     Use cmo_get_intinfo for integer values
C     Use cmo_get_attinfo for attributes based on type 
C ----------------------------------------------------------

      call cmo_get_intinfo('nnodes',cmoname,nnodes,ilen,ityp,ierr)
      call cmo_get_intinfo('nelements',cmoname,nelements,ilen,ityp,ierr)

      if(nnodes .le. 0)then
        write(logmess,'(a)') 'ERROR: No nodes in mesh object!'
        call writloga('default',1,logmess,0,ierrw)
        write(logmess,'(a)') 'RETURN NO ACTION'
        call writloga('default',0,logmess,1,ierrw)
        ierr = -1
        go to 9999
      endif

      call cmo_get_info('xic',cmoname,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmoname,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmoname,ipzic,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get xyz')

      call cmo_get_info('imt1',cmoname,ipimt1,ilen,ityp,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'get_info imt1')

      call cmo_get_attinfo('epsilonl',cmoname,iout,xreal,
     *                      cout,iprout,ilen,ityp,ierr)

C ----------------------------------------------------------
C     Report values
C ----------------------------------------------------------
      write(logmess,'(a)') 'Test should show:'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a)') 'nnodes:              27'
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a)') 'nelements:            8'
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a)') 'xic(1:3):  1.00 1.25 1.50'
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a)') 'imt(1:3):     1    1    1'
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a)') 'epsilonl:   1.9229627E-13'
      call writloga('default',0,logmess,1,ierrw)

      write(logmess,'(a)') 'Test result:'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a,i12)') "nnodes:    ",nnodes
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i12)') "nelements: ",nelements
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,f5.2,f5.2,f5.2)') "xic(1:3): ",
     *                xic(1),xic(2),xic(3)
      call writloga('default',0,logmess,0,ierrw)
      call cmo_get_info('imt1',cmoname,ipimt1,ilen,ityp,ierr)
      write(logmess,'(a,i5,i5,i5)') "imt(1:3): ",
     *                imt1(1),imt1(2),imt1(3)
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,1pe15.7)') "epsilonl: ",xreal
      call writloga('default',0,logmess,1,ierrw)

 9999 continue 

      cbuf='cmo/delete/test_hex ; finish'
      call dotaskx3d(cbuf,ierr)

      write(logmess,"(a)")'lagrit test done.'
      if (ierr .ne. 0) then
          write(logmess,"(a,i4)")'lagrit test exit with error: ',ierr 
      endif
      call writloga('default',1,logmess,1,ierrw)

      return
      end

