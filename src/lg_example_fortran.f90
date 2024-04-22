! #####################################################################
! This file has examples of dotask and get info routines
! Added routines for testing strings

      subroutine test_string(cbuf) 
! #####################################################################
!     PURPOSE -
!        test string passing between C and fortran calls 
!     using print to avoid formatting
! #####################################################################

      implicit none
      character*132 cbuf
      integer icharlnf, ilen
     
      ilen = icharlnf(cbuf) 
      print*
      print*,"test_string() received string:"
      print*,"length: ",icharlnf(cbuf)
      print*,"string: ",cbuf(1:ilen)

      return
      end

!
!
      subroutine lg_example_fortran(ierror) 
! #####################################################################
!
!     PURPOSE -
!        Use lagrit fortran calls 
!        dotask
!        cmo_get_info for mesh data
!
!     See C++ wrappers in lg_example.cpp
!
! #####################################################################

      implicit none

      integer ierror

! Define variables
      integer i, ilen,ityp, ierr, ierrw, leni,itype,icscode 
      integer nnodes,nelements,ndim,nper
      integer ival,itmp

      real*8 xn,yn,zn,xx,yx,zx
      real*8 xreal
      real*8 xarray(1)

! cray style pointers for memory managed arrays

      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*)
      
      pointer (iprout_ptr,rout_ptr),(ipxtmp_ptr,xtmp_ptr)
      real*8 rout_ptr(*), xtmp_ptr(*)

! testing for  call cmo_get_attinfo('epsilonl',cmoname,
!           iout,dsmin,cout, ipout, ilen,ityp,ierr)
      integer dsmin
      integer iout
      pointer (ipout,out)
      real*8 out(*)

      pointer (ipimt1,imt1)
      pointer (ipiout_ptr,iout_ptr)
      integer iout_ptr(*), imt1(*)

! character buffers and variables
      character*32  cmoname, cattname, cmodf, cout
      character*32  isubname 
      character*132 logmess
      character*8092 cbuf
      integer icharlnf
       

! Begin
      isubname="example_fortran"
      ierror= 0

      write(logmess,'(a)') 'Begin FORTRAN examples for get cmo info.'
      call writloga('default',1,logmess,1,ierrw)

! ---------------------------------------------------------------------
!  Create a mesh object using dotask
!  This will be used for the cmo_get calls
! ---------------------------------------------------------------------

      write(logmess,'(a)') 'Create mesh object using dotask calls.'
      call writloga('default',1,logmess,1,ierrw)

!  Delete possible mesh object first
!  Create hex mesh named tmp_hex
!  Set node imt1 to 1 and set boundary tags in itp1

      cmoname="tmp_hex"
      cbuf='cmo/delete/tmp_hex ; finish '
      call dotaskx3d(cbuf,ierr)
      cbuf = 'cmo/create/tmp_hex/ / /hex ; finish '
      call dotaskx3d(cbuf,ierr)
      cbuf = 'createpts/brick/xyz/3,3,3 /1.,2.,3./' // &
             '1.5,2.5,3.5/ ; finish '
      call dotaskx3d(cbuf,ierr)

      if(ierr .ne. 0) then
          call x3d_error(isubname, cmoname )
          ierror = -1
          goto 9999
      endif
      cbuf='cmo/setatt/tmp_hex/imt/1 ; resetpts/itp ; finish'
      call dotaskx3d(cbuf,ierr)
      cbuf='cmo/status/tmp_hex ; finish'
      call dotaskx3d(cbuf,ierr)

      ival = 0
      itmp = 0
      xreal = 0.

! ---------------------------------------------------------------------
!     Get mesh info integer values using cmo_get_intinfo
!     integer type INT assigned to integer 
! ---------------------------------------------------------------------

      write(logmess,'(a,a)') "CMO name: ",cmoname 
      call writloga('default',1,logmess,1,ierrw)

      write(logmess,'(a)') 'CMO_GET_INTINFO for type INT.'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a)') 'integer type INT assigned to integer.'
      call writloga('default',0,logmess,0,ierrw)

      call cmo_get_intinfo('nnodes',cmoname,nnodes,ilen,ityp,ierr)
      if(nnodes .le. 0)then
        write(logmess,'(a)') 'ERROR: No nodes in mesh object!'
        call writloga('default',1,logmess,0,ierrw)
        write(logmess,'(a)') 'RETURN NO ACTION'
        call writloga('default',0,logmess,1,ierrw)
        go to 9999
      endif
      write(logmess,'(a,i12)') "return nnodes: ",nnodes 
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5,i5,i5)') "ilen, ityp, ierr: ", & 
                      ilen,ityp,ierr
      call writloga('default',0,logmess,0,ierrw)

      call cmo_get_intinfo('nelements',cmoname,nelements, &
            ilen,ityp,ierr)
      call cmo_get_intinfo('ndimensions_geom',cmoname,ndim, &
            ilen,ityp,ierr)
      call cmo_get_intinfo('nodes_per_element',cmoname,nper, &
            ilen,ityp,ierr)

      write(logmess,'(a,i12)') "return nelements: ",nelements 
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5)') "return ndimensions_geom: ",ndim 
      call writloga('default',0,logmess,0,ierrw)

      
! ----------------------------------------------------------
!     Get mesh VINT arrays and INT values using cmo_get_info 
!     type VINT assigned to pointer real*8
!     this routine is the most commonly used in routines
! ----------------------------------------------------------

      write(logmess,'(a)')'CMO_GET_INFO for type INT'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a)') & 
      'real*8 pointer assigned to integer with cmo_attparam_idefault.'
      call writloga('default',0,logmess,0,ierrw)

!     INT scalar values will be correct using integer data

      call cmo_get_info('nnodes',cmoname,nnodes,ilen,ityp,ierr)
      if(nnodes .le. 0)then
        write(logmess,'(a)') 'ERROR: No nodes in mesh object!'
        call writloga('default',1,logmess,0,ierrw)
        write(logmess,'(a)') 'RETURN NO ACTION'
        call writloga('default',0,logmess,1,ierrw)
        go to 9999
      endif
      write(logmess,'(a,i12)') "return nnodes: ",nnodes
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5,i5,i5)') "ilen, ityp, ierr: ", &
                      ilen,ityp,ierr
      call writloga('default',0,logmess,0,ierrw)

      call cmo_get_info('nelements',cmoname,nelements, &
            ilen,ityp,ierr)
      call cmo_get_info('ndimensions_geom',cmoname,ndim, &
            ilen,ityp,ierr)

      write(logmess,'(a,i12)') "return nelements: ",nelements
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5)') "return ndimensions_geom: ",ndim
      call writloga('default',0,logmess,0,ierrw)

!     VINT memory array 
!     using mmfindbk to assign memory to pointer real*8 

      write(logmess,'(a)') 'CMO_GET_INFO for type VINT'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a)') &
      'real*8 pointer assigned to integer pointer with mmfindbk.'
      call writloga('default',0,logmess,0,ierrw)

      call cmo_get_info('imt1',cmoname,ipimt1,ilen,ityp,ierr)
      write(logmess,'(a,i5,i5,i5)') "return imt(1:3): ", &
                      imt1(1),imt1(2),imt1(3)
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5,i5,i5)') "ilen, ityp, ierr: ", &
                      ilen,ityp,ierr
      call writloga('default',0,logmess,0,ierrw)


!     VDOUBLE memory array
!     using mmfindbk to assign memory to pointer real*8 

      write(logmess,'(a)') 'CMO_GET_INFO for type VDOUBLE'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a)') &
      'real*8 pointer assigned to real*8 pointer with mmfindbk.'
      call writloga('default',0,logmess,0,ierrw)

      call cmo_get_info('xic',cmoname,ipxic,ilen,ityp,ierr)
      write(logmess,'(a,f5.2,f5.2,f5.2)') "return xic(1:3): ", &
                      xic(1),xic(2),xic(3)
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5,i5,i5)') "ilen, ityp, ierr: ", &
                      ilen,ityp,ierr
      call writloga('default',0,logmess,0,ierrw)


!     REAL scalar values will not return correct value
!     write(logmess,'(a)') 'CMO_GET_INFO for type REAL'
!     call writloga('default',1,logmess,0,ierrw)
!     write(logmess,'(a)') &
!     'real assigned to real*8 pointer with cmo_attparam_rdefault.'
!     call writloga('default',0,logmess,0,ierrw)
!
!     cmo_get_info values good, print result shows garbage 
!     call cmo_get_info('xmax',cmoname,xreal,ilen,ityp,ierr)
!
!     write(logmess,'(a,f5.2)') "return xmax: ",xreal
!     call writloga('default',0,logmess,0,ierrw)
!     write(logmess,'(a,i5,i5,i5)') "ilen, ityp, ierr: ", &
!                     ilen,ityp,ierr
!     call writloga('default',0,logmess,0,ierrw)


! ----------------------------------------------------------------------
!     Get mesh info REAL and CHAR using cmo_get_attinfo 
!     Use cmo_get_attinfo
!     Return value in argument based on type, others are ignored 
!     int iout, real*8 rout_ptr, character*32 cout, pointer real*8 out(*)
!     most commonly used for the non-standard attributes
! ----------------------------------------------------------------------

!     REAL values assigned to real*8 rout

      write(logmess,'(a)') 'CMO_GET_ATTINFO for type REAL'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a)') &
      'real*8 assigned to real*8 rout'
      call writloga('default',0,logmess,0,ierrw)

      call cmo_get_attinfo('epsilonl',cmoname,iout,xreal,cout, &
                 ipout, ilen,ityp,ierr)
      write(logmess,'(a,1pe15.7)') "return epsilonl: ",xreal
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5,i5,i5)') "ilen, ityp, ierr: ", &
                      ilen,ityp,ierr
      call writloga('default',0,logmess,0,ierrw)

!     REAL value for the minmax values
      write(logmess,'(a)') 'CMO_GET_ATTINFO for type REAL XMAX'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a)') &
      'real*8 assigned to real*8 rout'
      call writloga('default',0,logmess,0,ierrw)

      call cmo_get_attinfo('xmax',cmoname,iout,xreal,cout, &
                 ipout, ilen,ityp,ierr) 
      write(logmess,'(a,1pe15.7)') "return xmax: ",xreal
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5,i5,i5)') "ilen, ityp, ierr: ", &
                      ilen,ityp,ierr
      call writloga('default',0,logmess,0,ierrw)


!     CHAR values assigned to character*32 cout

      write(logmess,'(a)') 'CMO_GET_ATTINFO for type CHAR'
      call writloga('default',1,logmess,0,ierrw)
      write(logmess,'(a)') &
      'character*32 assigned to character*32 cout'
      call writloga('default',0,logmess,0,ierrw)

      call cmo_get_attinfo('geom_name',cmoname,iout,xreal,cout, &
                            iprout_ptr,ilen,ityp,ierr)
      write(logmess,'(a,a)') "return geom_name: ",cout
      call writloga('default',0,logmess,0,ierrw)
      write(logmess,'(a,i5,i5,i5)') "ilen, ityp, ierr: ",  &
                      ilen,ityp,ierr
      call writloga('default',0,logmess,0,ierrw)



 9999 continue 

      call dotask('cmo/delete/tmp_hex/ ; finish',ierr)

      if (ierror .eq. 0) then
          write(logmess,"(a)")'example_fortran done.'
      else
          write(logmess,"(a,i4)")'example_fortran error: ',ierror 
      endif
      call writloga('default',1,logmess,1,ierrw)

      print*," "
      print*,"Exit example for FORTRAN get_info commands."
      print*,"-----------------------------------------------------"

      return
      end

