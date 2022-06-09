!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!   f90 wrappers calling cmo_get_info for C codes
!   pass mesh object pointer and scalars
!   These use cmo_get_attinfo for attribute name
!   check return type of data I=1,R=2,C=3,pointer=4
!!! -------------------------------------------------------------------

!!! -------------------------------------------------------------------
!      get INT value  fc_cmo_get_int
!      const char* cmo, const char* att,
!      integer ival
!!! -------------------------------------------------------------------
!!!
       subroutine fc_cmo_get_int(cmoname, attname, ival, ierr)

       implicit none

       !!!! arguments passed from C routine

       character*(*) cmoname, attname
       integer ival,ierr

       !!!! local variables and dummy args for cmo_get_attinfo
       pointer (ipdum, intdum)
       integer intdum(*)
       character*32 cdum
       real*8 xdum
       integer ilocal

       integer i,clen,alen
       integer ilen,ityp,ierror
       integer icharlnf

       !!!! begin
       ierr=0
       clen=icharlnf(cmoname)
       alen=icharlnf(attname)

!       WRITE(*,'(A,1X,A,1X,A)') &
!       'fc_cmo_get_int for cmo: ',cmoname(1:clen),attname(1:alen)

       if (sizeof(ival) .ne. 8) then
         print*,"ERROR: sizeof ival not 8: ",sizeof(ival)
         ierr= -1
         return
       endif

       call cmo_get_attinfo( &
           attname(1:alen), cmoname(1:clen), &
           ilocal,xdum,cdum,ipdum,ilen,ityp,ierror)

       ! INT type 1 and ilen 1 
       if (ityp.ne.1 .or. ilen.ne.1) then
         WRITE(*,'(1X,A)')'Warning: INT type and length should be: 1 1'
         WRITE(*,'(1X,A,I3,I3)')&
         'Got type and length: ',ityp,ilen
       endif

       if (ierror .ne. 0) then
         WRITE(*,'(1X,A)')'ERROR from cmo_get_attinfo: ',ierror
         return
       endif

       ival = ilocal

       return
       end subroutine fc_cmo_get_int

!!! -------------------------------------------------------------------
!      get DOUBLE value  fc_cmo_get_double
!      const char* cmo, const char* att,
!      double xval 
!!! -------------------------------------------------------------------
!!!
       subroutine fc_cmo_get_double(cmoname, attname, xval, ierr)

       implicit none

       !!!! arguments passed from C routine

       character*(*) cmoname, attname
       real*8 xval
       integer ierr

       !!!! local variables
       pointer (ipdum, intdum)
       integer intdum(*)
       integer idum
       character*32 cdum
       real*8 xlocal
       
       integer i,clen,alen
       integer ilen,ityp,ierror
       integer icharlnf


       !!!! begin
       ierr = 0
       clen=icharlnf(cmoname)
       alen=icharlnf(attname)

!       WRITE(*,'(A,1X,A,1X,A)') &
!       'fc_cmo_get_double for cmo: ',cmoname(1:clen),attname(1:alen)

       if (sizeof(xval) .ne. 8) then
         print*,"ERROR: sizeof xval not 8: ",sizeof(xval)
         ierr= -1
         return
       endif

       call cmo_get_attinfo( &
           attname(1:alen), cmoname(1:clen), &
           idum,xlocal,cdum,ipdum,ilen,ityp,ierror)

       ! DOUBLE type 2 and ilen 1
       if (ityp.ne.2 .or. ilen.ne.1) then
         WRITE(*,'(1X,A)') &
         'Warning: DOUBLE type and length should be: 2 1'
         WRITE(*,'(1X,A,I3,I3)')&
         'Got type and length: ',ityp,ilen
       endif

       if (ierror .ne. 0) then
         WRITE(*,'(1X,A)')'ERROR from cmo_get_attinfo: ',ierror
         return
       endif

       xval = xlocal

!DEBUG  WRITE(*,'(1X,A,1pe15.7)')  'xlocal     = ',xlocal
!       WRITE(*,'(1X,A,1pe15.7)')  'xval     = ',xval

       return
       end subroutine fc_cmo_get_double

!!! -------------------------------------------------------------------
!      get VINT pointer  fc_cmo_get_vint
!      const char* cmo, const char* att,
!      long** iptr, long* nlen,
!      size_t cmolen, size_t attlen
!!! -------------------------------------------------------------------

       subroutine fc_cmo_get_vint(cmoname,attname,ptr_int,nlength,ierr)

       implicit none

       !!!! arguments passed from C routine

       character*(*) cmoname, attname
       pointer (ptr_int, ipointee)
       integer ipointee(*)
       integer nlength
       integer ierr

       !!!! local vars
       integer ilocal(*)
       pointer (ip_local, ilocal)

       integer i,clen,alen
       integer ilen,ityp,ierror
       integer icharlnf

       !!!! begin
       ierr = 0
       clen=icharlnf(cmoname)
       alen=icharlnf(attname)

!       WRITE(*,'(A,1X,A,1X,A)') &
!       'fc_cmo_get_vint for cmo: ',cmoname(1:clen),attname(1:alen)

       if (sizeof(ptr_int) .ne. 8) then
         print*,"ERROR: sizeof ptr_int not 8: ",sizeof(ptr_int)
         ierr= -1
         return
       endif
       if (sizeof(nlength) .ne. 8) then
         print*,"ERROR: sizeof nlength not 8: ",sizeof(nlength)
         ierr= -1
         return
       endif

       call cmo_get_info(attname(1:alen),cmoname(1:clen), &
            ip_local,ilen,ityp,ierror)

       ! VINT pointer type 4 and length 
       if (ityp.ne.4 .or. ilen.le.0) then
         WRITE(*,'(1X,A)')'Warning: VINT pointer type should be: 4'
         WRITE(*,'(1X,A,I3,I3)') &
         'Got type and length: ',ityp,ilen
       endif

       if (ierror .ne. 0) then
         WRITE(*,'(1X,A)')'ERROR from cmo_get_attinfo: ',ierror
         return
       endif

       nlength = ilen
       ptr_int = loc(ilocal) 

!DEBUG WRITE(*,'(1X,A,5I6)') 'ilocal    = ', (ilocal(i), i=1,5)
!      WRITE(*,'(1X,A,5I6)') 'ipointee    = ', (ipointee(i), i=1,5)

       return
       end subroutine fc_cmo_get_vint

!!! -------------------------------------------------------------------
!      get VDOUBLE pointer  fc_cmo_get_vdouble
!      const char* cmo, const char* att,
!      double** iptr, long* nlen,
!      size_t cmolen, size_t attlen
!!! -------------------------------------------------------------------

       subroutine fc_cmo_get_vdouble( &
                  cmoname,attname,ptr_dbl,nlength,ierr)

       implicit none

       !!!! arguments passed from C routine

       character*(*) cmoname, attname
       pointer (ptr_dbl, xpointee)
       real*8 xpointee(*)
       integer nlength
       integer ierr

       !!!! local vars
       real*8 xlocal(*)
       pointer (ip_local, xlocal)

       integer i,clen,alen
       integer ilen,ityp,ierror
       integer icharlnf

       !!!! begin
       ierr = 0
       clen=icharlnf(cmoname)
       alen=icharlnf(attname)

!       WRITE(*,'(A,1X,A,1X,A)') &
!       'fc_cmo_get_vdouble for cmo: ',cmoname(1:clen),attname(1:alen)

       if (sizeof(ptr_dbl) .ne. 8) then
         print*,"ERROR: sizeof ptr_dbl not 8: ",sizeof(ptr_dbl)
         ierr= -1
         return
       endif
       if (sizeof(nlength) .ne. 8) then
         print*,"ERROR: sizeof nlength not 8: ",sizeof(nlength)
         ierr= -1
         return
       endif

       call cmo_get_info(attname(1:alen),cmoname(1:clen), &
            ip_local,ilen,ityp,ierror)

       ! VDOUBLE pointer type 4 and length
       if (ityp.ne.4 .or. ilen.le.0) then
         WRITE(*,'(1X,A)')'Warning: VDOUBLE pointer type should be: 4'
         WRITE(*,'(1X,A,I3,I3)')&
         'Got type and length: ',ityp,ilen
       endif

       if (ierror .ne. 0) then
         WRITE(*,'(1X,A)')'ERROR from cmo_get_attinfo: ',ierror
         return
       endif

       nlength = ilen
       ptr_dbl = loc(xlocal)

!DEBUG WRITE(*,'(1X,A,5f5.2)') 'xlocal   = ', (xlocal(i), i=1,5)
!      WRITE(*,'(1X,A,5f5.2)') 'xpointee = ', (xpointee(i), i=1,5)

       return
       end subroutine fc_cmo_get_vdouble

!!! -------------------------------------------------------------------
!    fpass_types
!    test routine for passing scalars and pointers
!
!    C++ declare:
!      void fpass_types_( double** xptr, long** iptr, long* nval, double* xval );
!
!    C++ code section:
!      double xval = 0;
!      long nval = 0;
!      double *xptr;
!      long *iptr;
!      fpass_types_(&xptr, &iptr, &nval, &xval);
!          printf("*xptr = ");
!          for( i = 0; i < nval; i = i + 1 ){
!            printf(" %6.1f ", *(xptr+i));
!          }
!!! -------------------------------------------------------------------

       subroutine fpass_types(ptr_real, ptr_int, nval, xval)

       implicit none

       !!!! arguments passed from C main
       pointer (ptr_real, xpointee)
       real*8 xpointee(*)
       pointer (ptr_int, ipointee)
       integer ipointee(*)

       integer nval
       real*8 xval

       !!!! local vars
       integer i

       real*8 xlocal(5)
       pointer (ptr_local, xlocal)

       !!!! important, pointees need allocated data
       real*8 darray(5)
       integer iarray(5)
       data darray /0.0,1.0,2.0,3.0,4.0/
       data iarray /5,6,7,8,9/

       !!!! begin

       ! assign array length and max value to passed vars
       nval=5
       xval=9.

       print*,"sizeof ptr_local   : ",sizeof(ptr_local)
       print*,"sizeof ptr_real    : ",sizeof(ptr_real)
       print*,"sizeof ptr_int     : ",sizeof(ptr_int)
       print*,"sizeof arg xval    : ",sizeof(xval)
       print*,"sizeof arg nval    : ",sizeof(nval)

       WRITE(*,*)
       WRITE(*,'(1X,A,5F6.1)') 'the data: ', darray
       ptr_local  = LOC(darray)
       ptr_real     = LOC(darray)
       ptr_int     = LOC(iarray)

       ! pointee declared size 5
       WRITE(*,'(1X,A,5F6.1)') 'xlocal = ', xlocal

       ! pointee declared unknown size
       WRITE(*,'(1X,A,5F6.1)') 'xpointee    = ', (xpointee(i), i=1,5)
       WRITE(*,'(1X,A,5I6)') 'ipointee    = ', (ipointee(i), i=1,5)

       xval=xpointee(5)
       WRITE(*,'(1X,A,F6.1)') 'xval    = ',xval
       WRITE(*,'(1X,A,I6)') 'nval    = ',nval

       return
       end subroutine fpass_types

