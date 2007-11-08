      subroutine cmo_minmax(cmo,attnam,rmin,rmax,
     >                      imin, imax,
     >                      mpno, mpary,
     >                      itypatt, ierr_return)
 
C
C
C #####################################################################
C
C     PURPOSE -
C
C         This routine returns the min and max values for x,y,z
C         Should be changed to get min max of any attribute
C
C      INPUT ARGUMENTS -
C
C         cmo    - (character) Mesh Object Name.
C         attnam - (character) Mesh Attribute Name.
c         mpary - array of nodes to be smoothed
c         mpno - length of mpary
C
C     OUTPUT ARGUMENTS -
C
C        rmin   - (real) Min value of attribute.
C        rmax   - (real) Max value of attribute.
C        imin   - (integer) Min value of attribute.
C        imax   - (integer) Max value of attribute.
C        itypatt - (integer) 1=DOUBLE 2=INTEGER
C        ierr_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C        $Log: cmo_minmax.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
CPVCS
CPVCS    Original version. - tcherry 9/97
CPVCS
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C ARGS
      character*(*) cmo, attnam
      real*8 rmin,rmax
      integer imin,imax,itypatt,mpno,index,ierr_return
      integer mpary(*) 
C LOCAL
      integer i, ipt,k,len,ilen,ityp,irank,length,ierr, ics, ierrw
      integer icharlnf
 
      pointer (iprwork, rwork)
      real*8  rwork(*)
      pointer (iprwork, iwork)
      integer iwork(*)
 
      character*132 logmess
 
C MEMORY
      character*32  clen, ctype, crank,cinter,cpers,cio
 
 
c BEGIN
      ierr_return=0
      ics=0
C
c  check that there are points -- if none return
      if (mpno.eq.0) then
        write(logmess,'(a)') 'npoints = 0 in subroutine cmo_minmax'
        call writloga('default',0,logmess,0,ierrw)
        ierr_return=1
        go to 9999
      endif
 
C  get type of attribute from mesh object
      len=icharlnf(attnam)
      call cmo_get_attparam(attnam(1:len),cmo,index,ctype,crank,
     *    clen,cinter,cpers,cio,ierr_return)
      if(ierr_return.ne.0) go to 9999
      if(ctype(1:4).eq.'VINT') then
         itypatt=1
      elseif (ctype(1:7).eq.'VDOUBLE') then
         if(attnam(1:len).eq.'isetwd'.or.
     *      attnam(1:len).eq.'xtetwd') then
            itypatt=1
         else
            itypatt=2
         endif
      elseif (ctype(1:3).eq.'INT') then
        call cmo_get_intinfo(attnam(1:len),cmo,imin,ilen,ityp,ics)
        imax=imin
        itypatt=1
        goto 9999
      elseif (ctype(1:4).eq.'REAL') then
        call cmo_get_attinfo(attnam(1:len),cmo,imin,rmin,cpers,
     *     iprwork,ilen,ityp,ics)
        rmax=rmin
        itypatt=2
        goto 9999 
      elseif (ctype(1:9).eq.'CHARACTER') then
        itypatt=3
        go to 9999
      elseif (ctype(1:5).eq.'VCHAR') then
        itypatt=4
        go to 9999
      else
        ierr_return=1
        go to 9999
      endif
 
c  get attribute field from mesh object
      call mmgetpr(attnam(1:len),cmo,iprwork,ics)
      call cmo_get_intinfo(crank,cmo,irank,ilen,ityp,ierr)
      if (ilen.eq.0 .or. ierr.ne.0) 
     *    call x3d_error('0 len minmax attribute: ',attnam)
      if (ics.ne.0 ) call x3d_error('get minmax attribute: ',attnam)
      if (ics.ne.0 .or. ilen.eq.0 .or. ierr.ne.0 ) goto 9999
      call cmo_get_intinfo(clen,cmo,length,ilen,ityp,ierr)
      if (ics.ne.0 .or. ierr.ne.0 ) then
         call x3d_error('minmax attribute length: ',clen)
         goto 9999
      endif
 
      if(length.eq.0) then
        write(logmess,'(a)')'attribute len = 0 in subroutine cmo_minmax'
        call writloga('default',0,logmess,0,ierrw)
        ierr_return=1
        go to 9999
      endif
 
      if (itypatt.eq.1) then
         imin = iwork(mpary(1))
         imax = iwork(mpary(1))
         do i = 1,mpno
           ipt=mpary(i)
           do k=1,irank
             if (iwork((ipt-1)*irank+k) .lt. imin) 
     *          imin = iwork((ipt-1)*irank+k)
             if (iwork((ipt-1)*irank+k) .gt. imax) 
     *          imax = iwork((ipt-1)*irank+k)
           enddo
         enddo
 
      else
         rmin = rwork(mpary(1))
         rmax = rwork(mpary(1))
         do i = 1,mpno
           ipt=mpary(i)
           do k=1,irank
             if (rwork((ipt-1)*irank+k) .lt. rmin) 
     *          rmin = rwork((ipt-1)*irank+k)
             if (rwork((ipt-1)*irank+k) .gt. rmax) 
     *          rmax = rwork((ipt-1)*irank+k)
           enddo
         enddo
       endif
9999  if (ics.ne.0) ierr_return = 1
      return
      end
 
 
C#######################################################################
      subroutine cmo_xyz_minmax(cmonam,
     >            xmin,ymin,zmin,xmax,ymax,zmax,ierr)
C
C      PURPOSE -
C
C         This routine returns the min and max values for x,y,z
C
C      INPUT ARGUMENTS -
C
C         cmonam - (character) Mesh Object Name.
C
C      OUTPUT ARGUMENTS -
C
C         ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C#######################################################################
C
      implicit none
C
C#######################################################################
C ARGS
      character cmonam*32
      real*8 xmin,ymin,zmin,xmax,ymax,zmax
      integer ierr
 
C LOCAL
      integer i
      integer nnodes,length,icmotype
      integer ilen,ityp
 
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8  xic(1000000), yic(1000000), zic(1000000)
 
c BEGIN
 
c  get information from  mesh object
c
      call cmo_get_info('nnodes',cmonam,
     *                  nnodes,length,icmotype,ierr)
      call cmo_get_info('xic',cmonam,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmonam,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmonam,ipzic,ilen,ityp,ierr)
      if (ierr .ne. 0 ) return
 
      xmin = xic(1)
      xmax = xic(1)
      ymin = yic(1)
      ymax = yic(1)
      zmin = zic(1)
      zmax = zic(1)
      do i = 2, nnodes
        if (xic(i) .lt. xmin) xmin = xic(i)
        if (yic(i) .lt. ymin) ymin = yic(i)
        if (zic(i) .lt. zmin) zmin = zic(i)
 
        if (xic(i) .gt. xmax) xmax = xic(i)
        if (yic(i) .gt. ymax) ymax = yic(i)
        if (zic(i) .gt. zmax) zmax = zic(i)
      enddo
 
      return
      end
 
