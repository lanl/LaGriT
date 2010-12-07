      subroutine read_lagrit_geom_old(iunit,cmo,iomode,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C    This routine reads the geometry information, surfaces,
C    regions and mregions from a file
C    Note: This file has been modified to avoid compiler errors
C    with gfortran that does not allow multiple instances of a pointer name
C    this uses new routines sbnread_xval sbnread_cval sbnread_catt sbnread_xatt
C    instead of read loops using pointers idx and catt
C
C
C     INPUT ARGUMENTS -
C
C     iunit  unit number of file
C     cmo    mesh object name
C
C     OUTPUT ARGUMENTS -
C
C
C
C     CHANGE HISTORY -
C
C        $Log: read_lagrit_geom_old.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   13 Apr 2000 10:18:20   dcg
CPVCS    remove duplicate declaration
CPVCS    
CPVCS       Rev 1.0   24 Feb 2000 16:27:28   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.5   Mon Nov 08 12:23:56 1999   dcg
CPVCS    don't abort is missing surface, region or mregion
CPVCS
CPVCS       Rev 1.4   Wed Sep 01 13:23:46 1999   dcg
CPVCS    use lengths with concatenation operator
CPVCS
CPVCS       Rev 1.3   Thu Aug 05 16:42:10 1999   dcg
CPVCS    get more memory when needed for temporary storage
CPVCS
CPVCS       Rev 1.2   Tue May 11 16:50:56 1999   dcg
CPVCS    allow for binary or ascii lagrit dumps
CPVCS
CPVCS       Rev 1.1   Tue Mar 09 15:05:34 1999   dcg
CPVCS     read in cmo and pset, eset info for lagrit dumps
CPVCS
CPVCS       Rev 1.0   Fri Mar 05 11:15:24 1999   dcg
CPVCS    Initial revision.
CC
C#######################################################################
C
      implicit none

C     preprocess include file not used so commented out
C     include 'machine.h'
      include 'geom_lg.h'

C     args
C     read_lagrit_geom_old(iunit,cmo,iomode,ierror)
      character*(*) cmo
      character *(*) iomode
      integer iunit,ierror

C
      integer nwval,nwatt,j,i,incr,length,lengthsparam,ierr,
     *  maxlen,rdeflen
      integer ns,icscode,nodnx,k,lidx,lenid,lengthsb
 
      pointer (ipidx,idx)
      integer idx(*)

      pointer (ipval,xval)
      pointer (ipatt,xatt)
      pointer (ipval,cval)
      pointer (ipatt,catt)
      real*8 xval(2,*)
      real*8 xatt(2,*)
      character*8 cval(2,*)
      character*8 catt(2,*)
 
      character*32 isubname, cname, geom_nm,idsb(2)
      character*132 logmess
C
C#######################################################################
C begin

      ierror=0
      isubname='readlggm'
      geom_nm='-defaultgeom-'
      number_of_geometries=1
 
C read surface information
C add this node to the storage block
c it contains the names of
C the surfaces in the 'val' area. The 'att' area is empty
c read in the node name,  of 'val' and 'att' area
c then read the data types and the data values.
      if(iomode(1:5).eq.'ascii') then
         read(iunit,8) idsb(1),idsb(2),nwval,nwatt
      else
         read(iunit) idsb(1),idsb(2),nwval,nwatt
      endif
      lidx=nwatt+nwval+200
      call mmgetblk('idx',isubname,ipidx,lidx,
     *       1,icscode)
      if(nwval+nwatt.eq.0) go to 50
 8    format (2a32,1x,2(i5,1x))
      lengthsb=2*max(nwatt,nwval,200)
      call mmgetblk('xval',isubname,ipval,lengthsb
     *       ,2,icscode)
      call mmgetblk('xatt',isubname,ipatt,lengthsb
     *       ,2,icscode)
      call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *   ierror)
      do i=1,nwval
         if(nint(xval(1,i)).eq.3) nsurf=nsurf+1
      enddo
c
c  make space for surface info
c
      call mmfindbk('csall',geom_nm,ipcsall,length,ierror)
      if (ierror.ne.0 ) then
         length=nsurf
         call mmgetblk('csall',geom_nm,ipcsall,length,3,ierror)
         call mmgetblk('istype',geom_nm,ipistype,length,3,ierror)
         call mmgetblk('ibtype',geom_nm,ipibtype,length,3,ierror)
         call mmgetblk('sheetnm',geom_nm,ipsheetnm,length,3,ierror)
         call mmgetblk('surfparam',geom_nm,ipsurfparam,20*length,
     *          2,ierror)
         call mmgetblk('offsparam',geom_nm,ipoffsparam,length,1,ierror)
         lengthsparam=20*length
      elseif(length.lt.nsurf) then
         call mmnewlen('csall',geom_nm,ipcsall,nsurf,ierror)
         call mmnewlen('istype',geom_nm,ipistype,nsurf,ierror)
         call mmnewlen('ibtype',geom_nm,ipibtype,nsurf,ierror)
         call mmnewlen('sheetnm',geom_nm,ipsheetnm,nsurf,ierror)
         call mmnewlen('offsparam',geom_nm,ipoffsparam,nsurf,ierror)
         call mmfindbk('surfparam',geom_nm,ipsurfparam,lengthsparam,
     *        ierror)
      else
         call mmfindbk('istype',geom_nm,ipistype,length,ierror)
         call mmfindbk('ibtype',geom_nm,ipibtype,length,ierror)
         call mmfindbk('sheetnm',geom_nm,ipsheetnm,length,ierror)
         call mmfindbk('surfparam',geom_nm,ipsurfparam,lengthsparam,
     *        ierror)
         call mmfindbk('offsparam',geom_nm,ipoffsparam,length,ierror)
      endif
c
c now for each surface
c read in the node id, length of 'val' and 'att' area
C also read surface storage block contents - this is the
c info on what type of surface and boundary type.
c It is stored in the 'att' part, the 'val' part contains
c the surface parameters
C each datum is stored by an type field where 2 means a
C floating point datum is stored , 3 means character data
C 1 should never appear
C
      lastsparam=0
      do ns=1,nsurf
         offsparam(ns)=lastsparam
         if(iomode(1:5).eq.'ascii') then
            read(iunit,8) idsb(1),idsb(2),nwval,nwatt
         else
            read(iunit) idsb(1),idsb(2),nwval,nwatt
         endif
         cname=idsb(2)
         csall(ns) = cname
 
         maxlen=max(2*nwval,2*nwatt)
         if(maxlen.gt.lengthsb) then
            incr=maxlen-lengthsb+100
            call mmincblk('xval',isubname,ipval,incr
     *       ,icscode)
            call mmincblk('xatt',isubname,ipatt,incr
     *       ,icscode)
            lengthsb=lengthsb+incr
         endif
         if(nwatt+nwval+200.gt.lidx) then
            incr=nwatt+nwval+200-lidx
            call mmincblk('idx',isubname,ipidx,incr
     *       ,icscode)
            lidx=lidx+incr
         endif
         call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *      ierror)
         ibtype(ns)=catt(2,1)
         istype(ns)=catt(2,2)
         if(istype(ns)(1:5).ne.'sheet') then
            lastsparam=lastsparam+nwval
            if(lengthsparam.lt.lastsparam) then
               call mmincblk('surfparam',geom_nm,ipsurfparam,500,ierr)
               lengthsparam=lengthsparam+500
            endif
            do i=1,nwval
               surfparam(offsparam(ns)+i)=xval(2,i)
            enddo
         else
            sheetnm(ns)=cval(2,1)
         endif
      enddo
C
C  read region information
C add this node to the storage block. It contains the names of
C the regions in the 'val' area the 'att' area contains number of
c tokens in the definition and number of words in the defn.
c read in the node name, length of 'val' and 'att' area
c then read in the data types and the data values.
C
 50   if(iomode(1:5).eq.'ascii') then
         read(iunit,8) idsb(1),idsb(2),nwval,nwatt
      else
         read(iunit) idsb(1),idsb(2),nwval,nwatt
      endif
      if(nwval+nwatt.eq.0) go to 100
      maxlen=max(2*nwval,2*nwatt)
      if(maxlen.gt.lengthsb) then
         incr=maxlen-lengthsb+100
         call mmincblk('xval',isubname,ipval,incr
     *       ,icscode)
         call mmincblk('xatt',isubname,ipatt,incr
     *       ,icscode)
         lengthsb=lengthsb+incr
      endif
      if(nwatt+nwval+200.gt.lidx) then
         incr=nwatt+nwval+200-lidx
         call mmincblk('idx',isubname,ipidx,incr
     *       ,icscode)
         lidx=lidx+incr
      endif
      nregs=0
      call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *      ierror)
      do i=1,nwval
         if(nint(xval(1,i)).eq.3) nregs=nregs+1
      enddo
C
c  make space for region info
c
      call mmfindbk('cregs',geom_nm,ipcregs,length,ierror)
      if(ierror.ne.0) then
         length=nregs+1
         call mmgetblk('cregs',geom_nm,ipcregs,length,3,ierror)
         call mmgetblk('offregdef',geom_nm,ipoffregdef,length,1,ierror)
         call mmgetblk('ndefregs',geom_nm,ipndefregs,length,1,ierror)
         call mmgetblk('regdef',geom_nm,ipregdef,10*length,3,ierror)
         rdeflen=10*length
      elseif ( length.lt.(nregs+1)) then
         length=nregs+1
         call mmnewlen('cregs',geom_nm,ipcregs,length,ierror)
         call mmnewlen('offregdef',geom_nm,ipoffregdef,length,ierror)
         call mmnewlen('ndefregs',geom_nm,ipndefregs,length,ierror)
         call mmnewlen('regdef',geom_nm,ipregdef,10*length,ierror)
         rdeflen=10*length
      else
         call mmfindbk('cregs',geom_nm,ipcregs,length,ierror)
         call mmfindbk('offregdef',geom_nm,ipoffregdef,length,ierror)
         call mmfindbk('ndefregs',geom_nm,ipndefregs,length,ierror)
         call mmfindbk('regdef',geom_nm,ipregdef,rdeflen,ierror)
      endif
      lastregdef=0
      maxdef=0
      do k=1,nregs
         offregdef(k)=lastregdef
         if(iomode(1:5).eq.'ascii') then
            read(iunit,8) idsb(1),idsb(2),nwval,nwatt
         else
            read(iunit) idsb(1),idsb(2),nwval,nwatt
         endif
         cname=idsb(2)
         cregs(k)=cname
         maxlen=max(2*nwval,2*nwatt)
         if(maxlen.gt.lengthsb) then
            incr=maxlen-lengthsb+100
            call mmincblk('xval',isubname,ipval,incr
     *       ,icscode)
            call mmincblk('xatt',isubname,ipatt,incr
     *       ,icscode)
            lengthsb=lengthsb+incr
         endif
         if(nwatt+nwval+200.gt.lidx) then
            incr=nwatt+nwval+200-lidx
            call mmincblk('idx',isubname,ipidx,incr
     *       ,icscode)
            lidx=lidx+incr
         endif
         call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *      ierror)
         ndefregs(k)=nwval
         if(rdeflen.le.ndefregs(nregs)+offregdef(nregs)) then
               call mmincblk('regdef',geom_nm,ipregdef,1000,ierror)
               rdeflen=rdeflen+1000
         endif
         maxdef=max(maxdef,nwval)
         do i=1,nwval
            regdef(offregdef(nregs)+i)=
     *            cval(2,i)
         enddo
         lastregdef=lastregdef+nwval
 
      enddo
C
C  read mregion storage blocks
C this node in the storage block contains the names of
C the regions in the 'val' area the 'att' area contains number of
c tokens in the definition and number of words in the defn
c write out the node name, length of 'val' and 'att' area
c then write out the data types and the data values.
C
100   if(iomode(1:5).eq.'ascii') then
         read(iunit,8) idsb(1),idsb(2),nwval,nwatt
      else
         read(iunit) idsb(1),idsb(2),nwval,nwatt
      endif
      if(nwval+nwatt.eq.0) go to 9999
      maxlen=max(2*nwval,2*nwatt)
      if(maxlen.gt.lengthsb) then
         incr=maxlen-lengthsb+100
         call mmincblk('xval',isubname,ipval,incr
     *       ,icscode)
         call mmincblk('xatt',isubname,ipatt,incr
     *       ,icscode)
         lengthsb=lengthsb+incr
      endif
      if(nwatt+nwval+200.gt.lidx) then
         incr=nwatt+nwval+200-lidx
         call mmincblk('idx',isubname,ipidx,incr
     *       ,icscode)
         lidx=lidx+incr
      endif
      nmregs=0
      call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *      ierror)
      do i=1,nwval
         if(nint(xval(1,i)).eq.3) nmregs=nmregs+1
      enddo
      lastmregdef=0
      length=nmregs
      maxmdef=0
      call mmfindbk('cmregs',geom_nm,ipcmregs,length,ierror)
      if(ierror.ne.0) then
         length=nmregs+1
         call mmgetblk('cmregs',geom_nm,ipcmregs,length,3,ierror)
         call mmgetblk('offmregdef',geom_nm,ipoffmregdef,length,1,
     *    ierror)
         call mmgetblk('ndefmregs',geom_nm,ipndefmregs,length,1,ierror)
         call mmgetblk('mregdef',geom_nm,ipmregdef,10*length,3,ierror)
         call mmgetblk('matregs',geom_nm,ipmatregs,length,1,ierror)
         rdeflen=length*10
      elseif ( length.lt.(nmregs+1)) then
         length=nmregs+1
         call mmnewlen('cmregs',geom_nm,ipcmregs,length,ierror)
         call mmnewlen('offmregdef',geom_nm,ipoffmregdef,length,ierror)
         call mmnewlen('ndefmregs',geom_nm,ipndefmregs,length,ierror)
         call mmnewlen('mregdef',geom_nm,ipmregdef,10*length,ierror)
         call mmnewlen('matregs',geom_nm,ipmatregs,length,ierror)
         rdeflen=length*10
      else
         call mmfindbk('cmregs',geom_nm,ipcmregs,length,ierror)
         call mmfindbk('offmregdef',geom_nm,ipoffmregdef,length,ierror)
         call mmfindbk('ndefmregs',geom_nm,ipndefmregs,length,ierror)
         call mmfindbk('mregdef',geom_nm,ipmregdef,rdeflen,ierror)
         call mmfindbk('matregs',geom_nm,ipmatregs,length,ierror)
      endif
      maxmdef=0
      do k=1,nmregs
         offmregdef(k)=lastmregdef
         if(iomode(1:5).eq.'ascii') then
            read(iunit,8) idsb(1),idsb(2),nwval,nwatt
         else
            read(iunit) idsb(1),idsb(2),nwval,nwatt
         endif
         cname=idsb(2)
         cmregs(k)=cname
         maxlen=max(2*nwval,2*nwatt)
         if(maxlen.gt.lengthsb) then
            incr=maxlen-lengthsb+100
            call mmincblk('xval',isubname,ipval,incr
     *       ,icscode)
            call mmincblk('xatt',isubname,ipatt,incr
     *       ,icscode)
            lengthsb=lengthsb+incr
         endif
         if(nwatt+nwval+200.gt.lidx) then
            incr=nwatt+nwval+200-lidx
            call mmincblk('idx',isubname,ipidx,incr
     *       ,icscode)
            lidx=lidx+incr
         endif
         call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *      ierror)
         ndefmregs(k)=nwval
         if(rdeflen.le.ndefmregs(nmregs)+offmregdef(nmregs)) then
               call mmincblk('mregdef',geom_nm,ipmregdef,1000,ierror)
               rdeflen=rdeflen+1000
         endif
         maxmdef=max(maxmdef,nwval)
         do i=1,nwval
            mregdef(offmregdef(nmregs)+i)=
     *            cval(2,i)
         enddo
         lastmregdef=lastmregdef+nwval
 
      enddo
 
c
c  read in material number for each mregion
c
      if(iomode(1:5).eq.'ascii') then
         read(iunit,22) (matregs(j),j=1,nmregs)
      else
         read(iunit) (matregs(j),j=1,nmregs)
      endif
 22   format (10i10)
 
C
 9999 call mmrelprt(isubname,icscode)
      return
      end
 
C
      subroutine sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *   ierror)
c
c#######################################################################
c
c     purpose -
c
c        this routine reads a storage block from iunit
c
c             ..........................................................
c
c     input arguments -
c
C        iunit   - unit number of file to read
C        ipval,nwval,ipatt,nwatt  - pointers and length of contents
C           of storage block
c        ipidx -  pointer to temporary array
c        iomode - 'binary' or 'ascii'
C
c     output arguments -
c
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
c
c
c#######################################################################
c
      integer iunit,ierror,ncount,i,j
      character *(*) iomode
      pointer(ipval,xval)
      pointer(ipatt,xatt)
      real*8 xval(2,*),xatt(2,*)
      pointer (ipidx,idx)
      integer idx(*)
c#######################################################################
c
      call sbnread_xval(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *   ierror)
      nwval=ncount
      call sbnread_cval(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *   ierror)
      do j=1,ncount
         if(idx(j).ne.0) xval(1,abs(idx(j)))=float(sign(3,idx(j)))
      enddo
      nwval=ncount+nwval
      call sbnread_xatt(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *   ierror)
      nwatt=ncount
      call sbnread_catt(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *   ierror)
      do j=1,ncount
         if(idx(j).ne.0) xatt(1,abs(idx(j)))=float(sign(3,idx(j)))
      enddo
      nwatt=ncount+nwatt
      return
      end
 
      subroutine sbnread_xval(iunit,ipval,nwval,ipatt,nwatt,ipidx,
     *     iomode, ierror)

      integer iunit,ierror,ncount,i,j
      character *(*) iomode
      pointer(ipval,xval)
      real*8 xval(2,*)
      pointer (ipidx,idx)
      integer idx(*)

 1005 format (i10)
 1010 format (10i10)
 1020 format (6e18.11)
 1030 format (12a8)

      if(iomode(1:5).eq.'ascii') then
         read(iunit,1005) ncount
         read(iunit,1010) (idx(i),i=1,ncount)
         read(iunit,1020) (xval(2,abs(idx(i))),i=1,ncount)
      else
         read(iunit) ncount,(idx(i),i=1,ncount),
     *                (xval(2,abs(idx(i))),i=1,ncount)
      endif
      do j=1,ncount
         if(idx(j).ne.0) xval(1,abs(idx(j)))=float(2)
      enddo
      return
      end
      
      subroutine sbnread_cval(iunit,ipval,nwval,ipatt,nwatt,ipidx,
     *     iomode, ierror)
      integer iunit,ierror,ncount,i,j
      character *(*) iomode
      pointer(ipval,cval)
      character*8 cval(2,*)
      pointer (ipidx,idx)
      integer idx(*)

 1105 format (i10)
 1110 format (10i10)
 1120 format (6e18.11)
 1130 format (12a8)

      if(iomode(1:5).eq.'ascii') then
         read(iunit,1105) ncount
         read(iunit,1110) (idx(i),i=1,ncount)
         read(iunit,1130) (cval(2,abs(idx(i))),i=1,ncount)
      else
         read(iunit) ncount,(idx(i),i=1,ncount),
     *                (cval(2,abs(idx(i))),i=1,ncount)
      endif
      return
      end

      subroutine sbnread_xatt(iunit,ipval,nwval,ipatt,nwatt,ipidx,
     *     iomode, ierror)

      integer iunit,ierror,ncount,i,j
      character *(*) iomode
      pointer(ipatt,xatt)
      real*8 xatt(2,*)
      pointer (ipidx,idx)
      integer idx(*)
      
 1205 format (i10)
 1210 format (10i10)
 1220 format (6e18.11)
 1230 format (12a8)

      if(iomode(1:5).eq.'ascii') then
          read(iunit,1205) ncount
          read(iunit,1210) (idx(i),i=1,ncount)
          read(iunit,1220) (xatt(2,abs(idx(i))),i=1,ncount)
      else
          read(iunit) ncount,(idx(i),i=1,ncount),
     *                (xatt(2,abs(idx(i))),i=1,ncount)
      endif
      do j=1,ncount
         if(idx(j).ne.0) xatt(1,abs(idx(j)))=float(2)
      enddo
      return
      end

      subroutine sbnread_catt(iunit,ipval,nwval,ipatt,nwatt,ipidx,
     *     iomode, ierror)

 1305 format (i10)
 1310 format (10i10)
 1320 format (6e18.11)
 1330 format (12a8)

      integer iunit,ierror,ncount,i,j
      character *(*) iomode
      pointer(ipatt,catt)
      character*8 catt(2,*)
      pointer (ipidx,idx)
      integer idx(*)
      
      if(iomode(1:5).eq.'ascii') then
          read(iunit,1305) ncount
          read(iunit,1310) (idx(i),i=1,ncount)
          read(iunit,1330) (catt(2,abs(idx(i))),i=1,ncount)
      else
          read(iunit) ncount,(idx(i),i=1,ncount),
     *                (catt(2,abs(idx(i))),i=1,ncount)
      endif
      return
      end

