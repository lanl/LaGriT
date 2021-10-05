      subroutine cmo_readdump_cmo_old(cmo_name,iunit,iomode,ierror)
      implicit none
c
c#######################################################################
c
c     purpose -
c
c        this routine reads a mesh object from iunit - is activated
c        by the read/lagrit command
c
c             ..........................................................
c
c     input arguments -
c
C        cmoname - name of mesh object
C        iunit   - unit number of file to read
C
c     output arguments -
c
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
c
c     change history -
C
C   $Log: cmo_readdump_cmo_old.f,v $
C   Revision 2.00  2007/11/05 19:45:49  spchu
C   Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Feb 28 12:28:12 2000   dcg
CPVCS    make format big enough to hold data
CPVCS    
CPVCS       Rev 1.0   24 Feb 2000 16:27:24   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.4   Fri Nov 05 13:27:02 1999   dcg
CPVCS    remove dictionary dependencies
CPVCS
CPVCS       Rev 1.3   Thu Aug 05 16:40:18 1999   dcg
CPVCS    allow for zero length attributes
CPVCS
CPVCS       Rev 1.2   Tue May 11 16:50:40 1999   dcg
CPVCS    allow for binary or ascii lagrit dumps
CPVCS
CPVCS       Rev 1.1   Tue Mar 23 14:23:48 1999   dcg
CPVCS    use '-def-' for default attribute name
CPVCS
CPVCS       Rev 1.0   Tue Mar 09 15:01:48 1999   dcg
CPVCS    Initial revision.
 
 
c
c#######################################################################
c
c
      integer iunit,j
      character *(*) iomode
      character*132 logmess
      character*512 cbuf
C
      pointer(ipxfield,xfield)
      real*8 xfield(*)
      pointer(ipxfield,ifield)
      integer ifield(*)
C
      integer  ierror
      integer   iend, ii
      integer ics,idum1,ier2,lentype,index,
     * len,ier,attlen,mmlength,icscode,i,
     * irank,nwcmoprm,idcmoprm,nwval,nwatt,nodnx,nnames,
     * iatt_index,length,n2,ierror_return,ilen,itype
c
      pointer (ipval,xval)
      pointer (ipval,cval)
      real*8 xval(2,*)
      CHARACTER*8 cval(2,*)
      pointer(ipatt, xatt)
      pointer(ipatt, catt)
      REAL*8 xatt(2,*)
      CHARACTER*8 catt(2,*)
      pointer (ipidx,idx)
      integer idx(*)
      pointer (ippsetnm,psetnames)
      pointer (ipesetnm,eltsetnames)
      character*32 psetnames(*), eltsetnames(*)
c
      real*8  xdefault,xvalue
      integer icharlnf
c
      character*32  cname, cinterp,cpersis,cioflag,parname
      character*32 isubname,c1,c2,c3,c4,c5,c6,att_name
      character*8 sbname
      character*32 cnameps
      character*5 cnamend
      character*32  cmo_name, crank, ctype,clen,cdef,nodnm,
     *   cvalue
 
 
C
C     ******************************************************************
c     set the memory management path name to be the subroutine name.
      isubname  = 'cmo_readdump_cmo'
 
c
c   loop through attributes one at a time
c
      if(iomode(1:5).eq.'ascii') then
         read(iunit,'(i5)') iend
      else
         read(iunit) iend
      endif
      do ii=1,iend
C
c  read name get type=attyp, length=ilength and rank=irank
c  interpolation, persistence, ioflag and default
c  set values of these fields in the mesh object
c
      if(iomode(1:5).eq.'ascii') then
        read(iunit,'(a32)',end=9999) cname
        read(iunit,10,end=9999) ctype,crank,clen,cinterp,
     *   cpersis,cioflag,xdefault,attlen,irank
      else
        read(iunit,end=9999) cname,ctype,crank,clen,cinterp,
     *   cpersis,cioflag,xdefault,attlen,irank
      endif
  10  format(3a32/3a32,e18.11/2i10)
      len=icharlnf(cname)
c
C  Check if this a new Attribute.
C
      att_name=cname
      call cmo_get_attparam(att_name,cmo_name,index,c1,c2,
     *    c3,c4,c5,c6,icscode)
C
      if(icscode.eq.0) then
C
C...     Existing Attribute.
C
         call cmo_set_attparam(cname,cmo_name,index,ctype,
     *     crank, clen,cinterp,cpersis,cioflag,ierror_return)
 
         if(cname(1:9).eq.'nelements')
     *      call cmo_newlen(cmo_name,icscode)
      else
C
C....    This is a new attribute.
C
         cbuf='cmo/addatt/' // cmo_name // '/'
     *       // cname    // '/' // ctype   // '/'
     *       // crank    // '/' // clen    // '/'
     *       // cinterp  // '/' // cpersis // '/'
     *       // cioflag  // '/ 0.0 ;finish'
         call dotask(cbuf,icscode)
 
         if(cname(1:9).eq.'nelements')
     *      call cmo_newlen(cmo_name,icscode)
      endif
 
C
C....    Set up the Memory Managed Arrays for the Attribute.
C
      lentype=icharlnf(ctype)
      call  cmo_set_info(clen,cmo_name,attlen,1,1,icscode)
      call  cmo_set_info(crank,cmo_name,irank,1,1,icscode)
      if(ctype(1:lentype).eq.'VINT'.or.
     *   cname(1:len).eq.'isetwd'.or.
     *   cname(1:len).eq.'xtetwd') then
C
         mmlength=irank*attlen
c
c  see if block exists if so fix length
c  otherwise create it
c
         call mmfindbk(cname(1:len),cmo_name,
     *                    ipxfield,length,ier)
         if(ier.ne.0) then
            mmlength=max(1,mmlength)
            call mmgetblk(cname(1:len),cmo_name,
     *                    ipxfield,mmlength,1,ier)
         else
            if(length.lt.mmlength) call mmnewlen(
     *        cname(1:len),cmo_name,ipxfield,mmlength,ier)
         endif
         if(ier.ne.0) then
            call cmo_mm_error('cmo_readdump_cmo')
         else
            if(iomode(1:5).eq.'ascii') then
               read(iunit,20) (ifield(j),j=1,attlen*irank)
            else
               read(iunit) (ifield(j),j=1,attlen*irank)
            endif
         endif
C
      elseif(ctype(1:lentype).eq.'VDOUBLE') then
C
         mmlength=irank*attlen
         call mmfindbk(cname(1:len),cmo_name,
     *                    ipxfield,length,ier)
         if(ier.ne.0) then
            mmlength=max(1,mmlength)
            call mmgetblk(cname(1:len),cmo_name,
     *                    ipxfield,mmlength,2,ier)
         else
            if(length.lt.mmlength) call mmnewlen(
     *        cname(1:len),cmo_name,ipxfield,mmlength,ier)
         endif
C
         if(ier.ne.0) then
            call cmo_mm_error('cmo_addatt_cmo')
         else
            if(iomode(1:5).eq.'ascii') then
               read(iunit,25) (xfield(j),j=1,attlen*irank)
            else
               read(iunit) (xfield(j),j=1,attlen*irank)
            endif
         endif
      else
         parname='default'
         if(iomode(1:5).eq.'ascii') then
            read(iunit,30) xdefault
         else
            read(iunit ) xdefault
         endif
         write(cbuf,37)cname,xdefault
 37      format('cmo/setatt//',a32, '////',f22.5,';finish')
         call dotask (cbuf,icscode)
      endif
      enddo
c
c  read next line if first word is psetnames then
c  this is old format
c  otherwise it is new format
c
      if(iomode(1:5).eq.'ascii') then
         read(iunit,38) cnameps,nnames,n2
 38      format(a9,1x,2i5)
      else
         read(iunit) cnameps,nnames,n2
      endif
      call mmgetblk('xval',isubname,ipval,512,2,icscode)
      call mmgetblk('xatt',isubname,ipatt,2,2,icscode)
      call mmgetblk('idx',isubname,ipidx,200,1,icscode)
      if(cnameps.ne.'psetnames') go to 50
c
c  read in psetnames and esetnames
c
 
 
 40   format(a10,i5)
      if (nnames.gt.0) then
         call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *      icscode)
         write(cbuf,51)n2
         call dotask (cbuf,icscode)
         call mmfindbk('psetnames',cmo_name,ippsetnm,ilen,
     *         itype,icscode)
         do i=1,nnames
            psetnames(i)=cval(2,i)
         enddo
      endif
c
 
      if(iomode(1:5).eq.'ascii') then
         read(iunit,40) cnameps,nnames
      else
         read(iunit) cnameps,nnames,n2
      endif
      if (nnames.gt.0) then
         call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,iomode,
     *      icscode)
         write(cbuf,52)n2
         call dotask (cbuf,icscode)
         call mmfindbk('eltsetnames',cmo_name,ipesetnm,ilen,
     *         itype,icscode)
         do i=1,nnames
            eltsetnames(i)=cval(2,i)
         enddo
      endif
      go to 100
c
c  new format read contents of sbcmoprm
c
 50   nwval=0
      nwatt=0
      if(iomode(1:5).eq.'ascii') then
         read(iunit,'(a)',end=100) nodnm
      else
         read(iunit) nodnm
      endif
c
c  check if name is special - psetnames or esetnames
c  read in pset and eset information
c
      if(nodnm.eq.'endsbcmoprm') go to 100
      if(nodnm(1:9).eq.'psetnames') then
            if(iomode(1:5).eq.'ascii') then
               read(iunit,'(2i5)') nnames,n2
            else
              read(iunit) nnames,n2
            endif
            write(cbuf,51)n2
51          format('cmo/setatt//number_of_psets////',i2,';finish')
            call dotask (cbuf,icscode)
 
            if (nnames.gt.0) then
               call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,
     *          iomode,icscode)
               call mmfindbk('psetnames',cmo_name,ippsetnm,ilen,
     *         itype,icscode)
               do i=1,nnames
                  psetnames(i)=cval(2,i)
               enddo
            endif
      elseif(nodnm(1:9).eq.'esetnames') then
            if(iomode(1:5).eq.'ascii') then
               read(iunit,'(2i5)') nnames,n2
            else
               read(iunit) nnames,n2
            endif
 
            write(cbuf,52)n2
52          format('cmo/setatt//number_of_eltsets////',i2,';finish')
            call dotask (cbuf,icscode)
            if (nnames.gt.0) then
               call sbnread(iunit,ipval,nwval,ipatt,nwatt,ipidx,
     *          iomode,icscode)
               call mmfindbk('eltsetnames',cmo_name,ipesetnm,ilen,
     *           itype,icscode)
               do i=1,nnames
                  eltsetnames(i)=cval(2,i)
               enddo
            endif
      else
C
C  just a regular variable read in type and value
c
         if(iomode(1:5).eq.'ascii') then
            read(iunit,42) ctype
 42         format(a32)
         else
            read(iunit) ctype
         endif
         if (ctype.eq.'h') then
            if(iomode(1:5).eq.'ascii') read(iunit,42) cvalue
            if(iomode(1:6).eq.'binary') read(iunit) cvalue
            write(cbuf,43)nodnm,cvalue
43          format('cmo/setatt//',a32, '////',a32,';finish')
            call dotask (cbuf,icscode)
         else
            if(iomode(1:5).eq.'ascii') read(iunit,'(e20.12)') xvalue
            if(iomode(1:6).eq.'binary') read(iunit) xvalue
            write(cbuf,37)nodnm,xvalue
            call dotask (cbuf,icscode)
         endif
      endif
      go to 50
C
 20         format(5i15)
 25         format(5e20.13)
 30         format(e20.13)
c
100   if(iomode(1:5).eq.'ascii') then
         read(iunit,'(a5)',end=200) cnamend
      else
         read(iunit,end=200) cnamend
      endif
200   if(cnamend(1:5).ne.'-end-') then
         write(logmess,'(a)') ' dump file not terminated'
         call writloga('default',0,logmess,0,icscode)
         ierror=1
      endif
c
 9999 continue
      call mmrelprt(isubname,icscode)
c
      return
      end
