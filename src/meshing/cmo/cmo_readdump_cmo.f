      subroutine cmo_readdump_cmo(cmo_name,iunit,iomode,ierror)
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
C   $Log: cmo_readdump_cmo.f,v $
C   Revision 2.00  2007/11/05 19:45:49  spchu
C   Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   07 Oct 2003 16:05:30   dcg
CPVCS    define cfield as a character*32 field
CPVCS
CPVCS       Rev 1.5   28 Mar 2000 14:09:02   dcg
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   Mon Mar 13 15:58:18 2000   dcg
CPVCS    really  get rid of length in call to cmo_set_attinfo
CPVCS
CPVCS       Rev 1.3   Thu Feb 17 15:14:36 2000   dcg
CPVCS    get rid of length in call to cmo_set_attinfo (probably a compiler bug)
CPVCS
CPVCS       Rev 1.2   07 Feb 2000 16:53:34   dcg
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
      integer iunit,j
      character *(*) iomode
C
      pointer(ipxfield,xfield)
      real*8 xfield(*)
      pointer(ipxfield,ifield)
      integer ifield(*)
      pointer(ipxfield,cfield)
      character*32 cfield(*)
C
      integer  ierror
      integer   iend, ii,iout,lout,itype
      integer ier2,lentype,index,ierror_return,
     * len,ier,attlen,mmlength,icscode,
     * irank,
     * length
      pointer(ipout,out)
      real*8 out(*),rout
      character*300 cbuf
      integer icharlnf
c
      character*32  cname, cinterp,cpersis,cioflag
      character*32 isubname,cout
      character*32  cmo_name, crank, ctype,clen
 
C
c#######################################################################
c
      isubname  = 'cmo_readdump_cmo'
c
c  if mesh object does not exist -create it
c
      call cmo_exist(cmo_name,ierror)
      if(ierror.ne.0) call cmo_create(cmo_name,ierror)
      call cmo_select(cmo_name,ierror)
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
        read(iunit,10,end=9999) cname,ctype,crank,clen,cinterp,
     *   cpersis,cioflag,attlen,irank
      else
        read(iunit,end=9999) cname,ctype,crank,clen,cinterp,
     *   cpersis,cioflag,attlen,irank
      endif
  10    format(a32/3a32/3a32/2i10)
      len=icharlnf(cname)
c
C  Check if this a new Attribute.
C
      call cmo_get_attinfo(cname,cmo_name,iout,rout,
     *  cout,ipout,lout,itype,icscode)
C
      if(icscode.eq.0) then
C
C...     Existing Attribute.
C
      call cmo_set_attparam(cname,cmo_name,index,ctype,crank,
     *    clen,cinterp,cpersis,cioflag,ierror_return)
 
      else
C
C....    This is a new attribute.
C
         cbuf='cmo/addatt/'//cmo_name//'/'//cname//'/'//
     *      ctype//'/'//crank//'/'//clen//'/'//
     *      cinterp//'/'//cpersis//'/'//cioflag//
     *      ' ; finish'
         call dotask(cbuf,ierror)
 
         if(cname(1:9).eq.'nelements')
     *      call cmo_newlen(cmo_name,icscode)
      endif
C
C....    Set up the Memory Managed Arrays for the Attribute.
C
      lentype=icharlnf(ctype)
      if(ctype(1:lentype).eq.'VINT'.or.
     *   cname(1:len).eq.'isetwd'.or.
     *   cname(1:len).eq.'xtetwd') then
C
         mmlength=max(1,irank*attlen)
c
c  see if block exists if so fix length
c  otherwise create it
c
         call mmfindbk(cname(1:len),cmo_name,
     *                    ipxfield,length,ier)
         if(ier.ne.0) then
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
               read(iunit,20) (ifield(j),j=1,mmlength)
            else
               read(iunit) (ifield(j),j=1,mmlength)
            endif
         endif
C
      elseif(ctype(1:lentype).eq.'VDOUBLE') then
C
         mmlength=max(1,irank*attlen)
         call mmfindbk(cname(1:len),cmo_name,
     *                    ipxfield,length,ier)
         if(ier.ne.0) then
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
               read(iunit,25) (xfield(j),j=1,mmlength)
            else
               read(iunit) (xfield(j),j=1,mmlength)
            endif
         endif
      elseif(ctype(1:lentype).eq.'VCHAR') then
C
         mmlength=max(1,irank*attlen)
         call mmfindbk(cname(1:len),cmo_name,
     *                    ipxfield,length,ier)
         if(ier.ne.0) then
            call mmgetblk(cname(1:len),cmo_name,
     *                    ipxfield,mmlength,3,ier)
         else
            if(length.lt.mmlength) call mmnewlen(
     *        cname(1:len),cmo_name,ipxfield,mmlength,ier)
         endif
C
         if(ier.ne.0) then
            call cmo_mm_error('cmo_readdump_cmo')
         else
            if(iomode(1:5).eq.'ascii') then
               read(iunit,22) (cfield(j),j=1,mmlength)
  22           format(4a32)
            else
               read(iunit) (cfield(j),j=1,mmlength)
            endif
         endif
      endif
      if(iomode(1:5).eq.'ascii') then
            read(iunit,30) iout,rout,cout,lout,itype
      else
            read(iunit ) iout,rout,cout,lout,itype
      endif
      call cmo_set_attinfo(cname,cmo_name,iout,rout,
     *                        cout,itype,ier2)
      enddo
C
 20   format(5i15)
 25   format(5e20.13)
 30   format(i10,e20.13,a32,2i10)
 
c
 
c
 9999 continue
      call mmrelprt(isubname,icscode)
c
      return
      end
