      subroutine cmo_dump_cmo(cmo,iunit,iomode,ierror)
      implicit none
c
c#######################################################################
c
c     purpose -
c
c        this routine dumps a mesh object to iunit - is activated
c        by the dump/lagrit command
c
c             ..........................................................
c
c     input arguments -
c
C        cmoname - name of mesh object
C        iunit   - unit number of file to write to
c        iomode  - 'ascii' or 'binary' for file type
C
c     output arguments -
c
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
c
c     change history -
C
C   $Log: cmo_dump_cmo.f,v $
C   Revision 2.00  2007/11/05 19:45:48  spchu
C   Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   Mon Feb 07 14:10:42 2000   dcg
CPVCS    
CPVCS       Rev 1.3   Fri Nov 05 13:26:58 1999   dcg
CPVCS    remove dictionary dependencies
CPVCS
CPVCS       Rev 1.2   Tue Jul 13 13:31:42 1999   dcg
CPVCS    do not write attribute if ioflag contains "L"
CPVCS
CPVCS       Rev 1.1   Tue May 11 16:50:20 1999   dcg
CPVCS    allow for binary or ascii lagrit dumps
CPVCS
CPVCS       Rev 1.0   Tue Mar 09 15:00:52 1999   dcg
CPVCS    Initial revision.
c

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
      integer  ierror,iout,lout,itype,index,ierror_return
      integer  i, iend
      integer ics,ierr,ier2,
     * len,ier,ilen,itin,attlen,
     * irank,iattrwrite,
     * attyp,ityp,icscode
c
      real*8  rout
      pointer(ipout,out)
      real*8 out(*)
      integer icharlnf
c
      character*32  cname, cinterp,cpersis,cioflag,cout
      character*32 isubname
      character*32  cmo, crank, ctype,clen
 
C
C     ******************************************************************
c     set the memory management path name to be the subroutine name.
 
      isubname  = 'cmo_dump_cmo'
c
c   get number of attributes and loop through them
c
      call cmo_get_info('number_of_attributes',cmo,iend,
     *                     ilen,ityp,ierror)
c
c  get all the attributes this time to see if they
c  are to be written to the file ('L' in ioflag means skip)
c
      iattrwrite=0
      do i=1,iend
c
c  read name and ioflag from mesh object
c
        call cmo_get_attribute_name(cmo,i,cname,ier)
        len=icharlnf(cname)
        call cmo_get_attparam(cname,cmo,index,ctype,crank,
     *    clen,cinterp,cpersis,cioflag,ierror_return)
         ilen=icharlnf(cioflag)
         do j=1,ilen
           if(cioflag(j:j).eq.'L') go to 8
         enddo
         iattrwrite=iattrwrite+1
 8       continue
      enddo
c
c  write out number of attributes dumped to this file
c
      if(iomode(1:5).eq.'ascii') then
         write(iunit,'(i5)') iattrwrite
      else
         write(iunit) iattrwrite
      endif
      do i=1,iend
C
c         get name get type=attyp lenght=attlen and rank=irank
c
        call cmo_get_attribute_name(cmo,i,cname,ier)
        len=icharlnf(cname)
        call cmo_get_attparam(cname,cmo,index,ctype,crank,
     *    clen,cinterp,cpersis,cioflag,ierror_return)
c
        call cmo_get_info(clen,cmo,attlen,ityp,itin,ierror)

        if(ier.eq.0) then
          if(ctype(1:4).eq.'VINT') then
            attyp=1
          elseif(ctype(1:7).eq.'VDOUBLE') then
            attyp=2
          elseif(ctype(1:7).eq.'VCHAR') then
            attyp=4
          else
            attyp=3
          endif
        endif
        call cmo_get_info(crank,cmo,irank,ilen,itin,ierr)
c
c  check ioflag to see if we want to skip this attribute
c  Upper case letters mean skip so if we find a 'L' skip this one
c  In the future we may implement look for 'l' to include
c
         ilen=icharlnf(cioflag)
         do j=1,ilen
           if(cioflag(j:j).eq.'L') go to 32
         enddo
c
c  write this header info to file
c
        if(iomode(1:5).eq.'ascii') then
           write(iunit,10) cname,ctype,crank,clen,cinterp,cpersis,
     *      cioflag,attlen,irank
        else
           write(iunit) cname,ctype,crank,clen,cinterp,cpersis,
     *      cioflag,attlen,irank
        endif
  10    format(a32/3a32/3a32/2i10)
c
c  access data and write it out
c
        ilen=attlen*irank
        if(ilen.le.0) ilen=1
        if(attyp.eq.1.or.attyp.eq.2.or.attyp.eq.4) then
          call mmgetpr(cname(1:len),cmo,ipxfield,ics)
          if (ics.ne.0 ) then
             call x3d_error(' get field in ',isubname)
             goto 9999
          endif
          if(attyp.eq.1) then
            if(iomode(1:5).eq.'ascii') then
               write(iunit,20) (ifield(j),j=1,ilen)
            else
               write(iunit) (ifield(j),j=1,ilen)
            endif
 20         format(5i15)
          elseif(attyp.eq.4) then
            if(iomode(1:5).eq.'ascii') then
               write(iunit,22) (cfield(j),j=1,ilen)
            else
               write(iunit) (cfield(j),j=1,ilen)
            endif
 22         format(4a32)
          elseif(attyp.eq.2) then
            if(cname(1:len).eq.'isetwd'.or.cname(1:len).eq.
     *         'xtetwd') then
               if(iomode(1:5).eq.'ascii') then
                  write(iunit,20) (ifield(j),j=1,ilen)
               else
                  write(iunit) (ifield(j),j=1,ilen)
               endif

            else
               if(iomode(1:5).eq.'ascii') then
                  write(iunit,25) (xfield(j),j=1,ilen)
               else
                  write(iunit) (xfield(j),j=1,ilen)
               endif
            endif
 25         format(5e20.13)
          endif
        endif
        call cmo_get_attinfo(cname,cmo,iout,rout,cout,
     *                        ipout,lout,itype,ierror_return)
        if(iomode(1:5).eq.'ascii') then
             write(iunit,30) iout,rout,cout,lout,itype
        else
             write(iunit) iout,rout,cout,lout,itype
        endif
 30     format(i10,e20.13,a32,2i10)
 
 32     continue
      enddo
c
 9999 continue
      call mmrelprt(isubname,icscode)
c
      return
      end
