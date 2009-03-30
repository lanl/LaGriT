      subroutine cmo_readatt(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                    ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         Reads cmo attributes from a file
C  cmo/readatt/cmo_name/attr1/attr2/.../pset,get,pset_name/file_name
C  cmo/readatt/cmo_name/attr1/attr2/.../add///file_name
c         if attr1, attr2 do not exist VDOUBLE attributes are
C         created.
C         if 'add' is specified then new nodes are added to the cmo
C         if a pset is given, the specified nodes are modified
C
C         Lines of an input file that have # in the first column or that do not
C         have a real or integer as their first word are skipped.
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_readatt.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 13:24:46   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.1   Fri Dec 03 16:50:18 1999   dcg
CPVCS    update ipointi, and ipointj if option is 'add'
CPVCS    
CPVCS       Rev 1.0   Tue Sep 29 14:22:26 1998   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
      integer ierror_return,icount,length,l,icscode,lenfile,
     *   i,j,ilen,itype,ierror,nvalues,lenout,iunit,npointsnew,
     *   npoints,ityp,ipt1,ipt2,ipt3,mpno
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds),ich1,ich2,ich3
C
C#######################################################################
C
C.... Subroutine name for memory management and errors.
C
      character*32 isubname
C
C.... Mesh Object Data.
C
      character*32 ifile
      character*32 cmo, cmoatt, cglobal, cdefault
      character*8192 cbuff
      character*1024 cline
      integer lenparse
      integer      msgt(128)
      real*8       xmsg(128)
      integer      imsg(128)
      character*32 cmsg(128)
C
C#######################################################################
C
      character*132 logmess
C
C#######################################################################
C
      integer icharlnf
C
      pointer ( ipmpary1 , mpary1 )
      pointer (ipxvalues,xvalues)
      real*8 xvalues(*)
      pointer (ipxarray, iarray)
      pointer (ipxarray, xarray)
      integer iarray(*), mpary1(*)
      real*8 xarray(*)
      pointer (ipitp1,itp1)
      pointer (ipisetwd,isetwd)
      integer itp1(*),isetwd(*)
      integer ipointi,ipointj
C
C#######################################################################
C
C
C     Define the subroutine name for memory management and errors.
C
      cglobal='global'
      cdefault='default'
      isubname='cmo_readatt'
C
      ifile=cmsgin(nwds)
      cmo=cmsgin(3)
C
C
C     ******************************************************************
C
C     CHECK TO SEE IF THE FILE EXISTS, IF NOT THEN BAIL OUT.
C     if there read values into temp array
C
      lenfile=icharlnf(ifile)
      call fexist(ifile(1:lenfile),ierror)
      if(ierror.eq.0) then
         write(logmess,9000) ifile(1:lenfile)
 9000    format('The  file, ',a,', does not exist.')
         call writloga('default',1,logmess,0,ierror)
         goto 9999
      endif
C
C
C     ******************************************************************
C
C     ASSIGN THE FILE TO THE NEXT AVAILABLE LOGICAL UNIT NUMBER.
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
C
      icount=0
      nvalues=nwds-7
C
C     temp array to hold values
C
      length=500000
      call mmgetblk('xvalues',isubname,ipxvalues,length,2,icscode)
 100  continue
         read(iunit,'(a)',end=110) cline
         lenparse = len(cline)
         call parse_string2(lenparse, cline,
     1                       imsg, msgt, xmsg, cmsg, nwds)
C
C        Check for comment lines '#' or lines that do not have either an
C        integer or real as the first word in the line. This will not get
C        everything but at least it will read past character headers such
C        as those found in the header of a TecPlot style file.
C
         if((cline(1:1).ne.'#').and. 
     1     (msgt(1).eq.1 .or. msgt(1) .eq.2)) then
           icount=icount+1
           if(icount*nvalues.gt.length) then
             l=max(int(0.5*length), 500000)
             call mmincblk('xvalues',isubname,ipxvalues,l,icscode)
             length=length+l
           endif
           read(cline,*) (xvalues(nvalues*(icount-1)+i),i=1,nvalues)
         endif
         goto 100
 110     continue
C
      close(iunit)
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,ityp,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,ierror)
      call cmo_get_info('ipointi',cmo,
     *                ipointi,ilen,ityp,icscode)
      call cmo_get_info('ipointj',cmo,
     *               ipointj,ilen,ityp,icscode) 
      if(npoints.eq.0.or.(msgtype(nwds-3).eq.3.and.
     *    cmsgin(nwds-3)(1:3).eq.'add'))  then
         ilen=icount
      else
         ilen=npoints
      endif
      call mmgetblk('mpary1',isubname,ipmpary1,ilen,1,icscode)
      if(npoints.eq.0) then
          npointsnew=icount
          do i=1,icount
             mpary1(i)= i
          enddo
      elseif(msgtype(nwds-3).eq.3.and.cmsgin(nwds-3)(1:3).eq.'add')
     *    then
          npointsnew=icount+npoints
          ipointi=ipointj+1
          ipointj=ipointj+icount
          call set_info_i('ipointi',cmo,cglobal,cdefault,
     *                ipointi,icscode)
          call set_info_i('ipointj',cmo,cglobal,cdefault,
     *                ipointj,icscode)

          do i=1,icount
             mpary1(i)= i+npoints
          enddo
      elseif(msgtype(nwds-3).eq.1.and.imsgin(nwds-3).gt.npoints)
     *    then
          npointsnew=icount+npoints
          do i=1,icount
             mpary1(i)= i+npoints
          enddo
      elseif(msgtype(nwds-2).eq.1.and.imsgin(nwds-2).gt.npoints)
     *    then
          npointsnew=icount+imsgin(nwds-3)-1
          do i=1,icount
             mpary1(i)= i+imsgin(nwds-3)-1
          enddo
      else
        if (msgtype(nwds-1).eq.1) then
          ipt1=imsgin(nwds-3)
          ipt2=imsgin(nwds-2)
          ipt3=imsgin(nwds-1)
          call pntlimn(ipt1,ipt2,ipt3,ipmpary1,mpno,npoints,
     *            isetwd, itp1)
        else
          ich1=cmsgin(nwds-3)
          ich2=cmsgin(nwds-2)
          ich3=cmsgin(nwds-1)
          call pntlimc(ich1,ich2,ich3,ipmpary1,mpno,npoints,
     *            isetwd, itp1)
        endif
        npointsnew=max(npoints,icount,mpary1(mpno))
      endif
      call cmo_set_info('nnodes',cmo,npointsnew,1,1,ierror)
C
      call cmo_newlen(cmo,ierror)
C
C     ******************************************************************
C
C
      do i=1,nvalues
         cmoatt=cmsgin(i+3)
         call mmfindbk(cmoatt,cmo,ipxarray,lenout,icscode)
         if(icscode.ne.0) then
            cbuff='cmo/addatt/' //
     *            cmo(1:icharlnf(cmo)) //
     *            '/' //
     *            cmoatt(1:icharlnf(cmoatt)) //
     *            '/VDOUBLE' //
     *            '/scalar/nnodes/linear/permanent/gxaf/0.0' //
     *            ' ; finish '
            call dotaskx3d(cbuff,ierror)
            call mmfindbk(cmoatt,cmo,ipxarray,lenout,icscode)
         endif
         call mmgettyp(ipxarray,itype,icscode)
         do j=1,icount
            if(itype.eq.1) then
               iarray(mpary1(j))=int(xvalues(nvalues*(j-1)+i))
             else
               xarray(mpary1(j))=xvalues(nvalues*(j-1)+i)
             endif
         enddo
      enddo
C
C     Print status to screen
C
      cbuff='cmo/status/brief' //
     *       cmo(1:icharlnf(cmo)) //
     *       ' ; finish '
      call dotask(cbuff,ierror)
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
C
      return
      end
