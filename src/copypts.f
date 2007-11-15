      subroutine copypts(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS SUBROUTINE IS USED TO COPY POINTS FROM ONE MESH
C        OBJECT TO ANOTHER OR TO DUPLICATE POINTS WITHIN A MESH
C       OBJECT.
C
C        FORMAT: COPYPTS/ipstart/ipend/ipstep/istart/istride/
C  The above format retained for compatibility reasons
C  The following format is recommended.
C        FORMAT: COPYPTS/cmo sink/cmo source/ipsink/sink stride
C                        source start/source end/source stride/
C                        sink cmo attribute/source cmo attribute/
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: copypts.f,v $
C        Revision 2.00  2007/11/05 19:45:51  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   01 Oct 2007 08:16:06   gable
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   Tue Mar 21 15:47:56 2000   dcg
CPVCS    add call to setsize
CPVCS    
CPVCS       Rev 1.1   25 Jan 2000 13:46:22   dcg
CPVCS    
CPVCS       Rev 1.0   25 Jan 2000 09:30:58   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.26   Wed Jun 16 10:10:04 1999   dcg
CPVCS    remove extra comma from write statement
CPVCS
CPVCS       Rev 1.25   Wed May 12 11:43:18 1999   dcg
CPVCS    print range of new points resulting from copypts
CPVCS
CPVCS       Rev 1.24   Fri Apr 10 11:32:56 1998   dcg
CPVCS    check correct error flag for cmo-exist tests
CPVCS
CPVCS       Rev 1.23   Mon Apr 14 16:42:36 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.22   Fri Dec 20 15:08:16 1996   dcg
CPVCS    set sink stride correctly
CPVCS
CPVCS       Rev 1.21   Fri Dec 20 13:31:12 1996   dcg
CPVCS    allow copying one attribute values into a similar
CPVCS    attribute if source and sink mesh objects are the
CPVCS    same
CPVCS
CPVCS       Rev 1.20   Mon Nov 18 10:29:36 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS
CPVCS       Rev 1.19   Wed Oct 30 14:20:32 1996   dcg
CPVCS    refresh pointers to source and sink mesh objects
CPVCS    if they might be changed by a cmo/addatt command
CPVCS
CPVCS       Rev 1.18   Thu Jun 13 08:54:56 1996   dcg
CPVCS    fix npoints argument in pntlimn, pntlimc calls
CPVCS
CPVCS       Rev 1.17   Mon Jun 03 11:37:44 1996   dcg
CPVCS    fix type with pset name retrieval
CPVCS
CPVCS       Rev 1.16   Thu Feb 08 14:09:14 1996   dcg
CPVCS    add loop on mesh object attributes
CPVCS
CPVCS       Rev 1.15   Tue Jan 23 09:32:30 1996   het
CPVCS    Add the option to copy reals to integer and integers to reals.
CPVCS
CPVCS       Rev 1.14   Wed Jan 03 10:07:20 1996   het
CPVCS    Change maximum dimensions for UNICOS.
CPVCS
CPVCS       Rev 1.13   12/05/95 08:21:02   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.12   11/16/95 15:21:26   dcg
CPVCS    replace character literals in calls
CPVCS
CPVCS       Rev 1.11   11/07/95 17:15:48   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.10   09/12/95 12:58:28   dcg
CPVCS    get correct arguments for addatt command
CPVCS
CPVCS       Rev 1.9   09/12/95 06:56:02   het
CPVCS    Correct errors with sb_get_info
CPVCS
CPVCS       Rev 1.8   08/31/95 11:53:44   dcg
CPVCS    fix default points limits (ipointi,ipointj)
CPVCS
CPVCS       Rev 1.7   08/31/95 10:21:08   dcg
CPVCS    accomodate old as well as new formats
CPVCS
CPVCS       Rev 1.6   08/29/95 12:16:12   het
CPVCS    Add the cmowrk storage block for each CMO
CPVCS
CPVCS       Rev 1.5   07/13/95 09:03:22   ejl
CPVCS    Cleaned up interface of rotatept, rotateln, copypts.
CPVCS
CPVCS       Rev 1.4   05/26/95 13:13:38   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.3   05/23/95 06:50:24   het
CPVCS    Change dictionary so that they are CMO specific
CPVCS
CPVCS       Rev 1.2   02/18/95 06:56:24   het
CPVCS    Changed the parameter list to be the same as pntlimc
CPVCS
CPVCS       Rev 1.1   01/04/95 22:01:52   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:24   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
      character*132 logmess
C
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      character*32 isubname, cmo
      character*32 ich1,ich2,ich3
C
      pointer(ipmpary, mpary )
      integer mpary(1000000)
C
      character*32 cmo_save
      character*32 cmo_sink, att_sink
      character*32 cmo_source, att_source
C
      character*32 cname_sink
      character*32 ctype, clength, ctype_sink, clength_sink
      character*32 cname, crank, cinterp,
     *             cpersistence, cioflag
C
      integer  nmcmoatt_sink
C
      integer nmcmoatt_src
C
      pointer (ipicmo_source, icmo_source1)
      pointer (ipicmo_sink, icmo_sink1)
      pointer (ipxcmo_source, xcmo_source1)
      pointer (ipxcmo_sink, xcmo_sink1)
      integer icmo_source1(1000000), icmo_sink1(1000000)
      real*8 xcmo_source1(1000000), xcmo_sink1(1000000)
C
      integer icscode, ipsrc, ipsnk, ierrwrt, lenout,
     *        npoints, length, ilen, ityp, ipointi, ipointj,
     *        icmotype, istart, istride, ipts,itype,index
      integer ipt1, ipt2, ipt3
      integer mpno, lenatt, lenn, lenmax1, lent,
     *        len1, len2, len3, len4, len5, len6, len7,
     *        lenl, iflag, irank_src, lent_sink, lenl_sink , irank_sink
      integer i, j, k, i1, i2, l
      integer length_source, length_sink, ierror_return
C
      character*1024  cbuff
C
      pointer (ipout, xout)
      real*8 xout(1000000)
C
      pointer (ipitp1, itp1)
      pointer (ipisetwd, isetwd)
      integer itp1(1000000), isetwd(1000000)
 
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
C
      isubname='copypts'
C
      call cmo_get_name(cmo_save,icscode)
C
      if(msgtype(2).eq.3.and.cmsgin(2)(1:4).ne.'pset') then
         ipsrc=6
         ipsnk=4
         cmo_source=cmsgin(3)
         if(cmo_source(1:icharlnf(cmo_source)).eq.'-def-') then
            call cmo_get_name(cmo_sink,icscode)
         else
            call cmo_exist(cmo_source,icscode)
            if(icscode.ne.0) then
               write(logmess,9000) cmo_source(1:icharlnf(cmo_source))
 9000          format("Source CMO doesn't exist: ",a)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9999
            endif
         endif
C
         att_source='-all-'
         if(nwds.ge.10) att_source=cmsgin(10)
         if(att_source(1:icharlnf(att_source)).eq.'-all-') then
         elseif(att_source(1:icharlnf(att_source)).eq.'-def-') then
            att_source='-all-'
         else
            call mmfindbk(att_source,cmo_source,ipout,lenout,icscode)
            if(icscode.ne.0) then
               write(logmess,9010) cmo_source(1:icharlnf(cmo_source)),
     *                          att_source(1:icharlnf(att_source))
 9010          format("Source CMO attribute doesn't exist: ",a,' ',a)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9999
            endif
         endif
C
         cmo_sink=cmsgin(2)
         if(cmo_sink(1:icharlnf(cmo_sink)).eq.'-def-') then
            cmo_sink=cmo_source
         else
            call cmo_exist(cmo_sink,icscode)
            if(icscode.ne.0) then
               write(logmess,9020) cmo_sink(1:icharlnf(cmo_sink))
 9020          format("Sink CMO doesn't exist: ",a)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9999
            endif
         endif
C
         att_sink='-all-'
         if(nwds.ge.9) att_sink=cmsgin(9)
         if(att_sink(1:icharlnf(att_sink)).eq.'-all-') then
         elseif(att_sink(1:icharlnf(att_sink)).eq.'-def-') then
            att_sink=att_source
         else
            call mmfindbk(att_sink,cmo_sink,ipout,lenout,icscode)
            if(icscode.ne.0) then
               write(logmess,9030) cmo_sink(1:icharlnf(cmo_sink)),
     *                          att_sink(1:icharlnf(att_sink))
 9030          format("Sink CMO attribute doesn't exist: ",a,' ',a)
               call writloga('default',0,logmess,0,ierrwrt)
               goto 9999
            endif
         endif
         if(nwds.lt.6) then
            imsgin(ipsrc)=1
            msgtype(ipsrc)=1
            imsgin(ipsrc+1)=0
            msgtype(ipsrc+1)=1
            imsgin(ipsrc+2)=0
            msgtype(ipsrc+2)=1
         endif
         if(nwds.lt.4) then
            imsgin(ipsnk)=0
            msgtype(ipsnk)=1
            imsgin(ipsnk+1)=0
            msgtype(ipsnk+1)=1
          endif
      else
         ipsrc=2
         ipsnk=5
         cmo_source=cmo_save
         cmo_sink=cmo_save
         att_source='-all-'
         att_sink='-all-'
      endif
C
C
      cmo=cmo_source
      call cmo_select(cmo,icscode)
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('ipointi',cmo,ipointi,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('ipointj',cmo,ipointj,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
      ipointi = max(1,ipointi)
      if(ipointj.eq.0) ipointj = max(1,npoints)
C
C     ..................................................................
C     CHECK TO POINT LIMITS AND TRANSLATE THEM TO VALID LIMITS IF
C     NECESSARY.
C
      if(msgtype(ipsrc).eq.1.and.imsgin(ipsrc).eq.0) then
         imsgin(ipsrc)=ipointi
      endif
      if(msgtype(ipsrc+1).eq.1.and.imsgin(ipsrc+1).eq.0) then
         if(imsgin(ipsrc).eq.0) then
           imsgin(ipsrc+1)=ipointj
         else
           imsgin(ipsrc+1)=npoints
         endif
      endif
      if(msgtype(ipsrc+2).eq.1.and.imsgin(ipsrc+2).eq.0) then
         imsgin(ipsrc+2)=1
      endif
C
      call mmgetblk('mpary',isubname,ipmpary,npoints,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk')
C
C     Set the point index boundaries
C
      ich1=' '
      ich2=' '
      ich3=' '
C
      if(msgtype(ipsrc).eq.1) then
         ipt1=imsgin(ipsrc)
         ipt2=imsgin(ipsrc+1)
         ipt3=imsgin(ipsrc+2)
         if(nwds.eq.ipsrc) then
            ipt2=ipt1
            ipt3=1
         elseif(nwds.eq.ipsrc+1) then
            ipt3=1
         endif
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,npoints,
     x             isetwd,itp1)
      else
         ich1=cmsgin(ipsrc)
         ich2=cmsgin(ipsrc+1)
         ich3=cmsgin(ipsrc+2)
         call pntlimc(ich1,ich2,ich3,ipmpary,mpno,npoints,
     x             isetwd,itp1)
      endif
C
      cmo=cmo_sink
      call cmo_select(cmo,icscode)
C
      call cmo_get_info('nnodes',cmo,npoints,length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isetwd',cmo,ipisetwd,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('ipointi',cmo,ipointi,length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
      call cmo_get_info('ipointj',cmo,ipointj,length,icmotype,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
      if(msgtype(ipsnk).eq.1.and.imsgin(ipsnk).eq.0) then
         istart=ipointj+1
      else
         istart=imsgin(ipsnk)
      endif
      if(msgtype(ipsnk+1).eq.1.and.imsgin(ipsnk+1).eq.0) then
         istride=1
      else
         istride=imsgin(ipsnk+1)
      endif
C
C     ..................................................................
C     Before we copy, first adjust memory if necessary
C
      ipts=max(npoints,istart+istride*(mpno-1))
C
      call cmo_set_info('nnodes',cmo,ipts,1,1,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_newlen(cmo_sink,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'cmo_newlen')
C
      npoints=ipts
C
C     ..................................................................
C
C     CHECK TO SEE THAT ALL ATTRIBUTES THAT ARE IN THE SOURCE ARE ALSO
C        IN THE SINK. IF NOT THEN ADD THEM TO THE SINK.
C
      call cmo_get_info('number_of_attributes',cmo_source,
     *  nmcmoatt_src,ilen,itype,icscode)
      call cmo_get_info('number_of_attributes',cmo_sink,
     *  nmcmoatt_sink,ilen,itype,icscode)
C
      lenatt=icharlnf(att_source)
C
      do i=1,nmcmoatt_src
         call cmo_get_attribute_name(cmo_source,i,cname,icscode)
         call cmo_get_attparam(cname,cmo_source,index,ctype,crank,
     *    clength,cinterp,cpersistence,cioflag,ierror_return)
 
         lenl=icharlnf(clength)
         lenn=icharlnf(cname)
         lenmax1=max(lenatt,lenn)
         if(clength(1:lenl).eq.'nnodes' .and.
     *      att_source.eq.att_sink .and.
     *      (att_source(1:lenatt).eq.'-all-' .or.
     *      att_source(1:lenmax1).eq.cname(1:lenmax1)
     *      )) then
            lent=icharlnf(ctype)
            if(ctype(1:lent).eq.'VINT'     .or.
     *         ctype(1:lent).eq.'VDOUBLE') then
               len1=icharlnf(cname)
               iflag=0
               do j=1,nmcmoatt_sink
                  cname_sink=' '
                  call cmo_get_attribute_name(cmo_sink,j,
     *                 cname_sink,icscode)
                  len2=max(len1,icharlnf(cname_sink))
                  if(cname(1:len2).eq.cname_sink(1:len2)) then
                     iflag=1
                  endif
               enddo
               if(iflag.eq.0) then
                  len1=icharlnf(cname)
                  len2=icharlnf(ctype)
                  len3=icharlnf(crank)
                  len4=icharlnf(clength)
                  len5=icharlnf(cinterp)
                  len6=icharlnf(cpersistence)
                  len7=icharlnf(cioflag)
                  cbuff='cmo/addatt/' //
     *                  cmo_sink(1:icharlnf(cmo_sink)) //
     *                  '/' //
     *                  cname(1:len1) //
     *                  '/' //
     *                  ctype(1:len2) //
     *                  '/' //
     *                  crank(1:len3) //
     *                  '/' //
     *                  clength(1:len4) //
     *                  '/' //
     *                  cinterp(1:len5) //
     *                  '/' //
     *                  cpersistence(1:len6) //
     *                  '/' //
     *                  cioflag(1:len7) //
     *                  '/0.0' //
     *                  ' ; finish '
                  call dotaskx3d(cbuff,ierror)
               endif
            endif
         endif
      enddo
C
C     ..................................................................
C
C     COPY THE NODE LENGTH ATTRIBUTES FROM THE SOURCE TO THE SINK.
C
      do i=1,nmcmoatt_src
         call cmo_get_attribute_name(cmo_source,i,cname,icscode)
         call cmo_get_attparam(cname,cmo_source,index,ctype,crank,
     *    clength,cinterp,cpersistence,cioflag,ierror_return)
 
         lent=icharlnf(ctype)
         lenl=icharlnf(clength)
         lenn=icharlnf(cname)
         lenmax1=max(lenatt,lenn)
         if(clength(1:lenl).eq.'nnodes' .and.
     *      (att_source(1:lenatt).eq.'-all-' .or.
     *      att_source(1:lenmax1).eq.cname(1:lenmax1)
     *      )) then
            if(ctype(1:lent).eq.'VINT') then
               call cmo_get_length(cname,cmo_source,length,irank_src,
     *                             ierror_return)
               call cmo_get_length(cname,cmo_sink,length_sink,
     *                irank_sink,  ierror_return)
               call mmfindbk(cname,
     *                       cmo_source,
     *                       ipicmo_source,length_source,
     *                       ierror_return)
               if(att_sink(1:icharlnf(att_sink)).eq.'-all-') then
                  call mmfindbk(cname,
     *                          cmo_sink,
     *                          ipicmo_sink,length_sink,
     *                          ierror_return)
                  do k=1,mpno
                     do l=1,irank_sink
                        i1=(mpary(k)-1)*irank_src+min(l,irank_src)
                        i2=(istart+istride*(k-1)-1)*irank_sink+l
                        icmo_sink1(i2)=icmo_source1(i1)
                     enddo
                  enddo
               else
                  call cmo_get_attparam(att_sink,cmo_sink,index,
     *             ctype_sink,crank,clength_sink,cinterp,cpersistence,
     *             cioflag,ierror_return)
                  lent_sink=icharlnf(ctype_sink)
                  lenl_sink=icharlnf(clength_sink)
                  if(clength(1:lenl).eq.clength_sink(1:lenl_sink)) then
                     if(ctype_sink(1:lent_sink).eq.'VINT') then
                        call mmfindbk(att_sink,
     *                                cmo_sink,
     *                                ipicmo_sink,length_sink,
     *                                ierror_return)
                        do k=1,mpno
                           do l=1,irank_sink
                              i1=(mpary(k)-1)*irank_src+min(l,irank_src)
                              i2=(istart+istride*(k-1)-1)*irank_sink+l
                              icmo_sink1(i2)=icmo_source1(i1)
                           enddo
                        enddo
                     elseif(ctype_sink(1:lent_sink).eq.'VDOUBLE') then
                        call mmfindbk(att_sink,
     *                                cmo_sink,
     *                                ipxcmo_sink,length_sink,
     *                                ierror_return)
                        do k=1,mpno
                           do l=1,irank_sink
                              i1=(mpary(k)-1)*irank_src+min(l,irank_src)
                              i2=(istart+istride*(k-1)-1)*irank_sink+l
                              xcmo_sink1(i2)=icmo_source1(i1)
                           enddo
                        enddo
                     endif
                  endif
               endif
            elseif(ctype(1:lent).eq.'VDOUBLE') then
               call cmo_get_length(cname,cmo_source,length,irank_src,
     *                             ierror_return)
               call cmo_get_length(cname,cmo_sink,length_sink,
     *                irank_sink,  ierror_return)
               call mmfindbk(cname,
     *                       cmo_source,
     *                       ipxcmo_source,length_source,
     *                       ierror_return)
               if(att_sink(1:icharlnf(att_sink)).eq.'-all-') then
                  call mmfindbk(cname,
     *                          cmo_sink,
     *                          ipxcmo_sink,length_sink,
     *                          ierror_return)
                  do k=1,mpno
                     do l=1,irank_sink
                        i1=(mpary(k)-1)*irank_src+min(l,irank_src)
                        i2=(istart+istride*(k-1)-1)*irank_sink+l
                        xcmo_sink1(i2)=xcmo_source1(i1)
                     enddo
                  enddo
               else
                  call cmo_get_attparam(att_sink,cmo_sink,index,
     *             ctype_sink,crank,clength_sink,cinterp,cpersistence,
     *             cioflag,ierror_return)
                  lent_sink=icharlnf(ctype_sink)
                  lenl_sink=icharlnf(clength_sink)
                  if(clength(1:lenl).eq.clength_sink(1:lenl_sink)) then
                     if(ctype_sink(1:lent_sink).eq.'VINT') then
                        call mmfindbk(att_sink,
     *                                cmo_sink,
     *                                ipicmo_sink,length_sink,
     *                                ierror_return)
                        do k=1,mpno
                           do l=1,irank_sink
                              i1=(mpary(k)-1)*irank_src+min(l,irank_src)
                              i2=(istart+istride*(k-1)-1)*irank_sink+l
                              icmo_sink1(i2)=xcmo_source1(i1)
                           enddo
                        enddo
                     elseif(ctype_sink(1:lent_sink).eq.'VDOUBLE') then
                        call mmfindbk(att_sink,
     *                                cmo_sink,
     *                                ipxcmo_sink,length_sink,
     *                                ierror_return)
                        do k=1,mpno
                           do l=1,irank_sink
                              i1=(mpary(k)-1)*irank_src+min(l,irank_src)
                              i2=(istart+istride*(k-1)-1)*irank_sink+l
                              xcmo_sink1(i2)=xcmo_source1(i1)
                           enddo
                        enddo
                     endif
                  endif
               endif
            endif
         endif
      enddo
C
      ipointi=istart
      ipointj=istart+istride*(mpno-1)
      write(logmess,9070) ipointi,ipointj
 9070 format('copypts added nodes ',i10,
     *  ' to ',i10)
       call writloga('default',0,logmess,0,ierrwrt)
      call cmo_set_info('ipointi',cmo,ipointi,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'set_info_i')
      call cmo_set_info('ipointj',cmo,ipointj,1,1,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'set_info_i')
C
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
C
      ierror=0
C
      goto 9999
9999  continue
C
c  set problem size parameters
c
      call setsize()
C
      return
      end
