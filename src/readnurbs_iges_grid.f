*dk,readnurbs_iges_grid
      subroutine readnurbs_iges_grid(ifile,
     *                               iopt_nurbs,
     *                               iopt_nurbl,
     *                               iopt_nurbp,
     *                               ks1point,ks2point,kl1point,
     *                               ierror)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE READS AND PROCESSES AN IGES FILE CONTAINING
C            SURFACES, CURVES, AND POINTS IN NURBS IGES FORMAT.
C
C     INPUT ARGUMENTS -
C
C
C     OUTPUT ARGUMENTS -
C
C
C      CHANGE HISTORY -
C$Log: readnurbs_iges_grid.f,v $
CRevision 2.00  2007/11/09 20:04:00  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.5   18 Oct 2005 16:06:36   gable
CPVCS    Extend input file name length to 132 characters.
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 16:57:58 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   Thu Oct 10 08:41:26 1996   het
CPVCS    Fix some errors.
CPVCS    
CPVCS       Rev 1.2   Mon Aug 19 16:13:06 1996   het
CPVCS    Correct a memory management error with the s,t,x,y,z,w arrays.
CPVCS    
CPVCS       Rev 1.1   Thu Jun 27 14:52:52 1996   het
CPVCS    Correct an error in the calling sequence to parse_string.
CPVCS    
CPVCS       Rev 1.0   Tue Jan 30 15:20:16 1996   dcg
CPVCS    Initial revision.
C
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
      character*132 logmess
C
C ######################################################################
C
      parameter (nwordsmax=128)
      integer imsgin(nwordsmax), msgtype(nwordsmax)
      real*8 xmsgin(nwordsmax)
      character*32 cmsgin(nwordsmax)
C
      pointer (ipitoken, itoken)
      pointer (ipxtoken, xtoken)
      real*8 xtoken(100000)
      integer itoken(100000)
C
      pointer (ips, s)
      pointer (ipt, t)
      pointer (ipx, x)
      pointer (ipy, y)
      pointer (ipz, z)
      pointer (ipw, w)
      real*8 s(100000),t(100000),x(100000),y(100000),z(100000)
      real*8 w(100000)
      real*8 u(10),v(10)
C
      character*32 isubname
      character*132 iline
      character*(*) ifile
      character*32 cmo, cmonam
C
      data itermax / 10 /
C
      isubname='readnurbs_iges_grid'
C
      iunit=-1
      call hassign(iunit,ifile,ierror)
C
      call cmo_get_name(cmonam,ierror)
C
      if(iopt_nurbs.le.0.or.iopt_nurbs.eq.1) then
         nsd=3
         nsdgeom=3
         nsdtopo=2
         nen=4
         nef=4
         nnodes=0
         nelements=0
      else
         nsd=3
         nsdgeom=3
         nsdtopo=2
         nen=3
         nef=3
         nnodes=0
         nelements=0
      endif
      call cmo_set_info('nnodes',cmonam,
     *                  nnodes,1,1,ierror)
      call cmo_set_info('nelements',cmonam,
     *                  nelements,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmonam,
     *                  nsdgeom,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmonam,
     *                  nsdtopo,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmonam,
     *                  nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmonam,
     *                  nef,1,1,ierror)
C
      length=100
      call mmgetblk('itoken',isubname,ipitoken,length,2,icscode)
      call mmgetblk('xtoken',isubname,ipxtoken,length,2,icscode)
      call mmgetblk('s',isubname,ips,length,2,icscode)
      call mmgetblk('t',isubname,ipt,length,2,icscode)
      call mmgetblk('x',isubname,ipx,length,2,icscode)
      call mmgetblk('y',isubname,ipy,length,2,icscode)
      call mmgetblk('z',isubname,ipz,length,2,icscode)
      call mmgetblk('w',isubname,ipw,length,2,icscode)
C
      ifnurbs=0
      ifnurbl=0
      ifnurbp=0
      npnurbs=0
      npnurbl=0
      npnurbp=0
      npoints=0
C
      irow=-1
      itoken(1)=0
      ipt=0
      ict=0
      icttot=0
      itoff=0
      jtoff=0
      inurbp=0
      inurbl=0
      inurbs=0
 200     continue
         read(iunit,'(a132)',end=300) iline
         len1=icharlnb(iline)
         if(len1.lt.73) goto 200
         if(iline(73:73).eq.'D') then
            lenline=icharlnb(iline)
            call parse_string(lenline,iline,
     *                        imsgin,msgtype,xmsgin,cmsgin,nwds)
            if(imsgin(1).eq.116) then
               inurbp=inurbp+1
            elseif(imsgin(1).eq.126) then
               inurbl=inurbl+1
            elseif(imsgin(1).eq.128) then
               inurbs=inurbs+1
            endif
            goto 200
         endif
         if(iline(73:73).eq.'P'.or.iline(73:73).eq.'T') then
         else
            goto 200
         endif
         if(irow.lt.0) then
            write(logmess,'(a,a)') 'IGES file: ',
     *                             ifile(1:icharlnf(ifile))
            call writloga('default',0,logmess,0,ierrwrt)
            write(logmess,9000) 'Number of NURBS surfaces, lines, ',
     *                          'points: ',inurbs/2,inurbl/2,inurbp/2
            call writloga('default',0,logmess,0,ierrwrt)
 9000       format(a,a,3i10)
         endif
         lenline=icharlnb(iline)
         call parse_string(lenline,iline,
     *                     imsgin,msgtype,xmsgin,cmsgin,nwds)
         if(cmsgin(1)(1:6).eq.'finish') goto 300
         if(irow.lt.0) irow=0
         if(imsgin(1).eq.116 .or.
     *      imsgin(1).eq.126 .or.
     *      imsgin(1).eq.128 .or.
     *      cmsgin(1)(1:1).eq.'S') then
            if(itoken(1).eq.128) then
               k1=itoken(2)
               k2=itoken(3)
               m1=itoken(4)
               m2=itoken(5)
               iprop1=itoken(6)
               iprop2=itoken(7)
               iprop3=itoken(8)
               iprop4=itoken(9)
               iprop5=itoken(10)
               k1max=k1+1
               k2max=k2+1
               n1=k1-m1+1
               n2=k2-m2+1
               ia=n1+2*m1+1
               ib=n2+2*m2+1
               ic=k1max*k2max
               length=ia
               call mmnewlen('s',isubname,ips,length,icscode)
               length=ib
               call mmnewlen('t',isubname,ipt,length,icscode)
               length=k1max*k2max
               call mmnewlen('x',isubname,ipx,length,icscode)
               call mmnewlen('y',isubname,ipy,length,icscode)
               call mmnewlen('z',isubname,ipz,length,icscode)
               call mmnewlen('w',isubname,ipw,length,icscode)
               ntoken=10
               do 410 i=1,ia
                  ntoken=ntoken+1
                  s(i)=xtoken(ntoken)
 410           continue
               do 420 i=1,ib
                  ntoken=ntoken+1
                  t(i)=xtoken(ntoken)
 420           continue
               do 430 j=1,k2max
                  do 440 i=1,k1max
                     ntoken=ntoken+1
                     w(i+(j-1)*k1max)=xtoken(ntoken)
 440              continue
 430           continue
               do 450 j=1,k2max
                  do 460 i=1,k1max
                     ntoken=ntoken+1
                     x(i+(j-1)*k1max)=xtoken(ntoken)
                     ntoken=ntoken+1
                     y(i+(j-1)*k1max)=xtoken(ntoken)
                     ntoken=ntoken+1
                     z(i+(j-1)*k1max)=xtoken(ntoken)
 460              continue
 450           continue
               ntoken=ntoken+1
               u(1)=xtoken(ntoken)
               ntoken=ntoken+1
               u(2)=xtoken(ntoken)
               ntoken=ntoken+1
               v(1)=xtoken(ntoken)
               ntoken=ntoken+1
               v(2)=xtoken(ntoken)
               npointso=npoints
               if(iopt_nurbs.ge.1.and.ifnurbs.ge.0) then
                  irow=irow+1
                  ifnurbs=ifnurbs+1
                  call nurbs(iopt_nurbs,
     *                       k1max,k2max,ks1point,ks2point,
     *                       m1,m2,ia,ib,s,t,w,x,y,z,u,v,
     *                       irow,ipt,ict,icttot,
     *                       npoints,ntets,nbpoints,nbtets,
     *                       itoff,jtoff)
               endif
               npnurbs=npnurbs+(npoints-npointso)
            endif
            if(itoken(1).eq.126) then
               k1=itoken(2)
               m1=itoken(3)
               iprop1=itoken(4)
               iprop2=itoken(5)
               iprop3=itoken(6)
               iprop4=itoken(7)
               k1max=k1+1
               n1=k1-m1+1
               ia=n1+2*m1+1
               ic=k1max
               length=ia
               call mmnewlen('s',isubname,ips,length,icscode)
               length=k1max
               call mmnewlen('x',isubname,ipx,length,icscode)
               call mmnewlen('y',isubname,ipy,length,icscode)
               call mmnewlen('z',isubname,ipz,length,icscode)
               call mmnewlen('w',isubname,ipw,length,icscode)
               ntoken=7
               do 510 i=1,ia
                  ntoken=ntoken+1
                  s(i)=xtoken(ntoken)
 510           continue
               do 540 i=1,k1max
                  ntoken=ntoken+1
                  w(i)=xtoken(ntoken)
 540           continue
               do 560 i=1,k1max
                  ntoken=ntoken+1
                  x(i)=xtoken(ntoken)
                  ntoken=ntoken+1
                  y(i)=xtoken(ntoken)
                  ntoken=ntoken+1
                  z(i)=xtoken(ntoken)
 560           continue
               ntoken=ntoken+1
               v(1)=xtoken(ntoken)
               ntoken=ntoken+1
               v(2)=xtoken(ntoken)
               npointso=npoints
               if(iopt_nurbl.ge.1.and.ifnurbl.ge.0) then
                  irow=irow+1
                  ifnurbl=ifnurbl+1
                  call nurbl(iopt_nurbl,
     *                       k1max,kl1point,
     *                       m1,ia,s,w,x,y,z,v,
     *                       irow,ipt,ict,icttot,
     *                       npoints,ntets,nbpoints,nbtets,
     *                       itoff,jtoff)
               endif
               npnurbl=npnurbl+(npoints-npointso)
            endif
            if(itoken(1).eq.116) then
               k1=1
               ntoken=1
               ntoken=ntoken+1
               x(1)=xtoken(ntoken)
               ntoken=ntoken+1
               y(1)=xtoken(ntoken)
               ntoken=ntoken+1
               z(1)=xtoken(ntoken)
               npointso=npoints
               if(iopt_nurbp.ge.1.and.ifnurbp.ge.0) then
                  irow=irow+1
                  ifnurbp=ifnurbp+1
                  call nurbp(iopt_nurbp,
     *                       k1,
     *                       x,y,z,
     *                       irow,ipt,ict,icttot,
     *                       npoints,ntets,nbpoints,nbtets,
     *                       itoff,jtoff)
               endif
               npnurbp=npnurbp+(npoints-npointso)
            endif
            isave=imsgin(1)
            idummy=0
         endif
         if(isave.eq.116.or.isave.eq.126.or.isave.eq.128) then
            nwords=nwds
            if(msgtype(nwds-1).eq.3.and.msgtype(nwds).eq.1) then
               nwords=nwds-2
            endif
            call mmgetlen(ipxtoken,lenxtoken,icscode)
            length=max(lenxtoken,idummy+nwords)
            call mmnewlen('itoken',isubname,ipitoken,length,icscode)
            call mmnewlen('xtoken',isubname,ipxtoken,length,icscode)
            do 400 i=1,nwords
               idummy=idummy+1
               if(idummy.gt.10) then
                  if(msgtype(i).eq.1) then
                     xtoken(idummy)=imsgin(i)
                  elseif(msgtype(i).eq.2) then
                     xtoken(idummy)=xmsgin(i)
                  endif
               else
                  if(msgtype(i).eq.1) then
                     itoken(idummy)=imsgin(i)
                  elseif(msgtype(i).eq.2) then
                     itoken(idummy)=xmsgin(i)
                  endif
               endif
 400        continue
            do i=1,nwds
               msgtype(i)=-1
               imsgin(i)=0
               xmsgin(i)=0.0
               cmsgin(i)=' '
            enddo
         endif
         goto 200
 300     continue
      close(iunit)
      call cmo_get_name(cmo,ierror)
      if(iopt_nurbs.gt.0.or.iopt_nurbl.gt.0) then
         call geniee_cmo(cmo)
      endif
C
      write(logmess,'("IGES NURB-Surfaces(128) read =",i5)') ifnurbs
      call writloga('default',1,logmess,0,ierrwrt)
      write(logmess,'("IGES NURB-Curves(126) read =",i5)') ifnurbl
      call writloga('default',0,logmess,0,ierrwrt)
      write(logmess,'("IGES NURB-Points(116) read =",i5)') ifnurbp
      call writloga('default',0,logmess,1,ierrwrt)
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
