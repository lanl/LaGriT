*dk,grid_to_grid
      subroutine grid_to_grid(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C        $Log: grid_to_grid.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.10   21 Mar 2002 10:13:22   dcg
CPVCS    move subroutine from temphet to here
CPVCS    
CPVCS       Rev 1.9   Fri Jan 22 12:12:52 1999   dcg
CPVCS    fix typo it should have been its
CPVCS
CPVCS       Rev 1.8   Mon Apr 14 16:50:28 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.7   Mon Apr 07 14:55:22 1997   het
CPVCS    Add the degenerate quad to triangle option.
CPVCS
CPVCS       Rev 1.6   Thu Mar 06 21:53:34 1997   het
CPVCS    Add the amrtox3d routione.
CPVCS
CPVCS       Rev 1.5   Fri Jan 24 13:46:24 1997   het
CPVCS    Add the (degenerate)hex to hybrid grid option.
CPVCS
CPVCS       Rev 1.4   Tue Nov 26 13:51:42 1996   het
CPVCS    Fix an error with pyrs
CPVCS
CPVCS       Rev 1.3   Thu Nov 21 19:05:42 1996   het
CPVCS    Add a new routine to classify elements into tet, pyr, pri or hex.
CPVCS
CPVCS       Rev 1.2   Thu Mar 14 13:38:04 1996   het
CPVCS    Change the call to the refine commands to add names.
CPVCS
CPVCS       Rev 1.1   Fri Feb 16 21:52:20 1996   het
CPVCS    Correct errors.
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 15:20:46 1996   dcg
CPVCS    Initial revision.
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
C
      include "local_element.h"
C
C ######################################################################
C
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
      character*32 coption1, coption2
C
      if(nwds.le.1) then
         goto 9999
      elseif(nwds.le.2) then
         coption1=cmsgin(2)
         call cmo_get_name(coption2,ierror)
      elseif(nwds.le.3) then
         coption1=cmsgin(2)
         coption2=cmsgin(3)
      endif
C
      if(coption1(1:icharlnf(coption1)).eq.'hybrid') then
         call cmo_to_hybrid(coption2)
      elseif(coption1(1:icharlnf(coption1)).eq.'amrtox3d') then
         call amr_to_x3d(coption2)
      endif
C
      goto 9999
 9999 continue
      return
      end
*dk,cmo_to_hybrid
      subroutine cmo_to_hybrid(cmo)
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
C
      character*(*) cmo
C
      include "local_element.h"
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(1000000), itetoff(1000000), jtetoff(1000000)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(1000000), jtet1(1000000)
C
      pointer (ipipflag1, ipflag1)
      pointer (ipipflag2, ipflag2)
      pointer (ipipflag3, ipflag3)
      pointer (ipipflag4, ipflag4)
      integer ipflag1(1000000), ipflag2(1000000),
     *        ipflag3(1000000), ipflag4(1000000)
C
      character*32 isubname
C
      real*8 xicvol(100), yicvol(100), zicvol(100)
C
C ######################################################################
C
C
      isubname='cmo_to_hybrid'
C
      call cmo_get_info('nnodes',cmo,nnodes,ilen,itype,icscode)
      call cmo_get_info('nelements',cmo,nelements,ilen,itype,icscode)
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,icscode)
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,icscode)
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,icscode)
      call cmo_get_info('itettyp',cmo,ipitettyp,ilen,itype,icscode)
      call cmo_get_info('itetoff',cmo,ipitetoff,ilen,itype,icscode)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,ilen,itype,icscode)
      call cmo_get_info('itet',cmo,ipitet,ilen,itype,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,itype,icscode)
C
      length=nnodes
      call mmgetblk('ipflag1',isubname,ipipflag1,length,1,icscode)
      call mmgetblk('ipflag3',isubname,ipipflag3,length,1,icscode)
      length=2*nnodes
      call mmgetblk('ipflag2',isubname,ipipflag2,length,1,icscode)
      call mmgetblk('ipflag4',isubname,ipipflag4,length,1,icscode)
C
      do i1=1,nnodes
         ipflag1(i1)=0
         ipflag2(i1)=0
         ipflag3(i1)=0
         ipflag4(i1)=0
      enddo
      ntris=0
      ntets=0
      npyramids=0
      nprisms=0
      nothers=0
      do it=1,nelements
         ipcount=0
         do i=1,nelmnen(itettyp(it))
            i1=itet1(itetoff(it)+i)
            if(ipflag1(i1).eq.0) then
               ipcount=ipcount+1
               ipflag2(ipcount)=i1
            endif
            ipflag1(i1)=ipflag1(i1)+1
         enddo
         if(ipcount.ne.nelmnen(itettyp(it))) then
C*****            print *,"Degenerate element: ",it,
C*****     *              nelmnen(itettyp(it)),
C*****     *              ipcount,
C*****     *              (itet1(itetoff(it)+i),i=1,nelmnen(itettyp(it)))
            ipoints=0
            iedges=0
            itris=0
            iquads=0
            do i=1,nelmnef(itettyp(it))
               ifcount=0
               do j=1,ielmface0(i,itettyp(it))
                  i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
                  if(ipflag3(i1).eq.0) then
                     ifcount=ifcount+1
                     ipflag4(ifcount)=i1
                  endif
                  ipflag3(i1)=ipflag3(i1)+1
               enddo
               do j=1,ifcount
                  i1=ipflag4(j)
                  ipflag3(i1)=0
               enddo
               if(ifcount.eq.1) then
                  ipoints=ipoints+1
               elseif(ifcount.eq.2) then
                  iedges=iedges+1
               elseif(ifcount.eq.3) then
                  itris=itris+1
               elseif(ifcount.eq.4) then
                  iquads=iquads+1
               endif
C*****               print *,"Degenerate face: ",it,i,
C******              ielmface0(i,itettyp(it)),
C******              ifcount,
C******              (itet1(itetoff(it)+ielmface1(j,i,itettyp(it))),
C******                 j=1,ielmface0(i,itettyp(it)))
            enddo
C*****      print *,"points, edges, tris, quads: ",
C******              ipoints,iedges,itris,iquads
            if(itris.eq.4.and.iquads.eq.0) then
               ntets=ntets+1
C*****         print *,"Must be a tet"
               if(itettyp(it).eq.ifelmpyr .or.
     *            itettyp(it).eq.ifelmpri .or.
     *            itettyp(it).eq.ifelmhex) then
                  icount=0
                  do i=1,ipcount
                     i1=ipflag2(i)
                     if(i.eq.1) j1=i1
                     if(i.eq.2) j2=i1
                     if(i.eq.3) j3=i1
                     if(i.eq.4) j4=i1
                  enddo
                  xicvol(1)=xic(j1)
                  yicvol(1)=yic(j1)
                  zicvol(1)=zic(j1)
                  xicvol(2)=xic(j2)
                  yicvol(2)=yic(j2)
                  zicvol(2)=zic(j2)
                  xicvol(3)=xic(j3)
                  yicvol(3)=yic(j3)
                  zicvol(3)=zic(j3)
                  xicvol(4)=xic(j4)
                  yicvol(4)=yic(j4)
                  zicvol(4)=zic(j4)
                  call volume_element(ifelmtet,
     *                                xicvol,yicvol,zicvol,
     *                                xtetvol)
                  if(xtetvol.le.0.0d+00) then
                     jsave=j3
                     j3=j4
                     j4=jsave
                  endif
                  itettyp(it)=ifelmtet
                  itet1(itetoff(it)+1)=j1
                  itet1(itetoff(it)+2)=j2
                  itet1(itetoff(it)+3)=j3
                  itet1(itetoff(it)+4)=j4
               endif
            elseif(itris.eq.4.and.iquads.eq.1) then
               npyramids=npyramids+1
C*****         print *,"Must be a pyramid"
               if(itettyp(it).eq.ifelmhex) then
                  do l=1,ipcount
                     i1=ipflag2(l)
                     if(ipflag1(i1).eq.4) then
                        j5=i1
                        do i=1,nelmnef(itettyp(it))
                           ifcount=0
                           do j=1,ielmface0(i,itettyp(it))
                              i1=itet1(itetoff(it)+
     *                                 ielmface1(j,i,itettyp(it)))
                              if(ipflag3(i1).eq.0) then
                                 ifcount=ifcount+1
                                 ipflag4(ifcount)=i1
                                 ipflag3(i1)=ipflag3(i1)+1
                              endif
                           enddo
                           do j=1,ifcount
                              i1=ipflag4(j)
                              ipflag3(i1)=0
                           enddo
                           if(ifcount.eq.4) then
                              j1=itet1(itetoff(it)+
     *                                 ielmface1(1,i,itettyp(it)))
                              j2=itet1(itetoff(it)+
     *                                 ielmface1(2,i,itettyp(it)))
                              j3=itet1(itetoff(it)+
     *                                 ielmface1(3,i,itettyp(it)))
                              j4=itet1(itetoff(it)+
     *                                 ielmface1(4,i,itettyp(it)))
                              goto 100
                           endif
                        enddo
                     endif
                  enddo
               elseif(itettyp(it).eq.ifelmpri) then
                  do l=1,ipcount
                     i1=ipflag2(l)
                     if(ipflag1(i1).eq.2) then
                        j5=i1
                        do i=1,nelmnef(itettyp(it))
                           ifcount=0
                           do j=1,ielmface0(i,itettyp(it))
                              i1=itet1(itetoff(it)+
     *                                 ielmface1(j,i,itettyp(it)))
                              if(ipflag3(i1).eq.0) then
                                 ifcount=ifcount+1
                                 ipflag4(ifcount)=i1
                                 ipflag3(i1)=ipflag3(i1)+1
                              endif
                           enddo
                           do j=1,ifcount
                              i1=ipflag4(j)
                              ipflag3(i1)=0
                           enddo
                           if(ifcount.eq.4) then
                              j1=itet1(itetoff(it)+
     *                                 ielmface1(1,i,itettyp(it)))
                              j2=itet1(itetoff(it)+
     *                                 ielmface1(2,i,itettyp(it)))
                              j3=itet1(itetoff(it)+
     *                                 ielmface1(3,i,itettyp(it)))
                              j4=itet1(itetoff(it)+
     *                                 ielmface1(4,i,itettyp(it)))
                              goto 100
                           endif
                        enddo
                     endif
                  enddo
               endif
 100           continue
               xicvol(1)=xic(j1)
               yicvol(1)=yic(j1)
               zicvol(1)=zic(j1)
               xicvol(2)=xic(j2)
               yicvol(2)=yic(j2)
               zicvol(2)=zic(j2)
               xicvol(3)=xic(j3)
               yicvol(3)=yic(j3)
               zicvol(3)=zic(j3)
               xicvol(4)=xic(j4)
               yicvol(4)=yic(j4)
               zicvol(4)=zic(j4)
               xicvol(5)=xic(j5)
               yicvol(5)=yic(j5)
               zicvol(5)=zic(j5)
               call volume_element(ifelmpyr,
     *                             xicvol,yicvol,zicvol,
     *                             xtetvol)
               itettyp(it)=ifelmpyr
               itet1(itetoff(it)+1)=j1
               itet1(itetoff(it)+2)=j4
               itet1(itetoff(it)+3)=j3
               itet1(itetoff(it)+4)=j2
               itet1(itetoff(it)+5)=j5
            elseif(itris.eq.2.and.iquads.eq.3) then
               nprisms=nprisms+1
C*****         print *,"Must be a prism"
               if(itettyp(it).eq.ifelmhex) then
                  itricount=0
                  do i=1,nelmnef(itettyp(it))
                     ifcount=0
                     do j=1,ielmface0(i,itettyp(it))
                        i1=itet1(itetoff(it)+
     *                           ielmface1(j,i,itettyp(it)))
                        if(ipflag3(i1).eq.0) then
                           ifcount=ifcount+1
                           ipflag4(ifcount)=i1
                        endif
                        ipflag3(i1)=ipflag3(i1)+1
                     enddo
                     if(ifcount.eq.3) then
                        if(itricount.eq.0) then
                           itricount=itricount+1
                           do j=1,ifcount
                              i1=ipflag4(j)
                              if(ipflag3(i1).eq.2) then
                                 ipsave=j
                              endif
                           enddo
                           if(ipsave.eq.1) then
                              j1=ipflag4(1)
                              j2=ipflag4(2)
                              j3=ipflag4(3)
                           elseif(ipsave.eq.2) then
                              j1=ipflag4(2)
                              j2=ipflag4(3)
                              j3=ipflag4(1)
                           elseif(ipsave.eq.3) then
                              j1=ipflag4(3)
                              j2=ipflag4(1)
                              j3=ipflag4(2)
                           endif
                        else
                           itricount=itricount+1
                           do j=1,ifcount
                              i1=ipflag4(j)
                              if(ipflag3(i1).eq.2) then
                                 ipsave=j
                              endif
                           enddo
                           if(ipsave.eq.1) then
                              j4=ipflag4(1)
                              j5=ipflag4(2)
                              j6=ipflag4(3)
                           elseif(ipsave.eq.2) then
                              j4=ipflag4(2)
                              j5=ipflag4(3)
                              j6=ipflag4(1)
                           elseif(ipsave.eq.3) then
                              j4=ipflag4(3)
                              j5=ipflag4(1)
                              j6=ipflag4(2)
                           endif
                        endif
                     endif
                     do j=1,ifcount
                        i1=ipflag4(j)
                        ipflag3(i1)=0
                     enddo
                  enddo
                  itettyp(it)=ifelmpri
                  itet1(itetoff(it)+1)=j1
                  itet1(itetoff(it)+2)=j3
                  itet1(itetoff(it)+3)=j2
                  itet1(itetoff(it)+4)=j4
                  itet1(itetoff(it)+5)=j5
                  itet1(itetoff(it)+6)=j6
               endif
            elseif(iedges.eq.3) then
               ntris=ntris+1
               icount=0
               do i=1,nelmnen(itettyp(it))
                  if(i.eq.1) then
                     im1=nelmnen(itettyp(it))
                  else
                     im1=i-1
                  endif
                  j1=itet1(itetoff(it)+im1)
                  j2=itet1(itetoff(it)+i)
                  if(j2.ne.j1) then
                     icount=icount+1
                     itet1(itetoff(it)+icount)=j2
                  endif
               enddo
               itettyp(it)=ifelmtri
            else
               nothers=nothers+1
C*****         print *,"Must be an other "
            endif
         endif
         do i=1,ipcount
            i1=ipflag2(i)
            ipflag1(i1)=0
         enddo
      enddo
C
C*****print *,"Degenerate elements: ",ntets,npyramids,nprisms,nothers
C
      if(ntris.gt.0 .or.
     *   ntets.gt.0  .or.
     *   npyramids.gt.0 .or.
     *   nprisms.gt.0) then
         call cmo_set_info('nodes_per_element',cmo,
     *                     nen1,ilen,itype,ierr)
         call cmo_set_info('faces_per_element',cmo,
     *                     nef1,ilen,itype,ierr)
         nen3 = nelmnen(ifelmhyb)
         nef3 = nelmnef(ifelmhyb)
         call cmo_set_info('nodes_per_element',cmo,nen3,1,1,ierr)
         call cmo_set_info('faces_per_element',cmo,nef3,1,1,ierr)
C
         call geniee_cmo(cmo)
C
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end

      subroutine amr_to_x3d(cmo)
C
C #####################################################################
C
C     PURPOSE -
C
C        WRITE AN AMR DUMPFILE.
C
C     INPUT ARGUMENTS -
C
C        None
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: grid_to_grid.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'
C
      character*(*) cmo
C
      pointer (ipimt1, imt1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer  imt1(1000000)
      real*8 xic(1000000), yic(1000000), zic(1000000)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      integer itet1(1000000), jtet1(1000000)
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000), itetkid(1000000), itetlev(1000000)
C
      pointer (ipitetbnd, itetbnd)
      integer itetbnd(1000000)
C
      pointer (ipktetcnt, ktetcnt)
      pointer (ipktetoff, ktetoff)
      pointer (ipktet, ktet1)
      integer ktetcnt(1000000), ktetoff(1000000), ktet1(1000000)
C
      integer nnodes, nelements, mbndry, nen_cmo, nef_cmo
      integer length, icmotype, ierror, ilen, ityp, ierr, icscode
      integer it, it2, if2, itpar,
     *        i, j, jt, jf, kt, i1, i2, i3,  k1, k2,
     *        itoff, jtoff,
     *        icount, naddpts, naddelm,
     *        npointsnew, ntetsnew, ipointi, ipointj,
     *        npointsinc, ntetsinc, ninc, inc,
     *        nelementsmm, nnodesmm, iflag
C
      real*8 epsilonl
      real*8 xavg, yavg, zavg
C
      character*32 isubname
      character*8 cglobal, cdefault
C
      integer ifaddit(7), ifaddif(7)
C
C
C ######################################################################
C
C
      isubname='amr_to_x3d'
      cglobal='global'
      cdefault='default'
C
C
C     ******************************************************************
C
      call cmo_get_name(cmo,ierror)
C
      call get_epsilon('epsilonl', epsilonl)
C
      call cmo_get_info('nnodes',cmo,nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,
     *                  nelements,length,icmotype,ierror)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen_cmo,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef_cmo,length,icmotype,ierror)
C
C  get mesh object information
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      call cmo_get_info('itetclr',cmo,
     *                        ipitetclr,ilen,ityp,ierr)
      call cmo_get_info('itettyp',cmo,
     *                        ipitettyp,ilen,ityp,ierr)
      call cmo_get_info('itetoff',cmo,
     *                        ipitetoff,ilen,ityp,ierr)
      call cmo_get_info('jtetoff',cmo,
     *                        ipjtetoff,ilen,ityp,ierr)
      call cmo_get_info('itet',cmo,ipitet,ilen,ityp,ierr)
      call cmo_get_info('jtet',cmo,ipjtet,ilen,ityp,ierr)
C
      call mmfindbk('itetpar',cmo,ipitetpar,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetpar',isubname,ipitetpar,length,1,icscode)
         do it=1,nelements
            itetpar(it)=0
         enddo
      endif
      call mmfindbk('itetkid',cmo,ipitetkid,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetkid',isubname,ipitetkid,length,1,icscode)
         do it=1,nelements
            itetkid(it)=0
         enddo
      endif
      call mmfindbk('itetlev',cmo,ipitetlev,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetlev',isubname,ipitetlev,length,1,icscode)
         do it=1,nelements
            itetlev(it)=0
         enddo
      endif
C
      length=nelements
      call mmgetblk('ktetcnt',isubname,ipktetcnt,length,1,icscode)
      call mmgetblk('ktetoff',isubname,ipktetoff,length,1,icscode)
      length=nef_cmo*nelements
      call mmgetblk('ktet',isubname,ipktet,length,1,icscode)
      call get_node_connectivity('-active-',
     *                           cmo,
     *                           ipktetcnt,
     *                           ipktetoff,
     *                           ipktet,
     *                           ierror)
      call get_element_connectivity('-active-','jtet',
     *                              cmo,
     *                              ipktetcnt,
     *                              ipktetoff,
     *                              ipktet,
     *                              ierror)
C
      length=nelements
      call mmgetblk('itetbnd',isubname,ipitetbnd,length,1,icscode)
      do it=1,nelements
         itetbnd(it)=0
      enddo
C
      do it=1,nelements
         if(ktetcnt(it).gt.0) then
            do i=1,ktetcnt(it)
               if(ktet1(ktetoff(it)+i).gt.0 .and.
     *            ktet1(ktetoff(it)+i).lt.mbndry) then
                  it2=1+(ktet1(ktetoff(it)+i)-1)/nef_cmo
                  if2=ktet1(ktetoff(it)+i)-nef_cmo*(it2-1)
                  if(it2.eq.it) then
                     itpar=itetpar(it)
                     if(jtet1(jtetoff(itpar)+if2).lt.0 .or.
     *                  jtet1(jtetoff(itpar)+if2).ge.mbndry) then
                     else
                        jt=1+(jtet1(jtetoff(itpar)+if2)-1)/nef_cmo
                        jf=jtet1(jtetoff(itpar)+if2)-nef_cmo*(jt-1)
                        if(itetlev(jt).lt.itetlev(it)) then
                           itetbnd(jt)=itetbnd(jt)+1
                        endif
                     endif
                  endif
               endif
            enddo
         endif
      enddo
C
      npointsnew=nnodes
      ntetsnew=nelements
C
      itoff=itetoff(nelements)+nelmnen(itettyp(nelements))
      jtoff=jtetoff(nelements)+nelmnef(itettyp(nelements))
      iflag=0
      do it=1,nelements
         if(itetbnd(it).gt.0) then
            iflag=iflag+1
            xavg=0.0
            yavg=0.0
            zavg=0.0
            do i=1,nelmnen(itettyp(it))
               i1=itet1(itetoff(it)+i)
               xavg=xavg+xic(i1)
               yavg=yavg+yic(i1)
               zavg=zavg+zic(i1)
            enddo
            xavg=xavg/nelmnen(itettyp(it))
           yavg=yavg/nelmnen(itettyp(it))
            zavg=zavg/nelmnen(itettyp(it))
            naddpts=1
            naddelm=0
            do i=1,nelmnef(itettyp(it))
               if(jtet1(jtetoff(it)+i).gt.0 .and.
     *            jtet1(jtetoff(it)+i).lt.mbndry) then
                  jt=1+(jtet1(jtetoff(it)+i)-1)/nef_cmo
                  jf=jtet1(jtetoff(it)+i)-nef_cmo*(jt-1)
                  if(itetkid(jt).ne.0) then
                     naddpts=naddpts+1
                     naddelm=naddelm+2
                     ifaddit(i)=jt
                     ifaddif(i)=jf
                  else
                     naddelm=naddelm+1
                     ifaddit(i)=0
                     ifaddif(i)=0
                  endif
               elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
                  jt=1+(jtet1(jtetoff(it)+i)-mbndry-1)/nef_cmo
                  jf=jtet1(jtetoff(it)+i)-mbndry-nef_cmo*(jt-1)
                  if(itetkid(jt).ne.0) then
                     naddpts=naddpts+1
                     naddelm=naddelm+2
                     ifaddit(i)=jt
                     ifaddif(i)=jf
                  else
                     naddelm=naddelm+1
                     ifaddit(i)=0
                     ifaddif(i)=0
                  endif
               else
                  naddelm=naddelm+1
                  ifaddit(i)=-1
                  ifaddif(i)=-1
               endif
            enddo
            npointsnew=npointsnew+1
            call mmfindbk('xic',cmo,ipxic,length,icscode)
            if(npointsnew.gt.length) then
               npointsinc=npointsnew+1000
               call cmo_set_info('nnodes',cmo,npointsinc,1,1,ierror)
               call mmgetlen(ipitetclr,nelementsmm,icscode)
               call cmo_set_info('nelements',cmo,nelementsmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('xic',cmo,ipxic,ilen,icmotype,ierror)
               call cmo_get_info('yic',cmo,ipyic,ilen,icmotype,ierror)
               call cmo_get_info('zic',cmo,ipzic,ilen,icmotype,ierror)
            endif
            ninc=naddelm
            call mmgetlen(ipitetbnd,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               ntetsinc=ntetsnew+inc
               call mmnewlen('itetbnd',isubname,ipitetbnd,ntetsinc,
     *                       icscode)
            endif
            call mmgetlen(ipitetclr,length,icscode)
            if((ntetsnew+ninc).gt.length) then
               inc=1000
               ntetsinc=ntetsnew+inc
               call cmo_set_info('nelements',cmo,ntetsinc,1,1,ierror)
               call mmfindbk('xic',cmo,ipxic,nnodesmm,icscode)
               call cmo_set_info('nnodes',cmo,nnodesmm,1,1,ierror)
               call cmo_newlen(cmo,ierror)
               call cmo_get_info('itetpar',cmo,
     *                           ipitetpar,ilen,icmotype,ierror)
               call cmo_get_info('itetkid',cmo,
     *                           ipitetkid,ilen,icmotype,ierror)
               call cmo_get_info('itetlev',cmo,
     *                           ipitetlev,ilen,icmotype,ierror)
               call cmo_get_info('itetclr',cmo,
     *                           ipitetclr,ilen,icmotype,ierror)
               call cmo_get_info('itettyp',cmo,
     *                           ipitettyp,ilen,icmotype,ierror)
               call cmo_get_info('itetoff',cmo,
     *                           ipitetoff,ilen,icmotype,ierror)
               call cmo_get_info('jtetoff',cmo,
     *                           ipjtetoff,ilen,icmotype,ierror)
               call cmo_get_info('itet',cmo,
     *                           ipitet,ilen,icmotype,ierror)
               call cmo_get_info('jtet',cmo,
     *                           ipjtet,ilen,icmotype,ierror)
            endif
            xic(npointsnew)=xavg
            yic(npointsnew)=yavg
            zic(npointsnew)=zavg
            do i=1,nelmnef(itettyp(it))
               if(ifaddit(i).gt.0) then
                  jt=ifaddit(i)
                  jf=ifaddif(i)
                  i1=itet1(itetoff(jt)+ielmface1(1,jf,itettyp(jt)))
                  i2=itet1(itetoff(jt)+ielmface1(2,jf,itettyp(jt)))
                  j=0
                  i3=0
                  dowhile(j.lt.ktetcnt(it).and.i3.eq.0)
                     j=j+1
                     kt=1+(ktet1(ktetoff(it)+j)-1)/nef_cmo
                     if(itetpar(kt).eq.jt) then
                        k1=itet1(itetoff(kt) +
     *                           ielmface1(1,jf,itettyp(kt)))
                        k2=itet1(itetoff(kt) +
     *                           ielmface1(2,jf,itettyp(kt)))
                        if(i1.eq.k1) then
                           i3=k2
                        elseif(i1.eq.k2) then
                           i3=k1
                        elseif(i2.eq.k1) then
                           i3=k2
                        elseif(i2.eq.k2) then
                           i3=k1
                        endif
                     endif
                  enddo
                  ntetsnew=ntetsnew+1
                     itetbnd(ntetsnew)=0
                     itetpar(ntetsnew)=0
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=0
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmtri
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                     itet1(itetoff(ntetsnew)+1)=npointsnew
                     itet1(itetoff(ntetsnew)+2)=i3
                     itet1(itetoff(ntetsnew)+3)=i1
                     jtet1(jtetoff(ntetsnew)+1)=-1
                     jtet1(jtetoff(ntetsnew)+2)=-1
                     jtet1(jtetoff(ntetsnew)+3)=-1
                  ntetsnew=ntetsnew+1
                     itetbnd(ntetsnew)=0
                     itetpar(ntetsnew)=0
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=0
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmtri
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                     itet1(itetoff(ntetsnew)+1)=npointsnew
                     itet1(itetoff(ntetsnew)+2)=i2
                     itet1(itetoff(ntetsnew)+3)=i3
                     jtet1(jtetoff(ntetsnew)+1)=-1
                     jtet1(jtetoff(ntetsnew)+2)=-1
                     jtet1(jtetoff(ntetsnew)+3)=-1
               else
                  i1=itet1(itetoff(it)+ielmface1(1,i,itettyp(it)))
                  i2=itet1(itetoff(it)+ielmface1(2,i,itettyp(it)))
                  ntetsnew=ntetsnew+1
                     itetbnd(ntetsnew)=0
                     itetpar(ntetsnew)=0
                     itetkid(ntetsnew)=0
                     itetlev(ntetsnew)=0
                     itetclr(ntetsnew)=itetclr(it)
                     itettyp(ntetsnew)=ifelmtri
                     itetoff(ntetsnew)=itoff
                     jtetoff(ntetsnew)=jtoff
                     itoff=itoff+nelmnen(itettyp(ntetsnew))
                     jtoff=jtoff+nelmnef(itettyp(ntetsnew))
                     itet1(itetoff(ntetsnew)+1)=npointsnew
                     itet1(itetoff(ntetsnew)+2)=i1
                     itet1(itetoff(ntetsnew)+3)=i2
                     jtet1(jtetoff(ntetsnew)+1)=-1
                     jtet1(jtetoff(ntetsnew)+2)=-1
                     jtet1(jtetoff(ntetsnew)+3)=-1
               endif
            enddo
         endif
      enddo
C
      if(iflag.gt.0) then
C
         ipointi=nnodes+1
         ipointj=npointsnew
         call set_info_i('ipointi',cmo,cglobal,cdefault,
     *                   ipointi,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
         call set_info_i('ipointj',cmo,cglobal,cdefault,
     *                   ipointj,icscode)
         if (icscode .ne. 0) call x3d_error(isubname,'get_info_i')
C
         nen_cmo=nelmnen(ifelmhyb)
         nef_cmo=nelmnef(ifelmhyb)
         nnodes=npointsnew
         nelements=ntetsnew
         call cmo_set_info('nnodes',cmo,nnodes,1,1,ierror)
         call cmo_set_info('nelements',cmo,nelements,1,1,ierror)
         call cmo_set_info('nodes_per_element',cmo,nen_cmo,1,1,ierr)
         call cmo_set_info('faces_per_element',cmo,nef_cmo,1,1,ierr)
C
         icount=0
         itoff=0
         jtoff=0
         do it=1,nelements
            if(itetkid(it).eq.0.and.itetbnd(it).eq.0) then
               icount=icount+1
               itetpar(icount)=0
               itetkid(icount)=0
               itetlev(icount)=0
               itetclr(icount)=itetclr(it)
               itettyp(icount)=itettyp(it)
               itetoff(icount)=itoff
               jtetoff(icount)=jtoff
               itoff=itoff+nelmnen(itettyp(icount))
               jtoff=jtoff+nelmnef(itettyp(icount))
               do i=1,nelmnen(itettyp(it))
                  i1=itet1(itetoff(it)+i)
                  itet1(itetoff(icount)+i)=i1
               enddo
               do i=1,nelmnef(itettyp(it))
                  jtet1(jtetoff(icount)+i)=-1
               enddo
            endif
         enddo
C
         nelements=icount
         call cmo_set_info('nelements',cmo,nelements,1,1,ierror)
C
         call geniee_cmo(cmo)
C
      endif
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
c
      subroutine get_element_connectivity(coption1,coption2,cmo,
     *                                    ipktetcnt,ipktetoff,
     *                                    ipktet,
     *                                    ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine constructs a JTET of AMR grids by assembling
C           a list of faces for each polyhedral element. The data
C           structure contains the element number across each face
C           and the local face number of the opposite element.
C           CMO. Inactive active elements are created by:
C              - AMR parent elements.
C              - Dudding elements by setting there color LE zero.
C
C     INPUT ARGUMENTS -
C
C        coption1  - Option to be performed by this routine:
C                    == '-all-' ==> Do all elements.
C                    == '-active-' ==> Do all active elements.
C        coption2  - Type of nearest neighbors to be returned
C                    == 'jtet' ==> JTET style neighbors with
C                                  nef*(element-1)+face, mbndry,etc.
C                    == 'flag3d' ==> FLAG3D style neighbors.
C        cmo      - Character name of the CMO to work on.
C
C     OUTPUT ARGUMENTS -
C
C        ipktetcnt - The number of faces for each element.
C        ipktetoff - The offset to the start of the list of neiboring
C                       faces and elements for each element. This is
C                       zero based.
C        ipktet - The list of neighboring elements and faces.
C        ierror     - A return error flag (=0 ==> OK, <>0 ==> ERROR)
C
C     CHANGE HISTORY -
C
C        $Log: grid_to_grid.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'
C
      character coption1*(*), coption2*(*), cmo*(*)
      integer ierror
      pointer (ipktetcnt, ktetcnt)
      pointer (ipktetoff, ktetoff)
      pointer (ipktet, ktet1)
      integer ktetcnt(1000000), ktetoff(1000000), ktet1(1000000)
C
C
C ######################################################################
C
      integer nelements
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000), itetkid(1000000), itetlev(1000000)
C
      pointer (ipitettyp, itettyp)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(1000000), jtetoff(1000000)
      pointer (ipjtet, jtet1)
      integer jtet1(1000000)
C
      pointer (ipitactive, itactive)
      integer itactive(1000000)
C
      integer icscode, length, it, lencmo, itpcmo, icount, jcount,
     *        i, nef_cmo, iflag, itpar, jt, jf, nface,
     *        ntactive, iactive, mbndry, jtoff
C
      integer icharlnf
C
      character*32 isubname, cblknam, cprtnam
C
C
C ######################################################################
C
C
C     ..................................................................
C     DEFINE THE NAME OF THE MEMORY MANAGEMENT NAME.
C
      isubname='get_element_connectivity'
C
C
C     ..................................................................
C     CHECK TO SEE IF THIS CMO EXISTS. IF NOT THE SET ERROR CODE AND
C        RETURN.
C
      call cmo_exist(cmo,icscode)
      if(icscode.ne.0) then
         ierror=-1
         goto 9999
      endif
C
C
C     ..................................................................
C     FETCH CMO INFORMATION NEEDED BY THIS ROUTINE.
C
      call cmo_get_info('nelements',cmo,nelements,lencmo,itpcmo,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,lencmo,itpcmo,icscode)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef_cmo,lencmo,itpcmo,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,lencmo,itpcmo,icscode)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lencmo,itpcmo,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,lencmo,itpcmo,icscode)
C
C
C     ..................................................................
C     MAKE SURE THE INPUT COUNTER ARRAYS HAVE BEEN DEFINED AND GIVEN
C        THE CORRECT LENGTH.
C
      if(ipktetcnt.le.0) then
         ierror=-1
         goto 9999
      else
         length=nelements
         call mmgetnam(ipktetcnt,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktetcnt,length,icscode)
      endif
      if(ipktetoff.le.0) then
         ierror=-1
         goto 9999
      else
         length=nelements
         call mmgetnam(ipktetoff,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktetoff,length,icscode)
      endif
      do it=1,nelements
         ktetcnt(it)=0
         ktetoff(it)=-1
      enddo
C
C
C     ..................................................................
C     CHECK TO SEE IF THIS IS AN AMR GRID.
C
C
      call mmfindbk('itetpar',cmo,ipitetpar,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetpar',isubname,ipitetpar,length,1,icscode)
         do it=1,nelements
            itetpar(it)=0
         enddo
      endif
      call mmfindbk('itetkid',cmo,ipitetkid,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetkid',isubname,ipitetkid,length,1,icscode)
         do it=1,nelements
            itetkid(it)=0
         enddo
      endif
      call mmfindbk('itetlev',cmo,ipitetlev,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetlev',isubname,ipitetlev,length,1,icscode)
         do it=1,nelements
            itetlev(it)=0
         enddo
      endif
C
C
C     ..................................................................
C     GET THE LIST OF ACTIVE ELEMENTS FOR THIS CMO.
C
      length=nelements
      call mmgetblk('itactive',isubname,ipitactive,length,1,icscode)
      length=icharlnf(coption1)
      if(coption1(1:length).eq.'-all-') then
         ntactive=nelements
         do it=1,nelements
            itactive(it)=it
         enddo
      elseif(coption1(1:length).eq.'-active-') then
         ntactive=0
         call get_active_elements(cmo,ntactive,ipitactive)
      else
         ierror=-1
         goto 9999
      endif
C
C
C     ..................................................................
C     CONSTRUCT THE LIST OF FACES FOR EACH ACTIVE ELEMENT.
C
      nface=0
      do iactive=1,ntactive
         it=itactive(iactive)
         do i=1,nelmnef(itettyp(it))
            if(jtet1(jtetoff(it)+i).eq.mbndry) then
               if(itetpar(it).eq.0) then
                  nface=nface+1
                  ktetcnt(it)=ktetcnt(it)+1
               else
                  itpar=itetpar(it)
                  iflag=0
                  dowhile(iflag.eq.0)
                     if(itetpar(itpar).eq.0) then
                        iflag=1
                     elseif(jtet1(jtetoff(itpar)+i).lt.
     *                      mbndry) then
                        iflag=1
                     elseif(jtet1(jtetoff(itpar)+i).gt.
     *                      mbndry) then
                        iflag=1
                     else
                        itpar=itetpar(itpar)
                     endif
                  enddo
                  jtoff=jtetoff(itpar)+i
                  if(jtet1(jtoff).eq.mbndry) then
                     nface=nface+1
                     ktetcnt(it)=ktetcnt(it)+1
                  elseif(jtet1(jtoff).gt.mbndry) then
                     jt=1+(jtet1(jtoff)-mbndry-1)/nef_cmo
                     jf=jtet1(jtoff)-mbndry-nef_cmo*(jt-1)
                     if(itetkid(jt).eq.0) then
                        nface=nface+1
                        ktetcnt(it)=ktetcnt(it)+1
                        ktetcnt(jt)=ktetcnt(jt)+1
                     endif
                  else
                     jt=1+(jtet1(jtoff)-1)/nef_cmo
                     jf=jtet1(jtoff)-nef_cmo*(jt-1)
                     if(itetkid(jt).eq.0) then
                        nface=nface+1
                        ktetcnt(it)=ktetcnt(it)+1
                        ktetcnt(jt)=ktetcnt(jt)+1
                     endif
                  endif
               endif
            elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
               jt=1+(jtet1(jtetoff(it)+i)-mbndry-1)/nef_cmo
               jf=jtet1(jtetoff(it)+i)-mbndry-nef_cmo*(jt-1)
               if(itetkid(jt).eq.0) then
                  if(it.lt.jt) then
                     nface=nface+1
                     ktetcnt(it)=ktetcnt(it)+1
                     ktetcnt(jt)=ktetcnt(jt)+1
                  endif
               endif
            else
               jt=1+(jtet1(jtetoff(it)+i)-1)/nef_cmo
               jf=jtet1(jtetoff(it)+i)-nef_cmo*(jt-1)
               if(itetkid(jt).eq.0) then
                  if(it.lt.jt) then
                     nface=nface+1
                     ktetcnt(it)=ktetcnt(it)+1
                     ktetcnt(jt)=ktetcnt(jt)+1
                  endif
               endif
            endif
         enddo
      enddo
      icount=0
      do it=1,nelements
         if(ktetcnt(it).eq.0) then
            ktetoff(it)=-1
         else
            jcount=ktetcnt(it)
            ktetoff(it)=icount
            icount=icount+jcount
         endif
      enddo
      if(ipktet.le.0) then
         ierror=-1
         goto 9999
      else
         length=icount
         call mmgetnam(ipktet,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktet,length,icscode)
      endif
      do it=1,icount
         ktet1(it)=0
      enddo
      do it=1,nelements
         ktetcnt(it)=0
      enddo
C
      nface=0
      do iactive=1,ntactive
         it=itactive(iactive)
         do i=1,nelmnef(itettyp(it))
            if(jtet1(jtetoff(it)+i).eq.mbndry) then
               if(itetpar(it).eq.0) then
                  nface=nface+1
                  ktetcnt(it)=ktetcnt(it)+1
                  ktet1(ktetoff(it)+ktetcnt(it))=nef_cmo*(it-1)+i
               else
                  itpar=itetpar(it)
                  iflag=0
                  dowhile(iflag.eq.0)
                     if(itetpar(itpar).eq.0) then
                        iflag=1
                     elseif(jtet1(jtetoff(itpar)+i).lt.
     *                      mbndry) then
                        iflag=1
                     elseif(jtet1(jtetoff(itpar)+i).gt.
     *                      mbndry) then
                        iflag=1
                     else
                        itpar=itetpar(itpar)
                     endif
                  enddo
                  jtoff=jtetoff(itpar)+i
                  if(jtet1(jtoff).eq.mbndry) then
                        nface=nface+1
                        ktetcnt(it)=ktetcnt(it)+1
                        ktet1(ktetoff(it)+ktetcnt(it))=nef_cmo*(it-1)+i
                  elseif(jtet1(jtoff).gt.mbndry) then
                     jt=1+(jtet1(jtoff)-mbndry-1)/nef_cmo
                     jf=jtet1(jtoff)-mbndry-nef_cmo*(jt-1)
                     if(itetkid(jt).eq.0) then
                        nface=nface+1
                        ktetcnt(it)=ktetcnt(it)+1
                        ktetcnt(jt)=ktetcnt(jt)+1
                        if(itetlev(it).gt.itetlev(jt)) then
                           ktet1(ktetoff(it)+ktetcnt(it))=
     *                        nef_cmo*(it-1)+i
                        else
                           ktet1(ktetoff(it)+ktetcnt(it))=
     *                        nef_cmo*(jt-1)+jf
                        endif
                        ktet1(ktetoff(jt)+ktetcnt(jt))=nef_cmo*(it-1)+i
                     endif
                  else
                     jt=1+(jtet1(jtoff)-1)/nef_cmo
                     jf=jtet1(jtoff)-nef_cmo*(jt-1)
                     if(itetkid(jt).eq.0) then
                        nface=nface+1
                        ktetcnt(it)=ktetcnt(it)+1
                        ktetcnt(jt)=ktetcnt(jt)+1
                        if(itetlev(it).gt.itetlev(jt)) then
                           ktet1(ktetoff(it)+ktetcnt(it))=
     *                        nef_cmo*(it-1)+i
                        else
                           ktet1(ktetoff(it)+ktetcnt(it))=
     *                        nef_cmo*(jt-1)+jf
                        endif
                        ktet1(ktetoff(jt)+ktetcnt(jt))=nef_cmo*(it-1)+i
                     endif
                  endif
               endif
            elseif(jtet1(jtetoff(it)+i).gt.mbndry) then
               jt=1+(jtet1(jtetoff(it)+i)-mbndry-1)/nef_cmo
               jf=jtet1(jtetoff(it)+i)-mbndry-nef_cmo*(jt-1)
               if(itetkid(jt).eq.0) then
                  if(it.lt.jt) then
                     nface=nface+1
                     ktetcnt(it)=ktetcnt(it)+1
                     ktetcnt(jt)=ktetcnt(jt)+1
                     ktet1(ktetoff(it)+ktetcnt(it))=nef_cmo*(jt-1)+jf
                     ktet1(ktetoff(jt)+ktetcnt(jt))=nef_cmo*(it-1)+i
                  endif
               endif
            else
               jt=1+(jtet1(jtetoff(it)+i)-1)/nef_cmo
               jf=jtet1(jtetoff(it)+i)-nef_cmo*(jt-1)
               if(itetkid(jt).eq.0) then
                  if(it.lt.jt) then
                     nface=nface+1
                     ktetcnt(it)=ktetcnt(it)+1
                     ktetcnt(jt)=ktetcnt(jt)+1
                     ktet1(ktetoff(it)+ktetcnt(it))=nef_cmo*(jt-1)+jf
                     ktet1(ktetoff(jt)+ktetcnt(jt))=nef_cmo*(it-1)+i
                  endif
               endif
            endif
         enddo
      enddo
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
      subroutine get_node_connectivity(coption,cmo,
     *                                 ipktetcnt,ipktetoff,
     *                                 ipktet,
     *                                 ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine constructs a nodal connectivity matrix.
C
C     INPUT ARGUMENTS -
C
C        coption  - Option to be performed by this routine:
C        cmo      - Character name of the CMO to work on.
C
C     OUTPUT ARGUMENTS -
C
C        ipktetcnt - The number of faces for each element.
C        ipktetoff - The offset to the start of the list of neiboring
C                       faces and elements for each element. This is
C                       zero based.
C        ipktet - The list of neighboring elements and faces.
C        ierror     - A return error flag (=0 ==> OK, <>0 ==> ERROR)
C
C     CHANGE HISTORY -
C
C        $Log: grid_to_grid.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'
C
      character coption*(*), cmo*(*)
      integer ierror
      pointer (ipktetcnt, ktetcnt)
      pointer (ipktetoff, ktetoff)
      pointer (ipktet, ktet1)
      integer ktetcnt(1000000), ktetoff(1000000), ktet1(1000000)
C
C
C ######################################################################
C
      integer nnodes, nelements, mbndry, nen_cmo, nef_cmo
C
      pointer (ipitp1, itp1)
      pointer (ipisn1, isn1)
      integer itp1(1000000), isn1(1000000)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(1000000), jtetoff(1000000), itetoff(1000000)
      pointer (ipitet, itet1)
      integer itet1(1000000)
      pointer (ipjtet, jtet1)
      integer jtet1(1000000)
C
      integer ntactive
      pointer (ipitactive, itactive)
      integer itactive(1000000)
C
      pointer (ipireal1, ireal1)
      integer ireal1(1000000)
C
      pointer (ipiparent, iparent)
      integer iparent(1000000)
C
      integer icscode, lencmo, itpcmo, length
      integer itdum, it, ie, i, i1, i2, j1, j2, ktoff1, ktoff2
      integer isum, iflag
C
      integer icharlnf
C
      character*32 isubname, cblknam, cprtnam
C
C
C ######################################################################
C
C
C     ..................................................................
C     DEFINE THE NAME OF THE MEMORY MANAGEMENT NAME.
C
      isubname='get_node_connectivity'
C
C
C     ..................................................................
C     CHECK TO SEE IF THIS CMO EXISTS. IF NOT THE SET ERROR CODE AND
C        RETURN.
C
      call cmo_exist(cmo,icscode)
      if(icscode.ne.0) then
         ierror=-1
         goto 9999
      endif
C
C
C     ..................................................................
C     FETCH CMO INFORMATION NEEDED BY THIS ROUTINE.
C
      call cmo_get_info('nnodes',cmo,nnodes,lencmo,itpcmo,icscode)
      call cmo_get_info('nelements',cmo,nelements,lencmo,itpcmo,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,lencmo,itpcmo,icscode)
      call cmo_get_info('nodes_per_element',cmo,
     *                  nen_cmo,lencmo,itpcmo,ierror)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef_cmo,lencmo,itpcmo,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lencmo,itpcmo,icscode)
      call cmo_get_info('isn1',cmo,ipisn1,lencmo,itpcmo,icscode)
      call cmo_get_info('itettyp',cmo,ipitettyp,lencmo,itpcmo,icscode)
      call cmo_get_info('itetoff',cmo,ipitetoff,lencmo,itpcmo,icscode)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lencmo,itpcmo,icscode)
      call cmo_get_info('itet',cmo,ipitet,lencmo,itpcmo,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,lencmo,itpcmo,icscode)
C
C     ******************************************************************
C     SET UP AN ARRARY THAT IDENTIFIES THE ALL REAL NODES.
C          IREAL1 = 1  -> Real Node.
C          IREAL1 = 0  -> Not a real node.
      length=nnodes
      call mmgetblk('ireal1',isubname,ipireal1,length,1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'mmgetblk')
C
      call unpacktp('allreal','set',length,ipitp1,ipireal1,icscode)
      if(icscode.ne.0) call x3d_error(isubname,'unpacktp')
C
C
C     ************************************************************
C
C     Get the parents for each node.
C
      length=nnodes
      call mmgetblk("iparent",isubname,ipiparent,length,1,icscode)
      call unpackpc(nnodes,itp1,isn1,iparent)
C
C
C     ..................................................................
C     MAKE SURE THE INPUT COUNTER ARRAYS HAVE BEEN DEFINED AND GIVEN
C        THE CORRECT LENGTH.
C
      if(ipktetcnt.le.0) then
         ierror=-1
         goto 9999
      else
         length=nnodes
         call mmgetnam(ipktetcnt,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktetcnt,length,icscode)
      endif
      if(ipktetoff.le.0) then
         ierror=-1
         goto 9999
      else
         length=nnodes
         call mmgetnam(ipktetoff,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktetoff,length,icscode)
      endif
      do it=1,nnodes
         ktetcnt(it)=0
         ktetoff(it)=-1
      enddo
C
C
C     ..................................................................
C     GET THE LIST OF ACTIVE ELEMENTS FOR THIS CMO.
C
      length=nelements
      call mmgetblk('itactive',isubname,ipitactive,length,1,icscode)
      length=icharlnf(coption)
      if(coption(1:length).eq.'-all-') then
         ntactive=nelements
         do it=1,nelements
            itactive(it)=it
         enddo
      elseif(coption(1:length).eq.'-active-') then
         ntactive=0
         call get_active_elements(cmo,ntactive,ipitactive)
      else
         ierror=-1
         goto 9999
      endif
C
C
C     ..................................................................
C     CONSTRUCT THE LIST OF FACES FOR EACH ACTIVE ELEMENT.
C
      do i=1,nnodes
         ktetcnt(i)=1
         ktetoff(i)=0
      enddo
      do itdum=1,ntactive
         it=itactive(itdum)
         do ie=1,nelmnee(itettyp(it))
            i1=iparent(itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it))))
            i2=iparent(itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it))))
            ktetcnt(i1)=ktetcnt(i1)+1
            ktetcnt(i2)=ktetcnt(i2)+1
         enddo
      enddo
      isum=0
      do i=1,nnodes
         if(ktetcnt(i).gt.0) then
            ktetoff(i)=isum
            isum=isum+ktetcnt(i)
         endif
         ktetcnt(i)=1
      enddo
      length=isum+1
      if(ipktet.le.0) then
         ierror=-1
         goto 9999
      else
         call mmgetnam(ipktet,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktet,length,icscode)
      endif
      do i=1,length
         ktet1(i)=0
      enddo
      do i=1,nnodes
         ktet1(ktetoff(i)+1)=i
      enddo
      do itdum=1,ntactive
         it=itactive(itdum)
         do ie=1,nelmnee(itettyp(it))
            i1=iparent(itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it))))
            i2=iparent(itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it))))
            iflag=0
            if(ktetcnt(i1).gt.0) then
               do i=1,ktetcnt(i1)
                  j1=ktet1(ktetoff(i1)+i)
                  if(j1.eq.i2) then
                     iflag=i
                  endif
               enddo
            endif
            if(iflag.eq.0) then
               ktetcnt(i1)=ktetcnt(i1)+1
               ktet1(ktetoff(i1)+ktetcnt(i1))=i2
            endif
            iflag=0
            if(ktetcnt(i2).gt.0) then
               do i=1,ktetcnt(i2)
                  j2=ktet1(ktetoff(i2)+i)
                  if(j2.eq.i1) then
                     iflag=i
                  endif
               enddo
            endif
            if(iflag.eq.0) then
               ktetcnt(i2)=ktetcnt(i2)+1
               ktet1(ktetoff(i2)+ktetcnt(i2))=i1
            endif
         enddo
      enddo
C
      ktoff2=0
      do i1=1,nnodes
         ktoff1=ktetoff(i1)
         ktetoff(i1)=ktoff2
         do i=1,ktetcnt(i1)
            ktoff2=ktoff2+1
            ktet1(ktoff2)=ktet1(ktoff1+i)
         enddo
      enddo
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
      subroutine get_node_element_connectivity(coption1,cmo,
     *                                         ipktetcnt,ipktetoff,
     *                                         ipktet,
     *                                         ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine constructs a list of elements that surrounds
C           each node.
C
C     INPUT ARGUMENTS -
C
C        coption1  - Option to be performed by this routine:
C                    == '-all-' ==> Do all elements.
C                    == '-active-' ==> Do all active elements.
C        cmo      - Character name of the CMO to work on.
C
C     OUTPUT ARGUMENTS -
C
C        ipktetcnt - The number of faces for each element.
C        ipktetoff - The offset to the start of the list of neiboring
C                       faces and elements for each element. This is
C                       zero based.
C        ipktet - The list of neighboring elements and faces.
C        ierror     - A return error flag (=0 ==> OK, <>0 ==> ERROR)
C
C     CHANGE HISTORY -
C
C        $Log: grid_to_grid.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
C
C ######################################################################
C
      implicit none
C
      include 'local_element.h'
C
      character coption1*(*), cmo*(*)
      integer ierror
      pointer (ipktetcnt, ktetcnt)
      pointer (ipktetoff, ktetoff)
      pointer (ipktet, ktet1)
      integer ktetcnt(1000000), ktetoff(1000000), ktet1(1000000)
C
C
C ######################################################################
C
      integer nnodes, nelements
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000), itetkid(1000000), itetlev(1000000)
C
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itettyp(1000000), itetoff(1000000), jtetoff(1000000)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(1000000), jtet1(1000000)
C
      pointer (ipitactive, itactive)
      integer itactive(1000000)
C
      integer icscode, length, it, lencmo, itpcmo, icount, jcount,
     *        i, j, i1, nef_cmo,  nface,
     *         ntactive, iactive, mbndry
C
      integer icharlnf
C
      character*32 isubname, cblknam, cprtnam
C
C
C ######################################################################
C
C
C     ..................................................................
C     DEFINE THE NAME OF THE MEMORY MANAGEMENT NAME.
C
      isubname='get_element_connectivity'
C
C
C     ..................................................................
C     CHECK TO SEE IF THIS CMO EXISTS. IF NOT THE SET ERROR CODE AND
C        RETURN.
C
      call cmo_exist(cmo,icscode)
      if(icscode.ne.0) then
         ierror=-1
         goto 9999
      endif
C
C
C     ..................................................................
C     FETCH CMO INFORMATION NEEDED BY THIS ROUTINE.
C
      call cmo_get_info('nnodes',cmo,nnodes,lencmo,itpcmo,icscode)
      call cmo_get_info('nelements',cmo,nelements,lencmo,itpcmo,icscode)
      call cmo_get_info('mbndry',cmo,mbndry,lencmo,itpcmo,icscode)
      call cmo_get_info('faces_per_element',cmo,
     *                  nef_cmo,lencmo,itpcmo,ierror)
      call cmo_get_info('itettyp',cmo,ipitettyp,lencmo,itpcmo,icscode)
      call cmo_get_info('itetoff',cmo,ipitetoff,lencmo,itpcmo,icscode)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lencmo,itpcmo,icscode)
      call cmo_get_info('itet',cmo,ipitet,lencmo,itpcmo,icscode)
      call cmo_get_info('jtet',cmo,ipjtet,lencmo,itpcmo,icscode)
C
C
C     ..................................................................
C     MAKE SURE THE INPUT COUNTER ARRAYS HAVE BEEN DEFINED AND GIVEN
C        THE CORRECT LENGTH.
C
      if(ipktetcnt.le.0) then
         ierror=-1
         goto 9999
      else
         length=nnodes
         call mmgetnam(ipktetcnt,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktetcnt,length,icscode)
      endif
      if(ipktetoff.le.0) then
         ierror=-1
         goto 9999
      else
         length=nnodes
         call mmgetnam(ipktetoff,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktetoff,length,icscode)
      endif
      do i1=1,nnodes
         ktetcnt(i1)=0
         ktetoff(i1)=-1
      enddo
C
C
C     ..................................................................
C     CHECK TO SEE IF THIS IS AN AMR GRID.
C
C
      call mmfindbk('itetpar',cmo,ipitetpar,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetpar',isubname,ipitetpar,length,1,icscode)
         do it=1,nelements
            itetpar(it)=0
         enddo
      endif
      call mmfindbk('itetkid',cmo,ipitetkid,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetkid',isubname,ipitetkid,length,1,icscode)
         do it=1,nelements
            itetkid(it)=0
         enddo
      endif
      call mmfindbk('itetlev',cmo,ipitetlev,length,icscode)
      if(icscode.ne.0) then
         length=nelements
         call mmgetblk('itetlev',isubname,ipitetlev,length,1,icscode)
         do it=1,nelements
            itetlev(it)=0
         enddo
      endif
C
C
C     ..................................................................
C     GET THE LIST OF ACTIVE ELEMENTS FOR THIS CMO.
C
      length=nelements
      call mmgetblk('itactive',isubname,ipitactive,length,1,icscode)
      length=icharlnf(coption1)
      if(coption1(1:length).eq.'-all-') then
         ntactive=nelements
         do it=1,nelements
            itactive(it)=it
         enddo
      elseif(coption1(1:length).eq.'-active-') then
         ntactive=0
         call get_active_elements(cmo,ntactive,ipitactive)
      else
         ierror=-1
         goto 9999
      endif
C
C
C     ..................................................................
C     CONSTRUCT A LIST OF THE NUMBER OF ELEMENTS ASSOCIATED WITH EACH
C        NODE.
C
      do iactive=1,ntactive
         it=itactive(iactive)
         do i=1,nelmnef(itettyp(it))
            do j=1,ielmface0(i,itettyp(it))
               i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
               ktetcnt(i1)=ktetcnt(i1)+1
            enddo
         enddo
      enddo
      icount=0
      do i1=1,nnodes
         if(ktetcnt(i1).eq.0) then
            ktetoff(i1)=-1
         else
            jcount=ktetcnt(i1)
            ktetoff(i1)=icount
            icount=icount+jcount
         endif
      enddo
      if(ipktet.le.0) then
         ierror=-1
         goto 9999
      else
         length=icount
         call mmgetnam(ipktet,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipktet,length,icscode)
      endif
      do i=1,icount
         ktet1(i)=0
      enddo
      do i1=1,nnodes
         ktetcnt(i1)=0
      enddo
C
      nface=0
      do iactive=1,ntactive
         it=itactive(iactive)
         do i=1,nelmnef(itettyp(it))
            do j=1,ielmface0(i,itettyp(it))
               i1=itet1(itetoff(it)+ielmface1(j,i,itettyp(it)))
               ktetcnt(i1)=ktetcnt(i1)+1
               ktet1(ktetoff(i1)+ktetcnt(i1))=it
            enddo
         enddo
      enddo
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end

      subroutine get_active_elements(cmo,ntactive,ipitactive)
C
C #####################################################################
C
C     PURPOSE -
C
C        This routine constructs a list of active elements for a given
C           CMO. Inactive active elements are created by:
C              - AMR parent elements.
C              - Dudding elements by setting there color LE zero.
C
C     INPUT ARGUMENTS -
C
C        cmo      - Character name of the CMO to work on.
C
C     OUTPUT ARGUMENTS -
C
C        ntactive   - The number of active elements for this CMO.
C        ipitactive - The pointer to the array containing the list of
C                      active elements.
C        ierror     - A return error flag (=0 ==> OK, <>0 ==> ERROR)
C
C     CHANGE HISTORY -
C
C        $Log: grid_to_grid.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
C
C ######################################################################
C
      implicit none
C
      character cmo*(*)
      integer ntactive, ierror
      pointer (ipitactive, itactive)
      integer itactive(1000000)
C
C
C ######################################################################
C
      integer nelements
      pointer (ipitetclr, itetclr)
      integer itetclr(1000000)
C
      pointer (ipitetpar, itetpar)
      pointer (ipitetkid, itetkid)
      pointer (ipitetlev, itetlev)
      integer itetpar(1000000), itetkid(1000000), itetlev(1000000)
C
      pointer (ipitetact, itetact)
      integer itetact(1000000)
C
      integer icscode, length, it, lencmo, itpcmo
      character*32 isubname, cblknam, cprtnam
C
C
C ######################################################################
C
C
C     ..................................................................
C     DEFINE THE NAME OF THE MEMORY MANAGEMENT NAME.
C
      isubname='get_active_elements'
C
C
C     ..................................................................
C     CHECK TO SEE IF THIS CMO EXISTS. IF NOT THE SET ERROR CODE AND
C        RETURN.
C
      call cmo_exist(cmo,icscode)
      if(icscode.ne.0) then
         ierror=-1
         goto 9999
      endif
C
C
C     ..................................................................
C     FETCH CMO INFORMATION NEEDED BY THIS ROUTINE.
C
      call cmo_get_info('nelements',cmo,nelements,lencmo,itpcmo,icscode)
      call cmo_get_info('itetclr',cmo,ipitetclr,lencmo,itpcmo,icscode)
C
C     ..................................................................
C     CHECK TO SEE IF THE OUTPUT ARRAY HAS BEEN ASSIGN AND IS OF THE
C        CORRECT LENGTH. IF NOT MAKE SURE IT IS CORRECT.
C
      if(ipitactive.le.0) then
         ierror=-1
         goto 9999
      else
         length=nelements
         call mmgetnam(ipitactive,cblknam,cprtnam,icscode)
         call mmnewlen(cblknam,cprtnam,ipitactive,length,icscode)
      endif
      do it=1,nelements
         itactive(it)=0
      enddo
C
C     ..................................................................
C     DEFINE AN ELEMENT SIZE MASK ARRAY THAT INDICATES IF AN ELEMENT IS
C        ACTIVE (=1) OR INACTIVE (=0).
C
      length=nelements
      call mmgetblk('itetact',isubname,ipitetact,length,1,icscode)
      do it=1,nelements
         itetact(it)=1
      enddo
C
C
C     ..................................................................
C     ZERO OR NEGATIVE TET COLORS INDICATE INACTIVE ELEMENTS.
C
      do it=1,nelements
         if(itetclr(it).gt.0) itetact(it)=1
      enddo
C
C
C     ..................................................................
C     IF THIS IS AN AMR GRID THIS THERE IS A PARENTS/KID ELEMENT ARRAY.
C        IF THE ARM PARENT/KID ARRAYS EXIST THEN PARENT-ELEMENTS ARE
C        CONSIDERED TO BE INACTIVE AND ONLY KID-ELEMENTS ARE CONSIDERED
C        ACTIVE.
C
C
      call mmfindbk('itetpar',cmo,ipitetpar,length,icscode)
      if(icscode.ne.0) then
         goto 9998
      endif
      call mmfindbk('itetkid',cmo,ipitetkid,length,icscode)
      if(icscode.ne.0) then
         goto 9998
      endif
      call mmfindbk('itetlev',cmo,ipitetlev,length,icscode)
      if(icscode.ne.0) then
         goto 9998
      endif
C
C     ,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,,
C     SET PARENT ELEMENTS TO BE INACTIVE.
C
      do it=1,nelements
         if(itetkid(it).ne.0) itetact(it)=0
      enddo
C
C
C     ..................................................................
C
C
 9998 continue
C
C
C     ..................................................................
C     COUNT THE NUMBER OF ACTIVE ELEMENTS AN CONSTRUCT THE LIST.
C
      ntactive=0
      do it=1,nelements
         if(itetact(it).gt.0) then
            ntactive=ntactive+1
            itactive(ntactive)=it
         endif
      enddo
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
      return
      end
