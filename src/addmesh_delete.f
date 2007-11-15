      subroutine addmesh_delete(cmoc,
     *                          mbndry_old,
     *                          mbndry_new,
     *                          itp1_boundary,
     *                          ipitet_delete,
     *                          ierror)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine delete elements from a mesh_object.
C
C     INPUT ARGUMENTS -
C
C        cmoc          - The mesh_object (source1).
C        mbndry_old    - The value of the old boundary flag.
C        mbndry_new    - The value of the new boundary flag.
C        itp1_boundary - The value of the new boundary points.
C        ipitet_delete - The pointer to the element array containing
C                            the tets to be kept and deleted.
C
C     OUTPUT ARGUMENTS -
C
C        cmoc   - The mesh_object (sink).
C        ierror - Error flag.
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_delete.f,v $
C        Revision 2.00  2007/11/05 19:45:45  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   03 Feb 2000 09:08:26   dcg
CPVCS    
CPVCS       Rev 1.0   26 Jan 2000 14:01:36   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.22   24 Jan 2000 14:51:02   jtg
CPVCS    modifed to use jtet_cycle_max to limit jtet loops
CPVCS    does not yet have jtet_reduce_nnd (elements with repeated nodes) modifications
CPVCS
CPVCS       Rev 1.21   10 Jan 2000 09:23:26   jtg
CPVCS    infinite cycles from bad input jtet now caught
CPVCS    (elements giving infinite jtet cycles assumed new boundary elements)
CPVCS
CPVCS       Rev 1.20   Tue Nov 30 16:51:14 1999   jtg
CPVCS    now can handle jtet loops.
CPVCS
CPVCS       Rev 1.19   Fri Jan 22 09:21:36 1999   dcg
CPVCS    remove duplicate declaration for ierror
CPVCS
CPVCS       Rev 1.18   Thu Jun 25 08:53:50 1998   dcg
CPVCS    ignore merged and dudded nodes when marking nodes
CPVCS    active before element remove operation
CPVCS
CPVCS       Rev 1.17   Mon Jun 01 10:48:04 1998   dcg
CPVCS    clean up isn1 chains when deleting elements
CPVCS
CPVCS       Rev 1.16   Wed Oct 08 12:49:00 1997   gable
CPVCS    Copy changes from x3d version to t3d version.
CPVCS
CPVCS       Rev 1.15   Mon Sep 15 10:13:40 1997   het
CPVCS    Compress attributes as part of the delete process
CPVCS
CPVCS       Rev 1.14   Mon Dec 02 08:54:46 1996   het
CPVCS    Account for interface boundaries
CPVCS
CPVCS       Rev 1.13   Tue Nov 26 13:51:26 1996   het
CPVCS    Fix an error
CPVCS
CPVCS       Rev 1.12   Thu Nov 21 19:08:54 1996   het
CPVCS
CPVCS
CPVCS       Rev 1.11   Mon Nov 11 20:59:24 1996   het
CPVCS    Make changes for adding hybrid grids.
CPVCS
CPVCS       Rev 1.10   Tue Mar 05 12:36:22 1996   het
CPVCS    Correct an error with delete elements.
CPVCS
CPVCS       Rev 1.9   Fri Feb 02 14:20:46 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.8   11/07/95 17:15:10   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.7   05/30/95 07:52:40   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include "local_element.h"
C
C ######################################################################
C
      character*(*) cmoc
      integer ierror,mbndry_old,idumpdel,
     * itp1_boundary,mbndry_new
      pointer(ipitet_delete,itet_delete)
      integer itet_delete(*)
C
C ######################################################################
C
      character*32 isubname
C
      data idumpdel / 1 /
C
C ######################################################################
C
C
      isubname="addmesh_delete"
C
      call addmesh_delete_compress(cmoc,
     *                             mbndry_old,
     *                             mbndry_new,
     *                             itp1_boundary,
     *                             ipitet_delete,
     *                             ierror)
C
      goto 9999
 9999 continue
C
      return
      end
C======================================================================
C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C|C
C======================================================================
*dk,addmesh_delete_compress
      subroutine addmesh_delete_compress(cmoc,
     *                                   mbndry_old,
     *                                   mbndry_new,
     *                                   itp1_boundary,
     *                                   ipitet_delete,
     *                                   ierror)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine delete elements from a mesh_object.
C
C     INPUT ARGUMENTS -
C
C        cmoc          - The mesh_object (source1).
C        mbndry_old    - The value of the old boundary flag.
C                          (Should be same as mbndry of cmo for code
C                           to make sense - it is no longer used)
C        mbndry_new    - The value of the new boundary flag.
C        itp1_boundary - The value of the new boundary points.
C        ipitet_delete - The pointer to the element array containing the
C                        tets to be kept (.le.0) and deleted (.gt.0).
C
C     OUTPUT ARGUMENTS -
C
C        cmoc   - The mesh_object (sink).
C        ierror - Error flag.
C
C     CHANGE HISTORY -
C
C        $Log: addmesh_delete.f,v $
C        Revision 2.00  2007/11/05 19:45:45  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.12   Thu Nov 21 19:08:54 1996   het
CPVCS
CPVCS
CPVCS       Rev 1.11   Mon Nov 11 20:59:24 1996   het
CPVCS    Make changes for adding hybrid grids.
CPVCS
CPVCS       Rev 1.10   Tue Mar 05 12:36:22 1996   het
CPVCS    Correct an error with delete elements.
CPVCS
CPVCS       Rev 1.9   Fri Feb 02 14:20:46 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.8   11/07/95 17:15:10   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.7   05/30/95 07:52:40   het
CPVCS    Replace mesh_object subroutine parameters by cmo-calls
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      include "chydro.h"
      include "local_element.h"
C
C ######################################################################
C
      integer nmulti
      parameter (nmulti=100)
      character*(*) cmoc
      integer ierror,ierror_return,index
C
C ######################################################################
C
C
      pointer (ipitp1, itp1)
      integer itp1(*)
      pointer (ipisn1, isn1)
      integer isn1(*)
C
      pointer (ipitetclr1, itetclr1)
      pointer (ipitettyp1, itettyp1)
      pointer (ipitetoff1, itetoff1)
      pointer (ipjtetoff1, jtetoff1)
      pointer (ipitet1, itet1)
      pointer (ipjtet1, jtet1)
      integer itetclr1(*),itettyp1(*),itetoff1(*)
     &       ,jtetoff1(*),itet1(*),jtet1(*)
C
      pointer (ipitalias, italias)
      integer italias(*)
C
      pointer (ipitetclr2, itetclr2)
      pointer (ipitettyp2, itettyp2)
      pointer (ipitetoff2, itetoff2)
      pointer (ipjtetoff2, jtetoff2)
      pointer (ipitet2, itet2)
      pointer (ipjtet2, jtet2)
      integer itetclr2(*),itettyp2(*),itetoff2(*)
     &       ,jtetoff2(*),itet2(*),jtet2(*)
C
      pointer (ipitet_delete, itet_delete)
      integer itet_delete(*)
C
C ######################################################################
C
      pointer (ipipflag_before, ipflag_before)
      integer ipflag_before(*)
      pointer (ipipflag_after, ipflag_after)
      integer ipflag_after(*)
C
      pointer (ipxcmo,xcmo)
      pointer (ipxcmo,icmo)
      integer icmo(*)
      real*8 xcmo(*)
C
      character*32 ctype,crank,cattr_name,clength,cio,cpers,cinter
 
C
      character*32 isubname
      integer npoints1,ilen,icmotp, mbndry,length,nsdtopo1,
     *  nef1,nen1,nsdgeom1,icscode,numtet1,ntetdel,ntet,itoff,jtoff,
     *  it,i,jt,jf,idel,numtet1_save,itout,itnew,ier,lout,i1,
     *  isave,isnext,ict,iatt,natt,iorphan,nlen,icmotype
     *  ,kf,lt,lf,kt,icycle,jtet_cycle_max,jtet_reduce_nnd
      integer icharln,jtnew,mbndry_old,mbndry_new,itp1_boundary
     *  ,min_mbndry_new,local_debug
      character*132 logmess
C
C ######################################################################
C
C
      isubname="addmesh_delete"
 
      local_debug=0
      if (local_debug.gt.0) call mmverify()
C
C
      call cmo_get_info('nnodes',cmoc,npoints1,ilen,icmotp,ierror)
      call cmo_get_info('nelements',cmoc,numtet1,ilen,icmotp,ierror)
      if (numtet1.lt.1.or.npoints1.lt.1) goto 9999
      call cmo_get_info('jtet_cycle_max',cmoc,jtet_cycle_max
     &                  ,ilen,icmotp,ierror)
      if (ierror.ne.0.or.jtet_cycle_max.lt.2) jtet_cycle_max=2
      call cmo_get_info('jtet_reduce_nnd',cmoc,jtet_reduce_nnd
     &                  ,ilen,icmotp,ierror)
      if (ierror.ne.0) jtet_reduce_nnd=0
      if (jtet_reduce_nnd.ne.0) then
         logmess='WARNING: addmesh_delete not jtet_reduce_nnd=1 safe'
         call writloga('default',0,logmess,0,ier)
      endif
 
      call cmo_get_info('mbndry',cmoc,mbndry,ilen,icmotp,ierror)
      if (ierror.ne.0) goto 9999
      call cmo_get_info('itp1',cmoc,ipitp1,ilen,icmotp,ierror)
      call cmo_get_info('isn1',cmoc,ipisn1,ilen,icmotp,ierror)
      call cmo_get_info('ndimensions_topo',cmoc,
     *                  nsdtopo1,length,icmotype,ierror)
      call cmo_get_info('ndimensions_geom',cmoc,
     *                  nsdgeom1,length,icmotype,ierror)
      call cmo_get_info('nodes_per_element',cmoc,
     *                  nen1,length,icmotype,ierror)
      call cmo_get_info('faces_per_element',cmoc,
     *                  nef1,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmoc,
     *                  ipitetclr1,ilen,icmotp,ierror)
      call cmo_get_info('itettyp',cmoc,
     *                  ipitettyp1,ilen,icmotp,ierror)
      call cmo_get_info('itetoff',cmoc,
     *                  ipitetoff1,ilen,icmotp,ierror)
      call cmo_get_info('jtetoff',cmoc,
     *                  ipjtetoff1,ilen,icmotp,ierror)
      call cmo_get_info('itet',cmoc,ipitet1,ilen,icmotp,ierror)
      call cmo_get_info('jtet',cmoc,ipjtet1,ilen,icmotp,ierror)
C
      min_mbndry_new=nef1*numtet1
      if (mbndry.le.min_mbndry_new) goto 9999
C
      length=numtet1
      call mmgetblk('italias',isubname,ipitalias,length,1,icscode)
      call mmgetblk('itetclr2',isubname,ipitetclr2,length,1,icscode)
      call mmgetblk('itettyp2',isubname,ipitettyp2,length,1,icscode)
      call mmgetblk('itetoff2',isubname,ipitetoff2,length,1,icscode)
      call mmgetblk('jtetoff2',isubname,ipjtetoff2,length,1,icscode)
      length=nelmnen(ifelmhyb)*numtet1
      call mmgetblk('itet2',isubname,ipitet2,length,1,icscode)
      length=nelmnef(ifelmhyb)*numtet1
      call mmgetblk('jtet2',isubname,ipjtet2,length,1,icscode)
C
      length=npoints1
      call mmgetblk('ipflag_before',isubname,
     *              ipipflag_before,length,2,icscode)
      call mmgetblk('ipflag_after',isubname,
     *              ipipflag_after,length,2,icscode)
      do i=1,npoints1
         ipflag_before(i)=0
         ipflag_after(i)=0
      enddo
      do it=1,numtet1
         do i=1,nelmnen(itettyp1(it))
            i1=itet1(itetoff1(it)+i)
            if(itp1(i1).ne.ifitpmrg.and.itp1(i1).ne.ifitpdud)
     *         ipflag_before(i1)=1
         enddo
      enddo
C
C
C     ******************************************************************
C     NOW REMOVE THE TETS FROM MESH 1 THAT OVERLAP MESH 2.
C
      ntetdel=0
      do it=1,numtet1
         if(itet_delete(it).gt.0) ntetdel=ntetdel+1
      enddo
      if(ntetdel.gt.0) then
         ntet=0
         itoff=0
         jtoff=0
         do it=1,numtet1
            italias(it)=0
            if(itet_delete(it).le.0) then
C
C  element is kept: increment ntet and "2" arrays (except itet and jtet)
C
               ntet=ntet+1
               italias(it)=ntet
               itettyp2(ntet)=itettyp1(it)
               itetclr2(ntet)=itetclr1(it)
               itetoff2(ntet)=itoff
               jtetoff2(ntet)=jtoff
               itoff=itoff+nelmnen(itettyp2(ntet))
               jtoff=jtoff+nelmnef(itettyp2(ntet))
 
            else
C
C  element is to be deleted -> reset jtet1 to indicate new boundary
C  this loop needs to be fixed when jtet_reduce_nnd=1
C
               do i=1,nelmnef(itettyp1(it))
                  jf=jtet1(jtetoff1(it)+i)
                  if (jf.ge.mbndry) jf=jf-mbndry
                  if (jf.le.0) then
                     jtet1(jtetoff1(it)+ i)=-1
                  else ! if (jf.gt.0) then
                     jt=1+(jf-1)/nef1
                     jf=jf-nef1*(jt-1)
                     kt=jtet1(jtetoff1(jt)+jf)
                     if (kt.ge.mbndry) kt=kt-mbndry
                     kf=kt
                     kt=1+(kt-1)/nef1
                     kf=kf-nef1*(kt-1)
                     if (kt.eq.it.and.kf.eq.i) then
                        jtet1(jtetoff1(it)+ i)=-1
                        jtet1(jtetoff1(jt)+jf)=-1
                     elseif (kt.gt.0) then
                        ! for networks: just remove from chain
                        icycle=0
                        do while (kt.ne.it.or.kf.ne.i)
                           icycle=icycle+1
                           if (icycle.gt.jtet_cycle_max+1) goto 300
                           jf=jtet1(jtetoff1(jt)+jf)
                           if (jf.ge.mbndry) jf=jf-mbndry
                           jt=1+(jf-1)/nef1
                           jf=jf-nef1*(jt-1)
                           kt=jtet1(jtetoff1(jt)+jf)
                           if (kt.ge.mbndry) kt=kt-mbndry
                           kf=kt
                           kt=1+(kt-1)/nef1
                           kf=kf-nef1*(kt-1)
                        enddo
300                     if (icycle.gt.jtet_cycle_max+1) then
                           ! this should not happen, but can if jtet1 not valid.
                           ! For now, set ierror=1,
                           ! assume this is a boundary tet, and press on
                           ! (although ierror will likely be overwritten below).
                           ierror=1
                           jtet1(jtetoff1(it)+ i)=-1
                           if (local_debug.gt.0) write(*,*)
     &                         'addmesh_delete jtet err element ',it,i
                        else
                           jtet1(jtetoff1(jt)+jf)=jtet1(jtetoff1(it)+i)
                           jtet1(jtetoff1(it)+ i)=-1
                           kf=jtet1(jtetoff1(jt)+jf)
                           if (kf.ge.mbndry) kf=kf-mbndry
                           kt=1+(kf-1)/nef1
                           kf=kf-nef1*(kt-1)
                           lt=jtet1(jtetoff1(kt)+kf)
                           if (lt.ge.mbndry) lt=lt-mbndry
                           lf=lt
                           lt=1+(lt-1)/nef1
                           lf=lf-nef1*(lt-1)
                           if (lt.eq.jt.and.lf.eq.jf
     *                         .and. itetclr1(jt).eq.itetclr1(kt)) then
                             jtet1(jtetoff1(kt)+kf)
     *                           =jtet1(jtetoff1(kt)+kf)-mbndry
                             jtet1(jtetoff1(jt)+jf)
     *                           =jtet1(jtetoff1(jt)+jf)-mbndry
                         endif
                        endif
                     endif
                  endif
               enddo
 
            endif
 
         enddo
C
C  for elements kept, set itet2 and jtet2
C
         do it=1,numtet1
            if(itet_delete(it).le.0) then
               itnew=italias(it)
               do i=1,nelmnen(itettyp2(itnew))
                  itet2(itetoff2(itnew)+i)=itet1(itetoff1(it)+i)
               enddo
               do i=1,nelmnef(itettyp1(it))
                  if(jtet1(jtetoff1(it)+i).le.0 .or.
     *               jtet1(jtetoff1(it)+i).eq.mbndry) then
                     jtet2(jtetoff2(itnew)+i)=jtet1(jtetoff1(it)+i)
                  elseif(jtet1(jtetoff1(it)+i).gt.mbndry) then
                     jt=1+(jtet1(jtetoff1(it)+i)-mbndry-1)/nef1
                     jf=jtet1(jtetoff1(it)+i)-mbndry-nef1*(jt-1)
                     jtnew=italias(jt)
                     jtet2(jtetoff2(itnew)+i )=mbndry+nef1*(jtnew-1)+jf
                     jtet2(jtetoff2(jtnew)+jf)=mbndry+nef1*(itnew-1)+i
                  else
                     jt=1+(jtet1(jtetoff1(it)+i)-1)/nef1
                     jf=jtet1(jtetoff1(it)+i)-nef1*(jt-1)
                     jtnew=italias(jt)
                     jtet2(jtetoff2(itnew)+i )=nef1*(jtnew-1)+jf
                     jtet2(jtetoff2(jtnew)+jf)=nef1*(itnew-1)+i
                  endif
               enddo
            endif
         enddo
C
C  replace "1" info with "2" info, and set new length
C
         do it=1,ntet
            itetclr1(it)=itetclr2(it)
            itettyp1(it)=itettyp2(it)
            itetoff1(it)=itetoff2(it)
            jtetoff1(it)=jtetoff2(it)
            do i=1,nelmnen(itettyp1(it))
               itet1(itetoff1(it)+i)=itet2(itetoff2(it)+i)
            enddo
            do i=1,nelmnef(itettyp1(it))
               jtet1(jtetoff1(it)+i)=jtet2(jtetoff2(it)+i)
            enddo
         enddo
         numtet1_save=numtet1
         numtet1=ntet
         call cmo_set_info('nelements',cmoc,numtet1,1,1,ier)
C
C  "-1" was used above to flag "new" boundaries to avoid
C  repeating jtet loops -> reset jtet1 to reflect mbndry_new.
C  NOTE: previously, this loop used mbndry_old instead of mbndry.
C  Since in all current calls, mbndry_old is the same as mbndry,
C  and mbndry_new is either the same as mbndry or negative.
C  This loop only makes sense if mbndry_old=mbndry, as otherwise
C  mbndry_new is replaced by an "effective" mbndry_new
C  which is mbndry_new +(mbndry- mbndry_old).
C  Hence the loop now uses mbndry and mbndry_old is ignored.
C
         do it=1,numtet1
            do i=1,nelmnef(itettyp1(it))
               if(jtet1(jtetoff1(it)+i).eq.-1) then
                  jtet1(jtetoff1(it)+i)=mbndry_new
               elseif(mbndry_new.gt.numtet1
     *               .and. jtet1(jtetoff1(it)+i).ge.mbndry_old) then
                  jtet1(jtetoff1(it)+i)=mbndry_new - mbndry_old +
     *                                  jtet1(jtetoff1(it)+i)
               endif
            enddo
         enddo
         do i=1,npoints1
            ipflag_after(i)=0
         enddo
         do it=1,numtet1
            do i=1,nelmnen(itettyp1(it))
               ipflag_after(itet1(itetoff1(it)+i))=1
            enddo
         enddo
         idel=0
         do i=1,npoints1
            if(ipflag_before(i).eq.1.and.ipflag_after(i).eq.0) then
               idel=idel+1
               itp1(i)=ifitpdud
C
C  follow isn chain and clean up
C
               if (isn1(i).ne.0) then
                  isave=i
                  isnext=i
                  ict=0
                  do while(isn1(isnext).ne.isave.and.ict.le.nmulti)
                    ict=ict+1
                    if(ict.ge.nmulti) then
                      write (logmess,90) isave, isnext
 90                   format(' ATTENTION:ICT>NMULTI',
     *                ' IN ADDMESH_DELETE_COMPRESS: 1st point=',
     *                i10,' last point=',i10)
                      call writloga('default',0,logmess,0,ier)
                      go to 120
                    endif
                    isnext=isn1(isnext)
                    if(isnext.eq.0) then
C
C  zero out partial chain
C
                       isnext=isave
                       do while (isnext.ne.0)
                          ict=isn1(isnext)
                          isn1(isnext)=0
                          isnext=ict
                       enddo
                       go to 120
                     endif
                  enddo
                  if(ict.eq.1) then
                      isn1(isnext)=0
                      if(itp1(isnext).eq.ifitpcup)
     *                   itp1(isnext)=ifitpdud
                      isn1(isave)=0
                  elseif(ict.eq.2) then
                      if(itp1(isnext).eq.ifitpcup) then
                         itp1(isnext)=ifitpdud
                         isn1(isnext)=0
                         iorphan=isn1(isave)
                         isn1(iorphan)=0
                         isn1(isave)=0
                      elseif(itp1(isnext).eq.ifitpdud.or.
     *                       itp1(isnext).eq.ifitpmrg ) then
                         isn1(isnext)=0
                         iorphan=isn1(isave)
                         isn1(iorphan)=0
                         isn1(isave)=0
                      elseif(itp1(isn1(isave)).eq.ifitpcup) then
                         itp1(isn1(isave))=ifitpdud
                         isn1(isn1(isave))=0
                         iorphan=isnext
                         isn1(iorphan)=0
                         isn1(isave)=0
                      elseif(itp1(isn1(isave)).eq.ifitpdud.or.
     *                       itp1(isn1(isave)).eq.ifitpmrg ) then
                         isn1(isn1(isave))=0
                         isn1(isnext)=0
                         iorphan=isnext
                         isn1(isave)=0
                      else
                         write(logmess,101) isave
 101                     format(' BAD parent/child chain at ',i10)
                         call writloga('default',0,logmess,0,ier)
                         go to 105
                      endif
                      if(itp1(iorphan).eq.ifitpini) then
                         itp1(iorphan)=ifitpint
                      elseif(itp1(iorphan).eq.ifitpmrg.or.
     *                       itp1(iorphan).eq.ifitpdud) then
                         go to 120
                      elseif(itp1(iorphan).eq.ifitpirb) then
                         itp1(iorphan)=ifitprfl
                      elseif(itp1(iorphan).eq.ifitpifb) then
                         itp1(iorphan)=ifitpfre
                      elseif(itp1(iorphan).eq.ifitpirf) then
                         itp1(iorphan)=ifitprfb
                      elseif(itp1(iorphan).eq.ifitpvir) then
                         itp1(iorphan)=ifitpvrb
                      elseif(itp1(iorphan).eq.ifitpvif) then
                         itp1(iorphan)=ifitpvrb
                      elseif(itp1(iorphan).eq.ifitpalb) then
                         itp1(iorphan)=ifitpvrf
                      endif
                 elseif(ict.gt.2) then
                      isn1(isnext)=isn1(isave)
                      isn1(isave)=0
                 else
                      isn1(isave)=0
                 endif
               endif
            endif
 120        continue
         enddo
C
C        ***************************************************************
C        REMAP ANY ELEMENT QUANTITIES TO THE NEW ELEMENT ORDERING.
C
C
C        LOOP THROUGH MESH OBJECT ATTRIBUTES AND LOOK FOR ELEMENT
C           BASED SCALAR ATTRIBUTES. Note: itetclr, itettyp, itetoff,
C           and jtetoff are special and taken care of in the loops
C           above.
C
  105    call cmo_get_info('number_of_attributes',cmoc,natt,
     *                   ilen,itout,ierror)
         do iatt=1,natt
            call cmo_get_attribute_name(cmoc,iatt,cattr_name,ier)
            nlen=icharln(cattr_name)
            if(cattr_name(1:nlen).eq.'itetclr' .or.
     *         cattr_name(1:nlen).eq.'itettyp' .or.
     *         cattr_name(1:nlen).eq.'itetoff' .or.
     *         cattr_name(1:nlen).eq.'jtetoff' ) then
            else
               call cmo_get_attparam(cattr_name,cmoc,index,ctype,crank,
     *         clength,cinter,cpers,cio,ierror_return)
               if(ier.eq.0 .and.
     *            clength(1:9).eq.'nelements' .and.
     *            ctype(1:7).eq.'VDOUBLE' .and.
     *            crank(1:6).eq.'scalar') then
                  call cmo_get_info(cattr_name,cmoc,
     *                              ipxcmo,lout,itout,ier)
                  do it=1,numtet1_save
                     if(itet_delete(it).le.0) then
                        itnew=italias(it)
                        xcmo(itnew)=xcmo(it)
                     endif
                  enddo
               elseif(ier.eq.0 .and.
     *            clength(1:9).eq.'nelements' .and.
     *            ctype(1:4).eq.'VINT' .and.
     *            crank(1:6).eq.'scalar') then
                  call cmo_get_info(cattr_name,cmoc,
     *                              ipxcmo,lout,itout,ier)
                  do it=1,numtet1_save
                     if(itet_delete(it).le.0) then
                        itnew=italias(it)
                        icmo(itnew)=icmo(it)
                     endif
                  enddo
               endif
            endif
         enddo
C
C        ***************************************************************
C        RESET lower_d_flag IF EXISTS TO INDICATE LOWER D STRUCTURES NOT VALID.
C        (or could fix -> add code ...)
C
         call cmo_get_info('lower_d_flag',cmoc,i,lout,itout,ier)
         if ( ier.eq.0 .and. i.eq.1 ) then
            lout=1
            itout=1
            call cmo_set_info('lower_d_flag',cmoc,2,lout,itout,ier)
         endif
 
C
C        ***************************************************************
C        RESET nelements
 
         call cmo_set_info('nelements',cmoc,numtet1,1,1,ier)
C
C        ===============================================================
      elseif (mbndry_new.gt.min_mbndry_new
     *         .and. mbndry_new.ne.mbndry) then
 
C        ***************************************************************
C        IF mbndry_new>0 but no elements were deleted, still have to
C        RESET jtet to reflect mbndry_new
 
c see note above re mbndry vs mbndry_old
 
         do it=1,numtet1
            do i=1,nelmnef(itettyp1(it))
               if(jtet1(jtetoff1(it)+i).ge.mbndry) then
                  jtet1(jtetoff1(it)+i)=mbndry_new - mbndry +
     *                                  jtet1(jtetoff1(it)+i)
               endif
            enddo
         enddo
 
C        ***************************************************************
C        RESET lower_d_flag IF EXISTS TO INDICATE LOWER D STRUCTURES NOT VALID.
C        (or could fix -> add code ...)
C
         call cmo_get_info('lower_d_flag',cmoc,i,lout,itout,ier)
         if ( ier.eq.0 .and. i.eq.1 ) then
            lout=1
            itout=1
            call cmo_set_info('lower_d_flag',cmoc,2,lout,itout,ier)
         endif
C
      endif
 
C        ***************************************************************
C        RESET mbndry to mbndry_new
 
C  Since in all current calls, mbndry_new is either the same
C  as mbndry or negative, mbndry was not reset.
C  Since for future uses this may not be the case,
C  it is now reset if mbndry_new > nef1*numtet1 = (minimum allowable value) - 1
 
      if (mbndry_new.gt.min_mbndry_new .and. mbndry_new.ne.mbndry)
     *   call cmo_set_info('mbndry',cmoc,mbndry_new,1,1,ier)
 
C        ***************************************************************
C
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
      if (local_debug.gt.0) call mmverify()
C
      return
      end
