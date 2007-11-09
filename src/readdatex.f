      subroutine readdatex(ifile,ierror)
C
C #####################################################################
C
c     PURPOSE -
C
C        THIS ROUTINE READ A DATEX1.2 FORMATED ASCII FILE.
C
C     INPUT ARGUMENTS -
C
C        ifile - INPUT DATEX FILE BASE NAME. THE .GEO AND .DAT ARE
C                   APPENDED TO THE BASE NAME TO GET BOTH THE
C                   GEOMETRY FILE NAME AND THE FIELD DATA FILE NAME.
C
C     OUTPUT ARGUMENTS -
C
C        ierror - RETURN ERROR CODE (== 0 ==> OK, <> 0 ==> AN ERROR)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/readdatex_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   Tue Mar 07 08:24:16 2000   dcg
CPVCS    remove references to ign1
CPVCS
CPVCS       Rev 1.0   Mon Jan 31 12:02:08 2000   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.12   Wed Sep 01 13:25:06 1999   dcg
CPVCS    fix typo ie should have been id
CPVCS
CPVCS       Rev 1.11   Tue May 13 12:56:22 1997   dcg
CPVCS    set itetclr, itetoff, jtetoff; fix cmo calls
CPVCS    check for eof on reads
CPVCS
CPVCS       Rev 1.9   Tue Mar 05 12:50:26 1996   dcg
CPVCS    remove int1, icn1
CPVCS
CPVCS       Rev 1.8   Fri Feb 23 13:52:52 1996   dcg
CPVCS    remove uic,vic,wic,pic,ric,eic
CPVCS
CPVCS       Rev 1.7   11/07/95 17:23:10   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.6   07/14/95 23:31:40   het
CPVCS    Correct an error with the CMO name
CPVCS
CPVCS       Rev 1.5   05/26/95 13:17:32   het
CPVCS    Replace subroutine parameter list with subroutine calles.
CPVCS
CPVCS       Rev 1.4   01/23/95 12:45:20   het
CPVCS    Delete the call to cmo_increment with cmo_newlen.
CPVCS
CPVCS
CPVCS       Rev 1.3   01/04/95 22:04:22   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.2   12/09/94 22:47:18   het
CPVCS    Added calles to the new cmo_ routines.
CPVCS
CPVCS
CPVCS       Rev 1.1   12/06/94 19:04:16   het
CPVCS    Added "call cmo_get_name" to return the current mesh object name.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/13/94 11:44:38   pvcs
CPVCS    Orginal Version
C
C ######################################################################
C
      implicit real*8 (a-h,o-z)
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include 'local_element.h'
C
C ######################################################################
C
      character*132 logmess, cbuff
C
      character ifile*(*)
C
      pointer (ipxmic, xmic)
      pointer (iptic, tic)
      real*8 xmic(1), tic(1)
      pointer(ipout,out)
      real*8 out(*)
C
      pointer (ipielmclr, ielmclr)
      pointer (ipielmicn, ielmicn)
      pointer (ipielm, ielm)
      pointer (ipjelm1, jelm1)
      pointer (ipjelm2, jelm2)
      integer ielmicn(1000000), ielmclr(1000000), ielm(1000000),
     *        jelm1(1000000), jelm2(1000000)
      pointer (ipvels,vels)
      pointer (ipdens,dens)
      pointer (ippres,pres)
      pointer (ipener,ener)
      real*8 vels(3,1000000),ener(1000000),pres(1000000),dens(1000000)
      pointer (ipicn1, icn1)
      integer icn1(1000000)
      pointer (ipitetoff,itetoff)
      pointer (ipjtetoff,jtetoff)
      integer itetoff(1000000),jtetoff(1000000)
      character*80 iword
      character*80 jword
      character*32 isubname, cmonam, jfile, cvelnm, cdensnm, cpresnm,
     *    cenernm
      character*8 sbname,defname
      integer iconn(100)
C
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
C
      integer ihexface0(nfacehex), ihexface1(4,nfacehex)
C     top,bottom,front,right,back,left
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      integer iprismface0(nfaceprism), iprismface1(4,nfaceprism)
C     top,bottom,right,back,left
      data iprismface0 / 3, 3, 4, 4, 4 /
      data iprismface1 / 1, 2, 3, 0,
     *                   4, 6, 5, 0,
     *                   1, 4, 5, 2,
     *                   2, 5, 6, 3,
     *                   1, 3, 6, 4 /
C
      integer intpairhex(2,12)
      data intpairhex / 1,2, 2,3, 3,4, 4,1, 5,6, 6,7, 7,8,
     *                  8,5, 1,5, 2,6, 3,7, 4,8 /
 
      integer itetface0(nfacetet), itetface1(4,nfacetet)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 0,
     *                 1, 4, 3, 0,
     *                 1, 2, 4, 0,
     *                 1, 3, 2, 0 /
      integer itriface0(nfacetri), itriface1(3,nfacetri)
C     top,back,left,right
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
      integer intpairtet(2,6)
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
C
      integer jshape(6)
      data jshape / 3, 4, 4, 5, 6, 6 /
      integer x3d_to_simul_hex(8), simul_to_x3d_hex(8)
C                             1  2  3  4  5  6  7  8
      data x3d_to_simul_hex / 1, 2, 4, 5, 3, 6, 8, 7 /
      data simul_to_x3d_hex / 1, 2, 5, 3, 4, 6, 8, 7 /
 
C
C
      sbname='sbcmoprm'
      defname='default'
      ierror=0
      isubname='readdatex'
C
      iunit=-1
      lenfile=icharlnf(ifile)
      jfile=ifile(1:lenfile) // '.geo'
      call hassign(iunit,jfile,ierror)
      read(iunit,"(a80)") iword
      read(iunit,"(a80)") iword
      read(iunit,"(a80)") iword
      read(iunit,"(a80)") iword
      read(iunit,*) ndim,npoints,nelms,nregions
      length=npoints
      nnodes_inc=npoints
      nelements_inc=0
C
      nnodes=npoints
      call cmo_get_name(cmonam,ierror)
      call cmo_set_info('nnodes',cmonam,nnodes,1,1,ierror)
C
      call cmo_newlen(cmonam,ierror)
      call cmo_get_info('itp1',cmonam,ipitp1,len,itype,ierr)
      call cmo_get_info('imt1',cmonam,ipimt1,len,itype,ierr)
      call cmo_get_info('icr1',cmonam,ipicr1,len,itype,ierr)
      call cmo_get_info('isn1',cmonam,ipisn1,len,itype,ierr)
      call cmo_get_info('icn1',cmonam,ipicn1,len,itype,ierr)
      if (ierr.ne.0) then
         cbuff='cmo/addatt/-def-/icn1/VINT/scalar/nnodes' //
     *         '/min/permanent/agx/0/ ; finish'
         call dotaskx3d(cbuff,ierror)
         call cmo_get_info('icn1',cmonam,ipicn1,len,itype,ierr)
      endif
      call cmo_get_info('xic',cmonam,ipxic,len,itype,ierr)
      call cmo_get_info('yic',cmonam,ipyic,len,itype,ierr)
      call cmo_get_info('zic',cmonam,ipzic,len,itype,ierr)
      call cmo_get_info('itetclr',cmonam,ipitetclr,len,itype,ierr)
      call cmo_get_info('itet',cmonam,ipitet,len,itype,ierr)
      call cmo_get_info('itetoff',cmonam,ipitetoff,len,itype,ierr)
      call cmo_get_info('jtet',cmonam,ipjtet,len,itype,ierr)
      call cmo_get_info('jtetoff',cmonam,ipjtetoff,len,itype,ierr)
      call cmo_get_info('itettyp',cmonam,ipitettyp,len,itype,ierr)
C
      call mmgetblk("xmic",isubname,ipxmic,length,2,icscode)
      call mmgetblk("tic",isubname,iptic,length,2,icscode)
      read(iunit,"(a80)") iword
      do i=1,npoints
         imt1(i)=0
         itp1(i)=0
         icr1(i)=0
         isn1(i)=0
          icn1(i)=0
         xic(i)=0
         yic(i)=0
         zic(i)=0
         xmic(i)=0
         tic(i)=0
         if(ndim.eq.3) then
            read(iunit,*) xic(i),yic(i),zic(i)
         elseif(ndim.eq.2) then
            read(iunit,*) xic(i),yic(i)
            zic(i)=0.0
         endif
      enddo
      read(iunit,"(a80)") iword
      do it=1,nelms
         read(iunit,*) ishape,icontact,iclr
         if(ndim.eq.2.and.ishape.eq.1) then
            nen=3
            nef=3
         elseif(ndim.eq.3.and.ishape.eq.1) then
            nen=3
            nef=3
         elseif(ndim.eq.2.and.ishape.eq.2) then
            nen=4
            nef=4
         elseif(ndim.eq.3.and.ishape.eq.3) then
            nen=4
            nef=4
         elseif(ndim.eq.3.and.ishape.eq.4) then
            nen=5
            nef=5
         elseif(ndim.eq.3.and.ishape.eq.5) then
            nen=6
            nef=5
         elseif(ndim.eq.3.and.ishape.eq.6) then
            nen=6
            nef=8
         else
            write(logmess,'(a,2i10)')
     *         "Invalid Simul element: ",it,ishape
            call writloga('default',0,logmess,0,ierrw)
         endif
         backspace(iunit)
         read(iunit,*) ishape,icontact,iclr,
     *                 (iconn(i),i=1,nen)
         if(it.eq.1) then
            length=nelms
            call mmgetblk("ielmclr",isubname,ipielmclr,length,2,
     *                    icscode)
            call mmgetblk("ielmicn",isubname,ipielmicn,length,2,
     *                    icscode)
            length=max(4,nen)*nelms
            call mmgetblk("ielm",isubname,ipielm,length,2,icscode)
            length=max(4,nef)*nelms
            call mmgetblk("jelm1",isubname,ipjelm1,length,2,icscode)
            call mmgetblk("jelm2",isubname,ipjelm2,length,2,icscode)
         endif
         ielmicn(it)=icontact+1
         ielmclr(it)=iclr+1
         if(ishape.eq.6) then
            do i=1,nen
               ielm(i)=iconn(simul_to_x3d_hex(i))+1
            enddo
         else
            do i=1,nen
               ielm(nen*(it-1)+i)=iconn(i)+1
            enddo
         endif
         do i=1,nef
            jelm1(nef*(it-1)+i)=-1
            jelm2(nef*(it-1)+i)=-1
         enddo
      enddo
      read(iunit,"(a80)") iword
      do ir=1,nregions
         read(iunit,"(a80)") iword
      enddo
C
      mbndry=16000000
C
      if(ndim.eq.2.and.nen.eq.3.and.nef.eq.3) then
         ntets=nelms
         call geniee(ielm,jelm1,jelm2,nen,nef,
     *               ntets,npoints,ndim,npoints,ntets)
         call cmo_set_info('ndimensions_geom',cmonam,
     *                     ndim,1,1,ierror)
         call cmo_set_info('ndimensions_topo',cmonam,
     *                     ndim,1,1,ierror)
         call cmo_set_info('nodes_per_element',cmonam,
     *                     nen,1,1,ierror)
         call cmo_set_info('faces_per_element',cmonam,
     *                     nef,1,1,ierror)
C
         nelements=ntets
         call cmo_get_name(cmonam,ierror)
         call cmo_set_info('nelements',cmonam,nelements,1,1,ierror)
C
         call cmo_newlen(cmonam,ierror)
      call cmo_get_info('itp1',cmonam,ipitp1,len,itype,ierr)
      call cmo_get_info('imt1',cmonam,ipimt1,len,itype,ierr)
      call cmo_get_info('icr1',cmonam,ipicr1,len,itype,ierr)
      call cmo_get_info('isn1',cmonam,ipisn1,len,itype,ierr)
      call cmo_get_info('icn1',cmonam,ipicn1,len,itype,ierr)
      call cmo_get_info('xic',cmonam,ipxic,len,itype,ierr)
      call cmo_get_info('yic',cmonam,ipyic,len,itype,ierr)
      call cmo_get_info('zic',cmonam,ipzic,len,itype,ierr)
      call cmo_get_info('itetclr',cmonam,ipitetclr,len,itype,ierr)
      call cmo_get_info('itet',cmonam,ipitet,len,itype,ierr)
      call cmo_get_info('itetoff',cmonam,ipitetoff,len,itype,ierr)
      call cmo_get_info('jtet',cmonam,ipjtet,len,itype,ierr)
      call cmo_get_info('jtetoff',cmonam,ipjtetoff,len,itype,ierr)
      call cmo_get_info('itettyp',cmonam,ipitettyp,len,itype,ierr)
C
         do it=1,ntets
            itetclr(it)=ielmclr(it)
            do i=1,nen
               itet1(4*(it-1)+i)=ielm(3*(it-1)+i)
            enddo
            itet1(4*(it-1)+4)=itet1(4*(it-1)+1)
            do i=1,nef
               if(jelm1(4*(it-1)+i).eq.0) then
                  jtet1(4*(it-1)+i)=mbndry
               else
                  jtet1(4*(it-1)+i)=nef*(jelm1(nef*(it-1)+i)-1)+
     *                                  jelm2(nef*(it-1)+i)
               endif
            enddo
            do i=1,nef
               jtet1(4*(it-1)+i)=1
            enddo
            jtet1(4*(it-1)+4)=4*ntets+1
         enddo
         do it=1,ntets
            index=4*(it-1)
            do i=1,4
               imt1(itet1(index+i))=itetclr(it)
            enddo
         enddo
         do i=1,npoints
            itp1(i)=0
         enddo
         do it=1,ntets
            index=4*(it-1)
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+4)
            do i=1,4
               index=4*(it-1)+i
               if(jtet1(index).le.0.or.jtet1(index).ge.mbndry) then
                  jndex=4*(it-1)
                  j1=itet1(jndex+itetface1(4,i))
                  j2=itet1(jndex+itetface1(1,i))
                  j3=itet1(jndex+itetface1(2,i))
                  j4=itet1(jndex+itetface1(3,i))
                  itp1(j2)=ifitprfl
                  itp1(j3)=ifitprfl
                  itp1(j4)=ifitprfl
                  icn1(j2)=ielmicn(it)
                  icn1(j3)=ielmicn(it)
                  icn1(j4)=ielmicn(it)
               endif
            enddo
         enddo
         do it=1,ntets
            index=4*(it-1)
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+4)
            do i=1,3
               index=3*(it-1)+i
               if(jelm1(index).gt.0.and.jelm1(index).le.ntets) then
                  jt=jelm1(index)
                  jf=jelm2(index)
                  if(itetclr(it).ne.itetclr(jt)) then
                     jndex=3*(it-1)
                     j1=ielm(jndex+itriface1(3,i))
                     j2=ielm(jndex+itriface1(1,i))
                     j3=ielm(jndex+itriface1(2,i))
                     if(itp1(j2).eq.ifitprfl) then
                        itp1(j2)=ifitpinb
                     else
                        itp1(j2)=ifitpini
                     endif
                     if(itp1(j3).eq.ifitprfl) then
                        itp1(j3)=ifitpinb
                     else
                        itp1(j3)=ifitpini
                     endif
                  endif
               endif
            enddo
         enddo
      elseif(ndim.eq.3.and.nen.eq.4.and.nef.eq.4) then
         ntets=nelms
         call geniee(ielm,jelm1,jelm2,nen,nef,
     *               ntets,npoints,3,npoints,ntets)
         call cmo_set_info('ndimensions_geom',cmonam,
     *                     ndim,1,1,ierror)
         call cmo_set_info('ndimensions_topo',cmonam,
     *                     ndim,1,1,ierror)
         call cmo_set_info('nodes_per_element',cmonam,
     *                     nen,1,1,ierror)
         call cmo_set_info('faces_per_element',cmonam,
     *                     nef,1,1,ierror)
C
         nelements=ntets
         call cmo_get_name(cmonam,ierror)
         call cmo_set_info('nelements',cmonam,nelements,1,1,ierror)
C
         call cmo_newlen(cmonam,ierror)
      call cmo_get_info('itp1',cmonam,ipitp1,len,itype,ierr)
      call cmo_get_info('imt1',cmonam,ipimt1,len,itype,ierr)
      call cmo_get_info('icr1',cmonam,ipicr1,len,itype,ierr)
      call cmo_get_info('isn1',cmonam,ipisn1,len,itype,ierr)
      call cmo_get_info('icn1',cmonam,ipicn1,len,itype,ierr)
      call cmo_get_info('xic',cmonam,ipxic,len,itype,ierr)
      call cmo_get_info('yic',cmonam,ipyic,len,itype,ierr)
      call cmo_get_info('zic',cmonam,ipzic,len,itype,ierr)
      call cmo_get_info('itetclr',cmonam,ipitetclr,len,itype,ierr)
      call cmo_get_info('itet',cmonam,ipitet,len,itype,ierr)
      call cmo_get_info('itetoff',cmonam,ipitetoff,len,itype,ierr)
      call cmo_get_info('jtet',cmonam,ipjtet,len,itype,ierr)
      call cmo_get_info('jtetoff',cmonam,ipjtetoff,len,itype,ierr)
      call cmo_get_info('itettyp',cmonam,ipitettyp,len,itype,ierr)
C
         do it=1,ntets
            itetclr(it)=ielmclr(it)
            itettyp(it)=ifelmtet
            itetoff(it)=4*(it-1)
            jtetoff(it)=4*(it-1)
            do i=1,nen
               itet1(4*(it-1)+i)=ielm(4*(it-1)+i)
            enddo
            do i=1,nef
               if(jelm1(4*(it-1)+i).eq.0) then
                  jtet1(4*(it-1)+i)=mbndry
               else
                  jtet1(4*(it-1)+i)=nef*(jelm1(nef*(it-1)+i)-1)+
     *                                  jelm2(nef*(it-1)+i)
               endif
            enddo
         enddo
         do it=1,ntets
            index=4*(it-1)
            do i=1,4
               imt1(itet1(index+i))=itetclr(it)
            enddo
         enddo
         do i=1,npoints
            itp1(i)=0
         enddo
         do it=1,ntets
            index=4*(it-1)
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+4)
            do i=1,4
               index=4*(it-1)+i
               if(jtet1(index).le.0.or.jtet1(index).ge.mbndry) then
                  jndex=4*(it-1)
                  j1=itet1(jndex+itetface1(4,i))
                  j2=itet1(jndex+itetface1(1,i))
                  j3=itet1(jndex+itetface1(2,i))
                  j4=itet1(jndex+itetface1(3,i))
                  itp1(j2)=ifitprfl
                  itp1(j3)=ifitprfl
                  itp1(j4)=ifitprfl
                  icn1(j2)=ielmicn(it)
                  icn1(j3)=ielmicn(it)
                  icn1(j4)=ielmicn(it)
               endif
            enddo
         enddo
         do it=1,ntets
            index=4*(it-1)
            i1=itet1(index+1)
            i2=itet1(index+2)
            i3=itet1(index+3)
            i4=itet1(index+4)
            do i=1,4
               index=4*(it-1)+i
               if(jtet1(index).gt.0.and.jtet1(index).lt.mbndry) then
                  jt=1+(jtet1(index)-1)/4
                  jf=jtet1(index)-4*(jt-1)
                  if(itetclr(it).ne.itetclr(jt)) then
                     jndex=4*(it-1)
                     j1=itet1(jndex+itetface1(4,i))
                     j2=itet1(jndex+itetface1(1,i))
                     j3=itet1(jndex+itetface1(2,i))
                     j4=itet1(jndex+itetface1(3,i))
                     if(itp1(j2).eq.ifitprfl) then
                        itp1(j2)=ifitpinb
                     else
                        itp1(j2)=ifitpini
                     endif
                     if(itp1(j3).eq.ifitprfl) then
                        itp1(j3)=ifitpinb
                     else
                        itp1(j3)=ifitpini
                     endif
                     if(itp1(j4).eq.ifitprfl) then
                        itp1(j4)=ifitpinb
                     else
                        itp1(j4)=ifitpini
                     endif
                     jtet1(index)=mbndry
                     jtet1(4*(jt-1)+jf)=mbndry
                  endif
               endif
            enddo
         enddo
      elseif(ndim.eq.3.and.nen.eq.8.and.nef.eq.6) then
         call cmo_set_info('ndimensions_geom',cmonam,
     *                     ndim,1,1,ierror)
         call cmo_set_info('ndimensions_topo',cmonam,
     *                     ndim,1,1,ierror)
         call cmo_set_info('nodes_per_element',cmonam,
     *                     nen,1,1,ierror)
         call cmo_set_info('faces_per_element',cmonam,
     *                     nef,1,1,ierror)
         ioption=24
         call hextotet_att(ioption,nelms,ipielmclr,ipielm)
      endif
      close(iunit)
      iunit=-1
      lenfile=icharlnf(ifile)
      jfile=ifile(1:lenfile) // '.dat'
      call hassign(iunit,jfile,ierror)
      read(iunit,'(a8)') jword
      read(iunit,'(a10)') jword
      read(iunit,'(a8)') jword
      read(iunit,'(a27)') jword
      read(iunit,*) ndata
      read(iunit,*) jword
      read(iunit,"(a80)") jword
      read(iunit,"(a80)") jword
      read(iunit,"(a80)") jword
      do id=1,ndata
         if(id.eq.1) then
            call cmo_get_attinfo('densname',cmonam,iout,rout,cdensnm,
     *                        ipout,lout,itype,ierror_return)
            call cmo_get_info(cdensnm,cmonam,ipdens,lth,itype,ier)
            if(ier.ne.0) then
               cbuff='cmo/addatt/-def-/dens/VDOUBLE/scalar/nnodes' //
     *               '/linear/permanent/agx/0/ ; finish'
               call dotaskx3d(cbuff,ierror)
            endif
         elseif (id.eq.2) then
            call cmo_get_attinfo('presname',cmonam,iout,rout,cpresnm,
     *                        ipout,lout,itype,ierror_return)
            call cmo_get_info(cpresnm,cmonam,ippres,lth,itype,ier)
            if(ier.ne.0) then
               cbuff='cmo/addatt/-def-/pres/VDOUBLE/scalar/nnodes' //
     *               '/linear/permanent/agx/0/ ; finish'
               call dotaskx3d(cbuff,ierror)
            endif
         elseif (id.eq.3) then
            call cmo_get_attinfo('enername',cmonam,iout,rout,cenernm,
     *                        ipout,lout,itype,ierror_return)
            call cmo_get_info(cenernm,cmonam,ipener,lth,itype,ier)
            if(ier.ne.0) then
               cbuff='cmo/addatt/-def-/ener/VDOUBLE/scalar/nnodes' //
     *               '/linear/permanent/agx/0/ ; finish'
               call dotaskx3d(cbuff,ierror)
            endif
         elseif (id.eq.4) then
            call cmo_get_attinfo('velname',cmonam,iout,rout,cvelnm,
     *                        ipout,lout,itype,ierror_return)
            call cmo_get_info(cvelnm,cmonam,ipvels,lth,itype,ier)
            if(ier.ne.0) then
               cbuff='cmo/addatt/-def-/vels/VDOUBLE/vector/nnodes' //
     *               '/linear/permanent/agx/0/ ; finish'
               call dotaskx3d(cbuff,ierror)
            endif
         endif
         read(iunit,"(a80)",end=9998) iword
         do i=1,npoints
            read(iunit,*) value1
            value2=value1
C******            if(abs(value1).le.1.0d-30) then
C******               value2=0.0d+00
C******            else
C******               value2=sign(log(abs(value1)),value1)
C******            endif
            if(id.eq.1) then
                   dens(i)=value2
            elseif(id.eq.2) then
                   pres(i)=value2
            elseif(id.eq.3) then
                   ener(i)=value2
            elseif(id.eq.4) then
                   vels(1,i)=value2
            endif
         enddo
         read(iunit,"(a80)") iword
      enddo
 9998 close(iunit)
      goto 9999
 9999 continue
      call cmo_get_name(cmonam,ierror)
      call cmo_set_info('nnodes',cmonam,npoints,1,1,ier)
      call cmo_set_info('nelements',cmonam,ntets,1,1,ier)
      call cmo_set_info('mbndry',cmonam,mbndry,1,1,ier)
      call mmrelprt(isubname,icscode)
      return
      end
