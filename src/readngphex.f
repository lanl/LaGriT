*DK readngphex
      subroutine readngphex(ifile,ierror)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE READS AN "NGP.plt" FILE.
C
C     INPUT ARGUMENTS -
C
C        ifile - INPUT NGP PLT UNFORMATTED FILE.
C
C     OUTPUT ARGUMENTS -
C
C        ierror - RETURN ERROR CODE (== 0 ==> OK, <> 0 ==> AN ERROR)
C
C      CHANGE HISTORY -
C
C         $Log:   /pvcs.config/t3d/src/readngphex.f_a  $
CPVCS    
CPVCS       Rev 1.9   03 Oct 2000 09:48:14   dcg
CPVCS    remove unused references to ialias
CPVCS
CPVCS       Rev 1.8   21 Apr 2000 07:08:08   gable
CPVCS    Made setting and getting of mbndry value dynamic and problem size dependent.
CPVCS
CPVCS       Rev 1.7   Mon Apr 14 16:57:52 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.6   Mon Mar 04 11:12:06 1996   dcg
CPVCS    remove icn1, int1 unused in this routine
CPVCS
CPVCS       Rev 1.5   Fri Feb 02 14:22:54 1996   dcg
CPVCS    remove references to explicit vector attributes (u,w,v,e,r,pic)
CPVCS
CPVCS       Rev 1.4   11/07/95 17:23:18   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.3   06/05/95 10:36:16   het
CPVCS    Make changes for hybrid_grids
CPVCS
CPVCS       Rev 1.2   01/13/95 10:17:38   het
CPVCS    Changed routine to just create a hex_mesh_object.
CPVCS
CPVCS
CPVCS       Rev 1.0   12/11/94 09:28:00   het
CPVCS    Original version.
CPVCS
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
C
C ######################################################################
C
C     DEFINE THE MESH_OBJECT POINTERS.
C
      include 'chydro.h'
      include 'local_element.h'
C
C ######################################################################
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipign1, ign1)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000), ign1(1000000)
C
C
C     *****************************************************************
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C
C     *****************************************************************
C
      pointer (ipitetclr, itetclr)
      pointer (ipitettyp, itettyp)
      pointer (ipitetoff, itetoff)
      pointer (ipjtetoff, jtetoff)
      integer itetclr(1000000), itettyp(1000000),
     *        itetoff(1000000), jtetoff(1000000)
      pointer (ipitet, itet1)
      pointer (ipjtet, jtet1)
      integer itet1(1000000)
      integer jtet1(1000000)
C
C ######################################################################
C
C
C
      pointer (ipjtet2, jtet2(10000000))
C
      character ifile*(*)
      character*32 isubname, cmo
      character*32 cmotype
C
C#######################################################################
C
      parameter (nentri=3, nfacetri=3)
      parameter (nentet=4, nfacetet=4)
      parameter (nenprism=6, nfaceprism=5)
      parameter (nenhex=8, nfacehex=6)
C
      dimension ihexface0(nfacehex), ihexface1(4,nfacehex)
C     top,bottom,front,right,back,left
      data ihexface0 / 4, 4, 4, 4, 4, 4 /
      data ihexface1 / 1, 2, 3, 4,
     *                 5, 8, 7, 6,
     *                 1, 5, 6, 2,
     *                 2, 6, 7, 3,
     *                 3, 7, 8, 4,
     *                 1, 4, 8, 5 /
      dimension iprismface0(nfaceprism), iprismface1(4,nfaceprism)
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
 
      dimension itetface0(nfacetet), itetface1(4,nfacetet)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 1,
     *                 1, 4, 3, 2,
     *                 1, 2, 4, 3,
     *                 1, 3, 2, 4 /
      dimension itriface0(nfacetri), itriface1(3,nfacetri)
C     top,back,left,right
      data itriface0 / 2, 2, 2 /
      data itriface1 / 2, 3, 1,
     *                 3, 1, 2,
     *                 1, 2, 3 /
      integer intpairtet(2,6)
      data intpairtet / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
C
C#######################################################################
C
C
C
C     ******************************************************************
C
C     SET THE NAME TO USED AS THE PARTITION FOR THE TEMPORARY MEMORY
C     THAT WILL BE ALLOCATED FOR THIS SUBROUTINE.
C
      isubname='readngp'
C
      iunit=20
      open(unit=iunit,file=ifile)
C
      read(iunit,*) ntets, npoints
      nsd=3
      nen=8
      nef=6
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmo,nsd,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmo,nsd,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmo,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmo,nef,1,1,ierror)
      call cmo_newlen(cmo,ierror)
      call cmo_get_intinfo('mbndry',cmo,mbndry,
     *                  lenisetwd,icmotype,ierror)
      call cmo_get_info('isetwd',cmo,ipisetwd,lenisetwd,icmotype,ierror)
      call cmo_get_info('imt1',cmo,ipimt1,lenimt1,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,lenisn1,icmotype,ierror)
      call cmo_get_info('ign1',cmo,ipign1,lenign1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itettyp',cmo,ipitettyp,lenitettyp,icmotype,ier)
      call cmo_get_info('itetoff',cmo,ipitetoff,lenitetoff,icmotype,ier)
      call cmo_get_info('jtetoff',cmo,ipjtetoff,lenjtetoff,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
C
C     ******************************************************************
C
C     ALLOCATE SOME TEMPORARY MEMORY.
C
C
      print *,"Read NGP-hex mesh file and convert it to an X3D file"
C
      do i=1,npoints
         imt1(i)=0
         itp1(i)=0
         read(iunit,*) i1,xic(i),yic(i),zic(i)
      enddo
      do i=1,ntets
         read(iunit,*) ipoint,i1,i2,i3,i4,i5,i6,i7,i8,itetclr(i)
         index=nen*(i-1)
         itet1(1+index)=i1
         itet1(2+index)=i2
         itet1(3+index)=i3
         itet1(4+index)=i4
         itet1(5+index)=i5
         itet1(6+index)=i6
         itet1(7+index)=i7
         itet1(8+index)=i8
         itettyp(i)=ifelmhex
         itetoff(i)=nen*(i-1)
         jtetoff(i)=nef*(i-1)
      enddo
C
C     CLOSE THE DUMP FILE.
C
      close(iunit)
C
C
C     .................................................................
C     SET THE MATERIAL COLORS.
C
      imt1min=99999999
      imt1max=0
      do it=1,ntets
         imt1min=min(imt1min,itetclr(it))
         imt1max=max(imt1max,itetclr(it))
      enddo
      if(imt1min.le.0) then
         imt1min=1-imt1min
      else
         imt1min=0
      endif
      do it=1,ntets
         index=nen*(it-1)
         itetclr(it)=itetclr(it)+imt1min
         do i=1,nen
            imt1(itet1(index+i))=itetclr(it)
         enddo
      enddo
C
C
C     .................................................................
C     GENERATE THE CONNECTIVITY MATRIX FOR THE NGP HEXES.
C
      length=nef*ntets
      call mmgetblk("jtet2" ,isubname,ipjtet2,length,2,icscode)
      do j=1,ntets
         do i=1,nef
            index=nef*(j-1)+i
            jtet1(index)=-1
            jtet2(index)=-1
         enddo
      enddo
      call geniee(itet1,jtet1,jtet2,nen,nef,ntets,npoints,
     *            nsd,npoints,ntets)
      do j=1,ntets
         do i=1,nef
            index=nef*(j-1)+i
            if(jtet1(index).eq.0) then
               jtet1(index)=mbndry
            else
               jtet1(index)=nef*(jtet1(index)-1)+jtet2(index)
            endif
         enddo
      enddo
C
C     .................................................................
C     SET THE EXTERNAL BOUNDARY NODE TYPE BASED ON BOUNDARY FACES.
C
      cmotype='hex'
      do i=1,npoints
         itp1(i)=0
      enddo
      do it=1,ntets
         index=nef*(it-1)
         do i=1,nef
            index=nef*(it-1)+i
            if(jtet1(index).le.0.or.jtet1(index).ge.mbndry) then
               jndex=nef*(it-1)
               if(cmotype(1:3).eq.'tet') then
                  do j=1,3
                     j1=itet1(jndex+itetface1(j,i))
                     itp1(j1)=ifitprfl
                  enddo
               elseif(cmotype(1:3).eq.'hex') then
                  do j=1,4
                     j1=itet1(jndex+ihexface1(j,i))
                     itp1(j1)=ifitprfl
                  enddo
               endif
            endif
         enddo
      enddo
      do it=1,ntets
         index=nef*(it-1)
         do i=1,nef
            index=nef*(it-1)+i
            if(jtet1(index).gt.0.and.jtet1(index).lt.mbndry) then
               jt=1+(jtet1(index)-1)/nef
               jf=jtet1(index)-nef*(jt-1)
               if(itetclr(it).ne.itetclr(jt)) then
                  jndex=nef*(it-1)
                  if(cmotype(1:3).eq.'tet') then
                     do j=1,3
                        j1=itet1(jndex+itetface1(j,i))
                        if(itp1(j1).eq.ifitprfl) then
                           itp1(j1)=ifitpinb
                        else
                           itp1(j1)=ifitpini
                        endif
                     enddo
                  elseif(cmotype(1:3).eq.'hex') then
                     do j=1,4
                        j1=itet1(jndex+ihexface1(j,i))
                        if(itp1(j1).eq.ifitprfl) then
                           itp1(j1)=ifitpinb
                        else
                           itp1(j1)=ifitpini
                        endif
                     enddo
                  endif
                  jtet1(index)=mbndry+nef*(jt-1)+jf
                  jtet1(nef*(jt-1)+jf)=mbndry+index
               endif
            endif
         enddo
      enddo
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
      call cmo_set_info('nelements',cmo,ntets,1,1,ier)
C
C
C
C     ******************************************************************
C
      ierrsub=0
      goto 9999
 9999 continue
C
      call mmrelprt(isubname,icscode)
C
C     ******************************************************************
C
      return
      end
