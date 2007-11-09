*dk,readngptet
      subroutine readngptet(ifile,ierror)
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
C
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
      pointer (ipitet, itet)
      pointer (ipjtet, jtet)
      integer itet(4,1000000)
      integer jtet(4,1000000)
C
C ######################################################################
C
C ######################################################################
C
c
      pointer (ipjtet2, jtet2)
      integer jtet2(4,100000)
      pointer (ipxc, xc)
      real*4 xc(3*10000000)
C
      character ifile*(*)
      character*32 isubname, cmo
C
C ######################################################################
C
C
      integer itetface0(4), itetface1(4,4)
C     top,back,left,right
      data itetface0 / 3, 3, 3, 3 /
      data itetface1 / 2, 3, 4, 1,
     *                 1, 4, 3, 2,
     *                 1, 2, 4, 3,
     *                 1, 3, 2, 4 /
      integer itet_edge(2,6)
      data itet_edge / 1, 2,
     *                 1, 3,
     *                 1, 4,
     *                 2, 3,
     *                 2, 4,
     *                 3, 4 /
      integer itri_edge(2,3)
      data itri_edge / 1, 2,
     *                 1, 3,
     *                 2, 3 /
C
C ######################################################################
C
C
      isubname="readngptet"
Cc
c     Open the output file
c     --------------------
      open(unit=20,form='unformatted',status='unknown',file=ifile)
c
c     Output numbers of tetrahedra, points and boundary triangles
c     -----------------------------------------------------------
      read(20)  ntets,npoints,nface
      print *,"Read NGP .plt file: ",ifile
      print *,"Npoints: ",npoints,"  Ntets: ",ntets
C
      length=4*ntets
      call mmgetblk("jtet2",isubname,ipjtet2,length,2,icscode)
      length=3*npoints
      call mmgetblk("xc",isubname,ipxc,length,2,icscode)
      length=nface*5
      call mmgetblk("jface",isubname,ipjface,length,2,icscode)
C
C
      nsd=3
      nen=4
      nef=4
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ierror)
      call cmo_set_info('nelements',cmo,ntets,1,1,ierror)
      call cmo_set_info('ndimensions_geom',cmo,nsd,1,1,ierror)
      call cmo_set_info('ndimensions_topo',cmo,nsd,1,1,ierror)
      call cmo_set_info('nodes_per_element',cmo,nen,1,1,ierror)
      call cmo_set_info('faces_per_element',cmo,nef,1,1,ierror)
C
      call cmo_newlen(cmo,ierror)
C
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
c     Output the tetrahedra
c     ---------------------
      read(20) ((itet(i,j),j=1,ntets),i=1,4)
      do j=1,ntets
         do i=1,4
            jtet(i,j)=-1
            jtet2(i,j)=-1
         enddo
      enddo
c
c     Output the points as single precision
c     -------------------------------------
      read(20) (xc(i),i=1,3*npoints)
      do i = 1, npoints
        imt1(i) = 1
        itp1(i) = 0
        xic(i) = xc(i)
        yic(i) = xc(i+npoints)
        zic(i) = xc(i+2*npoints)
      enddo
 
      call geniee(itet,jtet,jtet2,4,4,ntets,npoints,
     *            3,npoints,ntets)
 
      do j=1,ntets
         itetclr(j)=2
         itettyp(j)=ifelmtet
         itetoff(j)=nen*(j-1)
         jtetoff(j)=nef*(j-1)
         do i=1,4
            index=4*(j-1)+i
            if(jtet(i,j).eq.0) then
               jtet(i,j)=mbndry
            else
               jtet(i,j)=4*(jtet(i,j)-1)+jtet2(i,j)
            endif
         enddo
      enddo
      do it=1,ntets
         i1=itet(1,it)
         i2=itet(2,it)
         i3=itet(3,it)
         i4=itet(4,it)
         imt1(i1)=itetclr(it)
         imt1(i2)=itetclr(it)
         imt1(i3)=itetclr(it)
         imt1(i4)=itetclr(it)
         do i=1,4
            if(jtet(i,it).gt.0.and.jtet(i,it).lt.mbndry) then
               jt=1+(jtet(i,it)-1)/4
               jf=jtet(i,it)-4*(jt-1)
               j1=itet(itetface1(4,i),it)
               j2=itet(itetface1(1,i),it)
               j3=itet(itetface1(2,i),it)
               j4=itet(itetface1(3,i),it)
               itp1(j2)=ifitprfl
               itp1(j3)=ifitprfl
               itp1(j4)=ifitprfl
            endif
         enddo
      enddo
      do it=1,ntets
         i1=itet(1,it)
         i2=itet(2,it)
         i3=itet(3,it)
         i4=itet(4,it)
         do i=1,4
            index=4*(it-1)+i
            if(jtet(i,it).gt.0.and.jtet(i,it).lt.mbndry) then
               jt=1+(jtet(i,it)-1)/4
               jf=jtet(i,it)-4*(jt-1)
               if(itetclr(it).ne.itetclr(jt)) then
                  j1=itet(itetface1(4,i),it)
                  j2=itet(itetface1(1,i),it)
                  j3=itet(itetface1(2,i),it)
                  j4=itet(itetface1(3,i),it)
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
                  jtet(i,it)=mbndry
                  jtet(jf,jt)=mbndry
               endif
            endif
         enddo
      enddo
C
C
c     Output the triangles
c     --------------------
C*****read(20) ((jface(i,j), i=1,nface ),j=1,5)
c
      close(20)
C
      call cmo_get_name(cmo,ierror)
      call cmo_set_info('nnodes',cmo,npoints,1,1,ier)
      call cmo_set_info('nelements',cmo,ntets,1,1,ier)
C
      call mmrelprt(isubname,icscode)
C
      goto 9999
 9999 continue
      return
      end
