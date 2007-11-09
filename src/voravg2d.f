*dk,voravg2d
      subroutine voravg2d(cmo,mpary,mpno,niters,field,ierror)
C
C #####################################################################
C
C     PURPOSE - VORAVG2D replaces the node values of a field with
C        volume averages over the Voronoi volumes.
C
C     INPUT ARGUMENTS -
C
C         cmo       - Character array giving current mesh object
C         mpary()   - integer array specifying which nodes to change
C                     the field values of.
C         mpno      - length of mpary
C         niters    - number of averaging sweeps to be performed
C         field()- real array of field values
C
C     OUTPUT ARGUMENTS -
C
C         ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/voravg2d.f_a  $
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 17:05:46 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   11/07/95 17:29:14   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS    
CPVCS       Rev 1.0   08/24/95 16:39:22   kuprat
CPVCS    Initial revision.
C
C ######################################################################
C    
C   For all points in the specified point set of the current cmo, we 
c   replace the values of FIELD with the average value of FIELD over
c   the associated Voronoi control volume.  The process will
c   be repeated NITERS times.  The affect of this process is to 'broaden'
c   the features of FIELD, similar to the affect of running the heat
c   equation forward in time.  (Caution:  strange results will occur 
c   if the mesh connectivity is not Delaunay!!)
c
C   For the point I, we replace the field value FIELD(I) with the 
C   Voronoi volume average which we take to be 
C
C                   Sum  S_ij * r_ij *  (2/3*FIELD(I) + 1/3*FIELD(J))
C                   -------------------------------------------------
C                                  Sum S_ij * r_ij
C 
C   where the sum is over the neighbours J of I, FIELD(J) is the field value 
C   at J, S_ij is the length of the Voronoi edge corresponding to the
C   node pair {I,J} and r_ij is the distance between
C   nodes I and J.
C
      implicit none
      include 'consts.h'

      integer ierror

      pointer (ipitet,itet)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      pointer (ipisetwd,isetwd)
      pointer (ipitp1, itp1)
      real*8 field(10000000)
      integer itet(3,10000000)
      real*8 xic(10000000),yic(10000000),zic(10000000)
      integer isetwd(10000000)
      integer itp1(10000000)

      pointer (ipdenom,denom)
      pointer (ipvalues,values)
      integer mpary(10000000)
      real*8 denom(10000000)
      real*8 values(10000000)

      integer icharln,length,icmotype,mpno,icscode
      character*32 cmo,isubname,blkname

      real*8 dist(3),d1,d2,d3,cosopp,ccj,denomj,value2,value3,
     &   cotopp
      integer i,nnodes,nelements,j,i1,i2,i3,iters,niters

      integer itriedge1(3,3)
      data itriedge1 / 2, 3, 1,
     &   3, 1, 2,
     &   1, 2, 3 /
      save itriedge1

      isubname = 'voravg2d'

      ierror=0
C
C  Allocate local arrays
C
      call cmo_get_info('nnodes',cmo,
     *                        nnodes,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')

      call mmgetblk('values',isubname,ipvalues,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk') 
      call mmgetblk('denom',isubname,ipdenom,nnodes,2,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmgetblk') 
c 
c    get cmo array pointers
c
      call cmo_get_info('nelements',cmo,
     *                        nelements,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('isetwd',cmo,
     *                        ipisetwd,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itp1',cmo,
     *                        ipitp1,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('itet',cmo,
     *                        ipitet,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('xic',cmo,
     *                        ipxic,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,
     *                        ipyic,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,
     *                        ipzic,length,icmotype,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'cmo_get_info')

      do iters=1,niters
c   
c     Initialize arrays.  VALUES is the numerator in our 
c     field average expression; DENOM is the denominator.
c
         do i=1,nnodes
            values(i)=0.
            denom(i)=0.
         enddo
 
c Loop over all triangles

         do i=1,nelements

c Calculate lengths of 3 edges

            do j=1,3
               i1=itet(itriedge1(1,j),i)
               i2=itet(itriedge1(2,j),i)
               dist(itriedge1(3,j))=sqrt((xic(i2)-xic(i1))**2+
     &            (yic(i2)-yic(i1))**2+
     &            (zic(i2)-zic(i1))**2)
            enddo

c For each edge, {I2,I3}, calculate contributions to
c VALUES(I2), VALUES(I3), DENOM(I2), DENOM(I3)

            do j=1,3
               i1=itet(itriedge1(1,j),i)
               i2=itet(itriedge1(2,j),i)
               i3=itet(itriedge1(3,j),i)
               d1=dist(itriedge1(1,j))
               d2=dist(itriedge1(2,j))
               d3=dist(itriedge1(3,j))
               cosopp=((xic(i3)-xic(i1))*(xic(i2)-xic(i1))+
     &            (yic(i3)-yic(i1))*(yic(i2)-yic(i1))+
     &            (zic(i3)-zic(i1))*(zic(i2)-zic(i1)))/
     &            (d2*d3)
               cotopp=sign(sqrt(cosopp**2/(1.-cosopp**2)),cosopp)

c CCJ is the coupling coefficient between I2 and I3 due
c to triangle I.  This is length of the Voronoi edge
c corresponding to the node pair {I2,I3} which is 
c due to triangle I.
               ccj=d1*0.5*cotopp
               denomj=d1*ccj
               value2=denomj*(2.*field(i2)+field(i3))/3.
               value3=denomj*(field(i2)+2.*field(i3))/3.
               denom(i2)=denom(i2)+denomj
               denom(i3)=denom(i3)+denomj
               values(i2)=values(i2)+value2
               values(i3)=values(i3)+value3
            enddo
         enddo
         do i=1,nnodes

c  We replace field values with the volume averages
c  only for points with positive associated volumes.

            if (denom(i).gt.zero) then
               field(i)=values(i)/denom(i)
            endif
         enddo
      enddo

 9999 continue
      call mmrelprt(isubname,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmrelprt')
      return
      end
