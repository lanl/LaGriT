      subroutine damp_pt_by_volume(node,nodhyb,nodhyboff,
     &   ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,ipivoloffoff,
     &   itettyp,itet,itetoff,x,y,z,absvoltol,relvoltol,dx,dy,dz)
C #####################################################################
C
C     PURPOSE -
C
C        DAMP_PT_BY_VOLUME considers the proposed point movement of
C     NODE by DX,DY,DZ, and limits it so that tet inversions
C     (negative volumes) are avoided.  In the case of tets that 
C     are already inverted, we prevent the situation from getting
C     worse.
C
C     INPUT ARGUMENTS -
C
C        NODE  -  The node whose movement we are limiting.
C        NODHYB-  Node-element relation.  Passed to polyfun.
C        NODHYBOFF-  Node-element relation offsets.  Passed to polyfun.
C        IELTARY- Array of relevant elements.
C        IELTNO-  Length of element array.
C        INVMPARY- Inverse mapping of that defined by mass point array
C                  MPARY.
C        VOLOFF-   Array of volume offsets.
C        LOCVOLOFF- Array giving which virtual tet volume offsets apply to.
C        VOLOFFOFF- Offset array of volume offsets.
C        ITETTYP-  Array of element types.
C        ITET-     Element-node relation.
C        ITETOFF-  Offsets for Element-node relation.
C        X,Y,Z -  The xic,yic,zic arrays for the mesh.
C        ABSVOLTOL - The smallest tet volume tolerated.
C        RELVOLTOL - The greatest fractional tet volume reduction tolerated.
C        DX,DY,-  Proposed changes to add to X(NODE), Y(NODE), Z(NODE).
C        DZ
C
C     OUTPUT ARGUMENTS -
C
C        DX,DY,-  Safe changes to add to X(NODE), Y(NODE), Z(NODE),
C        DZ       which prevent tet inversion.
C
C     CHANGE HISTORY -
C$Log:   /pvcs.config/t3d/src/damp_pt_by_volume.f_a  $
CPVCS    
CPVCS       Rev 1.2   09 Apr 2001 15:23:30   kuprat
CPVCS    We reinstate previous version.
CPVCS    
CPVCS       Rev 1.1   09 Apr 2001 14:23:44   kuprat
CPVCS    Took out absolute volume bailout:  We now entertain smaller
CPVCS    and smaller moves, so that theoretically we could get very
CPVCS    tiny tetrahedra.
CPVCS    
CPVCS       Rev 1.0   Wed Oct 29 16:57:46 1997   kuprat
CPVCS    Initial revision.
C
C ######################################################################
      implicit none

      integer lenptr
      parameter (lenptr=1000000)

      include 'consts.h'
      include 'local_element.h'
      include 'smooth.h'

      pointer (ipvoloff,voloff)
      pointer (iplocvoloff,locvoloff)
      pointer (ipivoloffoff,ivoloffoff)

      integer node,nodhyb(lenptr),nodhyboff(lenptr),ieltary(lenptr),
     &   ieltno,invmpary(lenptr),locvoloff(lenptr),ivoloffoff(lenptr),
     &   itettyp(lenptr),itet(lenptr),itetoff(lenptr)
      real*8 voloff(lenptr),x(lenptr),y(lenptr),z(lenptr),absvoltol,
     &   relvoltol,dx,dy,dz
      
      real*8 t,det,detreduction,detexcess,absdettol,a1x,a1y,a1z,
     &   voff(maxhybnumtetv)
      integer i,i2,i3,i4,mpk,ii,lochybnod,ihyb,k1,k,jtetvi,
     &   loctet,locnod
      integer itetface1(4,4)
      data itetface1 / 2, 3, 4, 0,
     *     1, 4, 3, 0,
     *     1, 2, 4, 0,
     *     1, 3, 2, 0 /
      save itetface1
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,crosx,crosy,crosz
 
c Statement functions for the components of the cross product
c ((x2,y2,z2)-(x1,y1,z1)) X ((x3,y3,z3)-(x1,y1,z1)) .
      crosx(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(y2-y1)*(z3-z1)-(z2-z1)*(y3-y1)
      crosy(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(z2-z1)*(x3-x1)-(x2-x1)*(z3-z1)
      crosz(x1,y1,z1,x2,y2,z2,x3,y3,z3)=(x2-x1)*(y3-y1)-(y2-y1)*(x3-x1)
 
      absdettol=6.*absvoltol
      t=1.

c...  Loop over all (possibly hybrid) elements in polyhedron
            
      mpk=invmpary(node)
      do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
         ii=1+(nodhyb(i)-1)/maxnen
         lochybnod=nodhyb(i)-maxnen*(ii-1)
         ihyb=ieltary(ii)

c.... Calculate volume offsets for virtual tetrahedra in 
c.... current element.

         do k1=1,ihybnumtetv(itettyp(ihyb))
            voff(k1)=0.
         enddo
         do k=ivoloffoff(ii)+1,ivoloffoff(ii+1)
            voff(locvoloff(k))=voloff(k)
         enddo

c.... Loop over all virtual tetrahedra in current element
c.... that involve the node at the center of the polyhedron.

         do k1=1,nodnumtetv(lochybnod,itettyp(ihyb))
            jtetvi=jtetv(k1,lochybnod,itettyp(ihyb))
            loctet=1+(jtetvi-1)/4
            locnod=jtetvi-4*(loctet-1)
            i2=itet(itetv(ielmface1(1,locnod,ifelmtet)
     &         ,loctet,itettyp(ihyb))+itetoff(ihyb))
            i3=itet(itetv(ielmface1(2,locnod,ifelmtet)
     &         ,loctet,itettyp(ihyb))+itetoff(ihyb))
            i4=itet(itetv(ielmface1(3,locnod,ifelmtet)
     &         ,loctet,itettyp(ihyb))+itetoff(ihyb))
                  
c...  Compute area vector of face opposite NODE
                  
            a1x=crosx(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &         x(i4),y(i4),z(i4))
            a1y=crosy(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &         x(i4),y(i4),z(i4))
            a1z=crosz(x(i2),y(i2),z(i2),x(i3),y(i3),z(i3),
     &         x(i4),y(i4),z(i4))
 
c.... Calculate determinant (i.e. six times the volume of the tet).
c.... Subtract off offset so that we can handle cases where the 
c.... tet is already inverted.

            det=(x(i2)-x(node))*a1x+(y(i2)-y(node))*a1y+
     &         (z(i2)-z(node))*a1z-voff(loctet)
c$$$            det=(x(i2)-x(node))*a1x+(y(i2)-y(node))*a1y+
c$$$     &         (z(i2)-z(node))*a1z
            detreduction=dx*a1x+dy*a1y+dz*a1z
 
C Possibly limit movement.

            if (detreduction.gt.zero) then
               detexcess=min(det-absdettol,det*relvoltol)
               if (detexcess.le.0.) then
                  dx=0.
                  dy=0.
                  dz=0.
                  goto 9999
               endif
               if (detreduction*t.gt.detexcess) then
                  t=detexcess/detreduction
               endif
            endif
         enddo
      enddo

      dx=t*dx
      dy=t*dy
      dz=t*dz
 
 9999 continue
      return
      end
 
