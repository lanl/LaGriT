      subroutine minsine_tet(x,y,z,qminsine)
C
C #####################################################################
C
C     PURPOSE -
C
C        Smallest sine of the six dihedral angles of a tet (quality
C        measure "minsine" of the quality command, Oct 2026).
C
C        For the edge e of length L shared by the faces with areas A1
C        and A2, the sine of the dihedral angle at e is
C
C              sin = 3 V L / (2 A1 A2)
C
C        with V the volume of the tet. Small and large dihedral angles
C        both give a small sine, so the measure is 1 for no angle and
C        near 0 for any tet with an angle near 0 or near 180 degrees
C        (sliver, cap, needle, wedge). A regular tet has 0.9428
C        (sin 70.53 degrees). The absolute volume is used, so an
C        inverted tet gets the value of its mirror image; a flat tet
C        gets 0.
C
C     INPUT ARGUMENTS -
C
C        x(4),y(4),z(4) - the coordinates of the four vertices
C
C     OUTPUT ARGUMENTS -
C
C        qminsine - smallest sine of the dihedral angles (0 to 1)
C
C #####################################################################
C
      implicit none
      real*8 x(4),y(4),z(4),qminsine
      real*8 ax,ay,az,bx,by,bz,cx,cy,cz,vol6,area(4),el,sn
      integer k,i1,i2,i3,e,f1,f2,ie1,ie2
      integer iface(3,4),iedge(2,6),iopp(2,6)
C     face i is opposite vertex i
      data iface / 2,3,4, 1,3,4, 1,2,4, 1,2,3 /
C     edge (iedge) is shared by the faces opposite the two other
C     vertices (iopp)
      data iedge / 1,2, 1,3, 1,4, 2,3, 2,4, 3,4 /
      data iopp  / 3,4, 2,4, 2,3, 1,4, 1,3, 1,2 /
C
      ax=x(2)-x(1)
      ay=y(2)-y(1)
      az=z(2)-z(1)
      bx=x(3)-x(1)
      by=y(3)-y(1)
      bz=z(3)-z(1)
      cx=x(4)-x(1)
      cy=y(4)-y(1)
      cz=z(4)-z(1)
      vol6=abs(ax*(by*cz-bz*cy)-ay*(bx*cz-bz*cx)+az*(bx*cy-by*cx))
      do k=1,4
         i1=iface(1,k)
         i2=iface(2,k)
         i3=iface(3,k)
         ax=x(i2)-x(i1)
         ay=y(i2)-y(i1)
         az=z(i2)-z(i1)
         bx=x(i3)-x(i1)
         by=y(i3)-y(i1)
         bz=z(i3)-z(i1)
         area(k)=0.5d0*sqrt((ay*bz-az*by)**2+(az*bx-ax*bz)**2
     &      +(ax*by-ay*bx)**2)
      enddo
      qminsine=1.0d0
      do e=1,6
         ie1=iedge(1,e)
         ie2=iedge(2,e)
         f1=iopp(1,e)
         f2=iopp(2,e)
         el=sqrt((x(ie2)-x(ie1))**2+(y(ie2)-y(ie1))**2
     &      +(z(ie2)-z(ie1))**2)
         if (area(f1)*area(f2).le.0.0d0) then
            sn=0.0d0
         else
C           V = vol6/6, so 3 V L / (2 A1 A2) = vol6 L / (4 A1 A2)
            sn=vol6*el/(4.0d0*area(f1)*area(f2))
         endif
         if (sn.gt.1.0d0) sn=1.0d0
         if (sn.lt.qminsine) qminsine=sn
      enddo
      return
      end
