*deck dihangle_element
      subroutine dihangle_element(ielmtyp,xice,yice,zice,
     *                            minangle,maxangle)
C
C
C #####################################################################
C
C     PURPOSE -
C
C        Finds the max and min dihedral angles between adjacent
C        faces of a 3D element or edges of a 2D element.
C
C     INPUT ARGUMENTS -
C
C        ielmtyp                : Element type.
C        (xice(),yice(),zice()) : The coordinates of the points of the
C                                 element.  They must be in the order
C                                 in which they appear in itet.
C
C     OUTPUT ARGUMENTS -
C
C        minangle,maxangle : The min and max angles of the element
C                            in radians.
C
C     CHANGE HISTORY -
C
C  $Log:   /pvcs.config/t3d/src/dihangle_element.f_a  $
CPVCS    
CPVCS       Rev 1.0   Fri Aug 29 14:06:56 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
C
      implicit none
C
      include "local_element.h"
C
      character* 132 logmess
      integer icscode
 
      integer ielmtyp
      real*8 xice(1000000),yice(1000000),zice(1000000)
      real*8 minangle,maxangle
 
      real*8 f_angle
      integer i,j,k,l,ptcount,pointsi,pointsj,k1,l1,k2,l2
      real*8 x1,y1,z1,x2,y2,z2,dot,norm1,norm2
      real*8 xici(8),yici(8),zici(8),xicj(8),yicj(8),zicj(8)
C
C ######################################################################
C
C
 
         minangle = 8.0
         maxangle = -8.0
 
      if ((ielmtyp .eq. ifelmtet) .or.
     *    (ielmtyp .eq. ifelmpyr) .or.
     *    (ielmtyp .eq. ifelmpri) .or.
     *    (ielmtyp .eq. ifelmhex) .or.
     *    (ielmtyp .eq. ifelmhyb)) then
         do i=1,nelmnef(ielmtyp)-1
            do j=i+1,nelmnef(ielmtyp)
               ptcount = 0
               do k=1,ielmface0(i,ielmtyp)
                  k1 = ielmface1(k,i,ielmtyp)
                  do l=1,ielmface0(j,ielmtyp)
C  Check to see how many points these faces meet in
                     l1 = ielmface1(l,j,ielmtyp)
                     if (k1 .eq. l1) then
                        ptcount = ptcount + 1
                     endif
                  enddo
C  Record the coordinates of the point k on face i
                  xici(k)=xice(k1)
                  yici(k)=yice(k1)
                  zici(k)=zice(k1)
               enddo
               pointsi = ielmface0(i,ielmtyp)
C  Record the coordinates of all points on face j
C  Didn't do it above to avoid doing it multiple times
               do l=1,ielmface0(j,ielmtyp)
                  l1 = ielmface1(l,j,ielmtyp)
                  xicj(l)=xice(l1)
                  yicj(l)=yice(l1)
                  zicj(l)=zice(l1)
               enddo
               pointsj = ielmface0(j,ielmtyp)
               if (ptcount .ge. 2) then
                  call dihangle_face(pointsi,xici,yici,zici,
     *                               pointsj,xicj,yicj,zicj,f_angle)
                  if (f_angle .lt. minangle) then
                     minangle = f_angle
                  endif
                  if (f_angle .gt. maxangle) then
                     maxangle = f_angle
                  endif
               endif
               if (ptcount .gt. 2) then
                  write (logmess,"('Too many face-to-face ',
     *                             'intersections.')")
                  call writloga('default',0,logmess,0,icscode)
                  return
               endif
            enddo
         enddo
      elseif ((ielmtyp .eq. ifelmtri) .or.
     *        (ielmtyp .eq. ifelmqud)) then
         do i=1,nelmnee(ielmtyp)-1
            do j=i+1,nelmnee(ielmtyp)
               ptcount = 0
               do k=1,2
                  k1 = ielmedge1(k,i,ielmtyp)
                  do l=1,2
C  Check to see if these edges meet
                     l1 = ielmedge1(l,j,ielmtyp)
                     if (k1 .eq. l1) then
                        ptcount = ptcount + 1
C  Get the other point on each edge
                        k2 = ielmedge1(3-k,i,ielmtyp)
                        l2 = ielmedge1(3-l,j,ielmtyp)
                        x1 = xice(k2) - xice(k1)
                        y1 = yice(k2) - yice(k1)
                        z1 = zice(k2) - zice(k1)
                        x2 = xice(l2) - xice(l1)
                        y2 = yice(l2) - yice(l1)
                        z2 = zice(l2) - zice(l1)
                        norm1 = sqrt(x1*x1 + y1*y1 + z1*z1)
                        norm2 = sqrt(x2*x2 + y2*y2 + z2*z2)
                        x1 = x1 / norm1
                        y1 = y1 / norm1
                        z1 = z1 / norm1
                        x2 = x2 / norm2
                        y2 = y2 / norm2
                        z2 = z2 / norm2
 
                        dot = x1*x2 + y1*y2 + z1*z2
                        f_angle = acos(dot)
                        if (f_angle .lt. minangle) then
                           minangle = f_angle
                        endif
                        if (f_angle .gt. maxangle) then
                           maxangle = f_angle
                        endif
                     endif
                  enddo
               enddo
               if (ptcount .gt. 1) then
                  write (logmess,"('Too many edge-to-edge ',
     *                             'intersections.')")
                  call writloga('default',0,logmess,0,icscode)
                  return
               endif
            enddo
         enddo
      else
         write (logmess,"('Only triangles, quads and 3D elements ',
     *                    'may have their angles computed.')")
         call writloga('default',0,logmess,0,icscode)
         return
      endif
 
      return
      end
