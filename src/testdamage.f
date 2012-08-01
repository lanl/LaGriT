C
      subroutine testdamage(i1,i2,i3,i4,iflag,toldamage)
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine determines whether or not replacing the
C        connection i1-i3 with the connection i2-i4 (or vice versa)
C        would case linear deformation ('damage') exceeding TOLDAMAGE.
C
C     INPUT ARGUMENTS -
C
C        i1-i4    - the points involved, in cyclic order.
C
C     OUTPUT ARGUMENTS -
C
C        iflag    - 0 => flip causes more than acceptable damage
C                   1 => flip causes acceptable damage
C
C     CHANGE HISTORY -
C
C        $Log: testdamage.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   30 Jan 2002 10:36:58   dcg
CPVCS    remove duplicate declarations
CPVCS
CPVCS       Rev 1.1   07 Jan 2002 13:40:24   dcg
CPVCS    add subroutine testdamage_refine
CPVCS
CPVCS       Rev 1.0   Mon May 25 00:50:50 1998   kuprat
CPVCS    Initial revision.
C
C ######################################################################
      implicit none
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "consts.h"

C arguments (i1,i2,i3,i4,iflag,toldamage)
      integer i1,i2,i3,i4,iflag
      real*8 toldamage

C variables
      integer i,j,k,ierror,lenxic,lenyic,lenzic,icmotype
      real*8 a134x,a134y,a134z,a312x,a312y,a312z,atotx,atoty,atotz,
     &   atot,xmid,ymid,zmid,dot134,dot312,dot1,dot2,dot3,dot4,dotmin,
     &   dotmax,damage,dist2,dist4
 
      real*8 crosx1,crosy1,crosz1
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
C
C ######################################################################
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
c.... Check that the 'damage' of performing a flip is less than
c.... TOLDAMAGE.  The damage will be nonzero if the points i1, i2,
c.... i3, i4 do not all lie in the same plane.  The damage is defined
c.... in a similar fashion as in the subroutine AGD3D.
 
      iflag=1
 
c.... Form aggregate normal:  Area-weighted normal formed from the
c.... two boundary triangles [triangle (i1,i3,i4) and triangle (i3,i1,i2)].
 
      a134x=crosx1(i1,i3,i4)
      a134y=crosy1(i1,i3,i4)
      a134z=crosz1(i1,i3,i4)
      a312x=crosx1(i3,i1,i2)
      a312y=crosy1(i3,i1,i2)
      a312z=crosz1(i3,i1,i2)
      atotx=a134x+a312x
      atoty=a134y+a312y
      atotz=a134z+a312z
      atot=sqrt(atotx**2+atoty**2+atotz**2)
      atotx=atotx/atot
      atoty=atoty/atot
      atotz=atotz/atot
 
c.... Calculate midpoint of edge (i1,i3)
 
      xmid=half*(xic(i1)+xic(i3))
      ymid=half*(yic(i1)+yic(i3))
      zmid=half*(zic(i1)+zic(i3))
 
c.... If both boundary triangle normals are in the same direction as
c.... aggregate normal, damage is defined to be separation between two
c.... planes (normal to agg. normal) that sandwich the points i1, i2,
c.... i3, i4.  If one of the boundary triangle normals points contrary
c.... to the agg. normal, damage is defined as minimum of 'merge
c.... distances' from the midpoint of edge (i1,i3) to the points i2, i4.
 
      dot134=a134x*atotx+a134y*atoty+a134z*atotz
      dot312=a312x*atotx+a312y*atoty+a312z*atotz
 
      if (dot134.gt.zero.and.dot312.gt.zero) then
 
         dot1=(xic(i1)-xmid)*atotx+(yic(i1)-ymid)*atoty+
     &      (zic(i1)-zmid)*atotz
         dot2=(xic(i2)-xmid)*atotx+(yic(i2)-ymid)*atoty+
     &      (zic(i2)-zmid)*atotz
         dot3=(xic(i3)-xmid)*atotx+(yic(i3)-ymid)*atoty+
     &      (zic(i3)-zmid)*atotz
         dot4=(xic(i4)-xmid)*atotx+(yic(i4)-ymid)*atoty+
     &      (zic(i4)-zmid)*atotz
         dotmin=min(dot1,dot2,dot3,dot4)
         dotmax=max(dot1,dot2,dot3,dot4)
         damage=dotmax-dotmin
         if (damage.gt.toldamage) iflag=0
 
      else
 
         dist2=sqrt((xic(i2)-xmid)**2+(yic(i2)-ymid)**2+
     &      (zic(i2)-zmid)**2)
         dist4=sqrt((xic(i4)-xmid)**2+(yic(i4)-ymid)**2+
     &      (zic(i4)-zmid)**2)
         damage=min(dist2,dist4)
         if (damage.gt.toldamage) iflag=0
 
      endif
 
      goto 9999
 9999 continue
      return
      end
C ######################################################################
      subroutine testdamage_refine(i1,i2,i3,i4,qx,qy,qz,toldamage,iflag)
C
C ######################################################################
C
C     PURPOSE -
C
C       This routine determines whether refining edge i1-i3 by replacing
C       the edge by the two line segments from i1 to qx,qy,qz and
c       from i3 to qx,qy,qz
C       would case linear deformation ('damage') exceeding TOLDAMAGE.
C
C     INPUT ARGUMENTS -
C
C        i1-i4    - the points involved, in cyclic order.
c        qx,qy,qz  - the proposed refine node
c        toldamage - the allowed damage (length)
C
C     OUTPUT ARGUMENTS -
C
C        iflag    - 0 => causes more than acceptable damage
C                   1 => causes acceptable damage
C
C
C ######################################################################
C
      implicit none
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "consts.h"
 
      integer i1,i2,i3,i4,iflag
      real*8 toldamage
      integer i,j,k,ierror,lenxic,lenyic,lenzic,icmotype
      real*8 a134x,a134y,a134z,a312x,a312y,a312z,atotx,atoty,atotz,
     &   atot,xmid,ymid,zmid,dot134,dot312,
     &   damage,qx,qy,qz
 
      real*8 crosx1,crosy1,crosz1
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
C
C ######################################################################
C BEGIN begin
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
C
      endif
C
C     ******************************************************************
C
C
c.... Check that the 'damage' of performing a flip is less than
c.... TOLDAMAGE.  The damage will be nonzero if the points i1, i2,
c.... i3, i4 do not all lie in the same plane.  The damage is defined
c.... in a similar fashion as in the subroutine AGD3D.
 
      iflag=1
 
c.... Form aggregate normal:  Area-weighted normal formed from the
c.... two boundary triangles [triangle (i1,i3,i4) and triangle (i3,i1,i2)].
 
      a134x=crosx1(i1,i3,i4)
      a134y=crosy1(i1,i3,i4)
      a134z=crosz1(i1,i3,i4)
      a312x=crosx1(i3,i1,i2)
      a312y=crosy1(i3,i1,i2)
      a312z=crosz1(i3,i1,i2)
      atotx=a134x+a312x
      atoty=a134y+a312y
      atotz=a134z+a312z
      atot=sqrt(atotx**2+atoty**2+atotz**2)
      atotx=atotx/atot
      atoty=atoty/atot
      atotz=atotz/atot
 
c.... Calculate midpoint of edge (i1,i3)
 
      xmid=half*(xic(i1)+xic(i3))
      ymid=half*(yic(i1)+yic(i3))
      zmid=half*(zic(i1)+zic(i3))
 
      damage=(qx-xmid)*atotx+(qy-ymid)*atoty+
     &      (qz-zmid)*atotz
      if (abs(damage).gt.toldamage) then
        iflag=0
      else
        iflag=1
      endif
      return
      end
c
c
c
c
      function lwontinvert(i,node,invmpary,isurvivor,nodhyb,ieltary,nelt
     &   ,itettyp,iparent,itet,itetoff,xic,yic,zic,vtol,synthx,synthy
     &   ,synthz,lsomereversed,lcheckaxy,epsilona,lcheckroughness
     &   ,tolroughness,gsynth,dirtol)
 
      implicit none
 
      include 'local_element.h'
      include 'consts.h'
 
      integer node,isurvivor,nodhyb(*),ieltary(*),nelt,itettyp(*),
     &   iparent(*),itet(*),itetoff(*),j,ii,lochybnod,ihyb,ityp,
     &   k,nbr,itemp(maxnen),i1,i2,i3,i4,i,invmpary(*),mpsurvivor
     &   ,nodadj1,nodadj2,nodadj,nodk,mpnodk
      real*8 xic(*),yic(*),zic(*),vtol,cros(3),vol,dirtol,dir,
     &   dot,synthx,synthy,synthz,tolroughness,gsynth(3,*),dx,dy,dz
     &   ,rough1,rough2,epsilona,projarea
      logical lwontinvert,lsomereversed,lcheckaxy,lcheckroughness
 
      real*8 epsilonaspect
 
      include 'statementfunctions.h'
 
      lwontinvert=.true.
 
      mpsurvivor=invmpary(isurvivor)
      nodadj1=0
      nodadj2=0
      do 10 j=1,nelt
         ii=1+(nodhyb(j)-1)/maxnen
         lochybnod=nodhyb(j)-maxnen*(ii-1)
         ihyb=ieltary(ii)
         ityp=itettyp(ihyb)
 
         do k=1,nelmnen(ityp)
            nbr=iparent(itet(k+itetoff(ihyb)))
            if (nbr.eq.isurvivor) then
               if (ityp.eq.ifelmtri) then
                  nodadj=iparent(itet(1+itetoff(ihyb)))+iparent(itet(2
     &               +itetoff(ihyb)))+iparent(itet(3+itetoff(ihyb)))
     &               -isurvivor-node
                  if (nodadj1.eq.0) then
                     nodadj1=nodadj
                  elseif (nodadj2.eq.0) then
                     nodadj2=nodadj
                  endif
               endif
               goto 10
            else
               if (nbr.eq.node) then
                  itemp(k)=isurvivor
               else
                  itemp(k)=nbr
               endif
            endif
         enddo
 
         if(ityp.eq.ifelmtet) then
            i1=itemp(1)
            i2=itemp(2)
            i3=itemp(3)
            i4=itemp(4)
            epsilonaspect=1.d-30
c$$$            call aratio_tet(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
c$$$     &         ,zic(i2),xic(i3),yic(i3),zic(i3),xic(i4),yic(i4),zic(i4)
c$$$     &         ,dar,epsilonaspect)
            dir=dirtet(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &         ,zic(i2),xic(i3),yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            vol=dvol(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2),zic(i2)
     &         ,xic(i3),yic(i3),zic(i3),xic(i4),yic(i4),zic(i4))
            if (vol.le.vtol) then
               lwontinvert=.false.
               return
            endif
            if (dir.le.dirtol) then
               lwontinvert=.false.
               return
            endif
         elseif(ityp.eq.ifelmtri) then
            i1=itemp(1)
            i2=itemp(2)
            i3=itemp(3)
c$$$            call aratio_tri(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
c$$$     &         ,zic(i2),xic(i3),yic(i3),zic(i3),dar)
            if (lcheckaxy) then
               dir=dszirtri(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &            ,zic(i2),xic(i3),yic(i3),zic(i3))
               if (dir.le.dirtol) then
                  lwontinvert=.false.
                  return
               endif
               projarea=0.5d0*dcrosz(xic(i1),yic(i1),zic(i1),xic(i2)
     &            ,yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
               if (projarea.le.epsilona) then
                  lwontinvert=.false.
                  return
               endif
c$$$               if (.not.lsomereversed) then
c$$$                  cros(1)=dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
c$$$     &               ,zic(i2),xic(i3),yic(i3),zic(i3))
c$$$                  cros(2)=dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
c$$$     &               ,zic(i2),xic(i3),yic(i3),zic(i3))
c$$$                  cros(3)=dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
c$$$     &               ,zic(i2),xic(i3),yic(i3),zic(i3))
c$$$                  dot=synthx*cros(1)+synthy*cros(2)+synthz*cros(3)
c$$$                  if(dot.le.0.0) then
c$$$                     lwontinvert=.false.
c$$$                     return
c$$$                  endif
c$$$               endif
            else
               dir=dirtri(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &            ,zic(i2),xic(i3),yic(i3),zic(i3))
               if (dir.le.dirtol) then
                  lwontinvert=.false.
                  return
               endif
               if (.not.lsomereversed) then
                  cros(1)=dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &               ,zic(i2),xic(i3),yic(i3),zic(i3))
                  cros(2)=dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &               ,zic(i2),xic(i3),yic(i3),zic(i3))
                  cros(3)=dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),yic(i2)
     &               ,zic(i2),xic(i3),yic(i3),zic(i3))
                  dot=synthx*cros(1)+synthy*cros(2)+synthz*cros(3)
                  if(dot.le.0.0) then
                     lwontinvert=.false.
                     return
                  endif
               endif
            endif
 
            if (lcheckroughness) then
               do k=1,3
                  nodk=iparent(itet(k+itetoff(ihyb)))
                  mpnodk=invmpary(nodk)
                  if ((nodk.ne.nodadj1).and.(nodk.ne.nodadj2).and
     &               .(nodk.ne.isurvivor)) then
                     dx=xic(isurvivor)-xic(nodk)
                     dy=yic(isurvivor)-yic(nodk)
                     dz=zic(isurvivor)-zic(nodk)
                     rough1=abs(gsynth(1,mpsurvivor)*dx
     &                  +gsynth(2,mpsurvivor)*dy+gsynth(3,mpsurvivor)*dz
     &                  )
                     rough2=abs(gsynth(1,mpnodk)*dx
     &                  +gsynth(2,mpnodk)*dy+gsynth(3,mpnodk)*dz)
                     if (max(rough1,rough2).ge.tolroughness) then
                        lwontinvert=.false.
                        return
                     endif
                  endif
               enddo
            endif
         endif
  10  continue
      return
 
      end
c
c
c
      function damage_est_2(node,isurvivor,nodhyb,ieltary,nelt,itettyp,
     &   iparent,itet,itetoff,jtet,jtetoff,mbndry,xic,yic,zic
     &   ,lignoremats)
 
c.... DAMAGE_EST_2 is computed as follows.  Let DX be the vector from
c.... the node to be deleted to the 'survivor' node.  Deform the grid
c.... by dragging the deleted node to the survivor node.  Compute the
c.... normal N to this resultant grid patch.  Now imagine that we were
c.... to reverse the process and move the grid a distance -DX from
c.... the survivor back to the initial position.  Then DAMAGE_EST_2
c.... is | DX (dot) N |.  This damage estimate allows detection
c.... of good merge directions for graphs that resemble the roof
c.... of a house  (i.e. graphs that have a one-dimensional degeneracy
c.... along the cusp at the top).
 
      implicit none
 
      include 'local_element.h'
      include 'consts.h'
 
      logical lignoremats
      integer node,isurvivor,nodhyb(*),ieltary(*),nelt,itettyp(*),
     &   iparent(*),itet(*),itetoff(*),j,ii,lochybnod,ihyb,ityp,
     &   itemp(maxnee1),i1,i2,i3,k,jtet(*),jtetoff(*),mbndry,
     &   kmatch,k1,nodek1
      real*8 xic(*),yic(*),zic(*),area(3),dx(3),areat
      real*8 damage_est_2
 
      include "statementfunctions.h"
 
      damage_est_2=0.
 
      dx(1)=xic(isurvivor)-xic(node)
      dx(2)=yic(isurvivor)-yic(node)
      dx(3)=zic(isurvivor)-zic(node)
 
      do 30 j=1,nelt
         ii=1+(nodhyb(j)-1)/maxnen
         lochybnod=nodhyb(j)-maxnen*(ii-1)
         ihyb=ieltary(ii)
         ityp=itettyp(ihyb)
         if(ityp.eq.ifelmtri) then
            do k1=1,nelmnen(ityp)
               nodek1=iparent(itet(k1+itetoff(ihyb)))
               if (nodek1.eq.isurvivor) then
                  goto 30
               else
                  if (nodek1.eq.node) then
                     itemp(k1)=isurvivor
                  else
                     itemp(k1)=nodek1
                  endif
               endif
            enddo
            i1=itemp(1)
            i2=itemp(2)
            i3=itemp(3)
            area(1)=dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),
     &         yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
            area(2)=dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),
     &         yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
            area(3)=dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),
     &         yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
            areat=sqrt(area(1)**2+area(2)**2+area(3)**2)
            area(1)=area(1)/safe(areat)
            area(2)=area(2)/safe(areat)
            area(3)=area(3)/safe(areat)
            damage_est_2=max(damage_est_2,abs(
     &         area(1)*dx(1)+area(2)*dx(2)+area(3)*dx(3)))
 
         elseif(ityp.eq.ifelmtet) then
 
           do 20 k=1,nelmnef(ityp)
             if ((lignoremats.and.(jtet(k+jtetoff(ihyb)).eq
     &           .mbndry)).or.((.not.lignoremats).and.(jtet(k
     &           +jtetoff(ihyb)).ge.mbndry))) then
               kmatch=0
               do k1=1,ielmface0(k,ityp)
                  if (lochybnod.eq.ielmface1(k1,k,ityp)) then
                     kmatch=k1
                  endif
               enddo
               if (kmatch.ne.0) then
                  do k1=1,ielmface0(k,ityp)
                     nodek1=iparent(itet(ielmface1(k1,k,ityp)+
     &                  itetoff(ihyb)))
                     if (nodek1.eq.isurvivor) then
                        goto 20
                     else
                        if (nodek1.eq.node) then
                           itemp(k1)=isurvivor
                        else
                           itemp(k1)=nodek1
                        endif
                     endif
                  enddo
                  i1=itemp(1)
                  i2=itemp(2)
                  i3=itemp(3)
                  area(1)=dcrosx(xic(i1),yic(i1),zic(i1),xic(i2),
     &               yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
                  area(2)=dcrosy(xic(i1),yic(i1),zic(i1),xic(i2),
     &               yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
                  area(3)=dcrosz(xic(i1),yic(i1),zic(i1),xic(i2),
     &               yic(i2),zic(i2),xic(i3),yic(i3),zic(i3))
                  areat=sqrt(area(1)**2+area(2)**2+area(3)**2)
                  area(1)=area(1)/safe(areat)
                  area(2)=area(2)/safe(areat)
                  area(3)=area(3)/safe(areat)
                  damage_est_2=max(damage_est_2,abs(
     &               area(1)*dx(1)+area(2)*dx(2)+area(3)*dx(3)))
               endif
             endif
 20        continue
         endif
 30   continue
c
      return
 
      end
c
c
c**********************************************************************
c
      function damage_est_discrete(node,xdis,ydis,zdis,nodhyb,
     &   ieltary,nelt,itettyp,iparent,itet,itetoff,jtet,jtetoff,mbndry,
     &   xic,yic,zic,
     &   xics,yics,zics,itets,itfound,linkt,sbox,eps,distpossleaf)
 
c.... node is the node to be moved. xdis,ydis,zdis are the coordinate
c.... of the new proposed location.
c.... on the coarse grid find the patch of triangles containing node
c.... get the distance to the fine grid of the patch by taking the
c.... max of the distances of the centers of the triangles to the fine
c.... grid.  Now do the same for the new patch. return the difference
c.... between the new and old distance if it is a positive difference
 
      implicit none
 
      include 'local_element.h'
      include 'consts.h'
 
      integer node,nodhyb(*),ieltary(*),nelt,itettyp(*),
     &   iparent(*),itet(*),itetoff(*),j,ii,lochybnod,ihyb,ityp,
     &   k,jtet(*),jtetoff(*),mbndry,
     &   kmatch,k1,nodek1
      real*8 xic(*),yic(*),zic(*),xdis,ydis,zdis
      real*8 x(3),y(3),z(3)
      real*8 xics(*),yics(*),zics(*)
      real*8 damage_est_discrete
      integer itets(*),itfound(*),linkt(*),ierr,mtfound
      real*8 distpossleaf(*),sbox(*),eps,distmaxold,distmaxnew,
     *  distshold,distshnew,xs,ys,zs,xq,yq,zq
 
      include "statementfunctions.h"
c
      damage_est_discrete=zero
      distmaxnew=-1.d50
      distmaxold=-1.d50
c
c.... loop through elements surrounding node and find the
c.... boundary faces and centers of boundary faces.
 
      do  j=1,nelt
         ii=1+(nodhyb(j)-1)/maxnen
         lochybnod=nodhyb(j)-maxnen*(ii-1)
         ihyb=ieltary(ii)
         ityp=itettyp(ihyb)
 
         do  k=1,nelmnef(ityp)
            if (jtet(k+jtetoff(ihyb)).eq.mbndry) then
               kmatch=0
               do k1=1,ielmface0(k,ityp)
                  if (lochybnod.eq.ielmface1(k1,k,ityp)) then
                     kmatch=k1
                  endif
               enddo
               if (kmatch.ne.0) then
                  xq=zero
                  yq=zero
                  zq=zero
 
                  do k1=1,ielmface0(k,ityp)
                     xq=xq+xic(itet(ielmface1(k1,k,ityp)+
     &                  itetoff(ihyb)))
                     yq=yq+yic(itet(ielmface1(k1,k,ityp)+
     &                  itetoff(ihyb)))
                     zq=zq+zic(itet(ielmface1(k1,k,ityp)+
     &                  itetoff(ihyb)))
                  enddo
                  xq=xq/ielmface0(k,ityp)
                  yq=yq/ielmface0(k,ityp)
                  zq=zq/ielmface0(k,ityp)
                  call distance_to_sheet_lg(xq,yq,zq,xics,yics,zics,
     &             itets, xs,ys,zs, linkt,sbox,eps,distpossleaf,
     *             mtfound,itfound, distshold,ierr)
                  if(distshold.gt.distmaxold.and.ierr.eq.0) then
                    distmaxold=distshold
                  endif
               endif
            endif
         enddo
      enddo
c
c.... loop through elements surrounding node and find the
c.... boundary faces and replace node with new position
c.... find max distance as before
c....
      do  j=1,nelt
         ii=1+(nodhyb(j)-1)/maxnen
         lochybnod=nodhyb(j)-maxnen*(ii-1)
         ihyb=ieltary(ii)
         ityp=itettyp(ihyb)
 
         do  k=1,nelmnef(ityp)
            if (jtet(k+jtetoff(ihyb)).eq.mbndry) then
               kmatch=0
               do k1=1,ielmface0(k,ityp)
                  if (lochybnod.eq.ielmface1(k1,k,ityp)) then
                     kmatch=k1
                  endif
               enddo
               if (kmatch.ne.0) then
                  xq=zero
                  yq=zero
                  zq=zero
                  do k1=1,ielmface0(k,ityp)
                     nodek1=iparent(itet(ielmface1(k1,k,ityp)+
     &                  itetoff(ihyb)))
                     if (nodek1.eq.node) then
                        x(k1)=xdis
                        y(k1)=ydis
                        z(k1)=zdis
                     else
                        x(k1)=xic(nodek1)
                        y(k1)=yic(nodek1)
                        z(k1)=zic(nodek1)
                     endif
                     xq=xq+x(k1)
                     yq=yq+y(k1)
                     zq=zq+z(k1)
                  enddo
                  xq=xq/ielmface0(k,ityp)
                  yq=yq/ielmface0(k,ityp)
                  zq=zq/ielmface0(k,ityp)
                  call distance_to_sheet_lg(xq,yq,zq,xics,yics,zics,
     &              itets,xs,ys,zs, linkt,sbox,eps,distpossleaf,
     *              mtfound,itfound,distshnew,ierr)
                  if(distshnew.gt.distmaxnew.and.ierr.eq.0) then
                    distmaxnew=distshnew
                  endif
               endif
            endif
         enddo
      enddo
      damage_est_discrete=distmaxnew-distmaxold
c
      return
 
      end
c**********************************************************************
c
      function damage_est_discrete_old(node,xdis,ydis,zdis,nodhyb,
     &   ieltary,nelt,itettyp,iparent,itet,itetoff,jtet,jtetoff,mbndry,
     &   xic,yic,zic)
 
c.... DAMAGE_EST_dicrete is computed as follows.  Let DX be the vector
c.... from the node position to be moved to the position of the
c.... selected discrete node.  Deform the grid
c.... by dragging the selected node to the discrete node.  For each
c.... element that contained the node to be moved, replace the
c.... node to be moved by the candidate discrete node.  Then for
c.... each surface face that contained the original node calculate
c.... the area normal, N. Calculate | DX (dot) N | for each of these
c.... faces.  Take the damage to be the max over all these faces.
c.... This damage estimate allows detection
c.... of good merge directions for graphs that resemble the roof
c.... of a house  (i.e. graphs that have a one-dimensional degeneracy
c.... along the cusp at the top).
 
      implicit none
 
      include 'local_element.h'
      include 'consts.h'
 
      integer node,nodhyb(*),ieltary(*),nelt,itettyp(*),
     &   iparent(*),itet(*),itetoff(*),j,ii,lochybnod,ihyb,ityp,
     &   k,jtet(*),jtetoff(*),mbndry,
     &   kmatch,k1,nodek1
      real*8 xic(*),yic(*),zic(*),area(3),dx(3),areat,xdis,ydis,zdis
      real*8 x(3),y(3),z(3)
      real*8 damage_est_discrete_old
 
      include "statementfunctions.h"
 
 
      damage_est_discrete_old=zero
 
      dx(1)=xdis-xic(node)
      dx(2)=ydis-yic(node)
      dx(3)=zdis-zic(node)
 
      do 30 j=1,nelt
         ii=1+(nodhyb(j)-1)/maxnen
         lochybnod=nodhyb(j)-maxnen*(ii-1)
         ihyb=ieltary(ii)
         ityp=itettyp(ihyb)
 
         do 20 k=1,nelmnef(ityp)
            if (jtet(k+jtetoff(ihyb)).eq.mbndry) then
               kmatch=0
               do k1=1,ielmface0(k,ityp)
                  if (lochybnod.eq.ielmface1(k1,k,ityp)) then
                     kmatch=k1
                  endif
               enddo
               if (kmatch.ne.0) then
                  do k1=1,ielmface0(k,ityp)
                     nodek1=iparent(itet(ielmface1(k1,k,ityp)+
     &                  itetoff(ihyb)))
                     if (nodek1.eq.node) then
                        x(k1)=xdis
                        y(k1)=ydis
                        z(k1)=zdis
                     else
                        x(k1)=xic(nodek1)
                        y(k1)=yic(nodek1)
                        z(k1)=zic(nodek1)
                     endif
                  enddo
                  area(1)=dcrosx(x(1),y(1),z(1),x(2),
     &               y(2),z(2),x(3),y(3),z(3))
                  area(2)=dcrosy(x(1),y(1),z(1),x(2),
     &               y(2),z(2),x(3),y(3),z(3))
                  area(3)=dcrosz(x(1),y(1),z(1),x(2),
     &               y(2),z(2),x(3),y(3),z(3))
                  areat=sqrt(area(1)**2+area(2)**2+area(3)**2)
                  area(1)=area(1)/safe(areat)
                  area(2)=area(2)/safe(areat)
                  area(3)=area(3)/safe(areat)
 
                  damage_est_discrete_old=max(damage_est_discrete_old
     *               ,abs(
     &               area(1)*dx(1)+area(2)*dx(2)+area(3)*dx(3)))
               endif
            endif
 20      continue
 30   continue
c
      return
 
      end
 
 
 
 
 
