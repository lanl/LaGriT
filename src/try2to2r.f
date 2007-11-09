*dk,try2to2r
      subroutine try2to2r(nface,n2to2,
     *                    npoints,ntets,toldamage)
       implicit real*8 (a-h,o-z)
C ######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE ATTEMPTS TO FLIP CONNECTIONS ON REFLECTING
C        BOUNDARY PLANES.
C
C     INPUT ARGUMENTS -
C
C        nface   - THE NUMBER OF FACES IN THE "IRCLST" ARRAY
C
C     OUTPUT ARGUMENTS -
C
C        n2to2   - THE NUMBER OF BOUNDARY FLIPS PERFORMED
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/try2to2r.f_a  $
CPVCS    
CPVCS       Rev 1.11   05 Jan 2001 12:58:18   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.10   Thu Oct 16 14:31:14 1997   kuprat
CPVCS    Added TOLDAMAGE feature where flips are rejected if their
CPVCS    damage exceeds TOLDAMAGE.
CPVCS
CPVCS       Rev 1.9   Mon Apr 14 17:05:04 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.8   Mon Jun 03 15:12:12 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.7   Fri May 17 09:16:48 1996   dcg
CPVCS    add mmgetblk for icontab
CPVCS
CPVCS       Rev 1.6   Mon May 06 12:26:42 1996   dcg
CPVCS    use itsttp to test for point types
CPVCS
CPVCS       Rev 1.5   Tue Apr 16 14:39:30 1996   dcg
CPVCS     replace pointer ipitets with array itets
CPVCS
CPVCS       Rev 1.4   02/10/95 08:27:52   het
CPVCS    Change the nfaces variable to nface because of cmo.h
CPVCS
CPVCS       Rev 1.3   12/02/94 15:07:08   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:49:40   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:55:26   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:42   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "cmerge.h"
      include "consts.h"
C
C ######################################################################
C
      dimension id(8),jd(8),ktmp(lenblk),itets(301)
C
      pointer (ipicontab,icontab)
      integer icontab(50,1000000)
      dimension ifindx(4,4)
      logical itsttp
C
C ######################################################################
C
C
      crosx1(i,j,k)=(yic(j)-yic(i))*(zic(k)-zic(i))-
     *              (yic(k)-yic(i))*(zic(j)-zic(i))
      crosy1(i,j,k)=(xic(k)-xic(i))*(zic(j)-zic(i))-
     *              (xic(j)-xic(i))*(zic(k)-zic(i))
      crosz1(i,j,k)=(xic(j)-xic(i))*(yic(k)-yic(i))-
     *              (xic(k)-xic(i))*(yic(j)-yic(i))
      volume(i1,i2,i3,i4)=(xic(i4)-xic(i1))*crosx1(i1,i2,i3)+
     *                    (yic(i4)-yic(i1))*crosy1(i1,i2,i3)+
     *                    (zic(i4)-zic(i1))*crosz1(i1,i2,i3)
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,lenicr1,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,lenzic,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,lenitetclr,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,lenjtet,icmotype,ierror)
C
      endif
      call cmo_get_info('icontab',cmo,ipicontab,length,icmotype,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_get_info')
C
C     ******************************************************************
C
C
c     ipitets=loc(itets)
C
      itwo=2
      xst=1.0e-11
      n2to2=0
C
C     ******************************************************************
C
C     ATTEMPT RECONNECTIONS ACROSS THE FACES
C
      do 100 i=1,nface
         iface=irclst(i)
C**JM    if(ijtet1(iface).le.0) goto 100
         it=0.25*dble(iface)+0.9
         if(kfix(1,it).ge.1.or.itet(1,it).le.0) goto 100
C
C        _______________________________________________________________
C
C        SKIP FACE IF IT IS A BOUNDARY FACE.
C
         ip2=jtet1(iface)
         if(ip2.ge.mbndry) goto 100
         i5=itet1(ip2)
         if(jtet1(ip2).ge.mbndry) goto 100
         i4=itet1(jtet1(ip2))
C
C        _______________________________________________________________
C
C        SKIP FACE UNLESS POINTS i4 AND i5 ARE ON REFLECTING BOUNDARY.
C
         icrnbr4=icr1(i4)
         icrnbr5=icr1(i5)
         if(icrnbr4.eq.0.or.icrnbr5.eq.0) goto 100
C
C        _______________________________________________________________
C
C        SKIP FACE IF EITHER TETRAHEDRON IS INVERTED.
C
         call face(iface,ip1,it,n2,n1,n3)
C**JM    if(kfix(1,it).ge.1) goto 100
         volit=volume(itet(1,it),itet(2,it),itet(3,it),itet(4,it))
         if(volit.le.0) goto 100
         it2=0.25*dble(jtet1(iface))+0.9
         volit2=volume(itet(1,it2),itet(2,it2),itet(3,it2),itet(4,it2))
         if(volit2.le.0) goto 100
C
C        _______________________________________________________________
C
C        FIND OTHER TWO POINTS ON EXTERNAL BOUNDARY AND TEST FOR
C        FURTHER GEOMETRIC RESTRICTIONS.
C
         do 50 m=1,3
            idup=0
            if(m.eq.1) then
               i1=n1
               i2=n2
               i3=n3
            elseif(m.eq.2) then
               i1=n2
               i2=n3
               i3=n1
            elseif(m.eq.3) then
               i1=n3
               i2=n1
               i3=n2
            endif
C
C           ............................................................
C
C           DO NOT BREAK AN EXTERNAL INTERFACE CONNECTION.
C
Cdcg        if(itp1(i1).eq.ifitpinb.and.itp1(i2).eq.ifitpinb) goto 50
            if (itsttp('intrface',itp1(i1)).and.
     *          itsttp('intrface',itp1(i2)))  go to 50
C
C           ............................................................
C
C           ENSURE THAT FACES OPPOSITE i3 ARE EXTERNAL BOUNDARY FACES.
C
            ipos1=4
            if(itet(1,it).eq.i3) then
               ipos1=1
            elseif(itet(2,it).eq.i3) then
               ipos1=2
            elseif(itet(3,it).eq.i3) then
               ipos1=3
            endif
            if(jtet(ipos1,it).ne.mbndry) goto 50
            ipos2=4
            if(itet(1,it2).eq.i3) then
               ipos2=1
            elseif(itet(2,it2).eq.i3) then
               ipos2=2
            elseif(itet(3,it2).eq.i3) then
               ipos2=3
            endif
            if(jtet(ipos2,it2).ne.mbndry) goto 50
C
C           ............................................................
C
C           ENSURE THAT POINTS i1, i2, i4, AND i5 LIE IN SAME
C           REFLECTING BOUNDARY PLANE.
C
            icrnbr1=icr1(i1)
            icrnbr2=icr1(i2)
            if(icrnbr1.eq.0.or.icrnbr2.eq.0) then
               goto 50
            else
               if(icrnbr2.ne.icrnbr1.or.icrnbr4.ne.icrnbr1.or.
     *            icrnbr5.ne.icrnbr1) then
                  do 200 j1=3,5
                     irb1=icontab(j1,icrnbr1)
                     do 210 j2=3,5
                        irb2=icontab(j2,icrnbr2)
                        if(irb2.eq.irb1) then
                           do 220 j4=3,5
                              irb4=icontab(j4,icrnbr4)
                              if(irb4.eq.irb1) then
                                 do 230 j5=3,5
                                    irb5=icontab(j5,icrnbr5)
                                    if(irb5.eq.irb1) goto 240
  230                            continue
                              endif
  220                      continue
                        endif
  210                continue
  200             continue
                  goto 50
  240             continue
                  if(irb1.eq.0) goto 50
               endif
            endif
 
c.... Check that the 'damage' of performing a flip is less than
c.... TOLDAMAGE.  The damage will be nonzero if the points i1, i2,
c.... i4, i5 do not all lie in the same plane.  The damage is defined
c.... in a similar fashion as in the subroutine AGD3D.
 
c.... Form aggregate normal:  Area-weighted normal formed from the
c.... two boundary triangles [triangle (i1,i2,i4) and triangle (i2,i1,i5)].
 
      a124x=crosx1(i1,i2,i4)
      a124y=crosy1(i1,i2,i4)
      a124z=crosz1(i1,i2,i4)
      a215x=crosx1(i2,i1,i5)
      a215y=crosy1(i2,i1,i5)
      a215z=crosz1(i2,i1,i5)
      atotx=a124x+a215x
      atoty=a124y+a215y
      atotz=a124z+a215z
      atot=sqrt(atotx**2+atoty**2+atotz**2)
      atotx=atotx/atot
      atoty=atoty/atot
      atotz=atotz/atot
 
c.... Calculate midpoint of edge (i1,i2)
 
      xmid=half*(xic(i1)+xic(i2))
      ymid=half*(yic(i1)+yic(i2))
      zmid=half*(zic(i1)+zic(i2))
 
c.... If both boundary triangle normals are in the same direction as
c.... aggregate normal, damage is defined to be separation between two
c.... planes (normal to agg. normal) that sandwich the points i1, i2,
c.... i3, i4.  If one of the boundary triangle normals points contrary
c.... to the agg. normal, damage is defined as minimum of 'merge
c.... distances' from the midpoint of edge (i1,i2) to the points i4, i5.
 
      dot124=a124x*atotx+a124y*atoty+a124z*atotz
      dot215=a215x*atotx+a215y*atoty+a215z*atotz
 
      if (dot124.gt.zero.and.dot215.gt.zero) then
 
         dot1=(xic(i1)-xmid)*atotx+(yic(i1)-ymid)*atoty+
     &      (zic(i1)-zmid)*atotz
         dot2=(xic(i2)-xmid)*atotx+(yic(i2)-ymid)*atoty+
     &      (zic(i2)-zmid)*atotz
         dot4=(xic(i4)-xmid)*atotx+(yic(i4)-ymid)*atoty+
     &      (zic(i4)-zmid)*atotz
         dot5=(xic(i5)-xmid)*atotx+(yic(i5)-ymid)*atoty+
     &      (zic(i5)-zmid)*atotz
         dotmin=min(dot1,dot2,dot4,dot5)
         dotmax=max(dot1,dot2,dot4,dot5)
         damage=dotmax-dotmin
         if (damage.gt.toldamage) goto 50
 
      else
 
         dist4=sqrt((xic(i4)-xmid)**2+(yic(i4)-ymid)**2+
     &      (zic(i4)-zmid)**2)
         dist5=sqrt((xic(i5)-xmid)**2+(yic(i5)-ymid)**2+
     &      (zic(i5)-zmid)**2)
         damage=min(dist4,dist5)
         if (damage.gt.toldamage) goto 50
 
      endif
C
C           ............................................................
C
C           PREVENT THIN TETRAHEDRA FROM FORMING.
C
            xn=xic(i5)-xic(i4)
            yn=yic(i5)-yic(i4)
            zn=zic(i5)-zic(i4)
            sn=sqrt(xn*xn+yn*yn+zn*zn)
            xnorm1=crosx1(i4,i1,i3)
            ynorm1=crosy1(i4,i1,i3)
            znorm1=crosz1(i4,i1,i3)
            snorm1=sqrt(xnorm1**2+ynorm1**2+znorm1**2)
            xnorm2=crosx1(i4,i3,i2)
            ynorm2=crosy1(i4,i3,i2)
            znorm2=crosz1(i4,i3,i2)
            snorm2=sqrt(xnorm2**2+ynorm2**2+znorm2**2)
            dot1=xn*xnorm1+yn*ynorm1+zn*znorm1
            dot2=xn*xnorm2+yn*ynorm2+zn*znorm2
            iflag=0
            if(dot1.ge.xst*sn*snorm1.and.
     *         dot2.ge.xst*sn*snorm2) iflag=1
            if(iflag.eq.0) goto 50
C
C           ............................................................
C
C           PERFORM THE FLIP
C
            call find2to2(it,it2,i1,i2,i3,i4,i5,id(1),jd(1),
     *                    npoints,ntets)
            call flip2to2(it,it2,id(1),jd(1),
     *                    npoints,ntets)
            n2to2=n2to2+1
            kfix(1,it)=it
            kfix(1,it2)=it2
C
C           ...........................................................
C
C           ATTEMPT A 2-TO-0 FLIP.
C
            itets(1)=it
            itets(2)=it2
            call try2to0(itets,itwo,nflips,itx,
     *                   npoints,ntets)
            n2to0=n2to0+nflips
C
C           ...........................................................
C
            goto 100
 50      continue
C
C        _______________________________________________________________
C
 100  continue
C
C     ******************************************************************
C
C     SET UP THE USUAL CFT IMMUNE STATEMENT 9999 IN CASE DDT IS NEEDED.
C
      goto 9999
 9999 continue
C
C     ******************************************************************
C
      return
      end
