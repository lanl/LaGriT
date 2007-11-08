      subroutine interpolate_hessian(node,nodhyb,nodhyboff,
     &   ieltary,invmpary,itettyp,itet,itetoff,x,y,z,dx,dy,dz,hxx,hxy
     &   ,hxz,hyy,hyz,hzz)
C #####################################################################
C
C     PURPOSE -
C
C        INTERPOLATE_HESSIAN considers the proposed point movement of
C     NODE by DX,DY,DZ, and modifies the Hessian (HXX,HXY,HXZ,HYY,HYZ
C     ,HZZ) to reflect this change.
C
C     INPUT ARGUMENTS -
C
C        NODE  -  The node at center of polyhedron.
C        NODHYB-  Node-element relation. 
C        NODHYBOFF-  Node-element relation offsets.
C        IELTARY- Array of relevant elements.
C        INVMPARY- Inverse mapping of that defined by mass point array
C                  MPARY.
C        ITETTYP-  Array of element types.
C        ITET-     Element-node relation.
C        ITETOFF-  Offsets for Element-node relation.
C        X,Y,Z -  The xic,yic,zic arrays for the mesh.
C        DX,DY,-  Proposed changes to add to X(NODE), Y(NODE), Z(NODE).
C        DZ
C        HXX,HXY,HXZ, - Six components of Hessian (element arrays)
C        HYY,HYZ,HZZ
C
C     OUTPUT ARGUMENTS -
C
C        HXX,HXY,HXZ, - Six components of Hessian (element arrays)
C        HYY,HYZ,HZZ
C
C     CHANGE HISTORY -
C     $Log:   /pvcs.config/t3d/src/interpolate_hessian.f_a  $
CPVCS    
CPVCS       Rev 1.3   08 Feb 2006 14:38:12   dcg
CPVCS     "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.2   20 Feb 2002 16:59:58   kuprat

CPVCS    Use MAXEL from smooth.h

CPVCS    

CPVCS       Rev 1.1   07 Jan 2002 20:27:30   kuprat

CPVCS    Corrected bug with loop index.

CPVCS    

CPVCS       Rev 1.0   21 Dec 2001 18:08:28   kuprat

CPVCS    Initial revision.

C
C ######################################################################
c 
c Given the set of elements incident upon NODE we divide them
c into two sets:  L, the set of elements which will shrink
c after NODE is moved by (DX,DY,DZ), and G, the set of elements
c which will grow.  Say the I'th element changes from VOLBEFORE(I) to
c VOLAFTER(I).  For all elements in L, we say the element-based 
c field H (i.e. a Hessian field) does not change.  That is,
c
c    H_new(I):= H(I).
c
c This makes sense because for elements in L, NODE is moving
c INTO those elements and so those H values should be correct.
c (Of course, this is a rough, intuitive argument.)  Now we strive
c to conserve HdV.  We sum up the entire volume lost by elements in L,
c VOLLOSS and sum up the entire amount of HdV lost by elements in L,
c HLOSS=sum over L of H(I)*(VBEFORE(I)-VAFTER(I)).
c Then we define the average H in this lost volume to be
c HAVE:=HLOSS/VOLLOSS.
c Now for all elements I in G, we define
c
c    H_new(I):= (VOLBEFORE(I)*H(I)+dV(I)*HAVE)/VOLAFTER(I).
c
c It is easy to see that this conserves HdV over the whole polyhedron.
c
C ######################################################################
      implicit none

      include 'consts.h'
      include 'local_element.h'
      include 'smooth.h'

      integer node,nodhyb(*),nodhyboff(*),ieltary(*),invmpary(*)
     &   ,itettyp(*),itet(*),itetoff(*)
      real*8 x(*),y(*),z(*),dx,dy,dz,hxx(*),hxy(*),hxz(*),hyy(*),hyz(*)
     &   ,hzz(*)
      
      integer i,mpk,ii,lochybnod,ihyb,ityp,j,ic
      real*8 xv(maxel),yv(maxel),zv(maxel),xva(maxel),yva(maxel)
     &   ,zva(maxel),volbefore(maxel),volafter(maxel),volloss,hxxloss
     &   ,hxyloss,hxzloss,hyyloss,hyzloss,hzzloss,hxxave,hxyave
     &   ,hxzave,hyyave,hyzave,hzzave
  
      include 'statementfunctions.h' 

c...  Loop over all (possibly hybrid) elements in polyhedron
            
      mpk=invmpary(node)
      if ((nodhyboff(mpk+1)-nodhyboff(mpk)).gt.maxel) then
         print*,'Interpolate_hessian: too many incident elements!'
         print*,'Increase MAXEL.'
         print*,'This should not have happened!  :-('
         stop
      endif

c.... Loop thru elements incident on NODE.  We will compute before/after
c.... volumes of these elements.  For all elements that get smaller,
c     sum up total lost volume, and total lost HdV.

      volloss=0.d0
      hxxloss=0.d0
      hxyloss=0.d0
      hxzloss=0.d0
      hyyloss=0.d0
      hyzloss=0.d0
      hzzloss=0.d0
      do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
         ic=i-nodhyboff(mpk)
         ii=1+(nodhyb(i)-1)/maxnen
         lochybnod=nodhyb(i)-maxnen*(ii-1)
         ihyb=ieltary(ii)
         ityp=itettyp(ihyb)

         do j=1,nelmnen(ityp)
            xv(j)=x(itet(itetoff(ihyb)+j))
            yv(j)=y(itet(itetoff(ihyb)+j))
            zv(j)=z(itet(itetoff(ihyb)+j))
            if (j.eq.lochybnod) then
               xva(j)=xv(j)+dx
               yva(j)=yv(j)+dy
               zva(j)=zv(j)+dz
            else
               xva(j)=xv(j)
               yva(j)=yv(j)
               zva(j)=zv(j)
            endif        
         enddo
         if (itettyp(ihyb).eq.ifelmtet) then
            volbefore(ic)=dvol(xv(1),yv(1),zv(1),xv(2),yv(2),zv(2),xv(3)
     &         ,yv(3),zv(3),xv(4),yv(4),zv(4))
            volafter(ic)=dvol(xva(1),yva(1),zva(1),xva(2),yva(2),zva(2)
     &         ,xva(3),yva(3),zva(3),xva(4),yva(4),zva(4))
         else
            call volume_element(ityp,xv,yv,zv,volbefore(ic))
            call volume_element(ityp,xva,yva,zva,volafter(ic))
         endif
         if (volafter(ic).lt.volbefore(ic)) then
            volloss=volloss+volbefore(ic)-volafter(ic)
            hxxloss=hxxloss+hxx(ihyb)*(volbefore(ic)-volafter(ic))
            hxyloss=hxyloss+hxy(ihyb)*(volbefore(ic)-volafter(ic))
            hxzloss=hxzloss+hxz(ihyb)*(volbefore(ic)-volafter(ic))
            hyyloss=hyyloss+hyy(ihyb)*(volbefore(ic)-volafter(ic))
            hyzloss=hyzloss+hyz(ihyb)*(volbefore(ic)-volafter(ic))
            hzzloss=hzzloss+hzz(ihyb)*(volbefore(ic)-volafter(ic))
         endif
      enddo

c.... Compute average H in 'lost volume' region.

      volloss=safe(volloss)
      hxxave=hxxloss/volloss
      hxyave=hxyloss/volloss
      hxzave=hxzloss/volloss
      hyyave=hyyloss/volloss
      hyzave=hyzloss/volloss
      hzzave=hzzloss/volloss

c.... Loop over elements and adjust H values for elements that 
c.... gain volume.

      do i=nodhyboff(mpk)+1,nodhyboff(mpk+1)
         ic=i-nodhyboff(mpk)
         ii=1+(nodhyb(i)-1)/maxnen
         lochybnod=nodhyb(i)-maxnen*(ii-1)
         ihyb=ieltary(ii)
         ityp=itettyp(ihyb)

         if (volafter(ic).gt.volbefore(ic)) then
            volafter(ic)=safe(volafter(ic))
            hxx(ihyb)=(hxx(ihyb)*volbefore(ic)+hxxave*(volafter(ic)
     &         -volbefore(ic)))/volafter(ic)
            hxy(ihyb)=(hxy(ihyb)*volbefore(ic)+hxyave*(volafter(ic)
     &         -volbefore(ic)))/volafter(ic)
            hxz(ihyb)=(hxz(ihyb)*volbefore(ic)+hxzave*(volafter(ic)
     &         -volbefore(ic)))/volafter(ic)
            hyy(ihyb)=(hyy(ihyb)*volbefore(ic)+hyyave*(volafter(ic)
     &         -volbefore(ic)))/volafter(ic)
            hyz(ihyb)=(hyz(ihyb)*volbefore(ic)+hyzave*(volafter(ic)
     &         -volbefore(ic)))/volafter(ic)
            hzz(ihyb)=(hzz(ihyb)*volbefore(ic)+hzzave*(volafter(ic)
     &         -volbefore(ic)))/volafter(ic)
         endif
      enddo

      return
      end
