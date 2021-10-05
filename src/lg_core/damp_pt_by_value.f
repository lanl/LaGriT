      subroutine damp_pt_by_value(action,node,nodhyb,nodhyboff,
     &   ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,
     &   ipivoloffoff,itettyp,itetclr,itet,itetoff,x
     &   ,y,z,nnodes,nelements,iedge,iedgeoff
     &   ,iedgemat,ichildary,ichildno,invchildary,imt1
     &   ,epsilonv,fvec,reffield,iparent,hxx,hxy,hxz,hyy
     &   ,hyz,hzz,range,fold,pfx,pfxx,dx,dy,dz)
C #####################################################################
C
C     PURPOSE -
C
C        DAMP_PT_BY_VALUE considers the proposed point movement of
C     NODE by DX,DY,DZ, and limits it so that the functional
C     POLYFUN decreases sufficiently.  It does a line search
C     a la the numerical recipes subroutine lnsrch.f (2nd ed.).  It is
C     REQUIRED that DX,DY,DZ not be an ascending direction for POLYFUN.
C
C     INPUT ARGUMENTS -
C
C        ACTION-  Type of smoothing done by polyfun.  Passed to polyfun.
C        NODE  -  The node whose movement we are limiting.
C        NODHYB-  Node-element relation.  Passed to polyfun.
C        NODHYBOFF-  Node-element relation offsets.  Passed to polyfun.
C        IELTARY- Array of relevant elements.
C        IELTNO-  Length of element array.
C        INVMPARY- Inverse mapping of that defined by mass point array
C                  MPARY.
C        VOLOFF-   Array of volume offsets.
C        VOLOFFOFF- Offset array of volume offsets.
C        ITETTYP-  Array of element types.
C        ITETCLR-  Array of element colors.
C        ITET-     Element-node relation.
C        ITETOFF-  Offsets for Element-node relation.
C        X,Y,Z -  The xic,yic,zic arrays for the mesh.
C        FOLD  -  The value of POLYFUN when DX=DY=DZ=0.
C        PFX   -  The gradient of POLYFUN at DX=DY=DZ=0.
C        DX,DY,-  Proposed changes to add to X(NODE), Y(NODE), Z(NODE).
C        DZ
C
C     OUTPUT ARGUMENTS -
C
C        DX,DY,-  Safe changes to add to X(NODE), Y(NODE), Z(NODE),
C        DZ       which decrease POLYFUN sufficiently.
C
C     CHANGE HISTORY -
C
C$Log: damp_pt_by_value.f,v $
CRevision 2.00  2007/11/05 19:45:51  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.4   30 Sep 2004 11:13:36   dcg
CPVCS    make alf,tolx double precision
CPVCS
CPVCS       Rev 1.3   07 Jan 2002 20:19:38   kuprat
CPVCS     Pass in Hessian for use by new (smaller) version of polyfun_nosb.
CPVCS
CPVCS       Rev 1.2   06 Nov 2001 15:39:34   kuprat
CPVCS    Pass ITETCLR on to polyfun.
CPVCS
CPVCS       Rev 1.1   14 Mar 2001 13:36:14   dcg
CPVCS    get rid of upper case
CPVCS
CPVCS       Rev 1.0   Tue Sep 02 23:05:50 1997   kuprat
CPVCS    Initial revision.
C
C ######################################################################
      implicit none
      include 'consts.h'
 
      pointer (ipvoloff,voloff),(iplocvoloff,locvoloff),
     &   (ipivoloffoff,ivoloffoff)
 
      integer node,nodhyb(*),nodhyboff(*),ieltary(*),
     &   ieltno,locvoloff(*),ivoloffoff(*),itettyp(*),
     &   itet(*),itetoff(*),invmpary(*),itetclr(*)
 
      real*8 x(*),y(*),z(*),fold,pfx(3),dx,dy,dz,
     &   voloff(*)
 
      integer ierrw,nnodes,nelements,iedge(*),iedgeoff(*),iedgemat(*)
     &   ,ichildary(*),ichildno,invchildary(*),imt1(*),iparent(*)
 
      real*8 slope,alamin,alam,dxnew,dynew,dznew,
     &   f1,alf,tmplam,rhs1,rhs2,a1,b,disc,alam2,f2,fold2,
     &   xold,yold,zold,tolx,epsilonv,fvec(*),reffield(*),hxx(*),hxy(*)
     &   ,hxz(*),hyy(*),hyz(*),hzz(*),range,pfxx(3,3)
      parameter (alf=1.d-4,tolx=1.d-7)
      character*32 action
      character*132 logmess
 
      xold=x(node)
      yold=y(node)
      zold=z(node)
 
      slope=pfx(1)*dx+pfx(2)*dy+pfx(3)*dz
 
      if (slope.gt.zero) then
         write(logmess,'(a)')
     *            'DAMP_PT_BY_VALUE:  Ascent direction.'
         call writloga('default',0,logmess,0,ierrw)
         dx=zero
         dy=zero
         dz=zero
         goto 9999
      endif
 
      alamin=tolx
      alam=1.
 
      do while (.true.)
        dxnew=alam*dx
        dynew=alam*dy
        dznew=alam*dz
 
         x(node)=xold+dxnew
         y(node)=yold+dynew
         z(node)=zold+dznew
 
 
         call polyfun(4,action,node,nodhyb,nodhyboff,
     &      ieltary,ieltno,invmpary,ipvoloff,iplocvoloff,ipivoloffoff,
     &      itettyp,itetclr,itet,itetoff,x,y,z,
     &      nnodes,nelements,iedge,iedgeoff,iedgemat,ichildary,
     &      ichildno,invchildary,imt1,epsilonv,fvec,reffield,iparent,
     &      hxx,hxy,hxz,hyy,hyz,hzz,range,f1,pfx,pfxx)
 
         x(node)=xold
         y(node)=yold
         z(node)=zold
 
         if(f1.le.fold+alf*alam*slope)then
            dx=dxnew
            dy=dynew
            dz=dznew
            goto 9999
         elseif(alam.lt.alamin)then
            dx=0.
            dy=0.
            dz=0.
            goto 9999
         else
            if(alam.eq.1.)then
               tmplam=-slope/(2.*(f1-fold-slope))
               if(tmplam.gt..75)tmplam=.75
            else
               rhs1=f1-fold-alam*slope
               rhs2=f2-fold2-alam2*slope
               a1=(rhs1/alam**2-rhs2/alam2**2)/(alam-alam2)
               b=(-alam2*rhs1/alam**2+alam*rhs2/alam2**2)/(alam-alam2)
               if(a1.eq.0.)then
                  tmplam=-slope/(2.*b)
               else
                  disc=b*b-3.*a1*slope
                  tmplam=(-b+sqrt(max(zero,disc)))/(3.*a1)
               endif
               if(tmplam.gt..5*alam)tmplam=.5*alam
            endif
         endif
         alam2=alam
         f2=f1
         fold2=fold
         alam=max(tmplam,.1*alam)
      enddo
C  (C) Copr. 1986-92 Numerical Recipes Software
 
 9999 continue
      return
      end
 
