*dk,cinterpolate
      function cinterpolate_elem(coption,cinterp,xp,yp,zp,
     *          xnodes,ynodes,znodes,xfield,ielmtyp)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE PROVIDES INTERPOLATION FROM ELEMENT POINTS 
C     TO A POINT LOCATED WITHIN THAT ELEMENT.  
C
C     The interpolation method is detirmined by the token "coption".    
C     Currently there is only one interpolation method. It is named
C     "continuous" and it weights points by the volumes created by
C     the query point. Relative volumes of the divided element are 
C     detirmined by the POINT location. The method is applied only
C     where it makes sense for the interpolation function. See min
C     and max explanation below. 
C
C     Quads, pyramids, prisms and hexes are split around their
C     median into triangles or tets (Oct 2026, see that branch); aby
C     numbering and shape is handled, and a linear field is exact.
C
C     The interpolation function CINTERP is applied to the weighted 
C     field values, this includes linear, log and asinh. The new field
C     values are then multiplied by the relative volumes and divided by
C     the number of nodes on the element.
C     The inverse function operation is then applied to the final value.
C
C     If the interpolation function is min or max, then the min or max 
C     value of the element points is returned and weighted volumes
C     from continuous method is skipped. 
C     
C     Current valid interpolation functions are:
C     Functions MIN or MAX return mininum or maximum field value.
C     All other functions are applied through function cinterpolate.
C     cinterpolate recognizes LINEAR,ASINH,LOG. The remaining functions
C     result in the original value returning unchanged. This includes
C     the interpolation functions for attributes:
C        default,constant,copy,sequence,incmin,incmax,and,or
C
C
C     INPUT ARGUMENTS -
C
C        coption  - TYPE OF INTERPOLATION METHOD 
C                   only one type is implemented "default" or "continuous" 
C        cinterp  - TYPE OF FUNCTION TO APPLY TO FIELD
C                   If cinterp is min or max, then the resulting value
C                   is the min or max value found in the field values.
C                   Other wise the element interpolation is used. 
C        xp,yp,zp - POINT LOCATED IN ELEMENT 
C                   This point must be located within or on the element.
C        xnodes   - LIST OF ELEMENT NODE COORDINATES 
C        ynodes,
C        znodes
C        xfield   - LIST OF ELEMENT NODE VALUES 
C        ielmtyp  - INTEGER INDICATING TYPE OF ELEMENT 
C
C     OUTPUT ARGUMENTS -
C
C        cinterpolate_elem - RESULTANT FUNCTION VALUE
C
C     CHANGE HISTORY -
C
C        $Log: cinterpolate_elem.f,v $
C        Revision 2.00  2007/11/05 19:45:47  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   23 Jun 2006 08:02:44   tam
CPVCS    changed volume checks to use epsilonv instead of epsilonl
CPVCS    added epsilonv to messege writing
CPVCS    
CPVCS       Rev 1.1   15 May 2001 08:20:36   tam
CPVCS    fixed mistype for coption = min or max
CPVCS    
CPVCS       Rev 1.0   20 Mar 2001 12:45:36   tam
CPVCS    Initial revision.
C
C ######################################################################
C
C
      implicit none
C
      include "local_element.h"
      integer mxnodes
      parameter (mxnodes=8)
C
      integer ielmtyp,ierror
      character*32 coption,cinterp,cgeom
      real*8 xnodes(mxnodes),ynodes(mxnodes),znodes(mxnodes),
     *       xfield(mxnodes),xp,yp,zp
      real*8 cinterpolate_elem
C
C ######################################################################
C


      integer ierrw,ierr_zero

      real*8 local_eps, epsilonl, epsilonvol
      real*8 volelm,volsum,xvalue,favg
      real*8 xcross,ycross,zcross
      real*8 x1,x2,x3,x4,x5,x6,x7,x8,
     *       y1,y2,y3,y4,y5,y6,y7,y8,
     *       z1,z2,z3,z4,z5,z6,z7,z8,
     *       fv1,fv2,fv3,fv4,fv5,fv6,fv7,fv8,
     *       x56,y56,z56,x67,y67,z67,x78,y78,z78,x85,y85,z85,
     *       x15,y15,z15,x26,y26,z26,x37,y37,z37,x48,y48,z48,
     *       x13,y13,z13,x16,y16,z16,x18,y18,z18,x72,y72,z72,
     *       x74,y74,z74,x75,y75,z75,x23,y23,z23,x34,y34,z34,
     *       x41,y41,z41,x12,y12,z12,
     *       vol1,vol2,vol3,vol4,vol5,vol6,vol7,vol8
            
      real*8 cinterpolate
      integer i,nen,ierr_wts
      real*8 wts(mxnodes),sumf,sumw

      character*132 logmess
C
C ######################################################################
C
C
C
      local_eps=1.d-100
      ierr_zero = 0
      call get_epsilon('epsilonl',epsilonl)
      call get_epsilon('epsilonv',epsilonvol)


* todo - add cintrp attribute interpolation function to MIN/MAX
C*****************************************************************
C     MIN interpolation function
C*****************************************************************
      if (coption(1:3).eq.'min') then

        if(ielmtyp.eq.ifelmtri) then
          xvalue=min(xfield(1),xfield(2),xfield(3))

        elseif(ielmtyp.eq.ifelmqud .or. ielmtyp.eq.ifelmtet) then
          xvalue=min(xfield(1),xfield(2),xfield(3),xfield(4))

        else
C         only the element's own vertices (a prism has 6, a pyramid 5)
          xvalue=xfield(1)
          do i = 2,nelmnen(ielmtyp)
            xvalue=min(xvalue,xfield(i))
          enddo
        endif

C*****************************************************************
C     MAX interpolation function
C*****************************************************************
      elseif(coption(1:3).eq.'max') then

        if(ielmtyp.eq.ifelmtri) then
          xvalue=max(xfield(1),xfield(2),xfield(3))

        elseif(ielmtyp.eq.ifelmqud .or. ielmtyp.eq.ifelmtet) then
          xvalue=max(xfield(1),xfield(2),xfield(3),xfield(4))

        else
C         only the element's own vertices (a prism has 6, a pyramid 5)
          xvalue=xfield(1)
          do i = 2,nelmnen(ielmtyp)
            xvalue=max(xvalue,xfield(i))
          enddo

        endif

C********************************************************************
C     ALL OTHER interpolation functions
C       continuous - depends on principal axis
C       to add, continuous with weighted distance, trilinear
C    
C********************************************************************
      else
C     if  coption.eq.'continuous' .or. coption.eq.'default'

C  set up arguments for cmo_interpolate - weight by distance
C  between connection being broken
C
C

      x1=xnodes(1)
      y1=ynodes(1)
      z1=znodes(1)
      x2=xnodes(2)
      y2=ynodes(2)
      z2=znodes(2)
      x3=xnodes(3)
      y3=ynodes(3)
      z3=znodes(3)
      x4=xnodes(4)
      y4=ynodes(4)
      z4=znodes(4)
      x5=xnodes(5)
      y5=ynodes(5)
      z5=znodes(5)
      x6=xnodes(6)
      y6=ynodes(6)
      z6=znodes(6)
      x7=xnodes(7)
      y7=ynodes(7)
      z7=znodes(7)
      x8=xnodes(8)
      y8=ynodes(8)
      z8=znodes(8)

cc     Find a principal direction and interpolate in
cc     plane perpendicular to that direction
c      xcross = abs( (y3-y1)*(z2-z1) - (z3-z1)*(y2-y1) )
c      ycross = abs( (z3-z1)*(x2-x1) - (x3-x1)*(z2-z1) )
c      zcross = abs( (x3-x1)*(y2-y1) - (y3-y1)*(x2-x1) )
cC
cC     If kdir.eq.0, do YZ-plane, else if kdir.eq.1,
cC        do ZX-plane, else do XY-plane.
c       cgeom = 'yz'
c       if ( ycross .gt. xcross ) cgeom = 'zx'
c       if ((zcross .gt. xcross) .and. (zcross .gt. ycross)) cgeom = 'xy'
c
c      if (cgeom(1:2) .eq. 'yz' ) then
c        x1=ynodes(1)
c        y1=znodes(1)
c        z1=xnodes(1)
c        x2=ynodes(2)
c        y2=znodes(2)
c        z2=xnodes(2)
c        x3=ynodes(3)
c        y3=znodes(3)
c        z3=xnodes(3)
c        x4=ynodes(4)
c        y4=znodes(4)
c        z4=xnodes(4)
c        x5=ynodes(5)
c        y5=znodes(5)
c        z5=xnodes(5)
c        x6=ynodes(6)
c        y6=znodes(6)
c        z6=xnodes(6)
c        x7=ynodes(7)
c        y7=znodes(7)
c        z7=xnodes(7)
c        x8=ynodes(8)
c        y8=znodes(8)
c        z8=xnodes(8)
c      elseif (cgeom(1:2) .eq. 'zx' ) then
c        x1=znodes(1)
c        y1=xnodes(1)
c        z1=ynodes(1)
c        x2=znodes(2)
c        y2=xnodes(2)
c        z2=ynodes(2)
c        x3=znodes(3)
c        y3=xnodes(3)
c        z3=ynodes(3)
c        x4=znodes(4)
c        y4=xnodes(4)
c        z4=ynodes(4)
c        x5=znodes(5)
c        y5=xnodes(5)
c        z5=ynodes(5)
c        x6=znodes(6)
c        y6=xnodes(6)
c        z6=ynodes(6)
c        x7=znodes(7)
c        y7=xnodes(7)
c        z7=ynodes(7)
c        x8=znodes(8)
c        y8=xnodes(8)
c        z8=ynodes(8)
c      endif




C     ***** INTERPOLATION ON TRIANGLE *****
      if(ielmtyp.eq.ifelmtri) then

         call volume_tri(x1,y1,z1,x2,y2,z2,x3,y3,z3,volelm)

         call volume_tri(xp,yp,zp,x2,y2,z2,x3,y3,z3,vol1)
         call volume_tri(xp,yp,zp,x3,y3,z3,x1,y1,z1,vol2)
         call volume_tri(xp,yp,zp,x1,y1,z1,x2,y2,z2,vol3)
         volsum = vol1+vol2+vol3

         if (volsum .lt. epsilonvol) then
           ierr_zero = 1
         endif

C        Transform the field values by the desired function
         fv1 = cinterpolate('function',cinterp,xfield(1))
         fv2 = cinterpolate('function',cinterp,xfield(2))
         fv3 = cinterpolate('function',cinterp,xfield(3))
         xvalue = (fv1*vol1+fv2*vol2+fv3*vol3) / volsum
C
C        Do the inverse function operation
         favg = (xfield(1)+xfield(2)+xfield(3))/3.0d+00
         xvalue=cinterpolate('inverse',cinterp,xvalue)
         if(cinterp(1:3).eq.'log') then
           xvalue = sign(xvalue,favg)
         endif


C     ***** INTERPOLATION ON TET *****
      elseif(ielmtyp.eq.ifelmtet) then

         call volume_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,volelm)
c 1,2,3,P
         call volume_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,xp,yp,zp,vol4)
c 1,4,2,P
         call volume_tet(x1,y1,z1,x4,y4,z4,x2,y2,z2,xp,yp,zp,vol3)
c 1,3,4,P
         call volume_tet(x1,y1,z1,x3,y3,z3,x4,y4,z4,xp,yp,zp,vol2)
c 3,2,4,P
         call volume_tet(x3,y3,z3,x2,y2,z2,x4,y4,z4,xp,yp,zp,vol1)

         volsum = vol1+vol2+vol3+vol4

C        Transform the field values by the desired function
         fv1 = cinterpolate('function',cinterp,xfield(1))
         fv2 = cinterpolate('function',cinterp,xfield(2))
         fv3 = cinterpolate('function',cinterp,xfield(3))
         fv4 = cinterpolate('function',cinterp,xfield(4))

c        Volumes are zero, do some more work
         if (volsum .lt. epsilonvol) then
           ierr_zero = 1
         endif

cc       compute distance of query point from each tet point
c        vol1 = sqrt((xp-x1)**2 + (yp-y1)**2 + (zp-z1)**2)
c        vol2 = sqrt((xp-x2)**2 + (yp-y2)**2 + (zp-z2)**2)
c        vol3 = sqrt((xp-x3)**2 + (yp-y3)**2 + (zp-z3)**2)
c        vol4 = sqrt((xp-x4)**2 + (yp-y4)**2 + (zp-z4)**2)
c        print*,vol1,vol2,vol3,vol4
c
cc       points are all at position, take average of all
c        if ((vol1+vol2+vol3+vol4) .lt. epsilonl) then
c          xvalue = (fv1+fv2+fv3+fv4)/4.0d+00
c        else
c          xvalue = (fv1*vol1+fv2*vol2+
c     *                 fv3*vol3+fv4*vol4)/(vol1+vol2+vol3+vol4)
c        endif
c

         xvalue = (fv1*vol1+fv2*vol2+fv3*vol3+fv4*vol4)/ volsum

C        Do the inverse function operation
         favg = (xfield(1)+xfield(2)+xfield(3)+
     *           xfield(4))/4.0d+00
         xvalue=cinterpolate('inverse',cinterp,xvalue)
         if(cinterp(1:3).eq.'log') then
           xvalue = sign(xvalue,favg)
         endif


C     ***** INTERPOLATION ON QUAD, PYRAMID, PRISM, HEX *****
C     (Oct 2026) The element is split around its median: a quad into
C     4 triangles; a pyramid, prism or hex into tets, each face with 4
C     vertices into 4 triangles around the face median (a hex gives
C     the 24 tets of hextotet / 24 and of inside_hex). The median
C     carries the mean of the element's vertex values and a face
C     median the mean of the face's, so a linear field is exact, the
C     result does not depend on how the vertices are numbered or
C     whether the element is aligned with the axes, and two elements
C     sharing a face split it the same way. The point is taken in the
C     piece where its smallest barycentric weight is largest, so a
C     point on or just outside a piece still gets a sensible value.
C     The weights are folded back onto the element's own vertices.
C     Replaces the split by the planes x=xp, y=yp, z=zp, which
C     assumed edge 1-2 along x, 2-3 along y, 1-5 along z (aby other
C     numbering gave zero volumes and values near 1e14), and a prism
C     branch that read vertices 7 and 8.
      elseif(ielmtyp.eq.ifelmqud .or. ielmtyp.eq.ifelmpyr .or.
     *       ielmtyp.eq.ifelmpri .or. ielmtyp.eq.ifelmhex) then

         nen = nelmnen(ielmtyp)
         call cinterp_elem_wts(ielmtyp,xnodes,ynodes,znodes,
     *                         xp,yp,zp,wts,volsum,ierr_wts)

         sumf = 0.0d0
         sumw = 0.0d0
         favg = 0.0d0
         do i = 1,nen
            fv1 = cinterpolate('function',cinterp,xfield(i))
            sumf = sumf + wts(i)*fv1
            sumw = sumw + wts(i)
            favg = favg + xfield(i)
         enddo
         favg = favg/dble(nen)
         volelm = 0.0d0
         vol1 = wts(1)
         vol2 = wts(2)
         vol3 = wts(3)
         vol4 = wts(4)
         vol5 = wts(5)
         vol6 = wts(6)
         vol7 = wts(7)
         vol8 = wts(8)
         if (ierr_wts.ne.0 .or. abs(sumw).lt.local_eps) then
C           every piece has zero volume: report it, use the mean
            ierr_zero = 1
            sumf = 0.0d0
            do i = 1,nen
               sumf = sumf + cinterpolate('function',cinterp,xfield(i))
            enddo
            xvalue = sumf/dble(nen)
         else
            xvalue = sumf/sumw
         endif

C        Do the inverse function operation
         xvalue = cinterpolate('inverse',cinterp,xvalue)
         if(cinterp(1:3).eq.'log') then
            xvalue = sign(xvalue,favg)
         endif


C     Done with interpolation on element types 
      endif

C*****************************************************************
C     Done with interpolation options
      endif


9000  if (ierr_zero.eq.1) then
        write(logmess,'(a)')
     *  'Warning >> cinterpolate_elem: Zero Volume Found. '
        call writloga('default',1,logmess,0,ierrw)

        write(logmess,'(a,g15.7,a,g15.7)')
     *  '  Element Volume: ',volelm,' epsilonv:',epsilonvol
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(a,g15.7)')
     *  '  Sum of Internal Volumes: ',volsum
        call writloga('default',0,logmess,0,ierrw)

c       write volumes
        if(ielmtyp.eq.ifelmtri) then
          write(logmess,'(a,g15.7,g15.7,g15.7)')
     *        '  vols 1-3: ',vol1,vol2,vol3
          call writloga('default',0,logmess,0,ierrw)
        elseif(ielmtyp.eq.ifelmqud .or. ielmtyp.eq.ifelmtet) then
          write(logmess,'(a,g15.7,g15.7,g15.7,g15.7)')
     *        '  vols 1-4: ',vol1,vol2,vol3,vol4
          call writloga('default',0,logmess,0,ierrw)
        else
          write(logmess,'(a,g15.7,g15.7,g15.7,g15.7)')
     *        '  vols 1-4: ',vol1,vol2,vol3,vol4
          call writloga('default',0,logmess,0,ierrw)
          write(logmess,'(a,g15.7,g15.7,g15.7,g15.7)')
     *        '  vols 5-8: ',vol5,vol6,vol7,vol8
          call writloga('default',0,logmess,0,ierrw)
        endif

c       write coords
        write(logmess,'(a)')
     *  '  Point coordinates their values:' 
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *        x1,y1,z1,' elm point 1 = ',xfield(1)
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *        x2,y2,z2,' elm point 2 = ',xfield(2)
        call writloga('default',0,logmess,0,ierrw)
        write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *        x3,y3,z3,' elm point 3 = ',xfield(3)
        call writloga('default',0,logmess,0,ierrw)

        if(ielmtyp.ne.ifelmtri) then
          write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *        x4,y4,z4,' elm point 4 = ',xfield(4)
          call writloga('default',0,logmess,0,ierrw)

          if(ielmtyp.ne.ifelmqud .and. ielmtyp.ne.ifelmtet)then
          write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *          x5,y5,z5,' elm point 5 = ',xfield(5)
          call writloga('default',0,logmess,0,ierrw)
          write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *          x6,y6,z6,' elm point 6 = ',xfield(6)
          call writloga('default',0,logmess,0,ierrw)
          write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *          x7,y7,z7,' elm point 7 = ',xfield(7)
          call writloga('default',0,logmess,0,ierrw)
          write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *          x8,y8,z8,' elm point 8 = ',xfield(8)
          call writloga('default',0,logmess,0,ierrw)
          endif
        endif

        write(logmess,'(g14.7,g14.7,g14.7,a,g14.7)')
     *          xp,yp,zp,' query point = ',xvalue
        call writloga('default',0,logmess,1,ierrw)
      endif


      cinterpolate_elem = xvalue
      goto 9999
 9999 continue
      return
      end

*dk,cinterp_elem_wts
      subroutine cinterp_elem_wts(ielmtyp,xn,yn,zn,xp,yp,zp,wts,
     *                            bestmin,ierr)
C
C     PURPOSE - weights wts(1:nelmnen) of the vertices of a quad,
C     pyramid, prism or hex for the point (xp,yp,zp): the element is
C     split around its median (and the medians of its 4-vertex faces)
C     into triangles (quad) or tets, the point's barycentric weights
C     are found in the piece where the smallest weight is largest,
C     and a median's weight is shared equally by the vertices it is
C     the mean of. A linear field is reproduced exactly.
C
C     OUTPUT - wts, bestmin (the smallest barycentric weight in the
C     chosen piece: >= 0 inside, slightly < 0 just outside), ierr
C     (0, or 1 when every piece has zero size).
C     Faces come from LaGriT's element tables (ielmface0/1), so the
C     split is the same in two elements sharing a face.
C     Oct 2026.
C
      implicit none
      include "local_element.h"
      integer ielmtyp,ierr
      real*8 xn(8),yn(8),zn(8),xp,yp,zp,wts(8),bestmin

      integer nen,nf,nfn,i,j,f,ia,ib,ic
      integer kv(4)
      real*8 xc,yc,zc,xm,ym,zm,scale,vmin
      real*8 qx(4),qy(4),qz(4),lam(4),lmin,vol,dot
      real*8 ux,uy,uz,vx,vy,vz,wx,wy,wz,abx,aby,abz
      real*8 cie_det3

      nen = nelmnen(ielmtyp)
      ierr = 1
      bestmin = -1.0d+300
      do i = 1,8
         wts(i) = 0.0d0
      enddo

C     element median and size
      xc = 0.0d0
      yc = 0.0d0
      zc = 0.0d0
      do i = 1,nen
         xc = xc + xn(i)
         yc = yc + yn(i)
         zc = zc + zn(i)
      enddo
      xc = xc/dble(nen)
      yc = yc/dble(nen)
      zc = zc/dble(nen)
      scale = 0.0d0
      do i = 1,nen
         scale = max(scale,abs(xn(i)-xc),abs(yn(i)-yc),abs(zn(i)-zc))
      enddo
      if (scale.le.0.0d0) return

C     ----- QUAD: 4 triangles (vertex i, vertex i+1, median) -----
      if (ielmtyp.eq.ifelmqud) then
         vmin = 1.0d-14*scale*scale
         do i = 1,4
            ia = i
            ib = mod(i,4)+1
            qx(1) = xn(ia)
            qy(1) = yn(ia)
            qz(1) = zn(ia)
            qx(2) = xn(ib)
            qy(2) = yn(ib)
            qz(2) = zn(ib)
            qx(3) = xc
            qy(3) = yc
            qz(3) = zc
C           normal of the triangle; weights from sub-areas along it
            ux = qx(2)-qx(1)
            uy = qy(2)-qy(1)
            uz = qz(2)-qz(1)
            vx = qx(3)-qx(1)
            vy = qy(3)-qy(1)
            vz = qz(3)-qz(1)
            abx = uy*vz-uz*vy
            aby = uz*vx-ux*vz
            abz = ux*vy-uy*vx
            dot = abx*abx+aby*aby+abz*abz
            if (sqrt(dot).le.vmin) goto 100
            do j = 1,3
               ia = mod(j,3)+1
               ib = mod(j+1,3)+1
               ux = qx(ia)-xp
               uy = qy(ia)-yp
               uz = qz(ia)-zp
               vx = qx(ib)-xp
               vy = qy(ib)-yp
               vz = qz(ib)-zp
               lam(j) = (abx*(uy*vz-uz*vy)+aby*(uz*vx-ux*vz)
     *                  +abz*(ux*vy-uy*vx))/dot
            enddo
            lmin = min(lam(1),lam(2),lam(3))
            if (lmin.gt.bestmin) then
               bestmin = lmin
               ierr = 0
               do j = 1,4
                  wts(j) = lam(3)/4.0d0
               enddo
               wts(i) = wts(i) + lam(1)
               wts(mod(i,4)+1) = wts(mod(i,4)+1) + lam(2)
            endif
 100        continue
         enddo
         return
      endif

C     ----- PYRAMID, PRISM, HEX: tets on each face piece + median -----
      vmin = 1.0d-14*scale*scale*scale
      nf = nelmnef(ielmtyp)
      do f = 1,nf
         nfn = ielmface0(f,ielmtyp)
         do i = 1,nfn
            kv(i) = ielmface1(i,f,ielmtyp)
         enddo
         if (nfn.eq.4) then
            xm = 0.25d0*(xn(kv(1))+xn(kv(2))+xn(kv(3))+xn(kv(4)))
            ym = 0.25d0*(yn(kv(1))+yn(kv(2))+yn(kv(3))+yn(kv(4)))
            zm = 0.25d0*(zn(kv(1))+zn(kv(2))+zn(kv(3))+zn(kv(4)))
         endif
C        pieces of this face: one triangle, or 4 around the median
         do j = 1,nfn-2*(4-nfn)
            if (nfn.eq.3) then
               ia = kv(1)
               ib = kv(2)
               ic = kv(3)
               qx(3) = xn(ic)
               qy(3) = yn(ic)
               qz(3) = zn(ic)
            else
               ia = kv(j)
               ib = kv(mod(j,4)+1)
               ic = 0
               qx(3) = xm
               qy(3) = ym
               qz(3) = zm
            endif
            qx(1) = xn(ia)
            qy(1) = yn(ia)
            qz(1) = zn(ia)
            qx(2) = xn(ib)
            qy(2) = yn(ib)
            qz(2) = zn(ib)
            qx(4) = xc
            qy(4) = yc
            qz(4) = zc
C           barycentric weights in the tet q1 q2 q3 q4 (signed volumes,
C           so the orientation of the face does not matter)
            vol = cie_det3(qx(2)-qx(1),qy(2)-qy(1),qz(2)-qz(1),
     *                     qx(3)-qx(1),qy(3)-qy(1),qz(3)-qz(1),
     *                     qx(4)-qx(1),qy(4)-qy(1),qz(4)-qz(1))
            if (abs(vol).le.vmin) goto 200
            lam(1) = cie_det3(qx(2)-xp,qy(2)-yp,qz(2)-zp,
     *                        qx(3)-xp,qy(3)-yp,qz(3)-zp,
     *                        qx(4)-xp,qy(4)-yp,qz(4)-zp)/vol
            lam(2) = cie_det3(xp-qx(1),yp-qy(1),zp-qz(1),
     *                        qx(3)-qx(1),qy(3)-qy(1),qz(3)-qz(1),
     *                        qx(4)-qx(1),qy(4)-qy(1),qz(4)-qz(1))/vol
            lam(3) = cie_det3(qx(2)-qx(1),qy(2)-qy(1),qz(2)-qz(1),
     *                        xp-qx(1),yp-qy(1),zp-qz(1),
     *                        qx(4)-qx(1),qy(4)-qy(1),qz(4)-qz(1))/vol
            lam(4) = 1.0d0-lam(1)-lam(2)-lam(3)
            lmin = min(lam(1),lam(2),lam(3),lam(4))
            if (lmin.gt.bestmin) then
               bestmin = lmin
               ierr = 0
               do i = 1,nen
                  wts(i) = lam(4)/dble(nen)
               enddo
               wts(ia) = wts(ia) + lam(1)
               wts(ib) = wts(ib) + lam(2)
               if (ic.gt.0) then
                  wts(ic) = wts(ic) + lam(3)
               else
                  do i = 1,4
                     wts(kv(i)) = wts(kv(i)) + 0.25d0*lam(3)
                  enddo
               endif
            endif
 200        continue
         enddo
      enddo
      return
      end

*dk,cie_det3
      function cie_det3(ax,ay,az,bx,by,bz,cx,cy,cz)
C     determinant of the rows a, b, c: a . (b x c)
      implicit none
      real*8 cie_det3,ax,ay,az,bx,by,bz,cx,cy,cz
      cie_det3 = ax*(by*cz-bz*cy)+ay*(bz*cx-bx*cz)+az*(bx*cy-by*cx)
      return
      end
