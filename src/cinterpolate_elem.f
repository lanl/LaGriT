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
C     WARNING: rotated or non-orthonagol hex elements can not be
C     guaranteed to have positive summed volumes.
C     It is reccommended that hextotet be used on these hex grids.
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
          xvalue=min(xfield(1),xfield(2),xfield(3),xfield(4),
     *               xfield(5),xfield(6),xfield(7),xfield(8))
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
          xvalue=max(xfield(1),xfield(2),xfield(3),xfield(4),
     *               xfield(5),xfield(6),xfield(7),xfield(8))

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


C     ***** INTERPOLATION ON QUAD *****
      elseif(ielmtyp.eq.ifelmqud) then

         if(abs(x2-x1).lt.epsilonl) then
            x12=xp
            y12=yp
            z12=zp
         else
            x12=xp
            y12=y1+(y2-y1)*(xp-x1)/(x2-x1)
            z12=z1+(z2-z1)*(xp-x1)/(x2-x1)
         endif
         if(abs(y3-y2).lt.epsilonl) then
            x23=xp
            y23=yp
            z23=zp
         else
            x23=x2+(x3-x2)*(yp-y2)/(y3-y2)
            y23=yp
            z23=z2+(z3-z2)*(yp-y2)/(y3-y2)
         endif
         if(abs(x4-x3).lt.epsilonl) then
            x34=xp
            y34=yp
            z34=zp
         else
            x34=xp
            y34=y3+(y4-y3)*(xp-x3)/(x4-x3)
            z34=z3+(z4-z3)*(xp-x3)/(x4-x3)
         endif
         if(abs(y4-y1).lt.epsilonl) then
            x41=xp
            y41=yp
            z41=zp
         else
            x41=x1+(x4-x1)*(yp-y1)/(y4-y1)
            y41=yp
            z41=z1+(z4-z1)*(yp-y1)/(y4-y1)
         endif
C
        call volume_qud(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,volelm)
C
        call volume_qud(x1 ,y1 ,z1 ,x12,y12,z12,xp ,yp ,zp ,x41,y41,z41,
     *                  vol3)
        call volume_qud(x2 ,y2 ,z2 ,x23,y23,z23,xp ,yp ,zp ,x12,y12,z12,
     *                  vol4)
        call volume_qud(x3 ,y3 ,z3 ,x34,y34,z34,xp ,yp ,zp ,x23,y23,z23,
     *                  vol1)
        call volume_qud(x4 ,y4 ,z4 ,x41,y41,z41,xp ,yp ,zp ,x34,y34,z34,
     *                  vol2)
        volsum=vol1+vol2+vol3+vol4
C
         if (volsum .lt. epsilonvol) then
           ierr_zero = 1
         endif

C        Transform the field values by the desired function.
         fv1 = cinterpolate('function',cinterp,xfield(1))
         fv2 = cinterpolate('function',cinterp,xfield(2))
         fv3 = cinterpolate('function',cinterp,xfield(3))
         fv4 = cinterpolate('function',cinterp,xfield(4))

         xvalue = (fv1*vol1+fv2*vol2+fv3*vol3+fv4*vol4) / volsum
 
C        Do the inverse function operation
         favg = (xfield(1)+xfield(2)+xfield(3)+xfield(4))/4.0d+00
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


C     ***** INTERPOLATION ON HEX *****
      elseif(ielmtyp.eq.ifelmhex) then

c     Break hex into 8 hexs including the query point xp,yp,zp
c     the 8 hexs are propotional to full hex
c     this depends on orthonogal hexs

         if(abs(x2-x1).lt.epsilonl) then
            x12=xp
            y12=yp
            z12=zp
         else
            x12=xp
            y12=y1+(y2-y1)*(xp-x1)/(x2-x1)
            z12=z1+(z2-z1)*(xp-x1)/(x2-x1)
         endif

         if(abs(y3-y2).lt.epsilonl) then
            x23=xp
            y23=yp
            z23=zp
         else
            x23=x2+(x3-x2)*(yp-y2)/(y3-y2)
            y23=yp
            z23=z2+(z3-z2)*(yp-y2)/(y3-y2)
         endif

         if(abs(x4-x3).lt.epsilonl) then
            x34=xp
            y34=yp
            z34=zp
         else
            x34=xp
            y34=y3+(y4-y3)*(xp-x3)/(x4-x3)
            z34=z3+(z4-z3)*(xp-x3)/(x4-x3)
         endif

         if(abs(y4-y1).lt.epsilonl) then
            x41=xp
            y41=yp
            z41=zp
         else
            x41=x1+(x4-x1)*(yp-y1)/(y4-y1)
            y41=yp
            z41=z1+(z4-z1)*(yp-y1)/(y4-y1)
         endif

C
         if(abs(x6-x5).lt.epsilonl) then
            x56=xp
            y56=yp
            z56=zp
         else
            x56=xp
            y56=y5+(y6-y5)*(xp-x5)/(x6-x5)
            z56=z5+(z6-z5)*(xp-x5)/(x6-x5)
         endif

         if(abs(y7-y6).lt.epsilonl) then
            x67=xp
            y67=yp
            z67=zp
         else
            x67=x6+(x7-x6)*(yp-y6)/(y7-y6)
            y67=yp
            z67=z6+(z7-z6)*(yp-y6)/(y7-y6)
         endif

         if(abs(x8-x7).lt.epsilonl) then
            x78=xp
            y78=yp
            z78=zp
         else
            x78=xp
            y78=y7+(y8-y7)*(xp-x7)/(x8-x7)
            z78=z7+(z8-z7)*(xp-x7)/(x8-x7)
         endif

         if(abs(y8-y5).lt.epsilonl) then
            x85=xp
            y85=yp
            z85=zp
         else
            x85=x5+(x8-x5)*(yp-y5)/(y8-y5)
            y85=yp
            z85=z5+(z8-z5)*(yp-y5)/(y8-y5)
         endif

C
         if(abs(z5-z1).lt.epsilonl) then
            x15=xp
            y15=yp
            z15=zp
         else
            x15=x1+(x5-x1)*(zp-z1)/(z5-z1)
            y15=y1+(y5-y1)*(zp-z1)/(z5-z1)
            z15=zp
         endif

         if(abs(z6-z2).lt.epsilonl) then
            x26=xp
            y26=yp
            z26=zp
         else
            x26=x2+(x6-x2)*(zp-z2)/(z6-z2)
            y26=y2+(y6-y2)*(zp-z2)/(z6-z2)
            z26=zp
         endif

         if(abs(z7-z3).lt.epsilonl) then
            x37=xp
            y37=yp
            z37=zp
         else
            x37=x3+(x7-x3)*(zp-z3)/(z7-z3)
            y37=y3+(y7-y3)*(zp-z3)/(z7-z3)
            z37=zp
         endif

         if(abs(z8-z4).lt.epsilonl) then
            x48=xp
            y48=yp
            z48=zp
         else
            x48=x4+(x8-x4)*(zp-z4)/(z8-z4)
            y48=y4+(y8-y4)*(zp-z4)/(z8-z4)
            z48=zp
         endif

C
         if(sqrt((x3-x1)**2+(y3-y1)**2).lt.epsilonl) then
            x13=xp
            y13=yp
            z13=zp
         else
            x13=xp
            y13=yp
            z13=z1+(z3-z1)*sqrt(xp**2+yp**2)/sqrt((x3-x1)**2+(y3-y1)**2)
         endif

         if(sqrt((x6-x1)**2+(z6-z1)**2).lt.epsilonl) then
            x16=xp
            y16=yp
            z16=zp
         else
            x16=xp
            y16=y1+(y6-y1)*sqrt(xp**2+zp**2)/sqrt((x6-x1)**2+(z6-z1)**2)
            z16=zp
         endif

         if(sqrt((y8-y1)**2+(z8-z1)**2).lt.epsilonl) then
            x18=xp
            y18=yp
            z18=zp
         else
            x18=x1+(x8-x1)*sqrt(yp**2+zp**2)/sqrt((y8-y1)**2+(z8-z1)**2)
            y18=yp
            z18=zp
         endif

C
         if((sqrt((y7-y2)**2+(z7-z2)**2)).lt.epsilonl) then
           x72=xp
           y72=yp
           z72=zp
         else
           x72=x2+(x7-x2)*sqrt(yp**2+zp**2)/sqrt((y7-y2)**2+(z7-z2)**2)
           y72=yp
           z72=zp
         endif

         if(sqrt((x7-x4)**2+(z7-z4)**2).lt.epsilonl) then
           x74=xp
           y74=yp
           z74=zp
         else
           x74=xp
           y74=y4+(y7-y4)*sqrt(xp**2+zp**2)/sqrt((x7-x4)**2+(z7-z4)**2)
           z74=zp
         endif

         if((sqrt((x7-x5)**2+(y7-y5)**2)).lt.epsilonl) then
           x75=xp
           y75=yp
           z75=zp
         else
           x75=xp
           y75=yp
           z75=z5+(z7-z5)*sqrt(xp**2+yp**2)/sqrt((x7-x5)**2+(y7-y5)**2)
         endif

C
         call volume_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                   x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                   volelm)
C
        call volume_hex(x1 ,y1 ,z1 ,x12,y12,z12,x13,y13,z13,x41,y41,z41,
     *                  x15,y15,z15,x16,y16,z16,xp ,yp ,zp ,x18,y18,z18,
     *                  vol7)


        call volume_hex(x2 ,y2 ,z2 ,x23,y23,z23,x13,y13,z13,x12,y12,z12,
     *                  x26,y26,z26,x72,y72,z72,xp ,yp ,zp ,x16,y16,z16,
     *                  vol8)

        call volume_hex(x3 ,y3 ,z3 ,x34,y34,z34,x13,y13,z13,x23,y23,z23,
     *                  x37,y37,z37,x74,y74,z74,xp ,yp ,zp ,x72,y72,z72,
     *                  vol5)

        call volume_hex(x4 ,y4 ,z4 ,x41,y41,z41,x13,y13,z13,x34,y34,z34,
     *                  x48,y48,z48,x18,y18,z18,xp ,yp ,zp ,x74,y74,z74,
     *                  vol6)

        call volume_hex(x15,y15,z15,x16,y16,z16,xp ,yp ,zp, x18,y18,z18,
     *                  x5 ,y5 ,z5 ,x56,y56,z56,x75,y75,z75,x85,y85,z85,
     *                  vol3)

        call volume_hex(x26,y26,z26,x72,y72,z72,xp ,yp ,zp, x16,y16,z16,
     *                  x6 ,y6 ,z6 ,x67,y67,z67,x75,y75,z75,x56,y56,z56,
     *                  vol4)

        call volume_hex(x37,y37,z37,x74,y74,z74,xp ,yp ,zp, x72,y72,z72,
     *                  x7 ,y7 ,z7 ,x78,y78,z78,x75,y75,z75,x67,y67,z67,
     *                  vol1)

        call volume_hex(x48,y48,z48,x18,y18,z18,xp ,yp ,zp, x74,y74,z74,
     *                  x8 ,y8 ,z8 ,x85,y85,z85,x75,y75,z75,x78,y78,z78,
     *                  vol2)


         volsum=vol1+vol2+vol3+vol4+vol5+vol6+vol7+vol8

         if (volsum .lt. epsilonvol) then
           ierr_zero = 1
         endif

C        Transform the field values by the desired function
         fv1 = cinterpolate('function',cinterp,xfield(1))
         fv2 = cinterpolate('function',cinterp,xfield(2))
         fv3 = cinterpolate('function',cinterp,xfield(3))
         fv4 = cinterpolate('function',cinterp,xfield(4))
         fv5 = cinterpolate('function',cinterp,xfield(5))
         fv6 = cinterpolate('function',cinterp,xfield(6))
         fv7 = cinterpolate('function',cinterp,xfield(7))
         fv8 = cinterpolate('function',cinterp,xfield(8))

         xvalue = (fv1*vol1+fv2*vol2+
     &             fv3*vol3+fv4*vol4+
     &             fv5*vol5+fv6*vol6+
     &             fv7*vol7+fv8*vol8) / volsum

C        Do the inverse function operation
         favg = (xfield(1)+xfield(2)+xfield(3)+xfield(4)+
     *           xfield(5)+xfield(6)+xfield(7)+
     *           xfield(8))/8.0d+00
         xvalue=cinterpolate('inverse',cinterp,xvalue)
         if(cinterp(1:3).eq.'log') then
           xvalue = sign(xvalue,favg)
         endif



C     ***** INTERPOLATION ON PYRAMID *****
      elseif(ielmtyp.eq.ifelmpyr) then

         if(abs(x2-x1).lt.epsilonl) then
            x12=xp
            y12=yp
            z12=zp
         else
            x12=xp
            y12=y1+(y2-y1)*(xp-x1)/(x2-x1)
            z12=z1+(z2-z1)*(xp-x1)/(x2-x1)
         endif
         if(abs(y3-y2).lt.epsilonl) then
            x23=xp
            y23=yp
            z23=zp
         else
            x23=x2+(x3-x2)*(yp-y2)/(y3-y2)
            y23=yp
            z23=z2+(z3-z2)*(yp-y2)/(y3-y2)
         endif
         if(abs(x4-x3).lt.epsilonl) then
            x34=xp
            y34=yp
            z34=zp
         else
            x34=xp
            y34=y3+(y4-y3)*(xp-x3)/(x4-x3)
            z34=z3+(z4-z3)*(xp-x3)/(x4-x3)
         endif
         if(abs(y4-y1).lt.epsilonl) then
            x41=xp
            y41=yp
            z41=zp
         else
            x41=x1+(x4-x1)*(yp-y1)/(y4-y1)
            y41=yp
            z41=z1+(z4-z1)*(yp-y1)/(y4-y1)
         endif
C
         if(abs(x6-x5).lt.epsilonl) then
            x56=xp
            y56=yp
            z56=zp
         else
            x56=xp
            y56=y5+(y6-y5)*(xp-x5)/(x6-x5)
            z56=z5+(z6-z5)*(xp-x5)/(x6-x5)
         endif
         if(abs(y7-y6).lt.epsilonl) then
            x67=xp
            y67=yp
            z67=zp
         else
            x67=x6+(x7-x6)*(yp-y6)/(y7-y6)
            y67=yp
            z67=z6+(z7-z6)*(yp-y6)/(y7-y6)
         endif
         if(abs(x8-x7).lt.epsilonl) then
            x78=xp
            y78=yp
            z78=zp
         else
            x78=xp
            y78=y7+(y8-y7)*(xp-x7)/(x8-x7)
            z78=z7+(z8-z7)*(xp-x7)/(x8-x7)
         endif
         if(abs(y8-y5).lt.epsilonl) then
            x85=xp
            y85=yp
            z85=zp
         else
            x85=x5+(x8-x5)*(yp-y5)/(y8-y5)
            y85=yp
            z85=z5+(z8-z5)*(yp-y5)/(y8-y5)
         endif
C
         if(abs(z5-z1).lt.epsilonl) then
            x15=xp
            y15=yp
            z15=zp
         else
            x15=x1+(x5-x1)*(zp-z1)/(z5-z1)
            y15=y1+(y5-y1)*(zp-z1)/(z5-z1)
            z15=zp
         endif
         if(abs(z6-z2).lt.epsilonl) then
            x26=xp
            y26=yp
            z26=zp
         else
            x26=x2+(x6-x2)*(zp-z2)/(z6-z2)
            y26=y2+(y6-y2)*(zp-z2)/(z6-z2)
            z26=zp
         endif
         if(abs(z7-z3).lt.epsilonl) then
            x37=xp
            y37=yp
            z37=zp
         else
            x37=x3+(x7-x3)*(zp-z3)/(z7-z3)
            y37=y3+(y7-y3)*(zp-z3)/(z7-z3)
            z37=zp
         endif
         if(abs(z8-z4).lt.epsilonl) then
            x48=xp
            y48=yp
            z48=zp
         else
            x48=x4+(x8-x4)*(zp-z4)/(z8-z4)
            y48=y4+(y8-y4)*(zp-z4)/(z8-z4)
            z48=zp
         endif
C
         if(sqrt((x3-x1)**2+(y3-y1)**2).lt.epsilonl) then
            x13=xp
            y13=yp
            z13=zp
         else
            x13=xp
            y13=yp
            z13=z1+(z3-z1)*sqrt(xp**2+yp**2)/
     *                     sqrt((x3-x1)**2+(y3-y1)**2)
         endif
         if(sqrt((x6-x1)**2+(z6-z1)**2).lt.epsilonl) then
            x16=xp
            y16=yp
            z16=zp
         else
            x16=xp
            y16=y1+(y6-y1)*sqrt(xp**2+zp**2)/
     *                     sqrt((x6-x1)**2+(z6-z1)**2)
            z16=zp
         endif
         if(sqrt((y8-y1)**2+(z8-z1)**2).lt.epsilonl) then
            x18=xp
            y18=yp
            z18=zp
         else
            x18=x1+(x8-x1)*sqrt(yp**2+zp**2)/
     *                     sqrt((y8-y1)**2+(z8-z1)**2)
            y18=yp
            z18=zp
         endif
C
         if(sqrt((y7-y2)**2+(z7-z2)**2).lt.epsilonl) then
            x72=xp
            y72=yp
            z72=zp
         else
            x72=x2+(x7-x2)*sqrt(yp**2+zp**2)/
     *                     sqrt((y7-y2)**2+(z7-z2)**2)
            y72=yp
            z72=zp
         endif
         if(sqrt((x7-x4)**2+(z7-z4)**2).lt.epsilonl) then
            x74=xp
            y74=yp
            z74=zp
         else
            x74=xp
            y74=y4+(y7-y4)*sqrt(xp**2+zp**2)/
     *                     sqrt((x7-x4)**2+(z7-z4)**2)
            z74=zp
         endif
         if(sqrt((x7-x5)**2+(y7-y5)**2).lt.epsilonl) then
            x75=xp
            y75=yp
            z75=zp
         else
            x75=xp
            y75=yp
            z75=z5+(z7-z5)*sqrt(xp**2+yp**2)/
     *                     sqrt((x7-x5)**2+(y7-y5)**2)
         endif
C
         call volume_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                   x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                   volelm)
C
         call volume_hex(x1 ,y1 ,z1 ,x12,y12,z12,x13,y13,z13,
     *                   x41,y41,z41,x15,y15,z15,x16,y16,z16,
     *                   xp ,yp ,zp ,x18,y18,z18,
     *                   vol7)
         call volume_hex(x2 ,y2 ,z2 ,x23,y23,z23,x13,y13,z13,
     *                   x12,y12,z12,x26,y26,z26,x72,y72,z72,
     *                   xp ,yp ,zp ,x16,y16,z16,
     *                   vol8)
         call volume_hex(x3 ,y3 ,z3 ,x34,y34,z34,x13,y13,z13,
     *                   x23,y23,z23,x37,y37,z37,x74,y74,z74,
     *                   xp ,yp ,zp ,x72,y72,z72,
     *                   vol5)
         call volume_hex(x4 ,y4 ,z4 ,x41,y41,z41,x13,y13,z13,
     *                   x34,y34,z34,x48,y48,z48,x18,y18,z18,
     *                   xp ,yp ,zp ,x74,y74,z74,
     *                   vol6)
         call volume_hex(x15,y15,z15,x16,y16,z16, xp ,yp ,zp,
     *                   x18,y18,z18,x5 ,y5 ,z5 ,x56,y56,z56,
     *                   x75,y75,z75,x85,y85,z85,
     *                   vol3)
         call volume_hex(x26,y26,z26,x72,y72,z72, xp ,yp ,zp,
     *                   x16,y16,z16, x6 ,y6 ,z6 ,x67,y67,z67,
     *                   x75,y75,z75,x56,y56,z56,
     *                   vol4)
         call volume_hex(x37,y37,z37,x74,y74,z74, xp ,yp ,zp,
     *                   x72,y72,z72, x7 ,y7 ,z7 ,x78,y78,z78,
     *                   x75,y75,z75,x67,y67,z67,
     *                   vol1)
         call volume_hex(x48,y48,z48,x18,y18,z18, xp ,yp ,zp,
     *                   x74,y74,z74, x8 ,y8 ,z8 ,x85,y85,z85,
     *                   x75,y75,z75,x78,y78,z78,
     *                   vol2)
         volsum =  vol1+vol2+vol3+vol4+vol5+vol6+vol7+vol8

         if (volsum .lt. epsilonvol) then
           ierr_zero = 1
         endif

C        Transform the field values by the desired function
         fv1 = cinterpolate('function',cinterp,xfield(1))
         fv2 = cinterpolate('function',cinterp,xfield(2))
         fv3 = cinterpolate('function',cinterp,xfield(3))
         fv4 = cinterpolate('function',cinterp,xfield(4))
         fv5 = cinterpolate('function',cinterp,xfield(5))
         fv6 = cinterpolate('function',cinterp,xfield(6))
         fv7 = cinterpolate('function',cinterp,xfield(7))
         fv8 = cinterpolate('function',cinterp,xfield(8))
         xvalue = (fv1*vol1+fv2*vol2+
     &             fv3*vol3+fv4*vol4+
     &             fv5*vol5+fv6*vol6+
     &             fv7*vol7+fv8*vol8) / volsum

C        Do the inverse function operation
         favg = (xfield(1)+xfield(2)+xfield(3)+xfield(4)+
     *           xfield(5)+xfield(6)+xfield(7)+
     *           xfield(8))/8.0d+00
         xvalue = cinterpolate('inverse',cinterp,xvalue)
         if(cinterp(1:3).eq.'log') then
            xvalue = sign(xvalue,favg)
         endif


C     ***** INTERPOLATION ON PRISM *****
      elseif(ielmtyp.eq.ifelmpri) then

         if(abs(x2-x1).lt.epsilonl) then
            x12=xp
            y12=yp
            z12=zp
         else
            x12=xp
            y12=y1+(y2-y1)*(xp-x1)/(x2-x1)
            z12=z1+(z2-z1)*(xp-x1)/(x2-x1)
         endif
         if(abs(y3-y2).lt.epsilonl) then
            x23=xp
            y23=yp
            z23=zp
         else
            x23=x2+(x3-x2)*(yp-y2)/(y3-y2)
            y23=yp
            z23=z2+(z3-z2)*(yp-y2)/(y3-y2)
         endif
         if(abs(x4-x3).lt.epsilonl) then
            x34=xp
            y34=yp
            z34=zp
         else
            x34=xp
            y34=y3+(y4-y3)*(xp-x3)/(x4-x3)
            z34=z3+(z4-z3)*(xp-x3)/(x4-x3)
         endif
         if(abs(y4-y1).lt.epsilonl) then
            x41=xp
            y41=yp
            z41=zp
         else
            x41=x1+(x4-x1)*(yp-y1)/(y4-y1)
            y41=yp
            z41=z1+(z4-z1)*(yp-y1)/(y4-y1)
         endif
C
         if(abs(x6-x5).lt.epsilonl) then
            x56=xp
            y56=yp
            z56=zp
         else
            x56=xp
            y56=y5+(y6-y5)*(xp-x5)/(x6-x5)
            z56=z5+(z6-z5)*(xp-x5)/(x6-x5)
         endif
         if(abs(y7-y6).lt.epsilonl) then
            x67=xp
            y67=yp
            z67=zp
         else
            x67=x6+(x7-x6)*(yp-y6)/(y7-y6)
            y67=yp
            z67=z6+(z7-z6)*(yp-y6)/(y7-y6)
         endif
         if(abs(x8-x7).lt.epsilonl) then
            x78=xp
            y78=yp
            z78=zp
         else
            x78=xp
            y78=y7+(y8-y7)*(xp-x7)/(x8-x7)
            z78=z7+(z8-z7)*(xp-x7)/(x8-x7)
         endif
         if(abs(y8-y5).lt.epsilonl) then
            x85=xp
            y85=yp
            z85=zp
         else
            x85=x5+(x8-x5)*(yp-y5)/(y8-y5)
            y85=yp
            z85=z5+(z8-z5)*(yp-y5)/(y8-y5)
         endif
C
         if(abs(z5-z1).lt.epsilonl) then
            x15=xp
            y15=yp
            z15=zp
         else
            x15=x1+(x5-x1)*(zp-z1)/(z5-z1)
            y15=y1+(y5-y1)*(zp-z1)/(z5-z1)
            z15=zp
         endif
         if(abs(z6-z2).lt.epsilonl) then
            x26=xp
            y26=yp
            z26=zp
         else
            x26=x2+(x6-x2)*(zp-z2)/(z6-z2)
            y26=y2+(y6-y2)*(zp-z2)/(z6-z2)
            z26=zp
         endif
         if(abs(z7-z3).lt.epsilonl) then
            x37=xp
            y37=yp
            z37=zp
         else
            x37=x3+(x7-x3)*(zp-z3)/(z7-z3)
            y37=y3+(y7-y3)*(zp-z3)/(z7-z3)
            z37=zp
         endif
         if(abs(z8-z4).lt.epsilonl) then
            x48=xp
            y48=yp
            z48=zp
         else
            x48=x4+(x8-x4)*(zp-z4)/(z8-z4)
            y48=y4+(y8-y4)*(zp-z4)/(z8-z4)
            z48=zp
         endif

         if(sqrt((x3-x1)**2+(y3-y1)**2).lt.epsilonl) then
            x13=xp
            y13=yp
            z13=zp
         else
            x13=xp
            y13=yp
            z13=z1+(z3-z1)*sqrt(xp**2+yp**2)/
     *                      sqrt((x3-x1)**2+(y3-y1)**2)
         endif
         if(sqrt((x6-x1)**2+(z6-z1)**2).lt.epsilonl) then
            x16=xp
            y16=yp
            z16=zp
         else
            x16=xp
            y16=y1+(y6-y1)*sqrt(xp**2+zp**2)/
     *                     sqrt((x6-x1)**2+(z6-z1)**2)
            z16=zp
         endif
         if(sqrt((y8-y1)**2+(z8-z1)**2).lt.epsilonl) then
            x18=xp
            y18=yp
            z18=zp
         else
            x18=x1+(x8-x1)*sqrt(yp**2+zp**2)/
     *                     sqrt((y8-y1)**2+(z8-z1)**2)
            y18=yp
            z18=zp
         endif
C
         if(sqrt((y7-y2)**2+(z7-z2)**2).lt.epsilonl) then
            x72=xp
            y72=yp
            z72=zp
         else
            x72=x2+(x7-x2)*sqrt(yp**2+zp**2)/
     *                     sqrt((y7-y2)**2+(z7-z2)**2)
            y72=yp
            z72=zp
         endif
         if(sqrt((x7-x4)**2+(z7-z4)**2).lt.epsilonl) then
            x74=xp
            y74=yp
            z74=zp
         else
            x74=xp
            y74=y4+(y7-y4)*sqrt(xp**2+zp**2)/
     *                     sqrt((x7-x4)**2+(z7-z4)**2)
            z74=zp
         endif
         if(sqrt((x7-x5)**2+(y7-y5)**2).lt.epsilonl) then
            x75=xp
            y75=yp
            z75=zp
         else
            x75=xp
            y75=yp
            z75=z5+(z7-z5)*sqrt(xp**2+yp**2)/
     *                     sqrt((x7-x5)**2+(y7-y5)**2)
         endif

         call volume_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                   x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                   volelm)

         call volume_hex(x1 ,y1 ,z1 ,x12,y12,z12,x13,y13,z13,
     *                   x41,y41,z41,x15,y15,z15,x16,y16,z16,
     *                   xp ,yp ,zp ,x18,y18,z18,
     *                   vol7)
         call volume_hex(x2 ,y2 ,z2 ,x23,y23,z23,x13,y13,z13,
     *                   x12,y12,z12,x26,y26,z26,x72,y72,z72,
     *                   xp ,yp ,zp ,x16,y16,z16,
     *                   vol8)
         call volume_hex(x3 ,y3 ,z3 ,x34,y34,z34,x13,y13,z13,
     *                   x23,y23,z23,x37,y37,z37,x74,y74,z74,
     *                   xp ,yp ,zp ,x72,y72,z72,
     *                   vol5)
         call volume_hex(x4 ,y4 ,z4 ,x41,y41,z41,x13,y13,z13,
     *                   x34,y34,z34,x48,y48,z48,x18,y18,z18,
     *                   xp ,yp ,zp ,x74,y74,z74,
     *                   vol6)
         call volume_hex(x15,y15,z15,x16,y16,z16, xp ,yp ,zp ,
     *                   x18,y18,z18, x5 ,y5 ,z5 ,x56,y56,z56,
     *                   x75,y75,z75,x85,y85,z85,
     *                   vol3)
         call volume_hex(x26,y26,z26,x72,y72,z72, xp ,yp ,zp ,
     *                   x16,y16,z16, x6 ,y6 ,z6 ,x67,y67,z67,
     *                   x75,y75,z75,x56,y56,z56,
     *                   vol4)
         call volume_hex(x37,y37,z37,x74,y74,z74, xp ,yp ,zp ,
     *                   x72,y72,z72, x7 ,y7 ,z7 ,x78,y78,z78,
     *                   x75,y75,z75,x67,y67,z67,
     *                   vol1)
         call volume_hex(x48,y48,z48,x18,y18,z18, xp ,yp ,zp ,
     *                   x74,y74,z74, x8 ,y8 ,z8 ,x85,y85,z85,
     *                   x75,y75,z75,x78,y78,z78,
     *                   vol2)
         volsum=vol1+vol2+vol3+vol4+vol5+vol6+vol7+vol8

         if (volsum .lt. epsilonvol) then
           ierr_zero = 1
         endif

C        Transform the field values by the desired function
         fv1 = cinterpolate('function',cinterp,xfield(1))
         fv2 = cinterpolate('function',cinterp,xfield(2))
         fv3 = cinterpolate('function',cinterp,xfield(3))
         fv4 = cinterpolate('function',cinterp,xfield(4))
         fv5 = cinterpolate('function',cinterp,xfield(5))
         fv6 = cinterpolate('function',cinterp,xfield(6))
         fv7 = cinterpolate('function',cinterp,xfield(7))
         fv8 = cinterpolate('function',cinterp,xfield(8))
         xvalue = (fv1*vol1+fv2*vol2+
     *             fv3*vol3+fv4*vol4+
     *             fv5*vol5+fv6*vol6+
     *             fv7*vol7+fv8*vol8) / volsum

C        Do the inverse function operation
         favg = (xfield(1)+xfield(2)+xfield(3)+xfield(4)+
     *              xfield(5)+xfield(6)+xfield(7)+
     *              xfield(8))/8.0d+00
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

