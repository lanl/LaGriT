      subroutine getsfact(x1,y1,z1,x2,y2,z2,cmo,istype,surfdata,
     &               sheetnm,ckmin,ckmax,epsln,sfact,nsfact)
      implicit none
C
      character*132 logmess
C
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE FINDS THE S FACTOR ALONG A LINE WHERE THE LINE
C     INTERSECTS A SURFACE DEFINED BY ipsdat.  THE FACTOR IS CALCULATED
C     BY SETTING THE EQUATION OF THE LINE AND THE EQUATION OF THE
C     SURFACE EQUAL TO EACH OTHER.  THE LINEAR EQUATION IS THE SET
C     x=x1+s*(x2-x1), y=y1+s*(y2-y1) and z=z1+s(z2-z1).  THESE ARE
C     SUBSTITUTED IN THE EQUATION OF THE SURFACE AND SOLVED FOR s.
C
C
C     INPUT ARGUMENTS -
C
C        x1 - X COORDINATE OF THE FIRST POINT TO CHECK
C        y1 - Y COORDINATE OF THE FIRST POINT TO CHECK
C        z1 - Z COORDINATE OF THE FIRST POINT TO CHECK
C        x2 - X COORDINATE OF THE SECOND POINT TO CHECK
C        y2 - Y COORDINATE OF THE SECOND POINT TO CHECK
C        z2 - Z COORDINATE OF THE SECOND POINT TO CHECK
C        ipsdat - POINTER TO SURFACE DATA IN THE STORAGE BLOCK
C        ipsatt - POINTER TO SURFACE ATTRIBUTES IN STORAGE BLOCK
C        chmin - MINIMUM VALUE THAT s CAN BE
C        chmax - MAXIMUM VALUE THAT s CAN BE
C        epsln - EPSILON FOR SURFACE CHECKS
C
C
C     OUTPUT ARGUMENTS -
C
C        sfact - ARRAY OF S FACTORS FOUND FOR THIS SURFACE
C        nsfact - NO. OF S FACTORS FOUND FOR THIS SURFACE
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/getsfact_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.3   30 Sep 2004 11:16:46   dcg
CPVCS    make ckzero double precision
CPVCS    
CPVCS       Rev 1.2   Wed Apr 05 13:34:28 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.1   13 Jan 2000 14:48:02   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   04 Jan 2000 16:47:42   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.9   Mon Apr 14 16:50:02 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.8   Sat Oct 05 19:21:40 1996   kuprat
CPVCS    Corrected plane and box intersections by using EPSLN
CPVCS    tolerance instead of CKZERO.  Increased CKZERO to
CPVCS    1.e-14 from 1.e-20 --- CKZERO is still used for quadric
CPVCS    intersections.  Changed one of the EPSLN tolerances in
CPVCS    a quadric intersection calculation to CKZERO for
CPVCS    consistency.  NOTE:  A bit of analysis would probably
CPVCS    yield a better tolerance than the somewhat arbitrary
CPVCS    CKZERO for quadric intersections.
CPVCS
CPVCS       Rev 1.7   Wed Aug 14 14:48:58 1996   dcg
CPVCS    replace erroneous ismax call with iimax
CPVCS
CPVCS       Rev 1.6   Mon Jul 08 15:01:58 1996   kuprat
CPVCS    Added new GETSFACTV subroutine for vectorized s-factor computations.
CPVCS
CPVCS       Rev 1.5   08/28/95 11:36:56   ahmed
CPVCS    No change.
CPVCS
CPVCS       Rev 1.4   08/23/95 16:20:16   dcg
CPVCS    increase size of sfactor array
CPVCS
CPVCS       Rev 1.3   05/01/95 08:34:10   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.2   03/17/95 21:11:32   het
CPVCS    Add the model and dictionary calles
CPVCS
CPVCS       Rev 1.1   12/01/94 18:40:30   het
CPVCS    Change "cmo" calles to add data type
CPVCS    Alias the "decimate2d"  command to "decimate"
CPVCS    Alias the "settets" command to "mass"
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:34   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      real*8 sfact(100),ckmin,ckmax,epsln,ckzero,x1,y1,z1,
     * x2,y2,z2,rnorm,sck1,ts,xchk,ychk,zchk,xc,yc,zc,s1,s2,t1,t2,
     * sck2,xe,ye,ze,zdist,ar,br,cr,bc2,ac2,ab2,ax,bx,cx,ay,by,cy,
     * az,bz,cz,abc2,st1,st2,zchk2
      parameter(ckzero=1.0d-14)
      real*8 rt1(1000),rt2(1000),zt1(1000),zt2(1000)
      real*8 s1a(1000),s2a(1000),zv(1000),cva(1000)
      real*8 zr1t(1000),zr2t(1000),chksqrd(1000)
      real*8 aa(1000),bb(1000),cc(1000)
      real*8 a,b,c,d,denom,chksqr,cv
      real*8 xr1,yr1,zr1,xr2,yr2,zr2,r
      real*8 top1(1000),top2(1000),bot(1000)
      real*8 surfdata(*)
      logical surftst
      integer nsfact,istrt,i,iout,ntab,itype
C
      character*32 cgeom,sheetnm,istype,cmo
C
      nsfact=0
C
C     ***************************************************************
C     GET s VALUE FOR A PLANE SURFACE
C
      if (istype(1:5) .eq. 'plane') then
C
C        ------------------------------------------------------------
C        GET THE EQUATION OF THE PLANE AND SOLVE FOR s
C
         a=surfdata(10)
         b=surfdata(11)
         c=surfdata(12)
         d=surfdata(13)
         rnorm=sqrt(a*a+b*b+c*c)
         sck1=ckmin-1
         denom=a*(x2-x1)+b*(y2-y1)+c*(z2-z1)
         if (abs(denom) .gt. rnorm*epsln) sck1=(d-a*x1-b*y1-c*z1)/denom
         if (abs(sck1-ckmin) .lt. epsln) sck1=ckmin
         if (abs(sck1-ckmax) .lt. epsln) sck1=ckmax
         if (sck1.ge.ckmin .and. sck1.le.ckmax) then
            nsfact=1
            sfact(1)=sck1
         endif
C
      endif
C
C     ******************************************************************
C     GET s VALUE FOR BOX OR PARALLELPIPED TYPE
C
      if (istype(1:3) .eq. 'box' .or. istype(1:8) .eq. 'parallel') then
C
C        ---------------------------------------------------------------
C        LOOP THROUGH 6 PLANES AND FIND THE ONE BETWEEN THE POINTS
C
         istrt=12
         if (istype(1:3) .eq. 'box') istrt=6
C
         do 30 i=1,6
C
C           ............................................................
C           GET THE EQUATION OF THE PLANE AND SOLVE
C
            iout=istrt+(i-1)*4
            a=surfdata(iout+1)
            b=surfdata(iout+2)
            c=surfdata(iout+3)
            d=surfdata(iout+4)
            rnorm=sqrt(a*a+b*b+c*c)
            denom=a*(x2-x1)+b*(y2-y1)+c*(z2-z1)
            if (abs(denom) .lt. rnorm*epsln) go to 30
            ts=(d-a*x1-b*y1-c*z1)/denom
C
C           .........................................................
C           CHECK THAT THE INTERSECTION IS ON THE SURFACE AND NOT ON THE
C           PLANE OUTSIDE THE SURFACE.  THEN CHECK THAT THE SOLUTION
C           IS BETWEEN THE CHECK VALUES.
C
            if (abs(ts-ckmin) .lt. epsln) ts=ckmin
            if (abs(ts-ckmax) .lt. epsln) ts=ckmax
            xchk=x1+ts*(x2-x1)
            ychk=y1+ts*(y2-y1)
            zchk=z1+ts*(z2-z1)
            if (surftst(xchk,ychk,zchk,epsln,cmo,istype,
     *                  surfdata,sheetnm,'eq'))
     *        then
               if (ts.ge.ckmin .and. ts.le.ckmax) then
                  nsfact=nsfact+1
                  sfact(nsfact)=ts
               endif
            endif
C
   30    continue
C
      endif
C
C     ******************************************************************
C     GET s VALUE FOR A SPHERE
C
      if (istype(1:6) .eq. 'sphere') then
C
C        ---------------------------------------------------------------
C        GET CENTER AND RADIUS FROM STORAGE BLOCK.
C
         xc=surfdata(1)
         yc=surfdata(2)
         zc=surfdata(3)
         r=surfdata(4)
C
C        ---------------------------------------------------------------
C        TRANSPOSE POINTS TO CENTER OF THE SPHERE.
C
         xr1=x1-xc
         xr2=x2-xc
         yr1=y1-yc
         yr2=y2-yc
         zr1=z1-zc
         zr2=z2-zc
C
C        ---------------------------------------------------------------
C        SET UP COEFFICIENTS FOR QUADRATIC SOLUTION AND SOLVE
C
         a=(xr2-xr1)**2 + (yr2-yr1)**2 + (zr2-zr1)**2
         b=2.*((xr2-xr1)*xr1 + (yr2-yr1)*yr1 + (zr2-zr1)*zr1)
         c=xr1**2 + yr1**2 + zr1**2 - r**2
         s1=ckmin-1
         s2=ckmin-1
         chksqr=b*b-4*a*c
         if (abs(chksqr) .lt. ckzero) chksqr=0
         if (chksqr .ge. 0 .and.
     &       (abs(a) .gt. ckzero .or. abs(c) .gt. ckzero)) then
            if  (abs(a) .gt. ckzero) then
               s1=(-b + dsqrt(chksqr))/(2*a)
               s2=(-b - dsqrt(chksqr))/(2*a)
              else
               t1=(-b + dsqrt(chksqr))/(2*c)
               t2=(-b - dsqrt(chksqr))/(2*c)
               if (abs(t1) .gt. ckzero) s1=1./t1
               if (abs(t2) .gt. epsln) s2=1./t2
            endif
         endif
C
C        ---------------------------------------------------------------
C        USE THE SOLUTIONS THAT FALL BETWEEN THE CHECK VALUES
C
         sck1=min(s1,s2)
         sck2=max(s1,s2)
C
         if (abs(sck1-ckmin) .lt. epsln) sck1=ckmin
         if (abs(sck1-ckmax) .lt. epsln) sck1=ckmax
         if (sck1.ge.ckmin .and. sck1.le.ckmax) then
            nsfact=nsfact+1
            sfact(nsfact)=sck1
         endif
C
         if (abs(sck2-ckmin) .lt. epsln) sck2=ckmin
         if (abs(sck2-ckmax) .lt. epsln) sck2=ckmax
         if (sck2.ge.ckmin .and. sck2.le.ckmax) then
            nsfact=nsfact+1
            sfact(nsfact)=sck2
         endif
C
      endif
C
C     ******************************************************************
C     GET s VALUE FOR CYLINDER, CONE, ELLIPSOID OR ROTATED TABULAR
C
      if (istype(1:8) .eq. 'cylinder' .or. istype(1:4) .eq. 'cone' .or.
     &    istype(1:7) .eq. 'ellipse' .or. istype(1:7) .eq. 'tabular')
     &    then
C
C        ---------------------------------------------------------------
C        GET THE CENTER OR VERTEX POINT, THE END POINT AND THE RADIUS
C        FROM THE STORAGE BLOCK
C
         xc=surfdata(8)
         yc=surfdata(9)
         zc=surfdata(10)
         xe=surfdata(11)
         ye=surfdata(12)
         ze=surfdata(13)
         zdist=sqrt((xe-xc)**2 + (ye-yc)**2 + (ze-zc)**2)
         if (istype .ne. 'tabular') r=surfdata(14)
         if (istype .eq. 'tabular') then
            cgeom=sheetnm
         endif
         iout=14
C
C        ---------------------------------------------------------------
C        FOR A CONE, GET C CONSTANT FOR THE EQUATION OF A CONE
C
         if (istype(1:4) .eq. 'cone') then
            cv=surfdata(15)
            iout=15
         endif
C
C        ---------------------------------------------------------------
C        FOR AN ELLIPSOID, GET THE CENTER AND RADII
C
         if (istype(1:7) .eq. 'ellipse') then
            xc=surfdata(13)
            yc=surfdata(14)
            zc=surfdata(15)
            ar=surfdata(16)
            br=surfdata(17)
            cr=surfdata(18)
            iout=18
            bc2=br*br*cr*cr
            ac2=ar*ar*cr*cr
            ab2=ar*ar*br*br
            abc2=ar*ar*br*br*cr*cr
         endif
C
C        ---------------------------------------------------------------
C        FOR A ROTATED TABULAR PROFILE, SET UP TO GET THE PROFILE DATA
C
         if (istype(1:7) .eq. 'tabular') then
            ntab=surfdata(15)
            iout=ntab+15
         endif
C
C        ---------------------------------------------------------------
C        GET THE ROTATION MATRIX
C
         ax=surfdata(iout+1)
         bx=surfdata(iout+2)
         cx=surfdata(iout+3)
         ay=surfdata(iout+4)
         by=surfdata(iout+5)
         cy=surfdata(iout+6)
         az=surfdata(iout+7)
         bz=surfdata(iout+8)
         cz=surfdata(iout+9)
C
C        ---------------------------------------------------------------
C        TRANSPOSE AND ROTATE THE POINT TO THE NEW ORIGIN AND AXIS
C
         xr1=ax*(x1-xc) + bx*(y1-yc) + cx*(z1-zc)
         yr1=ay*(x1-xc) + by*(y1-yc) + cy*(z1-zc)
         zr1=az*(x1-xc) + bz*(y1-yc) + cz*(z1-zc)
         xr2=ax*(x2-xc) + bx*(y2-yc) + cx*(z2-zc)
         yr2=ay*(x2-xc) + by*(y2-yc) + cy*(z2-zc)
         zr2=az*(x2-xc) + bz*(y2-yc) + cz*(z2-zc)
C
C        ---------------------------------------------------------------
C        SET UP COEFFICIENTS FOR A QUADRATIC SOLUTION AND SOLVE
C        FOR A CYLINDER, CONE OR ELLIPSOID
C
         if (istype(1:7) .ne. 'tabular') then
            if (istype(1:8) .eq. 'cylinder') then
               a=(xr2-xr1)**2 + (yr2-yr1)**2
               b=2.*((xr2-xr1)*xr1 + (yr2-yr1)*yr1)
               c=xr1**2 + yr1**2 - r**2
             elseif (istype(1:4) .eq. 'cone') then
               a=cv**2*((xr2-xr1)**2 + (yr2-yr1)**2) -
     &           r**2*(zr2-zr1)**2
               b=2.*(cv**2*((xr2-xr1)*xr1 + (yr2-yr1)*yr1) -
     &               r**2*(zr2-zr1)*zr1)
               c=cv**2*(xr1**2 + yr1**2) - r**2*zr1**2
             else
               a=bc2*(xr2-xr1)**2 + ac2*(yr2-yr1)**2 +
     &           ab2*(zr2-zr1)**2
               b=2.*(bc2*(xr2-xr1)*xr1 + ac2*(yr2-yr1)*yr1 +
     &               ab2*(zr2-zr1)*zr1)
               c=bc2*xr1**2 + ac2*yr1**2 + ab2*zr1**2 - abc2
            endif
C
C           ............................................................
C           SOLVE THE QUADRATIC FOR s
C
            s1=ckmin-1
            s2=ckmin-1
            chksqr=b*b-4*a*c
            if (abs(chksqr) .lt. ckzero) chksqr=0
            if (chksqr .ge. 0 .and.
     &          (abs(a) .gt. ckzero .or. abs(c) .gt. ckzero)) then
               if (abs(a) .gt. ckzero) then
                  s1=(-b + dsqrt(chksqr))/(2*a)
                  s2=(-b - dsqrt(chksqr))/(2*a)
                 else
                  t1=(-b + dsqrt(chksqr))/(2*c)
                  t2=(-b - dsqrt(chksqr))/(2*c)
                  if (abs(t1) .gt. ckzero) s1=1./t1
                  if (abs(t2) .gt. ckzero) s2=1./t2
               endif
            endif
C
C           ............................................................
C           CHECK THAT THE INTERSECTION LIES BETWEEN THE END POINTS
C           FOR CYLINDER OR CONES
C
            if (istype(1:8).eq.'cylinder'.or.istype(1:4).eq.'cone') then
               zchk=zr1+s1*(zr2-zr1)
               if (zchk.lt.-epsln .or. zchk.gt.zdist+epsln) s1=ckmin-1
               zchk=zr1+s2*(zr2-zr1)
               if (zchk.lt.-epsln .or. zchk.gt.zdist+epsln) s2=ckmin-1
            endif
C
C           ............................................................
C           USE THE SOLUTIONS THAT FALL BETWEEN THE CHECK VALUES.
C
            sck1=min(s1,s2)
            sck2=max(s1,s2)
C
            if (abs(sck1-ckmin) .lt. epsln) sck1=ckmin
            if (abs(sck1-ckmax) .lt. epsln) sck1=ckmax
            if (sck1.ge.ckmin .and. sck1.le.ckmax) then
               nsfact=nsfact+1
               sfact(nsfact)=sck1
            endif
C
            if (abs(sck2-ckmin) .lt. epsln) sck2=ckmin
            if (abs(sck2-ckmax) .lt. epsln) sck2=ckmax
            if (sck2.ge.ckmin .and. sck2.le.ckmax) then
               nsfact=nsfact+1
               sfact(nsfact)=sck2
            endif
C
         endif
C
C        ---------------------------------------------------------------
C        SET UP COEFFICIENTS FOR A QUADRATIC SOLUTION AND SOLVE
C        FOR A ROTATED TABULAR PROFILE
C
         if (istype(1:7) .eq. 'tabular') then
C
C           ............................................................
C           PAIRS OF POINTS ARE PART OF A CONE OR CYLINDER.  LOOP
C           THROUGH PAIRS TO FIND ALL VALID INTERSECTIONS.
C           THE PAIRS FORM A CONE IF rt1(i) AND rt2(i) ARE DIFFERENT
C
            do 50 i=1,ntab/2-1
               rt1(i)=surfdata(16+2*(i-1))
               zt1(i)=surfdata(16+2*(i-1)+1)
               rt2(i)=surfdata(18+2*(i-1))
               zt2(i)=surfdata(18+2*(i-1)+1)
C
C              .........................................................
C              FIND THE VERTEX AND C CONSTANT IF THIS IS A CONE
C
               zv(i)=0
               if (abs(rt2(i)-rt1(i)) .gt. epsln)
     &            zv(i)=zt1(i)-rt1(i)*(zt2(i)-zt1(i))/(rt2(i)-rt1(i))
               cva(i)=dmax1(dabs(zt1(i)-zv(i)),dabs(zt2(i)-zv(i)))
               if (abs(rt2(i)-rt1(i)) .lt. epsln) cva(i)=1.
C
C              .........................................................
C              TRANSPOSE ROTATED TRANSPOSED POINTS TO VERTEX
C
               zr1t(i)=zr1-zv(i)
               if (abs(rt2(i)-rt1(i)) .lt. epsln) zr1t(i)=1.
               zr2t(i)=zr2-zv(i)
               if (abs(rt2(i)-rt1(i)) .lt. epsln) zr2t(i)=1.
   50       continue
C
C           ............................................................
C           SET UP QUADRATIC COEFFICIENTS FOR BOTH CYLINDER AND CONE
C           AND SOLVE
C
            do 51 i=1,ntab/2-1
               r=dmax1(rt1(i),rt2(i))
               aa(i)=cva(i)**2*((xr2-xr1)**2 + (yr2-yr1)**2) -
     &           r**2*(zr2t(i)-zr1t(i))**2
               bb(i)=2.*(cva(i)**2*((xr2-xr1)*xr1 + (yr2-yr1)*yr1) -
     &               r**2*(zr2t(i)-zr1t(i))*zr1t(i))
               cc(i)=cva(i)**2*(xr1**2 + yr1**2) - r**2*zr1t(i)**2
C
C              .........................................................
C              SOLVE THE QUADRATIC FOR s.
C
               chksqrd(i)=bb(i)*bb(i)-4*aa(i)*cc(i)
               if (abs(chksqrd(i)) .lt. ckzero) chksqrd(i)=0
               top1(i)=(-bb(i) + dsqrt(dabs(chksqrd(i))))
               if (chksqrd(i) .lt. 0) top1(i)=ckmin-1
               top2(i)=(-bb(i) - dsqrt(dabs(chksqrd(i))))
               if (chksqrd(i) .lt. 0) top2(i)=ckmin-1
               bot(i)=2.*aa(i)
               if (abs(aa(i)) .lt. ckzero) bot(i)=2.*cc(i)
               if (abs(bot(i)) .lt. ckzero) top1(i)=ckmin-1.
               if (abs(bot(i)) .lt. ckzero) top2(i)=ckmin-1.
               if (abs(bot(i)) .lt. ckzero) bot(i)=1.
               if (chksqrd(i) .lt. 0) bot(i)=1.
               s1a(i)=top1(i)/bot(i)
               s2a(i)=top2(i)/bot(i)
               if (abs(bot(i)-2.*cc(i)) .lt. ckzero  .and.
     &             abs(s1a(i)) .gt. ckzero) s1a(i)=1./s1a(i)
               if (abs(bot(i)-2.*cc(i)) .lt. ckzero  .and.
     &             abs(s2a(i)) .gt. ckzero) s2a(i)=1./s2a(i)
C
   51       continue
C
C           ............................................................
C           IF ZT1 AND ZT2 ARE THE SAME, THEN THE PAIRS FORM A FLAT
C           PLANE.  SOLVE FOR A PLANE WHERE a, AND b ARE 0 c IS 1
C           AND d IS zt1(i).
C
            do 52 i=1,ntab/2-1
               if (abs(zt2(i)-zt1(i)) .lt. epsln) s1a(i)=ckmin-1
               if (abs(zt2(i)-zt1(i)) .lt. epsln) s2a(i)=ckmin-1
               if ((abs(zt2(i)-zt1(i)) .lt. epsln) .and.
     &             (abs(zr2-zr1) .gt. ckzero))
     &            s1a(i)=(zt1(i)-zr1)/(zr2-zr1)
   52       continue
C
C           ....................................................................
C           USE THE SOLUTIONS THAT FALL BETWEEN THE CHECK VALUES
C
            do 55 i=1,ntab/2-1
               sck1=min(s1a(i),s2a(i))
               sck2=max(s1a(i),s2a(i))
               if (abs(sck1-ckmin) .lt. epsln) sck1=ckmin
               if (abs(sck1-ckmax) .lt. epsln) sck1=ckmax
               if (abs(sck2-ckmin) .lt. epsln) sck2=ckmin
               if (abs(sck2-ckmax) .lt. epsln) sck2=ckmax
               st1=ckmin-1
               st2=ckmin-1
               if (sck1.ge.ckmin .and. sck1.le.ckmax) st1=sck1
               if (sck2.ge.ckmin .and. sck2.le.ckmax) st2=sck2
C
C
C              ......................................................
C              CHECK THAT THE INTERSECTIONS ARE ON AN INTERFACE
C
               if (st1.ge.ckmin .and. st1.le.ckmax) then
                  zchk2=zr1+st1*(zr2-zr1)
                  if ((zt1(i)-epsln.le.zchk2 .and.
     &                 zchk2.le.zt2(i)+epsln)    .or.
     &                 (zt2(i)-epsln.le.zchk2 .and.
     &                  zchk2.le.zt1(i)+epsln))     then
                      zchk=z1+st1*(z2-z1)
                      xchk=x1+st1*(x2-x1)
                      ychk=y1+st1*(y2-y1)
                      if (surftst(xchk,ychk,zchk,epsln,
     &                   cmo,itype,surfdata,sheetnm, 'eq')) then
                         nsfact=nsfact+1
                         sfact(nsfact)=st1
                      endif
                  endif
               endif
C
               if (st2.ge.ckmin .and. st2.le.ckmax) then
                  zchk2=zr1+st2*(zr2-zr1)
                  if ((zt1(i)-epsln.le.zchk2 .and.
     &                 zchk2.le.zt2(i)+epsln)   .or.
     &                (zt2(i)-epsln.le.zchk2 .and.
     &                 zchk2.le.zt1(i)+epsln))   then
                      zchk=z1+st2*(z2-z1)
                      xchk=x1+st2*(x2-x1)
                      ychk=y1+st2*(y2-y1)
                      if (surftst(xchk,ychk,zchk,epsln,
     &                   cmo,itype,surfdata,sheetnm,'eq')) then
                         nsfact=nsfact+1
                         sfact(nsfact)=st2
                      endif
                  endif
               endif
C
C
   55       continue
C
         endif
C
      endif
C
C     ***************************************************************
C     GET s VALUES FOR A SHEET SURFACE
C
      if (istype(1:5) .eq. 'sheet')
     &   call shtgtsf(x1,y1,z1,x2,y2,z2,sheetnm,ckmin,ckmax,
     &                epsln,sfact,nsfact)
C
C
C     ******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      goto 9999
 9999 continue
C
      return
      end
C
C
      subroutine getsfactv(x1,y1,z1,x2,y2,z2,nvec,cmo,istype,surfdata,
     &               sheetnm,ckmin,ckmax,epsln,sfact,nsfact)
C
C
C#######################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE FINDS THE S FACTORS ALONG nvec LINES WHERE THE LINES
C     INTERSECTS A SURFACE DEFINED BY ipsdat.  BECAUSE DYNAMIC MEMORY
C     ALLOCATION IS NOT USED IN THIS SUBROUTINE, WE MUST HAVE THAT
C     nvec IS LESS THAN OR EQUAL TO kveclen IN machine.h.
C     THE FACTORS ARE CALCULATED
C     BY SETTING THE EQUATION OF EACH LINE AND THE EQUATION OF THE
C     SURFACE EQUAL TO EACH OTHER.  THE LINEAR EQUATION IS THE SET
C     x=x1+s*(x2-x1), y=y1+s*(y2-y1) and z=z1+s(z2-z1).  THESE ARE
C     SUBSTITUTED IN THE EQUATION OF THE SURFACE AND SOLVED FOR s.
C
C
C     INPUT ARGUMENTS -
C
C        x1 - X COORDINATES OF THE FIRST POINT TO CHECK
C        y1 - Y COORDINATES OF THE FIRST POINT TO CHECK
C        z1 - Z COORDINATES OF THE FIRST POINT TO CHECK
C        x2 - X COORDINATES OF THE SECOND POINT TO CHECK
C        y2 - Y COORDINATES OF THE SECOND POINT TO CHECK
C        z2 - Z COORDINATES OF THE SECOND POINT TO CHECK
C        nvec - NUMBER OF LINES TO INTERSECT WITH SURFACE
C        ipsdat - POINTER TO SURFACE DATA IN THE STORAGE BLOCK
C        ipsatt - POINTER TO SURFACE ATTRIBUTES IN STORAGE BLOCK
C        chmin - MINIMUM VALUE THAT s CAN BE
C        chmax - MAXIMUM VALUE THAT s CAN BE
C        epsln - EPSILON FOR SURFACE CHECKS
C
C
C     OUTPUT ARGUMENTS -
C
C        sfact - ARRAY OF S FACTORS FOUND FOR THIS SURFACE, FOR EACH
c LINE
C        nsfact - ARRAY OF NO. OF S FACTORS FOUND FOR THIS SURFACE
C                 FOR EACH LINE
C
C
C     CHANGE HISTORY -
C
C        $Log$
C
C#######################################################################
C
      implicit none
C
      include 'machine.h'
 
      integer lenptr
      parameter (lenptr=1000000)
 
      real*8 sfact(100,lenptr), x1(lenptr), y1(lenptr), z1(lenptr),
     &   x2(lenptr), y2(lenptr), z2(lenptr)
      real*8 xchk(6*KVECLEN), ychk(6*KVECLEN), zchk(6*KVECLEN),
     &   tsv(6*KVECLEN)
      integer nsfact(lenptr),isurftst(6*KVECLEN),line(6*KVECLEN)
 
      real*8 a,b,c,d,denom,chksqr,cv,rnorm,surfdata(*)
      real*8 aa,bb,cc,xr1,yr1,zr1,xr2,yr2,zr2,cva,r
      real*8 chksqrd,zv,rt1,rt2,zt1,zt2,zr1t,zr2t,ckzero,
     &   ckmin,ckmax,epsln,sck1,ts,xc,yc,zc,s1,s2,t1,t2,sck2,xe,
     &   ye,ze,zdist,ar,br,cr,bc2,ac2,ab2,abc2,ax,bx,cx,ay,by,cy,
     &   az,bz,cz,top1,top2,bot,s1a,s2a,st1,st2,zchk1,zchk2
      integer nvec,nwdat,i,j,istrt,ivec,iout,k,ntab,ioff,loclen,
     &   ierrw,iimax
      parameter(ckzero=1.0d-14)
      character*132 logmess
C
      character*32  istype, cgeom, cmo, sheetnm
C
C#######################################################################
C
      do i=1,nvec
         nsfact(i)=0
      enddo
C
C     ***************************************************************
C     GET s VALUE FOR A PLANE SURFACE
C
      if (istype(1:5) .eq. 'plane') then
C
C        ------------------------------------------------------------
C        GET THE EQUATION OF THE PLANE AND SOLVE FOR s
C
         a=surfdata(10)
         b=surfdata(11)
         c=surfdata(12)
         d=surfdata(13)
         rnorm=sqrt(a*a+b*b+c*c)
         do i=1,nvec
            sck1=ckmin-1
            denom=a*(x2(i)-x1(i))+b*(y2(i)-y1(i))+c*(z2(i)-z1(i))
            if (abs(denom) .gt. rnorm*epsln)
     &         sck1=(d-a*x1(i)-b*y1(i)-c*z1(i))/denom
            if (abs(sck1-ckmin) .lt. epsln) sck1=ckmin
            if (abs(sck1-ckmax) .lt. epsln) sck1=ckmax
            if (sck1.ge.ckmin .and. sck1.le.ckmax) then
               nsfact(i)=1
               sfact(1,i)=sck1
            endif
         enddo
C
      endif
C
C     ******************************************************************
C     GET s VALUE FOR BOX OR PARALLELPIPED TYPE
C
      if (istype(1:3) .eq. 'box' .or. istype(1:8) .eq. 'parallel') then
C
C        ---------------------------------------------------------------
C        LOOP THROUGH 6 PLANES AND FIND THE ONE BETWEEN THE POINTS
C
         istrt=12
         if (istype(1:3) .eq. 'box') istrt=6
C
         do ioff=0,nvec-1,KVECLEN
            loclen=min(KVECLEN,nvec-ioff)
            ivec=0
            do i=1,loclen
               do 30 j=1,6
C
C           ............................................................
C           GET THE EQUATION OF THE PLANE AND SOLVE
C
                  iout=istrt+(j-1)*4
                  a=surfdata(iout+1)
                  b=surfdata(iout+2)
                  c=surfdata(iout+3)
                  d=surfdata(iout+4)
                  rnorm=sqrt(a*a+b*b+c*c)
                  denom=a*(x2(i+ioff)-x1(i+ioff))+b*(y2(i+ioff)-y1(i
     &               +ioff))+c*(z2(i+ioff)-z1(i+ioff))
                  if (abs(denom) .lt. rnorm*epsln) goto 30
 
                  ts=(d-a*x1(i+ioff)-b*y1(i+ioff)-c*z1(i+ioff))/denom
C
C           .........................................................
C           CHECK THAT THE INTERSECTION IS ON THE SURFACE AND NOT ON THE
C           PLANE OUTSIDE THE SURFACE.  THEN CHECK THAT THE SOLUTION
C           IS BETWEEN THE CHECK VALUES.
C
                  if (abs(ts-ckmin) .lt. epsln) ts=ckmin
                  if (abs(ts-ckmax) .lt. epsln) ts=ckmax
                  if (ts.lt.ckmin .or. ts.gt.ckmax) goto 30
                  ivec=ivec+1
                  tsv(ivec)=ts
                  xchk(ivec)=x1(i+ioff)+ts*(x2(i+ioff)-x1(i+ioff))
                  ychk(ivec)=y1(i+ioff)+ts*(y2(i+ioff)-y1(i+ioff))
                  zchk(ivec)=z1(i+ioff)+ts*(z2(i+ioff)-z1(i+ioff))
                  line(ivec)=i+ioff
 30            continue
            enddo
            call surftstv(xchk,ychk,zchk,ivec,epsln,cmo,istype,
     *         surfdata,sheetnm,'eq'
     &         ,isurftst)
            do k=1,ivec
               if (isurftst(k).eq.1) then
                  nsfact(line(k))=nsfact(line(k))+1
                  sfact(nsfact(line(k)),line(k))=tsv(k)
               endif
            enddo
         enddo
C
      endif
C
C     ******************************************************************
C     GET s VALUE FOR A SPHERE
C
      if (istype(1:6) .eq. 'sphere') then
C
C        ---------------------------------------------------------------
C        GET CENTER AND RADIUS FROM STORAGE BLOCK.
C
         xc=surfdata(1)
         yc=surfdata(2)
         zc=surfdata(3)
         r=surfdata(4)
C
C        ---------------------------------------------------------------
C        TRANSPOSE POINTS TO CENTER OF THE SPHERE.
C
         do i=1,nvec
            xr1=x1(i)-xc
            xr2=x2(i)-xc
            yr1=y1(i)-yc
            yr2=y2(i)-yc
            zr1=z1(i)-zc
            zr2=z2(i)-zc
C
C        ---------------------------------------------------------------
C        SET UP COEFFICIENTS FOR QUADRATIC SOLUTION AND SOLVE
C
            a=(xr2-xr1)**2 + (yr2-yr1)**2 + (zr2-zr1)**2
            b=2.*((xr2-xr1)*xr1 + (yr2-yr1)*yr1 + (zr2-zr1)*zr1)
            c=xr1**2 + yr1**2 + zr1**2 - r**2
            s1=ckmin-1
            s2=ckmin-1
            chksqr=b*b-4*a*c
            if (abs(chksqr) .lt. ckzero) chksqr=0
            if (chksqr .ge. 0 .and.
     &         (abs(a) .gt. ckzero .or. abs(c) .gt. ckzero)) then
               if  (abs(a) .gt. ckzero) then
                  s1=(-b + dsqrt(chksqr))/(2*a)
                  s2=(-b - dsqrt(chksqr))/(2*a)
               else
                  t1=(-b + dsqrt(chksqr))/(2*c)
                  t2=(-b - dsqrt(chksqr))/(2*c)
                  if (abs(t1) .gt. ckzero) s1=1./t1
                  if (abs(t2) .gt. ckzero) s2=1./t2
               endif
            endif
C
C        ---------------------------------------------------------------
C        USE THE SOLUTIONS THAT FALL BETWEEN THE CHECK VALUES
C
            sck1=min(s1,s2)
            sck2=max(s1,s2)
C
            if (abs(sck1-ckmin) .lt. epsln) sck1=ckmin
            if (abs(sck1-ckmax) .lt. epsln) sck1=ckmax
            if (sck1.ge.ckmin .and. sck1.le.ckmax) then
               nsfact(i)=nsfact(i)+1
               sfact(nsfact(i),i)=sck1
            endif
C
            if (abs(sck2-ckmin) .lt. epsln) sck2=ckmin
            if (abs(sck2-ckmax) .lt. epsln) sck2=ckmax
            if (sck2.ge.ckmin .and. sck2.le.ckmax) then
               nsfact(i)=nsfact(i)+1
               sfact(nsfact(i),i)=sck2
            endif
         enddo
C
      endif
C
C     ******************************************************************
C     GET s VALUE FOR CYLINDER, CONE, ELLIPSOID OR ROTATED TABULAR
C
      if (istype(1:8) .eq. 'cylinder' .or. istype(1:4) .eq. 'cone' .or.
     &   istype(1:7) .eq. 'ellipse' .or. istype(1:7) .eq. 'tabular')
     &   then
C
C        ---------------------------------------------------------------
C        GET THE CENTER OR VERTEX POINT, THE END POINT AND THE RADIUS
C        FROM THE STORAGE BLOCK
C
         xc=surfdata(8)
         yc=surfdata(9)
         zc=surfdata(10)
         xe=surfdata(11)
         ye=surfdata(12)
         ze=surfdata(13)
         zdist=sqrt((xe-xc)**2 + (ye-yc)**2 + (ze-zc)**2)
         if (istype .ne. 'tabular') r=surfdata(14)
         if (istype .eq. 'tabular') then
            cgeom=sheetnm
         endif
         iout=14
C
C        ---------------------------------------------------------------
C        FOR A CONE, GET C CONSTANT FOR THE EQUATION OF A CONE
C
         if (istype(1:4) .eq. 'cone') then
            cv=surfdata(15)
            iout=15
         endif
C
C        ---------------------------------------------------------------
C        FOR AN ELLIPSOID, GET THE CENTER AND RADII
C
         if (istype(1:7) .eq. 'ellipse') then
            xc=surfdata(13)
            yc=surfdata(14)
            zc=surfdata(15)
            ar=surfdata(16)
            br=surfdata(17)
            cr=surfdata(18)
            iout=18
            bc2=br*br*cr*cr
            ac2=ar*ar*cr*cr
            ab2=ar*ar*br*br
            abc2=ar*ar*br*br*cr*cr
         endif
C
C        ---------------------------------------------------------------
C        FOR A ROTATED TABULAR PROFILE, SET UP TO GET THE PROFILE DATA
C
         if (istype(1:7) .eq. 'tabular') then
            ntab=surfdata(15)
            iout=ntab+15
         endif
C
C        ---------------------------------------------------------------
C        GET THE ROTATION MATRIX
C
         ax=surfdata(iout+1)
         bx=surfdata(iout+2)
         cx=surfdata(iout+3)
         ay=surfdata(iout+4)
         by=surfdata(iout+5)
         cy=surfdata(iout+6)
         az=surfdata(iout+7)
         bz=surfdata(iout+8)
         cz=surfdata(iout+9)
C
C        ---------------------------------------------------------------
C        SET UP COEFFICIENTS FOR A QUADRATIC SOLUTION AND SOLVE
C        FOR A CYLINDER, CONE OR ELLIPSOID
C
         if (istype(1:7) .ne. 'tabular') then
C
C        ---------------------------------------------------------------
C        TRANSPOSE AND ROTATE THE POINT TO THE NEW ORIGIN AND AXIS
C
            do i=1,nvec
               xr1=ax*(x1(i)-xc) + bx*(y1(i)-yc) + cx*(z1(i)-zc)
               yr1=ay*(x1(i)-xc) + by*(y1(i)-yc) + cy*(z1(i)-zc)
               zr1=az*(x1(i)-xc) + bz*(y1(i)-yc) + cz*(z1(i)-zc)
               xr2=ax*(x2(i)-xc) + bx*(y2(i)-yc) + cx*(z2(i)-zc)
               yr2=ay*(x2(i)-xc) + by*(y2(i)-yc) + cy*(z2(i)-zc)
               zr2=az*(x2(i)-xc) + bz*(y2(i)-yc) + cz*(z2(i)-zc)
               if (istype(1:8) .eq. 'cylinder') then
                  a=(xr2-xr1)**2 + (yr2-yr1)**2
                  b=2.*((xr2-xr1)*xr1 + (yr2-yr1)*yr1)
                  c=xr1**2 + yr1**2 - r**2
               elseif (istype(1:4) .eq. 'cone') then
                  a=cv**2*((xr2-xr1)**2 + (yr2-yr1)**2) -
     &               r**2*(zr2-zr1)**2
                  b=2.*(cv**2*((xr2-xr1)*xr1 + (yr2-yr1)*yr1) -
     &               r**2*(zr2-zr1)*zr1)
                  c=cv**2*(xr1**2 + yr1**2) - r**2*zr1**2
               else
                  a=bc2*(xr2-xr1)**2 + ac2*(yr2-yr1)**2 +
     &               ab2*(zr2-zr1)**2
                  b=2.*(bc2*(xr2-xr1)*xr1 + ac2*(yr2-yr1)*yr1 +
     &               ab2*(zr2-zr1)*zr1)
                  c=bc2*xr1**2 + ac2*yr1**2 + ab2*zr1**2 - abc2
               endif
C
C           ............................................................
C           SOLVE THE QUADRATIC FOR s
C
               s1=ckmin-1
               s2=ckmin-1
               chksqr=b*b-4*a*c
               if (abs(chksqr) .lt. ckzero) chksqr=0
               if (chksqr .ge. 0 .and.
     &            (abs(a) .gt. ckzero .or. abs(c) .gt. ckzero)) then
                  if (abs(a) .gt. ckzero) then
                     s1=(-b + dsqrt(chksqr))/(2*a)
                     s2=(-b - dsqrt(chksqr))/(2*a)
                  else
                     t1=(-b + dsqrt(chksqr))/(2*c)
                     t2=(-b - dsqrt(chksqr))/(2*c)
                     if (abs(t1) .gt. ckzero) s1=1./t1
                     if (abs(t2) .gt. ckzero) s2=1./t2
                  endif
               endif
C
C           ............................................................
C           CHECK THAT THE INTERSECTION LIES BETWEEN THE END POINTS
C           FOR CYLINDER OR CONES
C
               if (istype(1:8).eq.'cylinder'.or.istype(1:4).eq.'cone'
     &            ) then
                  zchk1=zr1+s1*(zr2-zr1)
                  if (zchk1.lt.-epsln .or. zchk1.gt.zdist+epsln) s1
     &               =ckmin-1
                  zchk1=zr1+s2*(zr2-zr1)
                  if (zchk1.lt.-epsln .or. zchk1.gt.zdist+epsln) s2
     &               =ckmin-1
               endif
C
C           ............................................................
C           USE THE SOLUTIONS THAT FALL BETWEEN THE CHECK VALUES.
C
               sck1=min(s1,s2)
               sck2=max(s1,s2)
C
               if (abs(sck1-ckmin) .lt. epsln) sck1=ckmin
               if (abs(sck1-ckmax) .lt. epsln) sck1=ckmax
               if (sck1.ge.ckmin .and. sck1.le.ckmax) then
                  nsfact(i)=nsfact(i)+1
                  sfact(nsfact(i),i)=sck1
               endif
C
               if (abs(sck2-ckmin) .lt. epsln) sck2=ckmin
               if (abs(sck2-ckmax) .lt. epsln) sck2=ckmax
               if (sck2.ge.ckmin .and. sck2.le.ckmax) then
                  nsfact(i)=nsfact(i)+1
                  sfact(nsfact(i),i)=sck2
               endif
C
            enddo
         endif
C
C        ---------------------------------------------------------------
C        SET UP COEFFICIENTS FOR A QUADRATIC SOLUTION AND SOLVE
C        FOR A ROTATED TABULAR PROFILE
C
         if (istype(1:7) .eq. 'tabular') then
C
C           ............................................................
C           PAIRS OF POINTS ARE PART OF A CONE OR CYLINDER.  LOOP
C           THROUGH PAIRS TO FIND ALL VALID INTERSECTIONS.
C           THE PAIRS FORM A CONE IF rt1(i) AND rt2(i) ARE DIFFERENT
C
            do j=1,ntab/2-1
               rt1=surfdata(16+2*(j-1))
               zt1=surfdata(16+2*(j-1)+1)
               rt2=surfdata(18+2*(j-1))
               zt2=surfdata(18+2*(j-1)+1)
C
C              .........................................................
C              FIND THE VERTEX AND C CONSTANT IF THIS IS A CONE
C
               zv=0
               if (abs(rt2-rt1) .gt. epsln)
     &            zv=zt1-rt1*(zt2-zt1)/(rt2-rt1)
               cva=dmax1(dabs(zt1-zv),dabs(zt2-zv))
               if (abs(rt2-rt1) .lt. epsln) cva=1.
C
C        ---------------------------------------------------------------
C        TRANSPOSE AND ROTATE THE POINT TO THE NEW ORIGIN AND AXIS
C
               do ioff=0,nvec-1,KVECLEN
                  loclen=min(KVECLEN,nvec-ioff)
                  ivec=0
                  do i=1,loclen
                     xr1=ax*(x1(i+ioff)-xc) + bx*(y1(i+ioff)-yc) + cx
     &                  *(z1(i+ioff)-zc)
                     yr1=ay*(x1(i+ioff)-xc) + by*(y1(i+ioff)-yc) + cy
     &                  *(z1(i+ioff)-zc)
                     zr1=az*(x1(i+ioff)-xc) + bz*(y1(i+ioff)-yc) + cz
     &                  *(z1(i+ioff)-zc)
                     xr2=ax*(x2(i+ioff)-xc) + bx*(y2(i+ioff)-yc) + cx
     &                  *(z2(i+ioff)-zc)
                     yr2=ay*(x2(i+ioff)-xc) + by*(y2(i+ioff)-yc) + cy
     &                  *(z2(i+ioff)-zc)
                     zr2=az*(x2(i+ioff)-xc) + bz*(y2(i+ioff)-yc) + cz
     &                  *(z2(i+ioff)-zc)
C
C              .........................................................
C              TRANSPOSE ROTATED TRANSPOSED POINTS TO VERTEX
C
                     zr1t=zr1-zv
                     if (abs(rt2-rt1) .lt. epsln) zr1t=1.
                     zr2t=zr2-zv
                     if (abs(rt2-rt1) .lt. epsln) zr2t=1.
C
C           ............................................................
C           SET UP QUADRATIC COEFFICIENTS FOR BOTH CYLINDER AND CONE
C           AND SOLVE
C
                     r=dmax1(rt1,rt2)
                     aa=cva**2*((xr2-xr1)**2 + (yr2-yr1)**2) -
     &                  r**2*(zr2t-zr1t)**2
                     bb=2.*(cva**2*((xr2-xr1)*xr1 + (yr2-yr1)*yr1) -
     &                  r**2*(zr2t-zr1t)*zr1t)
                     cc=cva**2*(xr1**2 + yr1**2) - r**2*zr1t**2
C
C              .........................................................
C              SOLVE THE QUADRATIC FOR s.
C
                     chksqrd=bb*bb-4*aa*cc
                     if (abs(chksqrd) .lt. ckzero) chksqrd=0
                     top1=(-bb + dsqrt(dabs(chksqrd)))
                     if (chksqrd .lt. 0) top1=ckmin-1
                     top2=(-bb - dsqrt(dabs(chksqrd)))
                     if (chksqrd .lt. 0) top2=ckmin-1
                     bot=2.*aa
                     if (abs(aa) .lt. ckzero) bot=2.*cc
                     if (abs(bot) .lt. ckzero) top1=ckmin-1.
                     if (abs(bot) .lt. ckzero) top2=ckmin-1.
                     if (abs(bot) .lt. ckzero) bot=1.
                     if (chksqrd .lt. 0) bot=1.
                     s1a=top1/bot
                     s2a=top2/bot
                     if (abs(bot-2.*cc) .lt. ckzero  .and.
     &                  abs(s1a) .gt. ckzero) s1a=1./s1a
                     if (abs(bot-2.*cc) .lt. ckzero  .and.
     &                  abs(s2a) .gt. ckzero) s2a=1./s2a
C
C           ............................................................
C           IF ZT1 AND ZT2 ARE THE SAME, THEN THE PAIRS FORM A FLAT
C           PLANE.  SOLVE FOR A PLANE WHERE a, AND b ARE 0 c IS 1
C           AND d IS zt1.
C
                     if (abs(zt2-zt1) .lt. epsln) s1a=ckmin-1
                     if (abs(zt2-zt1) .lt. epsln) s2a=ckmin-1
                     if ((abs(zt2-zt1) .lt. epsln) .and.
     &                  (abs(zr2-zr1) .gt. ckzero))
     &                  s1a=(zt1-zr1)/(zr2-zr1)
C
C           ............................................................
c ........
C           USE THE SOLUTIONS THAT FALL BETWEEN THE CHECK VALUES
C
                     sck1=min(s1a,s2a)
                     sck2=max(s1a,s2a)
                     if (abs(sck1-ckmin) .lt. epsln) sck1=ckmin
                     if (abs(sck1-ckmax) .lt. epsln) sck1=ckmax
                     if (abs(sck2-ckmin) .lt. epsln) sck2=ckmin
                     if (abs(sck2-ckmax) .lt. epsln) sck2=ckmax
                     st1=ckmin-1
                     st2=ckmin-1
                     if (sck1.ge.ckmin .and. sck1.le.ckmax) st1=sck1
                     if (sck2.ge.ckmin .and. sck2.le.ckmax) st2=sck2
C
C
C              ......................................................
C              CHECK THAT THE INTERSECTIONS ARE ON AN INTERFACE
C
                     if (st1.ge.ckmin .and. st1.le.ckmax) then
                        zchk2=zr1+st1*(zr2-zr1)
                        if ((zt1-epsln.le.zchk2 .and.
     &                     zchk2.le.zt2+epsln)    .or.
     &                     (zt2-epsln.le.zchk2 .and.
     &                     zchk2.le.zt1+epsln))     then
                           ivec=ivec+1
                           line(ivec)=i+ioff
                           tsv(ivec)=st1
                           zchk(ivec)=z1(i+ioff)+st1*(z2(i+ioff)-z1(i
     &                        +ioff))
                           xchk(ivec)=x1(i+ioff)+st1*(x2(i+ioff)-x1(i
     &                        +ioff))
                           ychk(ivec)=y1(i+ioff)+st1*(y2(i+ioff)-y1(i
     &                        +ioff))
                        endif
                     endif
C
                     if (st2.ge.ckmin .and. st2.le.ckmax) then
                        zchk2=zr1+st2*(zr2-zr1)
                        if ((zt1-epsln.le.zchk2 .and.
     &                     zchk2.le.zt2+epsln)   .or.
     &                     (zt2-epsln.le.zchk2 .and.
     &                     zchk2.le.zt1+epsln))   then
                           ivec=ivec+1
                           line(ivec)=i+ioff
                           tsv(ivec)=st2
                           zchk(ivec)=z1(i+ioff)+st2*(z2(i+ioff)-z1(i
     &                        +ioff))
                           xchk(ivec)=x1(i+ioff)+st2*(x2(i+ioff)-x1(i
     &                        +ioff))
                           ychk(ivec)=y1(i+ioff)+st2*(y2(i+ioff)-y1(i
     &                        +ioff))
                        endif
                     endif
                  enddo
                  call surftstv(xchk,ychk,zchk,ivec,epsln,cmo,
     *              istype,surfdata,sheetnm,
     &               'eq',isurftst)
                  do k=1,ivec
                     if (isurftst(k).eq.1) then
                        nsfact(line(k))=nsfact(line(k))+1
                        sfact(nsfact(line(k)),line(k))=tsv(k)
                     endif
                  enddo
               enddo
            enddo
         endif
      endif
C
C     ***************************************************************
C     GET s VALUES FOR A SHEET SURFACE
C
      if (istype(1:5) .eq. 'sheet')
     &   call shtgtsfp(x1,y1,z1,x2,y2,z2,nvec,sheetnm,ckmin,ckmax,
     &   epsln,sfact,nsfact)
 
C        ---------------------------------------------------------------
C        CHECK THAT MAXIMUM NUMBER OF INTERSECTIONS WITH SURFACE IS
C        NOT EXCEEDED.
C
C
         if (nsfact(iimax(nvec,nsfact,1)).gt.100) then
            write(logmess,9010)
 9010       format('ERROR:  MORE THAN 100 INTERSECTIONS ')
            call writloga('default',0,logmess,0,ierrw)
            go to 9999
         endif
C
C
C     ******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      goto 9999
 9999 continue
C
      return
      end
