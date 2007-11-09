      function surftst(x,y,z,epsln,cmo,istype,surfdata,sheetnm,ickin)
      implicit none
C
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE DETERMINES WHETHER A POINT IS LT, GT, OR EQ TO
C     SURFACE.  LT IS DEFINED AS HAVING A NEGATIVE VALUE WHEN THE
C     EQUATION OF THE SURFACE IS EVALUATED.  EQ IS DEFINED AS BEING
C     WITHIN AN epsln OF THE SURFACE CALCULATION.
C
C
C     INPUT ARGUMENTS -
C
C        x - X COORDINATE OF THE POINT TO CHECK
C        y - Y COORDINATE OF THE POINT TO CHECK
C        z - Z COORDINATE OF THE POINT TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
c        cmo - current mesh object name
c        istype - surface type
C        surfdata - surface definition
c        sheetnm - sheet or tabular surface name
C        ickin - TYPE OF CHECK TO PERFORM (lt, gt OR eq)
C
C
C     OUTPUT ARGUMENTS -
C
C        surftst - RETURNED AS A LOGICAL FUNCTION
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/surftst_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.2   Thu Mar 09 09:03:16 2000   dcg
CPVCS    pass sheet name correctly to sheet test routine
CPVCS    
CPVCS       Rev 1.1   04 Jan 2000 16:48:14   dcg
CPVCS     
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 17:02:24 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Tue Apr 30 10:26:02 1996   dcg
CPVCS    replace literal f.p. arguments with variables from consts.h
CPVCS
CPVCS       Rev 1.4   Mon Apr 29 14:37:30 1996   dcg
CPVCS    change call from cvmgt to cvmgtr to match argument types
CPVCS
CPVCS       Rev 1.3   05/01/95 08:34:38   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.2   12/09/94 22:42:30   het
CPVCS    Added the call to kmprsnrrr().
CPVCS
CPVCS
CPVCS       Rev 1.1   12/01/94 18:40:50   het
CPVCS    Change "cmo" calles to add data type
CPVCS    Alias the "decimate2d"  command to "decimate"
CPVCS    Alias the "settets" command to "mass"
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:04   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      include 'consts.h'
      logical surftst
C
      dimension zf1(1000),zf2(1000),rf1(1000),rf2(1000),rt(1000)
      real*8 surfdata(*)
      character*32  istype, cgeom, sheetnm, cmo
      character*8 ickin, ickout
      integer intvl,ii,ismin,ipkflg,istrt,i,iout,ntab,ifound,n,
     *  ifound1
      real*8 a,b,d,c,xc,yc,zc,r,xe,ye,ze,zdist,ar,br,cr,ax,bx,cx,
     *  ay,by,cy,az,bz,cz,xr,yr,zr,rchk,rfirst,zfirst,rlast,zlast,
     *  zstart,zend,z1,z2,cvmgtr,r1,r2,rt1,rt2,z1diff,z2diff,tstit,
     *  rt,rf2,rf1,zf1,epsln,x,y,z,zf2
C
C     ******************************************************************
C     TEST PLANE SURFACE
C
      if (istype(1:5) .eq. 'plane') then
C
C        ---------------------------------------------------------------
C        GET THE EQUATION OF THE PLANE AND SUBSTITUTE TO GET THE SIGN
C
         a=surfdata(10)
         b=surfdata(11)
         c=surfdata(12)
         d=surfdata(13)
         tstit=a*x + b*y + c*z - d
         ickout='gt'
         if (tstit .lt. -epsln) ickout='lt'
         if (abs(tstit) .le. epsln) ickout='eq'
C
      endif
C
C     ******************************************************************
C     TEST BOX OR PARALLELPIPED TYPE
C
      if (istype(1:3) .eq. 'box' .or. istype(1:8) .eq. 'parallel') then
C
C        ---------------------------------------------------------------
C        LOOP THROUGH 6 PLANES AND FIND THOSE OUT OR ON THE SURFACE
C
         istrt=12
         if (istype(1:3) .eq. 'box') istrt=6
         ickout='lt'
C
         do 10 i=1,6
C
C           ............................................................
C           GET THE EQUATION OF THE PLANE AND SUBSTITUTE.
C
            iout=istrt+(i-1)*4
            a=surfdata(iout+1)
            b=surfdata(iout+2)
            c=surfdata(iout+3)
            d=surfdata(iout+4)
            tstit=a*x + b*y + c*z - d
            if (tstit .gt. epsln) ickout='gt'
            if (abs(tstit).le.epsln .and. ickout.eq.'lt') ickout='eq'
C
   10    continue
C
      endif
C
C     ******************************************************************
C     TEST SPHERE
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
         tstit=(x-xc)**2 + (y-yc)**2 + (z-zc)**2 - r*r
         ickout='gt'
         if (tstit .lt. -epsln) ickout='lt'
         if (abs(tstit) .le. epsln) ickout='eq'
C
      endif
C
C     ******************************************************************
C     TEST CYLINDER, CONE, ELLIPSOID AND ROTATED TABULAR PROFILE TYPES
C
      if (istype(1:8).eq.'cylinder' .or. istype(1:4).eq.'cone' .or.
     &    istype(1:7).eq.'ellipse'  .or. istype(1:7).eq.'tabular') then
C
C        ---------------------------------------------------------------
C        GET THE CENTER OR VERTEX POINT, THE END POINT, AND THE RADIUS
C        FROM THE STORAGE BLOCK
C
         xc=surfdata(8)
         yc=surfdata(9)
         zc=surfdata(10)
         xe=surfdata(11)
         ye=surfdata(12)
         ze=surfdata(13)
         zdist=sqrt((xe-xc)**2 + (ye-yc)**2 + (ze-zc)**2)
         if (istype(1:7) .ne. 'tabular') r=surfdata(14)
         if (istype(1:7) .eq. 'tabular') then
            cgeom=sheetnm
         endif
         iout=14
C
C        ---------------------------------------------------------------
C        FOR A CONE, GET C CONSTANT FOR THE EQUATION OF A CONE
C
         if (istype(1:4) .eq. 'cone') then
            c=surfdata(15)
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
         xr=ax*(x-xc) + bx*(y-yc) + cx*(z-zc)
         yr=ay*(x-xc) + by*(y-yc) + cy*(z-zc)
         zr=az*(x-xc) + bz*(y-yc) + cz*(z-zc)
C
C        ---------------------------------------------------------------
C        TEST CYLINDER WITH ROTATED, TRANSPOSED POINT.
C
         if (istype(1:8) .eq. 'cylinder') tstit=xr*xr + yr*yr - r*r
C
C        ---------------------------------------------------------------
C        TEST CONE WITH ROTATED, TRANSPOSED POINT.
C
         if (istype(1:4) .eq. 'cone') tstit=(xr*xr)/(r*r) +
     &                                      (yr*yr)/(r*r) -
     &                                      (zr*zr)/(c*c)
C
C        ---------------------------------------------------------------
C        TEST ELLIPSOID WITH ROTATED, TRANSPOSED POINT.
C
         if (istype(1:7) .eq. 'ellipse')
     &      tstit=(xr*xr)/(ar*ar) + (yr*yr)/(br*br) + (zr*zr)/(cr*cr)
     &            - 1.
C
C        ---------------------------------------------------------------
C        TEST END POINTS FOR CYLINDER AND CONE.
C
         if ((istype(1:8).eq.'cylinder' .or. istype(1:4).eq.'cone').and.
     &       (zr.lt.-epsln .or. zr.gt.zdist+epsln)) tstit=epsln+1
C
C        ---------------------------------------------------------------
C        TEST ROTATED TABULAR PROFILE WITH ROTATED, TRANSPOSED POINT.
C
         if (istype(1:7) .eq. 'tabular') then
C
C           ............................................................
C           CALCULATE RADIUS TO CHECK AND GET TABULAR ENDS
C
            ifound=0
            rchk=sqrt(xr*xr + yr*yr)
            tstit=epsln+1.
            rfirst=surfdata(15+1)
            zfirst=surfdata(15+2)
            rlast=surfdata(15+ntab-1)
            zlast=surfdata(15+ntab)
            zstart=min(zfirst,zlast) - epsln
            zend=max(zfirst,zlast) + epsln
C
C           ............................................................
C           FIND Z PAIRS THAT THE POINT LIES BETWEEN AND INTERPOLATE TO
C           GET INTERSECTION RADII
C
            do 30 i=1,ntab/2-1
               rf1(i)=surfdata(16+2*(i-1))
               zf1(i)=surfdata(16+2*(i-1)+1)
               rf2(i)=surfdata(18+2*(i-1))
               zf2(i)=surfdata(18+2*(i-1)+1)
C
C              .........................................................
C              FIND z PAIRS THE POINT LIES BETWEEN
C
               z1=min(zf1(i),zf2(i))-epsln
               z2=max(zf1(i),zf2(i))+epsln
               rt(i)=cvmgtr(one,zero,(z1.le.zr .and. zr.le.z2))
   30       continue
C
C           ............................................................
C           COMPRESS ARRAYS
C
            n=ntab/2-1
            call kmprsnrrr(n,rt,1,rf1,1,rf1,1,ifound1)
            call kmprsnrrr(n,rt,1,zf1,1,zf1,1,ifound1)
            call kmprsnrrr(n,rt,1,rf2,1,rf2,1,ifound1)
            call kmprsnrrr(n,rt,1,zf2,1,zf2,1,ifound1)
C
C           ............................................................
C           CHECK THAT ANY INTERVAL FOUND
C
            if (ifound1 .lt. 1) then
               tstit=epsln+1.
               go to 50
            endif
C
C           ............................................................
C           CALCULATE INTERSECTION RADII
C
            ifound=0
            do 35 i=1,ifound1
               ifound=ifound+1
               rt(ifound)=rf2(i)
               if (abs(zf2(i)-zf1(i)) .gt. epsln)
     &             rt(ifound)=rf1(i)+(rf2(i)-rf1(i))*
     &                        ((zr-zf1(i))/(zf2(i)-zf1(i)))
C
C              ......................................................
C              SEE IF THE POINT IS ON A HORIZONTAL SURFACE
C
               r1=min(rf1(i),rf2(i))-epsln
               r2=max(rf1(i),rf2(i))+epsln
               if ((abs(zf2(i)-zf1(i)) .le. epsln) .and.
     &             (r1.le.rchk .and. rchk.le.r2)) tstit=0
C
C              ......................................................
C              DO NOT COUNT CORNERS MORE THAN ONCE EXCEPT PEAKS
C
               if (ifound .gt. 1) then
C
C                 ...................................................
C                 SEE IF WE ARE ON A PEAK
C
                  ipkflg=0
                  if ((abs(rf1(i)-rf2(i-1)) .lt. epsln) .and.
     &                (abs(zf1(i)-zf2(i-1)) .lt. epsln)) then
                     z1diff=zf1(i)-zf1(i-1)
                     z2diff=zf1(i)-zf2(i)
                     if (abs(z1diff).gt.epsln .and.
     &                   abs(z2diff).gt.epsln .and.
     &                   (z1diff*z2diff).gt.0) ipkflg=1
                  endif
C
C                 ...................................................
C                 COUNT NON-PEAK CORNERS ONCE
C
                  if (abs(rt(ifound-1)-rt(ifound)) .le. epsln .and.
     &                ipkflg .eq. 0) ifound=ifound-1
               endif
C
   35       continue
C
C
C           ............................................................
C           SEE WHICH RADIUS INTERVAL THE POINT LIES IN
C
            intvl=0
            rt2=-1.
            do 40 i=1,ifound
               rt1=rt2
               ii=ismin(ifound,rt,1)
               rt2=rt(ii)
               z1=zf1(ii)
               z2=zf2(ii)
               r1=min(rf1(ii),rf2(ii))
               r2=max(rf1(ii),rf2(ii))
               rt(ii)=1.0d+99
C
               if (rt1-epsln.le.rchk .and. rchk.le.rt2+epsln) then
                  intvl=i
C
C                 ...................................................
C                 MAKE SURE THAT A POINT IN THE FIRST INTERVAL OF AN
C                 OPEN SURFACE IS INSIDE
C
                  if (intvl.eq.1 .and. tstit.gt.0 .and.
     &               ((abs(zfirst-zlast).gt.epsln) .or.
     &                (abs(rfirst-rlast).gt.epsln))) then
                     if (zr.ge.zstart .and. zr.le.zend)
     &                  tstit=-(epsln+1)
                  endif
C
C                 ...................................................
C                 SEE IF THE POINT IS ON THE SURFACE
C
                  if (rt1.ge.0 .and. abs(rt1-rchk).le.epsln)
     &               tstit=0.
                  if (abs(rt2-rchk) .le. epsln) tstit=0.
C
               endif
C
   40       continue
C
C           ............................................................
C           IF ifound IS EVEN, EVEN INTERVALS ARE INSIDE THE SURFACE
C           IF ifound IS ODD, ODD INTERVALS ARE INSIDE THE SURFACE
C
            if (tstit .gt. 0) then
               if (mod(ifound,2).eq.0 .and. mod(intvl,2).eq.0)
     &             tstit=-tstit
               if (mod(ifound,2).eq.1 .and. mod(intvl,2).eq.1)
     &             tstit=-tstit
            endif
            if (intvl .eq. 0) tstit=epsln+1
C
C           ............................................................
C           CHECK END POINTS OF OPEN SURFACES
C
C           if (tstit .ne. 0 .and.
C    &         ((abs(zfirst-zlast).gt.epsln) .or.
C    &          (abs(rfirst-rlast).gt.epsln))) then
C              zstrt=zfirst-epsln
C              rstrt=rfirst-epsln
C              zend=zlast+epsln
C              rend=rlast-epsln
C              if ((zr.lt.zstrt .and. rchk.lt.rstrt) .or.
C    &             (zr.gt.zend .and. rchk.lt.rend))
C    &            tstit=-(epsln+1)
C           endif
C
         endif
C
   50    ickout='gt'
         if (tstit .lt. -epsln) ickout='lt'
         if (abs(tstit) .le. epsln) ickout='eq'
C
      endif
C
C     ******************************************************************
C     TEST SHEET SURFACE
C
      if (istype(1:5) .eq. 'sheet')
     &   call shttst(x,y,z,epsln,sheetnm,ickout)
C
C     ******************************************************************
C     SET RETURN
C
      if ((ickin(1:2).eq.'le') .and.
     &   (ickout(1:2).eq.'lt' .or. ickout(1:2).eq.'eq'))
     &   ickout='le'
      if ((ickin(1:2).eq.'ge') .and.
     &   (ickout(1:2).eq.'gt' .or. ickout(1:2).eq.'eq'))
     &   ickout='ge'
C
      surftst=(ickin(1:2) .eq. ickout(1:2))
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
