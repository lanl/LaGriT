      subroutine surftstv(x,y,z,npts,epsln,cmo,istype,surfdata,sheetnm,
     &                    ickin,isurftst)
      implicit none
C
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE DETERMINES WHETHER A POINT IS LT, GT, OR EQ TO
C     SURFACE.  LT HAS A NEGATIVE VALUE WHEN THE EQUATION
C     OF THE SURFACE IS EVALUATED.  EQ IS DEFINED AS BEING WITHIN AN
C     epsln OF THE SURFACE CALCULATION.
C
C
C     INPUT ARGUMENTS -
C
C        x - X COORDINATE OF THE POINTS TO CHECK
C        y - Y COORDINATE OF THE POINTS TO CHECK
C        z - Z COORDINATE OF THE POINTS TO CHECK
C        npts - NO. OF POINTS TO CHECK
C        epsln - EPSILON FOR SURFACE CHECKS
C        surfdata - SURFACE DATA IN THE STORAGE BLOCK
c        sheetnm - name of sheet for sheet type
c        istype - type of surface
C        ipsatt - POINTER TO THE SURFACE ATTRIBUTES THE STORAGE BLOCK
C        ickin - TYPE OF CHECK TO PERFORM (lt, gt OR eq)
C
C
C     OUTPUT ARGUMENTS -
C
C        isurftst - TEST VALUE PER POINT (0-FALSE, 1-TRUE)
C
C
C     CHANGE HISTORY -
C
C        $Log: surftstv.f,v $
C        Revision 2.00  2007/11/09 20:04:04  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Wed Apr 05 13:35:08 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.2   Thu Mar 09 08:57:22 2000   dcg
CPVCS    pass sheetname from surftst to shttst
CPVCS    
CPVCS       Rev 1.1   04 Jan 2000 16:48:14   dcg
CPVCS     
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 17:02:26 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   Mon Jul 08 15:18:38 1996   kuprat
CPVCS    Corrected bug by replacing appearances of ICKIN with ICKIN(1:2).
CPVCS    
CPVCS       Rev 1.2   05/01/95 08:34:42   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS    
CPVCS       Rev 1.1   12/01/94 18:40:52   het
CPVCS    Change "cmo" calles to add data type
CPVCS    Alias the "decimate2d"  command to "decimate"
CPVCS    Alias the "settets" command to "mass"
CPVCS
CPVCS       Rev 1.0   11/10/94 12:19:08   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      integer npts
      real*8 x(npts),y(npts),z(npts)
      integer isurftst(npts)
C
      real*8 rchk(256),rt(20,256),a,b,c,d,xc,yc,zc,r,xe,ye,ze,zdist,
     *  epsln,ar,br,cr,ax,bx,cx,ay,by,cy,az,bz,cz,rfirst,zfirst,
     *  rlast,zlast,zf1,zstart,zend,z1prev,zf2,z1,z2,r1,r2,z1diff,
     *  z2diff,rt2,rt1,rf1,rf2
      integer ifound(256),ifound1(256)
      character*8 ickout(256),ickin
      real*8 rtmp(256),rprv(256),tstit(256),xr(256),yr(256),zr(256)
      character*8 lt,le,gt,ge,eq
      real*8 surfdata(*)
      character*32 sheetnm,cmo
      character*32  istype, cgeom
      integer i,jp,i2,istrt,it,iout,ntab,isopen,izflg,ipkflg,
     *  ii,ismin,ifchk,intvl
C
      lt='lt'
      le='le'
      gt='gt'
      ge='ge'
      eq='eq'
C
C     ******************************************************************
C     INITIALIZE RETURN ARRAY.
C
      do 5 i=1,npts
         isurftst(i)=0
    5 continue
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
C
C        ---------------------------------------------------------------
C        LOOP THROUGH THE POINTS IN GROUPS OF 256
C
         do 20 jp=1,npts,256
            i2=256
            if ((jp+i2) .gt. npts) i2=npts-jp+1
C
            do 10 i=1,i2
               tstit(i)=a*x(jp+i-1) + b*y(jp+i-1) + c*z(jp+i-1) - d
               ickout(i)=gt
               if (tstit(i) .lt. -epsln) ickout(i)=lt
               if (abs(tstit(i)) .le. epsln) ickout(i)=eq
C
C              .........................................................
C              SET RETURN ARRAY
C
               if ((ickin(1:2).eq.le) .and.
     &               (ickout(i).eq.lt .or. ickout(i).eq.eq))
     &            ickout(i)=le
               if ((ickin(1:2).eq.ge) .and.
     &               (ickout(i).eq.gt .or. ickout(i).eq.eq))
     &            ickout(i)=ge
C
               if (ickin(1:2) .eq. ickout(i)) isurftst(jp+i-1)=1
   10       continue
   20    continue
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
C
C        ---------------------------------------------------------------
C        LOOP THROUGH THE POINTS IN GROUPS OF 256
C
         do 50 jp=1,npts,256
            i2=256
            if ((jp+i2) .gt. npts) i2=npts-jp+1
C
            do 25 i=1,i2
               ickout(i)=lt
   25       continue
C
C           ............................................................
C           LOOP THROUGH THE 6 PLANES FOR EACH GROUP OF 256
C
            do 40 it=1,6
C
C              .........................................................
C              GET THE EQUATION OF THE PLANE AND SUBSTITUTE.
C
               iout=istrt+(it-1)*4
               a=surfdata(iout+1)
               b=surfdata(iout+2)
               c=surfdata(iout+3)
               d=surfdata(iout+4)
C
               do 30 i=1,i2
                  tstit(i)=a*x(jp+i-1) + b*y(jp+i-1) + c*z(jp+i-1) - d
                  if (tstit(i) .gt. epsln) ickout(i)=gt
                  if (abs(tstit(i)).le.epsln .and. ickout(i).eq.lt)
     &               ickout(i)=eq
   30          continue
   40       continue
C
C           ............................................................
C           SET RETURN ARRAY
C
            do 45 i=1,i2
               if ((ickin(1:2).eq.le(1:2)) .and.
     &               (ickout(i).eq.lt .or. ickout(i).eq.eq))
     &            ickout(i)=le
               if ((ickin(1:2).eq.ge(1:2)) .and.
     &               (ickout(i).eq.gt .or. ickout(i).eq.eq))
     &            ickout(i)=ge
C
               if (ickin(1:2) .eq. ickout(i)) isurftst(jp+i-1)=1
   45       continue
C
   50    continue
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
         r =surfdata(4)
C
C        ---------------------------------------------------------------
C        LOOP THROUGH THE POINTS IN GROUPS OF 256
C
         do 80 jp=1,npts,256
            i2=256
            if ((jp+i2) .gt. npts) i2=npts-jp+1
C
            do 70 i=1,i2
               tstit(i)=(x(jp+i-1)-xc)**2 + (y(jp+i-1)-yc)**2 +
     &                  (z(jp+i-1)-zc)**2 - r*r
               ickout(i)=gt
               if (tstit(i) .lt. -epsln) ickout(i)=lt
               if (abs(tstit(i)) .le. epsln) ickout(i)=eq
C
C              .........................................................
C              SET RETURN ARRAY
C
               if ((ickin(1:2).eq.le) .and.
     &               (ickout(i).eq.lt .or. ickout(i).eq.eq))
     &            ickout(i)=le
               if ((ickin(1:2).eq.ge) .and.
     &               (ickout(i).eq.gt .or. ickout(i).eq.eq))
     &            ickout(i)=ge
C
               if (ickin(1:2) .eq. ickout(i)) isurftst(jp+i-1)=1
   70       continue
   80    continue
C
      endif
C
C     ******************************************************************
C     TEST CYLINDER, CONE, ELLIPSE AND ROTATED TABULAR PROFILE TYPES
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
C        LOOP THROUGH THE POINTS IN GROUPS OF 256
C
         do 200 jp=1,npts,256
            i2=256
            if ((jp+i2) .gt. npts) i2=npts-jp+1
C
C
C           ............................................................
C           TRANSPOSE AND ROTATE THE POINT TO THE NEW ORIGIN AND AXIS
C
            do 85 i=1,i2
               xr(i)=ax*(x(jp+i-1)-xc) + bx*(y(jp+i-1)-yc) +
     &               cx*(z(jp+i-1)-zc)
               yr(i)=ay*(x(jp+i-1)-xc) + by*(y(jp+i-1)-yc) +
     &               cy*(z(jp+i-1)-zc)
               zr(i)=az*(x(jp+i-1)-xc) + bz*(y(jp+i-1)-yc) +
     &               cz*(z(jp+i-1)-zc)
   85       continue
C
C           ............................................................
C           TEST CYLINDER WITH ROTATED, TRANSPOSED POINT.
C
            if (istype(1:8) .eq. 'cylinder') then
               do 86 i=1,i2
                      tstit(i)=xr(i)*xr(i) + yr(i)*yr(i) - r*r
   86          continue
            endif
C
C           ............................................................
C           TEST CONE WITH ROTATED, TRANSPOSED POINT.
C
            if (istype(1:4) .eq. 'cone') then
               do 87 i=1,i2
                  tstit(i)=(xr(i)*xr(i))/(r*r) + (yr(i)*yr(i))/(r*r) -
     &                                 (zr(i)*zr(i))/(c*c)
   87          continue
            endif
C
C           ............................................................
C           TEST ELLIPSOID WITH ROTATED, TRANSPOSED POINT.
C
            if (istype(1:7) .eq. 'ellipse') then
               do 88 i=1,i2
                  tstit(i)=(xr(i)*xr(i))/(ar*ar) +
     &                     (yr(i)*yr(i))/(br*br) +
     &                     (zr(i)*zr(i))/(cr*cr) - 1.
   88          continue
            endif
C
C           ............................................................
C           TEST END POINTS FOR CYLINDER AND CONE.
C
            if (istype(1:8).eq.'cylinder'.or.istype(1:4).eq.'cone') then
               do 90 i=1,i2
                  if (zr(i).lt.-epsln .or. zr(i).gt.zdist+epsln)
     &               tstit(i)=epsln+1
   90          continue
            endif
C
C           ............................................................
C           TEST ROTATED TABULAR PROFILE WITH ROTATED, TRANSPOSED POINT.
C
            if (istype(1:7) .eq. 'tabular') then
C
C              .........................................................
C              CALCULATE RADIUS TO CHECK AND GET TABULAR ENDS
C
               do 95 i=1,i2
                  rchk(i)=sqrt(xr(i)*xr(i) + yr(i)*yr(i))
                  tstit(i)=epsln+1.
                  ifound(i)=0
                  rprv(i)=0
   95          continue
               rfirst=surfdata(15+1)
               zfirst=surfdata(15+2)
               rlast=surfdata(15+ntab-1)
               zlast=surfdata(15+ntab)
               zf1=zfirst
               zstart=min(zfirst,zlast) - epsln
               zend=max(zfirst,zlast) + epsln
C
C              .........................................................
C              SEE IF THIS IS AN OPEN SURFACE
C
               isopen=0
               if ((abs(zfirst-zlast).gt.epsln) .or.
     &             (abs(rfirst-rlast).gt.epsln)) isopen=1
C
C              .........................................................
C              LOOP THROUGH TABULAR SEGMENTS
C
               do 130 it=1,ntab/2-1
C
C                 ......................................................
C                 GET SEGMENT ENDS AND ORDER THEM
C
                  z1prev=zf1
                  rf1=surfdata(16+2*(it-1))
                  zf1=surfdata(16+2*(it-1)+1)
                  rf2=surfdata(18+2*(it-1))
                  zf2=surfdata(18+2*(it-1)+1)
                  z1=min(zf1,zf2)-epsln
                  z2=max(zf1,zf2)+epsln
                  r1=min(rf1,rf2)-epsln
                  r2=max(rf1,rf2)+epsln
C
C                 ......................................................
C                 SEE WHICH POINTS LIE WITHIN THE Z PAIR
C
                  izflg=0
                  do 100 i=1,i2
                     ifound1(i)=0
                     if (z1.le.zr(i) .and. zr(i).le.z2) ifound1(i)=1
                     if (z1.le.zr(i) .and. zr(i).le.z2) izflg=izflg+1
  100             continue
C
C                 ......................................................
C                 IF NO POINTS LIE WITHIN THE Z PAIR, SKIP THIS SEGMENT
C
                  if (izflg .lt. 1) go to 130
C
C                 ......................................................
C                 IF THIS IS A HORIZONTAL SURFACE, SEE IF ANY OF THE
C                 POINTS LIE ON THIS SURFACE
C
                  if (abs(zf2-zf1) .le. epsln) then
                     do 110 i=1,i2
                        if (ifound1(i).eq.1 .and.
     &                      (r1.le.rchk(i).and.rchk(i).le.r2))
     &                     tstit(i)=0
  110                continue
                  endif
C
C                 ......................................................
C                 SEE IF THE PREVIOUS SEGMENT AND THIS ONE FORM A PEAK
C
                  ipkflg=0
                  if (it .gt. 1) then
                     z1diff=zf1-z1prev
                     z2diff=zf1-zf2
                     if (abs(z1diff).gt.epsln .and.
     &                   abs(z1diff).gt.epsln .and.
     &                   (z1diff*z2diff).gt.0) ipkflg=1
                  endif
C
C                 ......................................................
C                 CALCULATE INTERSECTION RADII AND SAVE
C
                  do 120 i=1,i2
                     ifound(i)=ifound(i)+ifound1(i)
                     rtmp(i)=rf2
                     if (abs(zf2-zf1) .gt. epsln)
     &                   rtmp(i)=rf1+(rf2-rf1)*
     &                            ((zr(i)-zf1)/(zf2-zf1))
C
C                    ................................................
C                    DO NOT COUNT CORNERS MORE THAN ONCE EXCEPT PEAKS
C
                     if ((ifound(i) .gt. 1) .and.
     &                   (ifound1(i) .eq. 1) .and.
     &                   (abs(rprv(i)-rtmp(i)) .le. epsln
     &                   .and. ipkflg .eq. 0))
     &                  ifound(i)=ifound(i)-1
  120             continue
                  do 125 i=1,i2
                     if (ifound1(i) .eq. 1) rt(ifound(i),i)=rtmp(i)
                     if (ifound1(i) .eq. 1) rprv(i)=rtmp(i)
  125             continue
C
  130          continue
C
C              .........................................................
C              SEE WHICH RADIUS INTERVAL EACH POINT LIES IN
C
               do 150 i=1,i2
                  if (ifound(i) .eq. 0) go to 150
C
                  intvl=0
                  rt2=-1.
                  do 140 it=1,ifound(i)
                     rt1=rt2
                     ii=ismin(ifound(i),rt(1,i),1)
                     rt2=rt(ii,i)
                     rt(ii,i)=1.0d+99
C
                     if (rt1-epsln.le.rchk(i) .and.
     &                   rchk(i).le.rt2+epsln) then
                        intvl=it
C
C                       ................................................
C                       MAKE SURE THAT A POINT IN THE FIRST INTERVAL
C                       OF AN OPEN SURFACE IS INSIDE
C
                        if (intvl.eq.1 .and. tstit(i).gt.0 .and.
     &                      isopen.eq.1) then
                           if (zr(i).ge.zstart .and. zr(i).le.zend)
     &                        tstit(i)=-(epsln+1.0)
                        endif
C
C                       ................................................
C                       SEE IF THE POINT IS ON THE SURFACE
C
                        if (rt1.ge.0 .and. abs(rt1-rchk(i)).le.epsln)
     &                     tstit(i)=0.
                        if (abs(rt2-rchk(i)) .le. epsln) tstit(i)=0.
C
                     endif
C
  140             continue
C
C                 ......................................................
C                 IF ifound IS EVEN, THEN EVEN INTERVALS ARE INSIDE
C                 IF ifound IS ODD, THEN ODD INTERVALS ARE INSIDE
C
                  if (tstit(i) .gt. 0) then
                     ifchk=ifound(i)
                     if (mod(ifchk,2).eq.0 .and. mod(intvl,2).eq.0)
     &                   tstit(i)=-tstit(i)
                     if (mod(ifchk,2).eq.1 .and. mod(intvl,2).eq.1)
     &                   tstit(i)=-tstit(i)
                  endif
                  if (intvl .eq. 0) tstit(i)=epsln+1
C
C                 ......................................................
C                 CHECK END POINTS OF OPEN SURFACES
C
C                 if (tstit(i) .ne. 0 .and.
C    &               ((abs(zfirst-zlast).gt.epsln) .or.
C    &                (abs(rfirst-rlast).gt.epsln))) then
C                    zstrt=zfirst-epsln
C                    rstrt=rfirst-epsln
C                    zend=zlast+epsln
C                    rend=rlast-epsln
C                    if ((zr(i).lt.zstrt .and. rchk.lt.rstrt) .or.
C    &                   (zr(i).gt.zend .and. rchk.lt.rend))
C    &                  tstit(i)=-(epsln+1)
C                 endif
C
  150          continue
C
            endif
C
C           ............................................................
C           SET return FOR CYLINDER, CONE OR ROTATED TABULAR
C
            do 160 i=1,i2
               ickout(i)=gt
               if (tstit(i) .lt. -epsln) ickout(i)=lt
               if (abs(tstit(i)) .le. epsln) ickout(i)=eq
C
C              .........................................................
C              SET RETURN ARRAY
C
               if ((ickin(1:2).eq.le) .and.
     &               (ickout(i)(1:2).eq.lt .or. ickout(i)(1:2).eq.eq))
     &            ickout(i)=le
               if ((ickin(1:2).eq.ge) .and.
     &               (ickout(i)(1:2).eq.gt .or. ickout(i)(1:2).eq.eq))
     &            ickout(i)=ge
C
               if (ickin(1:2) .eq. ickout(i)(1:2)) isurftst(jp+i-1)=1
  160       continue
C
  200    continue
C
      endif
C
C     ******************************************************************
C     TEST SHEET SURFACE
C
      if (istype(1:5) .eq. 'sheet')
     &   call shttstv(x,y,z,npts,epsln,sheetnm,ickin,isurftst)
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
