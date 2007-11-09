      subroutine shtgtsfp(x1,y1,z1,x2,y2,z2,npts,cmoin,
     &                    ckmin,ckmax,epsln,sfact,nsfact)
C
C########################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE FINDS THE S FACTOR ALONG A SET OF LINES WHERE
C     THE LINES INTERSECTS A SHEET SURFACE.
C
C
C     INPUT ARGUMENTS -
C
C        x1 - X COORDINATE OF THE FIRST SET OF POINTS TO CHECK
C        y1 - Y COORDINATE OF THE FIRST SET OF POINTS TO CHECK
C        z1 - Z COORDINATE OF THE FIRST SET OF POINTS TO CHECK
C        x2 - X COORDINATE OF THE SECOND SET OF POINTS TO CHECK
C        y2 - Y COORDINATE OF THE SECOND SET OF POINTS TO CHECK
C        z2 - Z COORDINATE OF THE SECOND SET OF POINTS TO CHECK
C        npts - NO. OF PAIRS OF POINTS TO CHECK
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
c$$$C        angle - ANGLE FORMED BY THE SURFACE NORMAL AND THE RAY.
C
C
C     CHANGE HISTORY -
C
C        $Log: shtgtsfp.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   30 Sep 2004 11:24:30   dcg
CPVCS    make chzero double precision
CPVCS
CPVCS       Rev 1.2   Wed Apr 05 13:35:04 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.1   Tue Feb 08 13:32:28 2000   dcg
CPVCS
CPVCS       Rev 1.11   Mon Apr 14 17:01:38 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.10   Mon Jul 08 15:17:42 1996   kuprat
CPVCS    Suppressed unwanted computation of angle between surface normal
CPVCS    and ray.
CPVCS
CPVCS       Rev 1.9   Wed Jun 12 14:49:32 1996   kuprat
CPVCS    Removed error message generated when ray completely misses
CPVCS    sheet and kD tree returns no triangles.
CPVCS
CPVCS       Rev 1.8   Wed Feb 28 18:12:50 1996   ahmed
CPVCS    fixed epsilon
CPVCS
CPVCS       Rev 1.7   Tue Feb 13 14:42:54 1996   ahmed
CPVCS    base the algorithm on k-D search
CPVCS
CPVCS       Rev 1.6   11/07/95 17:26:28   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.5   10/18/95 12:20:16   het
CPVCS    Allow greater that 8 character names for sheets in the surface command.
CPVCS
CPVCS       Rev 1.4   08/31/95 10:06:52   ahmed
CPVCS    Adjust the equation for computing interface points
CPVCS
CPVCS       Rev 1.3   08/28/95 11:25:48   ahmed
CPVCS    implement new definitions of inside outside wrt. sheets
CPVCS
C
C
C########################################################################
C
      implicit none
C
      integer lenptr
      real*8 ckzero,ckmin,ckmax,epsln
C
      parameter (lenptr=1000000)
      parameter (ckzero=1.0d-12)
C
C########################################################################
C
C
      pointer (ipsck1, sck1)
c$$$      pointer (ipack1, ack1)
      pointer (ipae, ae)
      pointer (ipbe, be)
      pointer (ipce, ce)
      pointer (ipde, de)
      pointer (ipitfound, itfound)
C
      integer npts
      real*8 sck1(lenptr)
      real*8 ae(lenptr),be(lenptr),ce(lenptr),de(lenptr)
      real*8 x1(npts),y1(npts),z1(npts),x2(npts),y2(npts),z2(npts)
      real*8 sfact(100,npts)
      integer nsfact(npts)
      integer itfound(lenptr)
C
C     SET POINTERS FOR THE SHEET cmo
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      pointer (iplinkt, linkt)
      pointer (ipsbox, sbox)
      pointer (ipitet, itet)
C
      real*8 xic(lenptr),yic(lenptr),zic(lenptr),sbox(lenptr)
      integer   linkt(lenptr),itet(3,lenptr)
C
      character*32 cmoin
      character*32 isubname
      integer ics,k,i,ierror,nelements,ilen,icmotype,itri,
     *  i1,i2,j,icscode,i3,it,nsf,nfound,lenmm1
      real*8 pi,u,v,qda,qdb,adb,bdb,ada,xck,yck,zck,s,d,tx3,ty3,tz3,
     *  tx1,ty1,tz1,tx2,ty2,tz2,xln1,yln1,zln1,zln2,yln2,xln2
C
C########################################################################
C     ierror - ERROR FLAG RETURNS (0 IF THERE IS NO ERROR,
C                                  1 IF THERE IS AN ERROR)
      ierror = 0
      pi=3.14159265385
C
C     *******************************************************************
C     GET THE SHEET DATA FROM THE cmo.
C
      call cmo_get_info('nelements',cmoin,
     &                   nelements,ilen,icmotype,ierror)
      call cmo_get_info('xic',cmoin,ipxic,ilen,icmotype,ierror)
      call cmo_get_info('yic',cmoin,ipyic,ilen,icmotype,ierror)
      call cmo_get_info('zic',cmoin,ipzic,ilen,icmotype,ierror)
      call cmo_get_info('sbox',cmoin,ipsbox,ilen,icmotype,ierror)
      call cmo_get_info('linkt',cmoin,iplinkt,ilen,icmotype,ierror)
      call cmo_get_info('itet',cmoin,ipitet,ilen,icmotype,ierror)
C
C     *******************************************************************
C     GET MEMORY FOR LOCAL ARRAYS.
C
      lenmm1=nelements
C
      call mmgetblk('sck1',isubname,ipsck1,lenmm1,2,icscode)
c$$$      call mmgetblk('ack1',isubname,ipack1,lenmm1,2,icscode)
      call mmgetblk('ae',isubname,ipae,lenmm1,2,icscode)
      call mmgetblk('be',isubname,ipbe,lenmm1,2,icscode)
      call mmgetblk('ce',isubname,ipce,lenmm1,2,icscode)
      call mmgetblk('de',isubname,ipde,lenmm1,2,icscode)
      call mmgetblk('itfound',isubname,ipitfound,lenmm1,2,icscode)
C
C     *******************************************************************
C     LOOP THROUGH POINT PAIRS.
C
      do 100 j=1,npts
C
C        ----------------------------------------------------------------
C        EXTEND A LINE TO CKMIN AND CKMAX.
C
         xln1=x1(j) + ckmin*(x2(j)-x1(j)) - epsln
         yln1=y1(j) + ckmin*(y2(j)-y1(j)) - epsln
         zln1=z1(j) + ckmin*(z2(j)-z1(j)) - epsln
         xln2=x1(j) + ckmax*(x2(j)-x1(j)) + epsln
         yln2=y1(j) + ckmax*(y2(j)-y1(j)) + epsln
         zln2=z1(j) + ckmax*(z2(j)-z1(j)) + epsln
C
C        ****************************************************************
C       GET A SUBSET OF TRIANGLES (USING THE k-D TREE) GUARANTEED TO
C       CONTAIN AN INTERSECTION WITH THE LINE SEGMENT.
C
        call lineseg_inter(xln1,yln1,zln1,xln2,yln2,zln2,linkt,sbox,
     &                     epsln,nfound,itfound,ierror)
C
c$$$        if (nfound .eq. 0) then
c$$$           write(logmess,'(a)') 'Error in subroutine shtgtsfp:
c$$$     &     kd tree returns no triangles'
c$$$           call writloga('default',0,logmess,0,ierror)
c$$$        endif
C
C        ----------------------------------------------------------------
C        LOOP THROUGH THE SELECTED TRIANGLES TO FIND THE POINTS OF
C        INTERSECTION.
C
         nsf=0
         do it=1,nfound
            itri=itfound(it)
C
C           -------------------------------------------------------------
C           LOOP THROUGH THE SELECTED ELEMENTS (TRIANGLES) TO COMPUTE
C           THEIR EQUATIONS. GET TRIANGLE VERTICES AND THEN FIND
C           INTERSECTION FACTORS BETWEEN THE POINTS AND THE FACETS.
C
            i1=itet(1,itri)
            i2=itet(2,itri)
            i3=itet(3,itri)
            tx1=xic(i1)
            ty1=yic(i1)
            tz1=zic(i1)
            tx2=xic(i2)
            ty2=yic(i2)
            tz2=zic(i2)
            tx3=xic(i3)
            ty3=yic(i3)
            tz3=zic(i3)
C
            ae(itri)=(ty2-ty1)*(tz3-tz1)-(ty3-ty1)*(tz2-tz1)
            be(itri)=(tx3-tx1)*(tz2-tz1)-(tx2-tx1)*(tz3-tz1)
            ce(itri)=(tx2-tx1)*(ty3-ty1)-(tx3-tx1)*(ty2-ty1)
            de(itri)=ae(itri)*tx1+be(itri)*ty1+ce(itri)*tz1
C
            d=ae(itri)*(x2(j)-x1(j))+
     &        be(itri)*(y2(j)-y1(j))+
     &        ce(itri)*(z2(j)-z1(j))
 
              if (abs(d) .gt. ckzero) then
                 s=(de(itri)-ae(itri)*x1(j)-be(itri)*y1(j)-
     &                       ce(itri)*z1(j))/d
C
C                --------------------------------------------------------
C                CALCULATE INTERSECTION POINTS AND SEE IF IT LIES IN THAT
C                TRIANGLE.
C
                 xck=s*x2(j) + (1.0-s)*x1(j)
                 yck=s*y2(j) + (1.0-s)*y1(j)
                 zck=s*z2(j) + (1.0-s)*z1(j)
C
                 ada=(tx3-tx1)*(tx3-tx1) +
     &               (ty3-ty1)*(ty3-ty1) +
     &               (tz3-tz1)*(tz3-tz1)
 
                 bdb=(tx2-tx1)*(tx2-tx1) +
     &               (ty2-ty1)*(ty2-ty1) +
     &               (tz2-tz1)*(tz2-tz1)
 
                 adb=(tx3-tx1)*(tx2-tx1) +
     &               (ty3-ty1)*(ty2-ty1) +
     &               (tz3-tz1)*(tz2-tz1)
 
                 qda=(xck-tx1)*(tx3-tx1) +
     &               (yck-ty1)*(ty3-ty1) +
     &               (zck-tz1)*(tz3-tz1)
 
                 qdb=(xck-tx1)*(tx2-tx1) +
     &               (yck-ty1)*(ty2-ty1) +
     &               (zck-tz1)*(tz2-tz1)
C
                 u=(bdb*qda - adb*qdb) / (ada*bdb - adb*adb)
                 v=(qdb - u*adb)/bdb
C
                 if (u .ge. -epsln .and. u .le. (1+epsln) .and.
     &               v .ge. -epsln .and. u .le. (1+epsln) .and.
     &               (u+v) .le. (1+epsln)) then
                    nsf=nsf+1
                    sck1(nsf)=s
c$$$                    amag=sqrt(ae(itri)*ae(itri) +
c$$$     &                        be(itri)*be(itri) +
c$$$     &                        ce(itri)*ce(itri))
c$$$                    aa=x2(j) - x1(j)
c$$$                    bb=y2(j) - y1(j)
c$$$                    cc=z2(j) - z1(j)
c$$$                    top=aa*ae(ii) + bb*be(ii) + cc*ce(ii)
c$$$                    bot=amag * sqrt(aa*aa + bb*bb + cc*cc)
c$$$                    if (bot .le. ckzero) bot=1.0
c$$$                    ack1(nsf)=abs(top/bot)
                 endif
              endif
         enddo
C
C        ----------------------------------------------------------------
C        IF TRIANGLES ARE FOUND, ELIMINATE COMMON INTERSECTIONS.
C
         if (nsf .gt. 1) then
            do i=1,nsf
               do k=i+1,nsf
                  if (abs(sck1(i)-sck1(k)) .lt. epsln)
     &               sck1(k)=2.0*ckmax
               enddo
            enddo
         endif
C
C        ----------------------------------------------------------------
C        USE SOLUTIONS THAT FALL BETWEEN THE CHECK VALUES.
C
         nsfact(j)=0
         if (nsf .gt. 0) then
            do i=1,nsf
               if (sck1(i) .ge. ckmin .and. sck1(i) .le. ckmax) then
                  nsfact(j)=nsfact(j) + 1
                  sfact(nsfact(j),j)=sck1(i)
c$$$                  angle(nsfact(j),j)=ack1(i)
               endif
            enddo
         endif
C
  100 continue
C
 9999 continue
C
C     *******************************************************************
C     RELEASE TEMPORARY MEMORY.
C
 9995 call mmrelprt(isubname,ics)
C
C     *******************************************************************
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C
      return
      end
