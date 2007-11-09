      subroutine rzstrat(nr,rmin,rmax,irz,irratio,rrz, dr,rstrt)
      implicit real*8 (a-h,o-z)
C
      character*132 logmess
C
C
C#######################################################################
C
C     PURPOSE -
C
C
C     THIS ROUTINE GENERATES RATIO ZONING START AND INCREMENT VALUES.
C
C
C     INPUT ARGUMENTS -
C
C        nr - NO. OF POINTS TO RATIO
C        rmin - MINIMUM RANGE VALUE
C        rmax - MAXIMUM RANGE VALUE
C        irz - REFLECTIVE OR ABSOLUTE ZONING SWITCHES
C              0 - rmin AND rmax ARE USED AS REFLECTIVE POINTS.
C              1 - rmin AND rmax ARE USED AS ABSOLUTE POINTS.
C        irratio - RATIO ZONING SWITCHES
C              0 - NO RATIO ZONING
C              1 - RATIO ZONING
C        rrz - RATIO ZONING FACTOR
C
C
C     OUTPUT ARGUMENTS -
C
C        dr - STARTING INCREMENT
C        rstrt - STARTING VALUE
C
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/rzstrat.f_a  $
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 17:00:38 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:18:32   pvcs
CPVCS    Original version.
C
C#######################################################################
C
C
C     -------------------------------------------------
C     CORRECT ERRORS IN RATIO ZONING INPUT DATA, IF ANY
C     -------------------------------------------------
      if (irz .ne. 0 .and. irz .ne. 1) irz=1
      if (rrz .le. 0.0 .or. irratio .eq. 0) rrz=1.0
C
C     ---------------------------------------------
C     SET UP dr AND START FOR REFLECTIVE END POINTS
C     ---------------------------------------------
      if (irz .eq. 0) then
         nr1=nr
         rsum=0
C
         if (nr1 .gt. 1) then
            do 20 i1=1,nr1-1
               rsum=rsum+rrz**float(i1)
   20       continue
         endif
C
         rsumr=0.5+rsum+0.5*rrz**float(nr1+1)
         dr=(rmax-rmin)/rsumr
         rstrt=rmin-0.5*dr
      endif
C
C     -------------------------------------------
C     SET UP dr AND START FOR ABSOLUTE END POINTS
C     -------------------------------------------
      if (irz .eq. 1) then
         rsum=0.
         nr1=nr
         if (nr1 .lt. 2) nr1=2
C
         if (nr1 .gt. 2) then
            do 30 i1=1,nr1-2
               rsum=rsum+rrz**float(i1)
   30       continue
         endif
C
         rsumr=1.0+rsum
         dr1=(rmax-rmin)/rsumr
         dr=dr1/rrz
         rstrt=rmin-dr
      endif
C
C     ---------------------------------------
C     SET UP THE CFT IMMUNE STATEMENT FOR DDT
C     ---------------------------------------
      goto 9999
 9999 continue
C
      return
      end
