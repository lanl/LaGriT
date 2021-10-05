      subroutine shtgtsf(x1,y1,z1,x2,y2,z2,cmoin,
     &                   ckmin,ckmax,epsln,sfact,nsfact)
C
C########################################################################
C
C     PURPOSE -
C
C     THIS ROUTINE FINDS THE S FACTOR ALONG A LINE WHERE THE LINE
C     INTERSECTS A SHEET SURFACE.
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
c        cmoin - name of mesh object
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
C        $Log: shtgtsf.f,v $
C        Revision 2.00  2007/11/09 20:04:03  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   30 Sep 2004 11:23:36   dcg
CPVCS    make chzero double precision
CPVCS    
CPVCS       Rev 1.3   Tue Feb 08 13:33:48 2000   dcg
CPVCS
CPVCS       Rev 1.13   Mon Apr 14 17:01:36 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.12   Wed Jun 12 14:48:14 1996   kuprat
CPVCS    Removed error message generated when ray completely misses
CPVCS    sheet and kD tree returns no triangles.
CPVCS
CPVCS       Rev 1.11   Wed Feb 28 18:12:36 1996   ahmed
CPVCS    fixed epsilon
CPVCS
CPVCS       Rev 1.10   Tue Feb 13 14:42:24 1996   ahmed
CPVCS    base the algorithm on k-D search
CPVCS
CPVCS       Rev 1.9   11/07/95 17:26:26   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.8   10/18/95 12:18:46   het
CPVCS    Allow greater that 8 character names for sheets in the surface command.
CPVCS
CPVCS       Rev 1.7   08/31/95 10:06:36   ahmed
CPVCS    Adjust the equation for computing interface points
CPVCS
CPVCS       Rev 1.6   08/28/95 11:25:36   ahmed
CPVCS    implement new definitions of inside outside wrt. sheets
CPVCS
C
C
C########################################################################
C
      implicit none
C
      integer lenptr,nsfact
      real*8 ckzero,x1,y1,z1,x2,y2,z2,epsln,ckmin,ckmax
      parameter (lenptr=1000000)
      parameter (ckzero=1.0d-12)
C
C########################################################################
C
      pointer (ipsck1, sck1)
      pointer (ipitfound, itfound)
C
      real*8 sck1(lenptr)
      real*8 sfact(1000)
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
      integer ics,k,i,nsf,itri,i1,i2,i3,nfound,icscode,lenmm1,
     *  ierror,nelements,ilen,icmotype,it
      real*8 u,v,qdb,qda,adb,bdb,xck,yck,zck,s,d,de,ce,be,ae,
     *  tx1,ty1,tz1,tx2,ty2,tz2,zln2,pi,xln1,yln1,zln1,xln2,yln2,
     *  ada,tx3,ty3,tz3
C
C########################################################################
C     ierror - ERROR FLAG RETURNS (0 IF THERE IS NO ERROR,
C                                  1 IF THERE IS AN ERROR)
      ierror = 0
      pi=3.14159265385
      cmoin = '-cmo-'
C
C     *******************************************************************
C     SET THE MEMORY MANAGED PARTITION NAME.
C
      isubname='shtgtsf'
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
C     EXTEND A LINE FROM CKMIN TO CKMAX.
C
      xln1=x1 + ckmin*(x2-x1)
      yln1=y1 + ckmin*(y2-y1)
      zln1=z1 + ckmin*(z2-z1)
      xln2=x1 + ckmax*(x2-x1)
      yln2=y1 + ckmax*(y2-y1)
      zln2=z1 + ckmax*(z2-z1)
C
C     *******************************************************************
C     GET MEMORY FOR LOCAL ARRAYS.
C
      lenmm1=nelements
C
      call mmgetblk('sck1',isubname,ipsck1,lenmm1,2,icscode)
      call mmgetblk('itfound',isubname,ipitfound,lenmm1,2,icscode)
C
C     ******************************************************************
C     GET A SUBSET OF TRIANGLES (USING THE k-D TREE) GUARANTEED TO
C     CONTAIN AN INTERSECTION WITH THE LINE SEGMENT.
C
      call lineseg_inter(xln1,yln1,zln1,xln2,yln2,zln2,linkt,sbox,
     &                   epsln,nfound,itfound,ierror)
C
c$$$      if (nfound .eq. 0) then
c$$$         write(logmess,'(a)') 'Error in subroutine shtgtsf:
c$$$     &   kd tree returns no triangles'
c$$$         call writloga('default',0,logmess,0,ierror)
c$$$      endif
C
C     *******************************************************************
C     LOOP THROUGH SELECTED TRIANGLES TO FIND THE POINTS OF INTERSECTION.
C
      nsf=0
      do it=1,nfound
         itri=itfound(it)
C
C        ----------------------------------------------------------------
C        GET TRIANGLES AND COMPUTE THEIR EQUATIONS THEN FIND INTERSECTION
C        FACTORS BETWEEN THE POINTS AND THE FACETS.
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
         ae=(ty2-ty1)*(tz3-tz1)-(ty3-ty1)*(tz2-tz1)
         be=(tx3-tx1)*(tz2-tz1)-(tx2-tx1)*(tz3-tz1)
         ce=(tx2-tx1)*(ty3-ty1)-(tx3-tx1)*(ty2-ty1)
         de=ae*tx1+be*ty1+ce*tz1
C
         d=ae*(x2-x1)+be*(y2-y1)+ce*(z2-z1)
         if (abs(d) .gt. ckzero) then
            s=(de-ae*x1-be*y1-ce*z1)/d
C
C           -------------------------------------------------------------
C           CALCULATE INTERSECTION POINTS AND SEE IF IT LIES IN THAT
C           TRIANGLE.
C
            xck=s*x2 + (1.0-s)*x1
            yck=s*y2 + (1.0-s)*y1
            zck=s*z2 + (1.0-s)*z1
C
            ada=(tx3-tx1)*(tx3-tx1) +
     &          (ty3-ty1)*(ty3-ty1) +
     &          (tz3-tz1)*(tz3-tz1)
C
            bdb=(tx2-tx1)*(tx2-tx1) +
     &          (ty2-ty1)*(ty2-ty1) +
     &          (tz2-tz1)*(tz2-tz1)
C
            adb=(tx3-tx1)*(tx2-tx1) +
     &          (ty3-ty1)*(ty2-ty1) +
     &          (tz3-tz1)*(tz2-tz1)
C
            qda=(xck-tx1)*(tx3-tx1) +
     &          (yck-ty1)*(ty3-ty1) +
     &          (zck-tz1)*(tz3-tz1)
C
            qdb=(xck-tx1)*(tx2-tx1) +
     &          (yck-ty1)*(ty2-ty1) +
     &          (zck-tz1)*(tz2-tz1)
C
            u=(bdb*qda - adb*qdb)/(ada*bdb - adb*adb)
            v=(qdb - u*adb)/bdb
C
            if (u .ge. -epsln .and. u .le. (1+epsln) .and.
     &          v .ge. -epsln .and. v .le. (1+epsln) .and.
     &          (u+v) .le. (1+epsln)) then
                nsf=nsf+1
                sck1(nsf)=s
             endif
         endif
      enddo
C
C     *******************************************************************
C     IF TRIANGLES ARE FOUND, ELIMINATE COMMON INTERSECTIONS.
C
      if (nsf .gt. 1) then
         do i=1,nsf
            do k=i+1,nsf
               if (abs(sck1(i)-sck1(k)) .lt. epsln)
     &            sck1(k)=2.0*ckmax
            enddo
         enddo
      endif
C
C     *******************************************************************
C     USE SOLUTIONS THAT FALL BETWEEN THE CHECK VALUES.
C
      nsfact=0
      if (nsf .gt. 0) then
         do i=1,nsf
            if (sck1(i) .ge. ckmin .and. sck1(i) .le. ckmax) then
               nsfact=nsfact + 1
               sfact(nsfact)=sck1(i)
            endif
         enddo
      endif
C
      goto 9999
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
