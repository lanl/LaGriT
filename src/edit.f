      subroutine edit(imsgin,xmsgin,cmsgin,msgtype,nwds,ierror)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE GENERATES EDITS OF VARIOUS QUANTITIES ASSOCIATED
C        WITH GROUPS OF POINTS OR PARTS.  THE FORMAT OF THE COMMAND IS
C
C           EDIT/iopt/point1,point2,increment/partname
C
C           EDIT/ANGULAR/point1,point2,increment/partname/xcen,ycen,zcen
C
C           EDIT/POINTS/point1,point2,increment/partname/array1,array2,
C                       array3,array4 (cell center arrays)
C
C           EDIT/RADIAL/point1,point2,increment/partname/xcen,ycen,zcen
C
C        FOR THE FOLLOWING VALUES OF iopt:
C
C           'partsmry'  - SUMMARY EDIT BY MATERIAL REGION
C
C           'parts'     - LIST OF MATERIAL REGIONS
C
C           'angular'   - ANGULAR VELOCITY SUMMARY FOR SET OF POINTS
C
C           'two'       - AVERAGES AND SUMS FOR point1 AND point2
C
C           'points'    - nodmesh ARRAY VALUES FOR SET OF POINTS
C
C           'radial'    - RADIAL DATA FOR SET OF POINTS
C
C           unspecified - AVERAGES AND SUMS FOR SPECIFIED SET OF POINTS.
C
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/edit_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.5   30 Sep 2004 09:02:58   dcg
CPVCS     change call to real to call to dble
CPVCS    
CPVCS       Rev 1.4   Wed Apr 05 13:34:18 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS    
CPVCS       Rev 1.3   Tue Feb 08 15:24:48 2000   dcg
CPVCS    
CPVCS       Rev 1.2   Tue Feb 08 14:53:22 2000   dcg
CPVCS    
CPVCS       Rev 1.1   Thu Feb 03 08:48:24 2000   dcg
CPVCS    
CPVCS       Rev 1.0   26 Jan 2000 16:23:36   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.3   Fri Jul 23 09:13:46 1999   dcg
CPVCS    removed unused code
CPVCS
CPVCS       Rev 1.2   Mon Sep 21 16:50:36 1998   dcg
CPVCS    replace single precision constants
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:44:40 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Mon Mar 18 11:21:58 1996   dcg
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit real*8 (a-h, o-z)
      character*32 geom_name
C
      character*132 logmess
      include 'chydro.h'
      include 'consts.h'
      include 'geom_lg.h'
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
C
      integer ierror
C
C#######################################################################
C
      pointer (ipisetwd, isetwd)
      pointer (ipimt1, imt1)
      pointer (ipitp1, itp1)
      pointer (ipicr1, icr1)
      pointer (ipisn1, isn1)
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      integer isetwd(1000000)
      integer imt1(1000000), itp1(1000000),
     *        icr1(1000000), isn1(1000000)
      dimension xic(1000000), yic(1000000), zic(1000000)
      pointer (ipitetclr, itetclr(1000000))
      pointer (ipitet, itet(4,1000000))
      pointer (ipjtet, jtet(4,1000000))
      pointer (ipitet, itet1(4*1000000))
      pointer (ipjtet, jtet1(4*1000000))
C
C
C#######################################################################
C
      pointer (iprsplic , rsplic(1000000))
      pointer (ipvolic  , volic(1000000))
      pointer (ipxopic  , xopic(1000000))
      pointer (ippbtic  , pbtic(1000000))
      pointer (ippric   , pric(1000000))
      pointer (iptric   , tric(1000000))
      pointer (ippbiic  , pbiic(1000000))
      pointer (ipyldic  , yldic(1000000))
      pointer (ipgic    , gic(1000000))
      pointer (ipetotic , etotic(1000000))
      pointer (ipdedmric, dedmric(1000000))
      pointer (ipcsqic  , csqic(1000000))
      pointer (ipepsic  , epsic(1000000))
      pointer (iptic    , tic(1000000))
      pointer (ipxmic   , xmic(1000000))
C
C#######################################################################
C
      character*32  caraynam(8)
C
      pointer       (ipitest , itest(1)  )
      pointer       (ipmaspnt, imaspnt(1))
C
      pointer       (ipedtx  , edtx(1)   )
      pointer       (iptmpedt, tmpedt(8,1))
      pointer       (ipctmpedt, ctmpedt(8,1))
C
      pointer       (ipmpary , mpary(1) )
      pointer   (ipvels,vels)
      pointer   (ipdens,dens)
      pointer   (ippres,pres)
      pointer   (ipener,ener)
      real*8 vels(3,1000000),dens(1000000),pres(1000000),ener(1000000)
      logical ifvel,ifpres,ifdens,ifener
C
      character*32 isubname, cmo, iprtname, iblknam, iprtnam
      character*32 iopt
      character*32 ich1,ich2,ich3, cvelnm, cpresnm, cdensnm, cenernm
      character*8 ctmpedt, cpart
      real*8 alargenumber,asmallnumber
      data alargenumber,asmallnumber/1.0d+100,1.0d-200/
 
C
C#######################################################################
C
C
C
      cpart='part'
      isubname='edit'
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('nnodes',cmo,nnodes,length,icmotype,ierror)
      call cmo_get_info('nelements',cmo,nelements,length,icmotype,ier)
      call cmo_get_info('mbndry',cmo,mbndry,length,icmotype,ierror)
C
      call cmo_get_info('isetwd',cmo,ipisetwd,lenisetwd,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,length,icmotype,ierror)
      call cmo_get_info('itp1',cmo,ipitp1,length,icmotype,ierror)
      call cmo_get_info('icr1',cmo,ipicr1,length,icmotype,ierror)
      call cmo_get_info('isn1',cmo,ipisn1,length,icmotype,ierror)
      call cmo_get_info('xic',cmo,ipxic,length,icmotype,ierror)
      call cmo_get_info('yic',cmo,ipyic,length,icmotype,ierror)
      call cmo_get_info('zic',cmo,ipzic,length,icmotype,ierror)
      call cmo_get_info('itetclr',cmo,ipitetclr,length,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,length,icmotype,ierror)
      call cmo_get_info('jtet',cmo,ipjtet,length,icmotype,ierror)
      ifvel=.true.
      ifpres=.true.
      ifdens=.true.
      ifener=.true.
      call cmo_get_attinfo('velname',cmo,iout,rout,cvelnm,
     *                        ipout,lout,itype,ierror_return)
      call cmo_get_info(cvelnm,cmo,ipvels,ilen,icmotype,ier)
      if (ier.ne.0) ifvel=.false.
      call cmo_get_attinfo('presname',cmo,iout,rout,cpresnm,
     *                        ipout,lout,itype,ierror_return)
      call cmo_get_info(cpresnm,cmo,ippres,ilen,icmotype,ier)
      if (ier.ne.0) ifpres=.false.
      call cmo_get_attinfo('densname',cmo,iout,rout,cdensnm,
     *                        ipout,lout,itype,ierror_return)
      call cmo_get_info(cdensnm,cmo,ipdens,ilen,icmotype,ier)
      if (ier.ne.0) ifdens=.false.
      call cmo_get_attinfo('enername',cmo,iout,rout,cenernm,
     *                        ipout,lout,itype,ierror_return)
      call cmo_get_info(cenernm,cmo,ipener,ilen,icmotype,ier)
         if (ier.ne.0) ifener=.false.
 
C
C
C
      length=nnodes
      call mmgetblk('volic',isubname,ipvolic,length,2,icscode)
      voltot=0.0
      do it=1,nelements
         i1=itet(1,it)
         i2=itet(2,it)
         i3=itet(3,it)
         i4=itet(4,it)
         x1=xic(i1)
         y1=yic(i1)
         z1=zic(i1)
         x2=xic(i2)
         y2=yic(i2)
         z2=zic(i2)
         x3=xic(i3)
         y3=yic(i3)
         z3=zic(i3)
         x4=xic(i4)
         y4=yic(i4)
         z4=zic(i4)
         dx=  (y2-y3)*(z4-z3)-(y4-y3)*(z2-z3)
         dy=-((x2-x3)*(z4-z3)-(x4-x3)*(z2-z3))
         dz=  (x2-x3)*(y4-y3)-(x4-x3)*(y2-y3)
         voltet=-((x3-x1)*dx+(y3-y1)*dy+(z3-y1)*dz) / 6.0
         volic(i1)=volic(i1)+voltet/4.0
         volic(i2)=volic(i2)+voltet/4.0
         volic(i3)=volic(i3)+voltet/4.0
         volic(i4)=volic(i4)+voltet/4.0
      enddo
C
      iprsplic = ippres
      ipxopic  = ippres
      ippbtic  = ippres
      ippric   = ippres
      iptric   = ippres
      ippbiic  = ippres
      ipyldic  = ippres
      ipgic    = ippres
      ipetotic = ippres
      ipdedmric= ippres
      ipcsqic  = ippres
      ipepsic  = ippres
      iptic    = ippres
      ipxmic   = ippres
 
C
C  ************************************************************
C
C     PRINT OUT HEADING FOR EDIT.
C
      write(logmess,9006) time
 9006 format(' Edit at ',1pe14.7)
      call writloga('default',2,logmess,1,ierrdum)
C
C     ******************************************************************
C
C     SET THE POINT INDEX BOUNDARIES.
C
      iopt = cmsgin(2)
C        **OPTION
C
C
C     ..................................................................
C     CHECK TO POINT LIMITS AND TRANSLATE THEM TO VALID LIMITS IF
C     NECESSARY.
C
C
C     CALL PNTLIMN TO SET POINT LIMITS.
C
      length = nnodes
      call mmgetblk('mpary',isubname,ipmpary,length,2,icscode)
C
C
C    set the point index boundaries
C
      ich1=' '
      ich2=' '
      ich3=' '
      if(msgtype(3).eq.1) then
         ipt1=imsgin(3)
         ipt2=imsgin(4)
         ipt3=imsgin(5)
c        ich1 = ipt1
         inpt2 = ipt2
         inpt3 = ipt3
C           **INCREMENT BETWEEN POINTS
         if (nwds .lt. 3) ipt1 = 0
         if (nwds .lt. 4) ipt2 = ipt1
         if (nwds .lt. 5) ipt3 = 1
         if (ipt1 .le. 0) ipt1 = 1
         if (ipt2 .le. 0) ipt2 = nnodes
         if (ipt3 .le. 0) ipt3 = 1
         if(ipt2.lt.ipt1) ipt2 = ipt1+ipt2
         ipt2 = min(ipt2,nnodes)
         if (iopt(1:icharlnf(iopt)) .eq. 'two') ipt3 = ipt2-ipt1
         call pntlimn(ipt1,ipt2,ipt3,ipmpary,mpno,nnodes,isetwd,itp1)
      else
        ich1=cmsgin(3)
        ich2=cmsgin(4)
        ich3=cmsgin(5)
        call pntlimc(ich1,ich2,ich3,ipmpary,mpno,nnodes,isetwd,itp1)
      endif
C
C     ******************************************************************
C
C     CHECK FOR REQUESTED MATERIAL IN COMMAND.
C
      iprtindx = 0
      if (nwds .gt. 5) then
         if (msgtype(6) .eq. 1) then
            iprtindx = imsgin(6)
         elseif (msgtype(6) .eq. 2) then
            iprtindx = int(xmsgin(6))
         else
            iprtname = cmsgin(6)
            ierrdum = 0
            call get_material_number(iprtname,iprtindx,ierrdum)
            if (ierrdum .ne. 0) iprtindx = 0
         endif
      endif
C
C     ******************************************************************
C
C     ADJUST DYNAMIC MEMORY.
C
      len1 = nnodes
      len2 = nnodes
      call mmgetblk('itp1',isubname,ipitp,len1,2,ics)
      call mmgetblk('itest',isubname,ipitest,len2,2,ics)
      call mmgetblk('imaspnt',isubname,ipmaspnt,len2,2,ics)
C
C     ******************************************************************
C
 
C     SQUEEZE OUT INACTIVE POINTS.
C
      do 100 masspt = 1,nnodes
         itest(masspt) = masspt
         imaspnt(masspt) = masspt
  100 continue
      do 110 ii = 1,mpno
         masspt = mpary(ii)
         itest(masspt) = cvmgt(0,itest(masspt),(itp1(masspt) .ge.
     *                   ifitpst1 .and. itp1(masspt) .le. ifitpen1) .or.
     *                   (itp1(masspt) .ge. ifitpst2 .and. itp1(masspt)
     *                   .le. ifitpen2))
  110 continue
      if (iprtindx .ne. 0) then
         do 115 ii = 1,mpno
            masspt = mpary(ii)
            itest(masspt) = cvmgt(itest(masspt),masspt,imt1(masspt) .eq.
     *                            iprtindx)
  115    continue
      endif
      call kmprsz(nnodes,itest,1,imaspnt,1,imaspnt,1,nmaspnt)
      if (mpno .eq. 0) nmaspnt = 0
      if (nmaspnt .le. 0) then
         if (ich1.eq.'pset' .or. ich1.eq.'pstatus') then
            write(logmess,9000) ich1,inpt2,inpt3
 9000       format(' No points that qualify: ',3a8)
          else
            write(logmess,9001) ipt1,ipt2,ipt3
 9001       format(' No points that qualify: ',3i6)
         endif
         call writloga('default',1,logmess,1,ierr)
         go to 9998
      endif
C
C     ******************************************************************
C
C     COMPUTE NUMBER OF MATERIAL REGIONS.
C
      numpart = 0
      do 120 masspt = 1,nmaspnt
         numpart = max0(numpart,imt1(imaspnt(masspt)))
  120 continue
      numpart = max0(numpart,iprtindx,1)
C
C     ******************************************************************
C
C     SET BOUNDS FOR LOOPS OVER MATERIAL NUMBERS.
C
      if (iprtindx .ne. 0) then
         kpart1 = iprtindx
         kpart2 = kpart1
      else
         kpart1 = 1
         kpart2 = numpart
      endif
C
C     ******************************************************************
C
C     PREPARE AND PRINT EDITS.
C
      leniopt=icharlnf(iopt)
      if (iopt(1:leniopt) .eq. 'parts') then
C
C        _______________________________________________________________
C
C        EDIT LIST OF MATERIAL REGIONS.
C
         icount = 0
         istart = imaspnt(1)
         imtsav = imt1(istart)
         do 300 masspt = 1,nmaspnt
            i1 = imaspnt(masspt)
            if (i1 .eq. imaspnt(1)) then
               icount = icount+1
               iend = i1
            elseif (imt1(i1) .eq. imt1(imaspnt(masspt-1))) then
               icount = icount+1
               iend = i1
            else
               call mmfindbk('cmregs',geom_name,ipcmregs,len,icscode)
               geom_name=current_geom_name
               iprtname=cmregs(imtsav)
               write(logmess,9510) imtsav,iprtname,icount,istart,iend
               if (icount .gt. 0)
     *            call writloga('default',0,logmess,0,ierr)
 9510          format('imt= ',i5,' name= ',a8,' count= ',i7,
     *                '  start=',i7,'  end=',i7)
               icount = 1
               istart = i1
               imtsav = imt1(istart)
            endif
 300     continue
         if (icount .gt. 0) then
            call mmfindbk('cmregs',geom_name,ipcmregs,len,icscode)
            geom_name=current_geom_name
            iprtname=cmregs(imtsav)
            write(logmess,9510) imtsav,iprtname,icount,istart,iend
            call writloga('default',0,logmess,0,ierr)
         endif
C
C        _______________________________________________________________
C
      elseif (iopt(1:leniopt) .eq. 'angular') then
C
C        _______________________________________________________________
C
C        EDIT OF ANGULAR MOMENTUM FOR SET OF POINTS.
C
         xcen = xmsgin(7)
         ycen = xmsgin(8)
         zcen = xmsgin(9)
         icount = 0
         xangular = 0.0
         do 200 masspt = 1,nmaspnt
            i1 = imaspnt(masspt)
            icount = icount+1
            vel=0.
            if(ifvel)vel=sqrt(vels(1,i1)**2+vels(2,i1)**2+vels(3,i1)**2)
            rad = sqrt((xic(i1)-xcen)**2 + (yic(i1)-ycen)**2 +
     *                (zic(i1)-zcen)**2 )
            xangular = xangular+rad*vel
 200     continue
         write(logmess,9500) icount,xangular
         call writloga('default',0,logmess,0,ierr)
 9500    format('number of points: ',i8,'  angular momentum: ',1pe15.8)
C
C        _______________________________________________________________
C
      elseif (iopt(1:leniopt) .eq. 'radial') then
C
C        _______________________________________________________________
C
C        EDIT OF RADIAL POSITIONS AND VELOCITIES FOR SET OF POINTS.
C
         write(logmess,9600)
 9600    format(8x,'ipt',7x,'R ',14x,'QR')
         call writloga('default',1,logmess,1,ierr)
         xcen = xmsgin(7)
         ycen = xmsgin(8)
         zcen = xmsgin(9)
         do 600 masspt = 1,nmaspnt
            i1 = imaspnt(masspt)
            radx = xic(i1)-xcen
            rady = yic(i1)-ycen
            radz = zic(i1)-zcen
            rad = sqrt(radx**2+rady**2+radz**2)
            if(ifvel) then
               velrad=(vels(1,i1)*radx+vels(2,i1)*rady+vels(3,i1)*radz)
     *          /(rad+epsilon)
            else
               velrad=0
            endif
            write(logmess,9610) i1,rad,velrad
 9610       format(1x,i10,1p2e16.7)
            call writloga('default',0,logmess,0,ierr)
  600    continue
C
C        _______________________________________________________________
C
      elseif (iopt(1:leniopt) .eq. 'points') then
C
C        _______________________________________________________________
C
C        PRINT OUT VALUES OF SPECIFIED ARRAY FOR SET OF POINTS.
C
         call mmgetblk('tmpedt',isubname,iptmpedt,8*nmaspnt,2,
     *                 icscode)
         call mmgetblk('ctmpedt',isubname,ipctmpedt,8*32*nmaspnt,2,
     *                 icscode)
         naray = 4
         do 500 masspt = 1,nmaspnt
            i1 = imaspnt(masspt)
            tmpedt(1,masspt) = imt1(i1)
            tmpedt(2,masspt) = itp1(i1)
            tmpedt(3,masspt) = icr1(i1)
            iep = 0
            isp = 0
            mlt = 0
C*****      iep = and(shiftr(ifitwc(i1),ifwshf(ifieppos)),1b)
C*****      isp = and(shiftr(ifitwc(i1),ifwshf(ifisppos)),1b)
C*****      mlt = and(shiftr(ifitwc(i1),ifwshf(ifmltpos)),1b)
            ctmpedt(4,masspt) = '    e   '
            if (iep .eq. 1) ctmpedt(4,masspt) = '    p   '
            if (isp .eq. 1) ctmpedt(4,masspt) = '    s   '
            if (mlt .eq. 1) ctmpedt(4,masspt) = '    m   '
  500    continue
         do 510 iaray = 6,9
            caraynam(iaray) = ' '
            if (nwds .gt. iaray) caraynam(iaray) = cmsgin(1+iaray)
            ipedtx = 0
            iprtnam=cmo
            iblknam=caraynam(iaray)
            call mmgetpr(iblknam,iprtnam,ipedtx,ierr)
            if (ierr .eq. 0) then
               naray = naray+1
               caraynam(naray) = caraynam(iaray)
               do 515 masspt = 1,nmaspnt
                  tmpedt(naray,masspt) = edtx(imaspnt(masspt))
  515          continue
            endif
  510    continue
         if (naray .gt. 4) then
            write(logmess,9520) (caraynam(iaray),iaray = 5,naray)
 9520       format(8x,'ipt',' imt',' itp',' icr',' state',4(4x,a8))
            call writloga('default',1,logmess,1,ierrdum)
            do 520 masspt = 1,nmaspnt
               i1 = imaspnt(masspt)
            write(logmess,9521)i1,(nint(tmpedt(iaray,masspt)),iaray=1,3)
     *         ,ctmpedt(4,masspt),(tmpedt(iaray,masspt),iaray=5,naray)
 9521          format(1x,i10,3i4,a6,1p4e12.4)
               call writloga('default',0,logmess,0,ierrdum)
  520       continue
         else
            write(logmess,9522)
 9522       format(8x,'ipt',' imt',' itp',' icr',' state')
            call writloga('default',1,logmess,1,ierrdum)
            do 525 masspt = 1,nmaspnt
               i1 = imaspnt(masspt)
               write(logmess,9523) i1,
     *            (nint(tmpedt(iaray,masspt)),iaray=1,3),
     *            ctmpedt(4,masspt)
 9523          format(1x,i10,3i4,a6)
               call writloga('default',0,logmess,0,ierrdum)
  525       continue
         endif
         call mmrelblk('tmpedt',isubname,ipctmpedt,icscode)
      else
C
C        _______________________________________________________________
C
C        EDIT OF SUMS AND AVERAGES --- ACCUMULATE DATA.
C
         xtot = 0.0
         ytot = 0.0
         ztot = 0.0
         utot = 0.0
         vtot = 0.0
         wtot = 0.0
         ptot = 0.0
         rtot = 0.0
         rsptot = 0.0
         etot = 0.0
         ttot = 0.0
         trtot = 0.0
         prtot = 0.0
         xoptot = 0.0
         xmtot = 0.0
         voltot = 0.0
         csqtot = 0.0
         ettot = 0.0
         dedmrtot = 0.0
         pbttot = 0.0
         pbitot = 0.0
         gtot = 0.0
         yldtot = 0.0
         epstot = 0.0
C
         xavg = 0.0
         yavg = 0.0
         zavg = 0.0
         uavg = 0.0
         vavg = 0.0
         wavg = 0.0
         pavg = 0.0
         ravg = 0.0
         rspavg = 0.0
         eavg = 0.0
         tavg = 0.0
         travg = 0.0
         pravg = 0.0
         xopavg = 0.0
         xmavg = 0.0
         volavg = 0.0
         csqavg = 0.0
         etavg = 0.0
         pbtavg = 0.0
         pbiavg = 0.0
         gavg = 0.0
         yldavg = 0.0
         epsavg = 0.0
C
         x1max = -alargenumber
         y1max = -alargenumber
         z1max = -alargenumber
         u1max = -alargenumber
         v1max = -alargenumber
         w1max = -alargenumber
         p1max = -alargenumber
         r1max = -alargenumber
         rsp1max = -alargenumber
         e1max = -alargenumber
         t1max = -alargenumber
         tr1max = -alargenumber
         pr1max = -alargenumber
         xop1max = -alargenumber
         xm1max = -alargenumber
         vol1max = -alargenumber
         csq1max = -alargenumber
         et1max = -alargenumber
         ded1max = -alargenumber
         pbt1max = -alargenumber
         pbi1max = -alargenumber
         g1max = -alargenumber
         yld1max = -alargenumber
         eps1max = -alargenumber
C
         x1min = alargenumber
         y1min = alargenumber
         z1min = alargenumber
         u1min = alargenumber
         v1min = alargenumber
         w1min = alargenumber
         p1min = alargenumber
         r1min = alargenumber
         rsp1min = alargenumber
         e1min = alargenumber
         t1min = alargenumber
         tr1min = alargenumber
         pr1min = alargenumber
         xop1min = alargenumber
         xm1min = alargenumber
         vol1min = alargenumber
         csq1min = alargenumber
         et1min = alargenumber
         ded1min = alargenumber
         pbt1min = alargenumber
         pbi1min = alargenumber
         g1min = alargenumber
         yld1min = alargenumber
         eps1min = alargenumber
C
         ix1max = 0
         iy1max = 0
         iz1max = 0
         iu1max = 0
         iv1max = 0
         iw1max = 0
         ip1max = 0
         ir1max = 0
         irsp1max = 0
         ie1max = 0
         it1max = 0
         itr1max = 0
         ipr1max = 0
         ixop1max = 0
         ixm1max = 0
         ivol1max = 0
         icsq1max = 0
         iet1max = 0
         ided1max = 0
         ipbt1max = 0
         ipbi1max = 0
         ig1max = 0
         iyld1max = 0
         ieps1max = 0
C
         ix1min = 0
         iy1min = 0
         iz1min = 0
         iu1min = 0
         iv1min = 0
         iw1min = 0
         ip1min = 0
         ir1min = 0
         irsp1min = 0
         ie1min = 0
         it1min = 0
         itr1min = 0
         ipr1min = 0
         ixop1min = 0
         ixm1min = 0
         ivol1min = 0
         icsq1min = 0
         iet1min = 0
         ided1min = 0
         ipbt1min = 0
         ipbi1min = 0
         ig1min = 0
         iyld1min = 0
         ieps1min = 0
C
         ipts = 0
         do 130 i = 1,nmaspnt
            i1 = imaspnt(i)
            ipts = ipts+1
            xtot = xtot+xic(i1)
            ytot = ytot+yic(i1)
            ztot = ztot+zic(i1)
            if (ifvel) then
               utot = utot+vels(1,i1)
               vtot = vtot+vels(2,i1)
               wtot = wtot+vels(3,i1)
            endif
            if(ifpres.and.ifdens.and.ifener) then
               ptot = ptot+pres(i1)
               rtot = rtot+dens(i1)
               rsptot = rsptot+rsplic(i1)
               etot = etot+ener(i1)
               ttot = ttot+tic(i1)
               trtot = trtot+tric(i1)
               prtot = prtot+pric(i1)
               xoptot = xoptot+xopic(i1)
               xmtot = xmtot+xmic(i1)
               voltot = voltot+volic(i1)
               csqtot = csqtot+csqic(i1)
               ettot = ettot+etotic(i1)
               dedmrtot = dedmrtot+dedmric(i1)
               pbttot = pbttot+pbtic(i1)
               pbitot = pbitot+pbiic(i1)
               gtot = gtot+gic(i1)
               yldtot = yldtot+yldic(i1)
               epstot = epstot+epsic(i1)
            endif
C
            if (xic(i1) .gt. x1max) then
               x1max = xic(i1)
               ix1max = i1
            endif
            if (yic(i1) .gt. y1max) then
               y1max = yic(i1)
               iy1max = i1
            endif
            if (zic(i1) .gt. z1max) then
               z1max = zic(i1)
               iz1max = i1
            endif
            if(ifvel) then
               if (vels(1,i1) .gt. u1max) then
                  u1max = vels(1,i1)
                  iu1max = i1
               endif
               if (vels(2,i1) .gt. v1max) then
                  v1max = vels(2,i1)
                  iv1max = i1
               endif
               if (vels(3,i1) .gt. w1max) then
                  w1max = vels(3,i1)
                  iw1max = i1
               endif
            endif
            if (ifpres.and.ifdens.and.ifener) then
               if (pres(i1) .gt. p1max) then
                  p1max = pres(i1)
                  ip1max = i1
               endif
               if (dens(i1) .gt. r1max) then
                  r1max = dens(i1)
                  ir1max = i1
               endif
               if (rsplic(i1) .gt. rsp1max) then
                  rsp1max = rsplic(i1)
                  irsp1max = i1
               endif
               if (ener(i1) .gt. e1max) then
                  e1max = ener(i1)
                  ie1max = i1
               endif
               if (tic(i1) .gt. t1max) then
                  t1max = tic(i1)
                  it1max = i1
               endif
               if (tric(i1) .gt. tr1max) then
                  tr1max = tric(i1)
                  itr1max = i1
               endif
               if (pric(i1) .gt. pr1max) then
                  pr1max = pric(i1)
                  ipr1max = i1
               endif
               if (xopic(i1) .gt. xop1max) then
                  xop1max = xopic(i1)
                  ixop1max = i1
               endif
               if (xmic(i1) .gt. xm1max) then
                  xm1max = xmic(i1)
                  ixm1max = i1
               endif
               if (volic(i1) .gt. vol1max) then
                  vol1max = volic(i1)
                  ivol1max = i1
               endif
               if (csqic(i1) .gt. csq1max) then
                  csq1max = csqic(i1)
                  icsq1max = i1
               endif
               if (etotic(i1) .gt. et1max) then
                  et1max = etotic(i1)
                  iet1max = i1
               endif
               if (dedmric(i1) .gt. ded1max) then
                  ded1max = dedmric(i1)
                  ided1max = i1
               endif
               if (pbtic(i1) .gt. pbt1max) then
                  pbt1max = pbtic(i1)
                  ipbt1max = i1
               endif
               if (pbiic(i1) .gt. pbi1max) then
                  pbi1max = pbiic(i1)
                  ipbi1max = i1
               endif
               if (gic(i1) .gt. g1max) then
                  g1max = gic(i1)
                  ig1max = i1
               endif
               if (yldic(i1) .gt. yld1max) then
                  yld1max = yldic(i1)
                  iyld1max = i1
               endif
               if (epsic(i1) .gt. eps1max) then
                  eps1max = epsic(i1)
                  ieps1max = i1
               endif
            endif
C
            if (xic(i1) .lt. x1min) then
               x1min = xic(i1)
               ix1min = i1
            endif
            if (yic(i1) .lt. y1min) then
               y1min = yic(i1)
               iy1min = i1
            endif
            if (zic(i1) .lt. z1min) then
               z1min = zic(i1)
               iz1min = i1
            endif
           if (ifvel) then
               if (vels(1,i1) .lt. u1min) then
                  u1min = vels(1,i1)
                  iu1min = i1
               endif
               if (vels(2,i1) .lt. v1min) then
                  v1min = vels(2,i1)
                  iv1min = i1
               endif
               if (vels(3,i1) .lt. w1min) then
                  w1min = vels(3,i1)
                  iw1min = i1
               endif
            endif
            if (ifpres.and.ifdens. and. ifener) then
               if (pres(i1) .lt. p1min) then
                  p1min = pres(i1)
                  ip1min = i1
               endif
               if (dens(i1) .lt. r1min) then
                  r1min = dens(i1)
                  ir1min = i1
               endif
               if (rsplic(i1) .lt. rsp1min) then
                  rsp1min = rsplic(i1)
                  irsp1min = i1
               endif
               if (ener(i1) .lt. e1min) then
                  e1min = ener(i1)
                  ie1min = i1
               endif
               if (tic(i1) .lt. t1min) then
                  t1min = tic(i1)
                  it1min = i1
               endif
               if (tric(i1) .lt. tr1min) then
                  tr1min = tric(i1)
                  itr1min = i1
               endif
               if (pric(i1) .lt. pr1min) then
                  pr1min = pric(i1)
                  ipr1min = i1
               endif
               if (xopic(i1) .lt. xop1min) then
                  xop1min = xopic(i1)
                  ixop1min = i1
               endif
               if (xmic(i1) .lt. xm1min) then
                  xm1min = xmic(i1)
                  ixm1min = i1
               endif
               if (volic(i1) .lt. vol1min) then
                  vol1min = volic(i1)
                  ivol1min = i1
               endif
               if (csqic(i1) .lt. csq1min) then
                  csq1min = csqic(i1)
                  icsq1min = i1
               endif
               if (etotic(i1) .lt. et1min) then
                  et1min = etotic(i1)
                  iet1min = i1
               endif
               if (dedmric(i1) .lt. ded1min) then
                  ded1min = dedmric(i1)
                  ided1min = i1
               endif
               if (pbtic(i1) .lt. pbt1min) then
                  pbt1min = pbtic(i1)
                  ipbt1min = i1
               endif
               if (pbiic(i1) .lt. pbi1min) then
                  pbi1min = pbiic(i1)
                  ipbi1min = i1
               endif
               if (gic(i1) .lt. g1min) then
                  g1min = gic(i1)
                  ig1min = i1
               endif
               if (yldic(i1) .lt. yld1min) then
                  yld1min = yldic(i1)
                  iyld1min = i1
               endif
               if (epsic(i1) .lt. eps1min) then
                  eps1min = epsic(i1)
                  ieps1min = i1
               endif
            endif
 130     continue
         if (ipts .gt. 0) then
            xavg = xtot/dble(ipts)
            yavg = ytot/dble(ipts)
            zavg = ztot/dble(ipts)
            uavg = utot/dble(ipts)
            vavg = vtot/dble(ipts)
            wavg = wtot/dble(ipts)
            pavg = ptot/dble(ipts)
            ravg = rtot/dble(ipts)
            rspavg = rsptot/dble(ipts)
            eavg = etot/dble(ipts)
            tavg = ttot/dble(ipts)
            travg = trtot/dble(ipts)
            pravg = prtot/dble(ipts)
            xopavg = xoptot/dble(ipts)
            xmavg = xmtot/dble(ipts)
            volavg = voltot/dble(ipts)
            csqavg = csqtot/dble(ipts)
            etavg = ettot/dble(ipts)
            dedmravg = dedmrtot/dble(ipts)
            pbtavg = pbttot/dble(ipts)
            pbiavg = pbitot/dble(ipts)
            gavg = gtot/dble(ipts)
            yldavg = yldtot/dble(ipts)
            epsavg = epstot/dble(ipts)
         endif
C
C        _______________________________________________________________
C
C        EDIT OF SUMS AND AVERAGES --- PRINT POINT INDEX BOUNDARIES.
C
         if (ich1.eq.'pset' .or. ich1.eq.'pstatus') then
            write(logmess,9010) ich1,ich2,ich3,ipts
 9010       format('Summary:   ',a8,2x,a8,2x,a8,'    tot=',i5)
          else
            write(logmess,9011) ipt1,ipt2,ipt3,ipts
 9011       format('Summary:   i=',i5,'   f=',i5,'   s=',i5,
     &             '    tot=',i5)
         endif
         call writloga('default',1,logmess,0,ierr)
C
C        _______________________________________________________________
C
C        EDIT OF AVERAGES, MAXIMA, AND MINIMA.
C
         write(logmess,9005) 'Variable','Minimum','Maximum','Average'
 9005    format(1x,a8,17x,a7,16x,a7,9x,a7)
         call writloga('default',1,logmess,1,ierr)
         write(logmess,9003) 'xic     ',ix1min,x1min,ix1max,x1max,xavg
 9003    format(1x,a8,1x,i7,2x,1pe14.7,i7,2x,1pe14.7,2x,1pe14.7)
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'yic     ',iy1min,y1min,iy1max,y1max,yavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'zic     ',iz1min,z1min,iz1max,z1max,zavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'uic     ',iu1min,u1min,iu1max,u1max,uavg
         call writloga('default',1,logmess,0,ierr)
         write(logmess,9003) 'vic     ',iv1min,v1min,iv1max,v1max,vavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'wic     ',iw1min,w1min,iw1max,w1max,wavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'pres     ',ip1min,p1min,ip1max,p1max,pavg
         call writloga('default',1,logmess,0,ierr)
         write(logmess,9003) 'dens     ',ir1min,r1min,ir1max,r1max,ravg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'rsplic  ',irsp1min,rsp1min,
     *                       irsp1max,rsp1max,rspavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'eic     ',ie1min,e1min,ie1max,e1max,eavg
         call writloga('default',1,logmess,0,ierr)
         write(logmess,9003) 'tic     ',it1min,t1min,it1max,t1max,tavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'tric    ',itr1min,tr1min,
     *                       itr1max,tr1max,travg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'pric    ',ipr1min,pr1min,
     *                       ipr1max,pr1max,pravg
         call writloga('default',1,logmess,0,ierr)
         write(logmess,9003) 'xopic   ',ixop1min,xop1min,
     *                       ixop1max,xop1max,xopavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'xmic    ',ixm1min,xm1min,
     *                       ixm1max,xm1max,xmavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'volic   ',ivol1min,vol1min,
     *                       ivol1max,vol1max,volavg
         call writloga('default',1,logmess,0,ierr)
         write(logmess,9003) 'csqic   ',icsq1min,csq1min,
     *                       icsq1max,csq1max,csqavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'etotic  ',iet1min,et1min,
     *                       iet1max,et1max,etavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'pbtic   ',ipbt1min,pbt1min,
     *                       ipbt1max,pbt1max,pbtavg
         call writloga('default',1,logmess,0,ierr)
         write(logmess,9003) 'pbiic   ',ipbi1min,pbi1min,
     *                       ipbi1max,pbi1max,pbiavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'dedmric ',ided1min,ded1min,
     *                       ided1max,ded1max,dedmravg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'gic     ',ig1min,g1min,ig1max,g1max,gavg
         call writloga('default',1,logmess,0,ierr)
         write(logmess,9003) 'yldic   ',iyld1min,yld1min,
     *                       iyld1max,yld1max,yldavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,9003) 'epsic   ',ieps1min,eps1min,
     *                       ieps1max,eps1max,epsavg
         call writloga('default',0,logmess,0,ierr)
C
C        _______________________________________________________________
C
      endif
C
C     ******************************************************************
C
C     RELEASE DYNAMIC MEMORY.
C
 9998 continue
      call mmrelprt(isubname,ics)
C
C     ******************************************************************
C
C     SET ERROR INDICATOR.
C
C
      ierr3=0
C
C     ******************************************************************
C
C     SET UP THE USUAL CFT IMMUNE STATEMENT 9999 IN CASE DDT IS NEEDED.
C
      goto 9999
 9999 continue
C
C     ******************************************************************
C
      return
      end
