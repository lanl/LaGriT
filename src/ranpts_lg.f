*dk,ranpts_lg
      subroutine ranpts_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
 
C#######################################################################
C
C     PURPOSE -
C       This routine is used to add random points to the region of space
C       defined by the minimum and maximum coordinate values
C       using the specified geometry, plus the specified local origin shift
C       (local origin specified in xyz coordinates!)
C
C     INPUT ARGUMENTS -
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C        ierr - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     LOCAL VARIABLES -
C        * GEOMETRY LABEL
C           cgeom =  xyz ==> x , y , z cartesian geometry
C                 =  rtz ==> r , theta , z ==> cylindrical geometry
C                 =  rtp ==> r , theta , phi  ==> spherical geometry
C                            theta runs from 0 to 180, 0 is +z axis
C           NOTE: angles are in degrees
C        * TARGET POINT SPACING
C           spacing (uniform)
C
C        * MIN,MAX COORDINATES (in "cgeom" coordinates)
C           rext(1,j)=(rmin1,rmin2,rmin3)
C           rext(2,j)=(rmax1,rmax2,rmax3)
C        * CARTESIAN COORDINATES OF LOCAL ORIGIN (in "xyz" coordinates)
C           roff(j)=(xoff,yoff,zoff)
C        * EDGE PROTECTION DISTANCE
C           edgedist   (recommended: spacing/2)
C        * RANDOM NUMBER SEEDS
C           ranseed1,ranseed2 (large-ish integers, ranseed1>ranseed2)
C
C     FORMAT -
C
C       RANPTS / cgeom / spacing /  rmin1,rmin2,rmin3 / rmax1,rmax2,rmax3 &
C                 [/ xoff,yoff,zoff / edgedist / ranseed1,ranseed2 ]
C
C     DEFAULTS if not present -
C       "ranpts" without any arguments will add a point at the origin
C       cgeom = 'xyz'
C            allowed values are [xyz|XYZ|rtz|RTZ|rtp|RTP]
C            (anything else except blank returns error)
C       spacing = 1
C       rext(1,j) = 0
C       rext(2,j) = rext(1,j)
C       roff(j) = 0
C       edgedist = 1/2*spacing
C       ranseed1,ranseed2 = -1 (random number generator not re-seeded)
C
C     EXAMPLES -
C       random points with target spacing 0.1 in a 1x1x1 box
C             ranpts / xyz / .1 / 0 0 0 / 1 1 1 /
C       random points with target spacing 0.1 in a cylinder
C          of radius 1 centered at xyz=(2,3,4) and with an
c          edge protection distance of 0.2
C             ranpts/ rtz / .1 /  0,0,0 / 1,180,360 / 2,3,4 / 0.2
C       random points with target spacing 0.5 on the surface of a sphere
C          of radius 5 with new random seeds
C             ranpts/ rtp / .5 /  5,0,0 / 5,180,360 /  , ,  /  / 98765 4321/
C
C     CAVEATS -
C       * filter should be used afterwards to remove possibly duplicate points
C       * mistyped input after "ranpts/[cgeom]" always returns successful
C         point addition, but may be very different than desired
C       * not clever about area(theta) for rtp geom -> constrained theta
C         to be between 0 and 180 so don't have to worry about negative areas.
C         Also constrained r to be positive for rtz,rtp
C
C     CHANGE HISTORY -
C
C        $Log: ranpts_lg.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.4   23 Aug 2001 12:58:14   jtg
CPVCS    generates warning instead of error if ndimensions_geom not 3
CPVCS    
CPVCS       Rev 1.3   Thu Apr 06 13:40:18 2000   dcg
CPVCS    replace get_info_i and set_info_i calls
CPVCS
CPVCS       Rev 1.2   Thu Oct 07 13:35:34 1999   gable
CPVCS    Changed format of screen output from i6 to i11
CPVCS
CPVCS       Rev 1.1   Sat Jul 10 22:02:46 1999   jtg
CPVCS    fixed typo in header examples.
CPVCS
CPVCS       Rev 1.0   Sat Jul 10 20:48:12 1999   jtg
CPVCS    Initial revision.
C
C#######################################################################
c ........................................................................
 
      implicit none
 
      include 'chydro.h'
      include 'consts.h'
 
      integer nwds, imsgin(nwds), msgtype(nwds)
      real*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      character*132 logmess
 
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      real*8 xic(*), yic(*), zic(*)
 
      character*32 cgeom
      character*32 cmo, isubname
 
      integer ierr,icscode,ipointi,ipointj,ilen,ityp
     &  ,icount,nnodes,nnodes_new,nnodes_save,icntin
     &  ,j,iwd,i1,i2,i3,i4,geom_dim
     &  ,nadd(14),ranseed1,ranseed2,ind,maxends(5)
 
      real*8 rrr,theta,phi,degtorad,pi,xxx,rand_lg
     &  ,spacing,edgedist,rext(2,3),roff(3)
     &  ,r0(14),r1(14)
 
C#######################################################################
C get some initial info
 
      isubname='ranpts_lg'
 
      ierr=0
 
      call cmo_get_name(cmo,ierr)
      if (ierr.ne.0) then
         ierr=1
         goto 9999
      endif
 
      call cmo_get_info('ipointi',cmo
     &                ,ipointi,ilen,ityp,ierr)
      if (ierr.ne.0) then
         ierr=2
         goto 9999
      endif
      call cmo_get_info('ipointj',cmo
     &                ,ipointj,ilen,ityp,ierr)
      if (ierr.ne.0) then
         ierr=3
         goto 9999
      endif
 
      call cmo_get_info('nnodes',cmo,nnodes,ilen,ityp,ierr)
      if (ierr.ne.0) then
         ierr=4
         goto 9999
      endif
 
      call cmo_get_info('ndimensions_geom',cmo,geom_dim
     &                  ,ilen,ityp,ierr)
      if (ierr.ne.0) then
         ierr=5
         goto 9999
      elseif (geom_dim.ne.3) then
         write(logmess,*) '  RANPTS WARNING - ndimensions_geom.ne.3'
         call writloga('default',0,logmess,0,icscode)
         ! ierr=6
         ! goto 9999
      endif
 
C ******************************************************************
C assign the constants
 
      pi=four*atan(one)
      degtorad=pi/180.0d0
 
C assign the local variables
 
C assign the geometry type
      if (msgtype(2).eq.1.or.msgtype(2).eq.2) then
         ! probably mistyped
         ierr=7
         goto 9999
      elseif (msgtype(2).ne.3) then
         cgeom='xyz'
      else
         cgeom=cmsgin(2)
         if (cgeom.eq.'RTZ') then
            cgeom='rtz'
         elseif (cgeom.eq.'RTP') then
            cgeom='rtp'
         elseif (cgeom.eq.'XYZ') then
            cgeom='xyz'
         elseif (cgeom.ne.'rtz'.and.cgeom.ne.'rtp'
     &       .and.cgeom.ne.'xyz') then
            ! probably mistyped
            ierr=8
            goto 9999
         endif
      endif
 
      icount=ipointj
 
C assign the spacing
      if (nwds.lt.3) then
         spacing=one
      elseif (msgtype(3) .eq. 1) then
         spacing=imsgin(3)
      elseif (msgtype(3) .eq. 2) then
         spacing=xmsgin(3)
      else
         spacing=one
      endif
      if (spacing.le.zero) goto 9999
 
C assign the minima
      do j=1,3
         iwd=j+3
         if (iwd.gt.nwds) then
            rext(1,j)=zero
         elseif (msgtype(iwd) .eq. 1) then
            rext(1,j)=imsgin(iwd)
         elseif (msgtype(iwd) .eq. 2) then
            rext(1,j)=xmsgin(iwd)
         else
            rext(1,j)=zero
         endif
      enddo
 
C assign the maxima
      do j=1,3
         iwd=j+6
         if (iwd.gt.nwds) then
            rext(2,j)=rext(1,j)
         elseif (msgtype(iwd) .eq. 1) then
            rext(2,j)=imsgin(iwd)
         elseif (msgtype(iwd) .eq. 2) then
            rext(2,j)=xmsgin(iwd)
         else
            rext(2,j)=rext(1,j)
         endif
      enddo
 
C check the extremes
 
      ! check min,max relation correct
      do j=1,3
         if (rext(1,j).gt.rext(2,j)) then
            goto 9999
         elseif (rext(1,j).eq.rext(2,j)) then
            maxends(j)=1
         else
            maxends(j)=2
         endif
      enddo
 
      i1=0
      ! make sure r positive for rtz,rtp geometries
      if (cgeom.eq.'rtp'.or.cgeom.eq.'rtz') then
         if (rext(2,1).lt.zero) then
            goto 9999
         elseif (rext(1,1).le.zero) then
            maxends(1)=-maxends(1)
            if (rext(1,1).lt.zero) i1=1
            rext(1,1)=zero
         endif
      endif
      if (cgeom.eq.'rtp') then
         ! restrict theta to [0,180]
         if (rext(2,2).lt.zero.or.rext(1,2).gt.180.d0) goto 9999
         maxends(4)=2
         if (rext(1,2).le.zero) then
            if (rext(1,2).lt.zero) i1=i1+2
            rext(1,2)=zero
            maxends(4)=1
         endif
         maxends(5)=2
         if (rext(2,2).ge.180.d0) then
            if (rext(2,2).gt.180.d0) i1=i1+4
            rext(2,2)=180.d0
            maxends(5)=1
         endif
         ! restrict phi range to 360 degrees
         phi=rext(2,3)-rext(1,3)
         if (phi.ge.360.d0) then
            if (phi.gt.360.d0) i1=i1+8
            maxends(3)=1
            rext(2,3)=rext(1,3)+360.d0
         endif
         ! convert angles to radians
         rext(1,2)=rext(1,2)*degtorad
         rext(2,2)=rext(2,2)*degtorad
         rext(1,3)=rext(1,3)*degtorad
         rext(2,3)=rext(2,3)*degtorad
      elseif (cgeom.eq.'rtz') then
         ! restrict theta range to 360 degrees
         theta=rext(2,2)-rext(1,2)
         if (theta.ge.360.d0) then
            if (theta.gt.360.d0) i1=i1+2
            maxends(2)=1
            rext(2,2)=rext(1,2)+360.d0
         endif
         ! convert angles to radians
         rext(1,2)=rext(1,2)*degtorad
         rext(2,2)=rext(2,2)*degtorad
      endif
      if (i1.gt.0) then
         if (cgeom.eq.'rtp') then
            write(logmess,*)
     &          'RANPTS WARNING: r,theta,phi range truncated ',i1
         else
            write(logmess,*)
     &          'RANPTS WARNING: r,theta range truncated ',i1
         endif
         call writloga('default',0,logmess,0,icscode)
      endif
 
C assign the origin shift
      do j=1,3
         iwd=j+9
         if (iwd.gt.nwds) then
            roff(j)=zero
         elseif (msgtype(iwd) .eq. 1) then
            roff(j)=imsgin(iwd)
         elseif (msgtype(iwd) .eq. 2) then
            roff(j)=xmsgin(iwd)
         else
            roff(j)=zero
         endif
      enddo
 
C assign the edge protection factor
      if (nwds.lt.13) then
         edgedist=half*spacing
      elseif (msgtype(13) .eq. 1) then
         edgedist=imsgin(13)
      elseif (msgtype(13) .eq. 2) then
         edgedist=xmsgin(13)
      else
         edgedist=half*spacing
      endif
 
C assign the random_number seeds (use golden mean x 10^8 as default)
      if (nwds.ge.15) then
         if (msgtype(14) .eq. 1) then
            ranseed1=imsgin(14)
         elseif (msgtype(14) .eq. 2) then
            ranseed1=xmsgin(14)
         else
            ranseed1=-1
         endif
         if (msgtype(15) .eq. 1) then
            ranseed2=imsgin(15)
         elseif (msgtype(15) .eq. 2) then
            ranseed2=xmsgin(15)
         else
            ranseed2=-1
         endif
         if (ranseed1.gt.0.and.ranseed2.gt.0) then
            call seed_rand_lg(ranseed1,ranseed2)
         endif
      endif
 
C ******************************************************************
C figure out number of points to add
c inside is offset from outside by edgedist
c add points on the 8 corners + the "insides" of the 12 lines + 6 faces + volume
c need to use "insides" in order to not screw up lagrit's connect algorithm
c (spacing between inside and surface determined by edgedist)
 
      if (cgeom.eq.'rtp') then ! ........................................
 
c n(j)=d(j)/spacing:
c   dr=rmax-rmin, rav=(rmax+rmin)/2,
c   dtheta=thetamax-thetamin, thetaav=(thetamax-thetamin)/2,
c       dcostheta=cos(thetamin)-cos(thetamax)
c   dphi=phimax-phimin
c   d(1)=dr, d(2)=rmin*dtheta, d(3)=rmax*dtheta
c   A(r,theta) = dtheta*(rmax^2-rmin^2)/2 = d(5)*d(1)
c   -> d(4) = rav*dtheta
c   d(5)=rmin*sin(thetamin)*dphi
c   d(6)=rmax*sin(thetamin)*dphi
c   d(7)=rmin*sin(thetamax)*dphi
c   d(8)=rmax*sin(thetamax)*dphi
c   A(r,phi) at thetamin = dphi*sin(thetamin)*(rmax^2-rmin^2)/2 = d(9)*d(1)
c   -> d(9)=dphi*sin(thetamin)*rav
c   A(r,phi) at thetamax = dphi*sin(thetamax)*(rmax^2-rmin^2)*/2 = d(10)*d(1)
c   -> d(10)=dphi*sin(thetamax)*rav
c   A(theta,phi) at rmin = dphi*rmin^2*dcostheta = d(2)*d(11)
c   -> d(11)=dphi*rmin*dcostheta/dtheta
c   A(theta,phi) at rmax = dphi*rmax^2*dcostheta = d(3)*d(12)
c   -> d(12)=dphi*rmax*dcostheta/dtheta
c   V(r,theta,phi) = dphi*dcostheta*(rmax^3-rmin^3)/3 = d(1)*d(4)*d(13)
c   -> d(13)=dphi*dcostheta*(rmax^2+rmax*rmin+rmin^2)/(3*dtheta)
c Note: 11,12,13 need theta range to be between 0-180 => constrained
c rescale and add xyz origin shift after assignment
 
         nadd(1)=(rext(2,1)-rext(1,1)-edgedist)/spacing
         r0(1)=rext(1,1)+edgedist
         r1(1)=rext(2,1)-rext(1,1)-two*edgedist
         theta=(rext(2,2)-rext(1,2))
         do i1=1,2
            i2=1+i1
            rrr=rext(i1,1)
            nadd(i2)=(rrr*theta-edgedist)/spacing
            if (rrr.le.zero) then
               if (edgedist.gt.zero) then
                  rrr=edgedist
               else
                  rrr=spacing
               endif
            endif
            if (rrr.gt.zero) then
               xxx=edgedist/rrr
               r0(i2)=rext(1,2)+xxx
               r1(i2)=rext(2,2)-rext(1,2)-two*xxx
            endif
         enddo
         rrr=half*(rext(1,1)+rext(2,1))
         nadd(4)=(rrr*theta-edgedist)/spacing
         phi=(rext(2,3)-rext(1,3))
         do i1=1,2
         do i2=1,2
            i3=4+i1+(i2-1)*2
            rrr=rext(i1,1)
            theta=rext(i2,2)
            rrr=rrr*sin(theta)
            nadd(i3)=(rrr*phi-edgedist)/spacing
            if (rrr.le.zero) then
               if (edgedist.gt.zero) then
                  rrr=edgedist
               else
                  rrr=spacing
               endif
            endif
            if (rrr.gt.zero) then
               xxx=edgedist/rrr
               r0(i3)=rext(1,3)+xxx
               r1(i3)=rext(2,3)-rext(1,3)-two*xxx
            endif
         enddo
         enddo
         rrr=half*(rext(1,1)+rext(2,1))
         xxx=sin(rext(1,2))
         nadd(9)=(rrr*xxx*phi-edgedist)/spacing
         xxx=sin(rext(2,2))
         nadd(10)=(rrr*xxx*phi-edgedist)/spacing
         theta=(rext(2,2)-rext(1,2))
         if (theta.gt.zero) then
            xxx=(cos(rext(1,2))-cos(rext(2,2)))/theta
            rrr=(rext(2,1)**2+rext(2,1)*rext(1,1)+rext(1,1)**2)*one3rd
            nadd(11)=(phi*xxx*rext(1,1)-edgedist)/spacing
            nadd(12)=(phi*xxx*rext(2,1)-edgedist)/spacing
            nadd(13)=(phi*xxx*rrr-edgedist)/spacing
         else
            nadd(11)=0
            nadd(12)=0
            nadd(13)=0
         endif
         do ind=1,13
            if (nadd(ind).lt.0) nadd(ind)=0
         enddo
 
         ! maxends(1)=-2 if rmin=0, rmax>0
         !            =-1 if rmin=rmax=0
         !            =1 if rmin=rmax>0
         !            =2 if rmin>0, rmax>rmin
         ! maxends(2)=1 if thetamin=thetamax
         !            =2 if thetamin<thetamax
         ! maxends(4)=2 if 0<thetamin
         !            =1 if 0=thetamin
         ! maxends(5)=2 if thetamax<pi
         !            =1 if thetamax=pi
         ! maxends(3)=1 if phimax-phimin=2*pi or phimax=phimin
         !            =2 if 0<phimax-phimin<2*pi
 
         ! only need to worry about r overwrites if rmin=0
         !    for the p-edges (see below)
         ! should modify nadds so, eg, use n_vol rather than n1*n2*n3
 
         ! corners ...........
         nnodes_new=maxends(3)*maxends(2)*abs(maxends(1))
         ! r edges ...........
         ! correct for double count of ends if maxends(3)=2
         ! but thetamin=0 (maxends(4)=1) or thetamax=pi (maxends(5)=1))
         ! => use maxends 3->4,5 for thetamin,max
         do i2=1,maxends(2)
         i4=3+i2
         do i3=1,min(maxends(i4),maxends(3))
            nnodes_new=nnodes_new+nadd(1)
         enddo
         enddo
         ! t edges ...........
         if (maxends(1).lt.0) then
            ! don't do r overwrites if rmin=0 -> only do rmax
            j=abs(maxends(1))
         else
            ! rmin>0 -> do both rmin and rmax
            j=1
         endif
         do i1=j,abs(maxends(1))
            i2=1+i1
            nnodes_new=nnodes_new+nadd(i2)*maxends(3)
         enddo
         ! p edges ...........
         do i1=1,abs(maxends(1))
         do i2=1,maxends(2)
            i3=4+i1+(i2-1)*2
            nnodes_new=nnodes_new+nadd(i3)
         enddo
         enddo
         ! rt faces ...........
         nnodes_new=nnodes_new+nadd(1)*nadd(4)*maxends(3)
         ! rp faces ...........
         do i2=1,maxends(2)
            i3=8+i2
            nnodes_new=nnodes_new+nadd(1)*nadd(i3)
         enddo
         ! tp faces ...........
         do i1=1,abs(maxends(1))
            i2=1+i1
            i3=10+i1
            nnodes_new=nnodes_new+nadd(i2)*nadd(i3)
         enddo
         ! rtp volume ..........
         nnodes_new=nnodes_new+nadd(1)*nadd(4)*nadd(13)
 
      else if (cgeom.eq.'rtz') then ! ...................................
 
c n(j)=d(j)/spacing:
c   dr=rmax-rmin, rav=(rmax+rmin)/2, dtheta=thetamax-thetamin, dz=zmax-zmin
c   d(1)=dr, d(2)=rmin*dtheta, d(4)=rmax*dtheta, d(3)=dz
c   A(r,theta)=dtheta*(rmax^2-rmin^2)/2=d(5)*d(1) -> d(5)=rav*dtheta
c rescale and add xyz origin shift after assignment
 
         do j=1,3,2
            nadd(j)=(rext(2,j)-rext(1,j)-edgedist)/spacing
            r0(j)=rext(1,j)+edgedist
            r1(j)=rext(2,j)-rext(1,j)-two*edgedist
         enddo
 
         theta=(rext(2,2)-rext(1,2))
         do i1=1,2
            i2=2*i1
            rrr=rext(i1,1)
            nadd(i2)=(rrr*theta-edgedist)/spacing
            if (rrr.le.zero) then
               if (edgedist.gt.zero) then
                  rrr=edgedist
               else
                  rrr=spacing
               endif
            endif
            if (rrr.gt.zero) then
               xxx=edgedist/rrr
               r0(i2)=rext(1,2)+xxx
               r1(i2)=rext(2,2)-rext(1,2)-two*xxx
            endif
         enddo
         rrr=half*(rext(1,1)+rext(2,1))
         nadd(5)=(rrr*theta-edgedist)/spacing
 
         do ind=1,5
            if (nadd(ind).lt.0) nadd(ind)=0
         enddo
 
         ! maxends(1)=-2 if rmin=0, rmax>0
         !            =-1 if rmin=rmax=0
         !            =1 if rmin=rmax>0
         !            =2 if rmin>0, rmax>rmin
         ! maxends(2)=1 if thetamax-thetamin=2*pi or thetamax=thetamin
         !            =2 if 0<thetamax-thetamin<2*pi
         ! maxends(3)=1 if zmin=zmax
         !            =2 if zmin<zmax
 
         ! z-edges r overwrite if rmin=0 fixed (see below)
         ! should modify nadds so, eg, use n_vol rather than n1*n2*n3
 
         ! corners ...........
         nnodes_new=maxends(3)*maxends(2)*abs(maxends(1))
         ! r edges ...........
         nnodes_new=nnodes_new+nadd(1)*maxends(2)*maxends(3)
         ! z edges ...........
         if (maxends(1).lt.0) then
            ! don't do r overwrites if rmin=0 -> only do 1 theta at rmin=0
            do i1=1,abs(maxends(1))
            do i2=1,min(i1,maxends(2))
               nnodes_new=nnodes_new+nadd(3)
            enddo
            enddo
         else
            ! do both theta's at rmin>0
            nnodes_new=nnodes_new+nadd(3)*abs(maxends(1))*maxends(2)
         endif
         ! t edges ...........
         do i1=1,abs(maxends(1))
            i2=2*i1
            nnodes_new=nnodes_new+nadd(i2)*maxends(3)
         enddo
         ! rz faces ...........
         nnodes_new=nnodes_new+nadd(1)*nadd(3)*maxends(2)
         ! tz faces ...........
         do i1=1,abs(maxends(1))
            i2=2*i1
            nnodes_new=nnodes_new+nadd(i2)*nadd(3)
         enddo
         ! rt faces ...........
         nnodes_new=nnodes_new+nadd(1)*nadd(5)*maxends(3)
         ! rtz volume ..........
         nnodes_new=nnodes_new+nadd(1)*nadd(5)*nadd(3)
 
      else ! if (geom.eq.'xyz') then ! .................................
 
c n(j)=d(j)/spacing:
c   dr(j)=rmax(j)-rmin(j)
 
         do j=1,3
            nadd(j)=(rext(2,j)-rext(1,j)-edgedist)/spacing
            r0(j)=rext(1,j)+edgedist
            r1(j)=rext(2,j)-rext(1,j)-two*edgedist
         enddo
 
         ! modify so, eg, use n_vol rather than n1*n2*n3:
         !   nadd(4) vs nadd(2)*nadd(3)
         !   nadd(5) vs nadd(1)*nadd(3)
         !   nadd(6) vs nadd(1)*nadd(2)
         !   nadd(7) vs nadd(1)*nadd(2)*nadd(3)
         xxx=spacing*spacing
         if (nadd(2).le.0.or.nadd(3).le.0) then
            nadd(4)=0
         else
            nadd(4)=(rext(2,2)-rext(1,2)-edgedist)
     &             *(rext(2,3)-rext(1,3)-edgedist)/xxx
         endif
         if (nadd(1).le.0.or.nadd(3).le.0) then
            nadd(5)=0
         else
            nadd(5)=(rext(2,1)-rext(1,1)-edgedist)
     &             *(rext(2,3)-rext(1,3)-edgedist)/xxx
         endif
         if (nadd(1).le.0.or.nadd(2).le.0) then
            nadd(6)=0
         else
            nadd(6)=(rext(2,2)-rext(1,2)-edgedist)
     &             *(rext(2,1)-rext(1,1)-edgedist)/xxx
         endif
         xxx=xxx*spacing
         if (nadd(1).le.0.or.nadd(2).le.0.or.nadd(3).le.0) then
            nadd(7)=0
         else
            nadd(7)=(rext(2,1)-rext(1,1)-edgedist)
     &             *(rext(2,2)-rext(1,2)-edgedist)
     &             *(rext(2,3)-rext(1,3)-edgedist)/xxx
         endif
 
         ! zero any negative n's
         do ind=1,7
            if (nadd(ind).lt.0) nadd(ind)=0
         enddo
 
         ! maxends(j)=1 if rmin=rmax
         !            =2 if rmin<rmax
 
         ! corners ...........
         nnodes_new=maxends(3)*maxends(2)*maxends(1)
         ! xyz edges ...........
         nnodes_new=nnodes_new+nadd(1)*maxends(2)*maxends(3)
         nnodes_new=nnodes_new+nadd(2)*maxends(1)*maxends(3)
         nnodes_new=nnodes_new+nadd(3)*maxends(1)*maxends(2)
         ! xyz faces ...........
         nnodes_new=nnodes_new+nadd(4)*maxends(1)
         nnodes_new=nnodes_new+nadd(5)*maxends(2)
         nnodes_new=nnodes_new+nadd(6)*maxends(3)
         ! xyz volume ..........
         nnodes_new=nnodes_new+nadd(7)
 
      endif !...........................................................
 
C ******************************************************************
C increment storage
 
      icntin=icount
      nnodes_save=ipointj
      nnodes=nnodes+nnodes_new
      call cmo_set_info('nnodes',cmo,nnodes,1,1,ierr)
      if (ierr.ne.0) then
        ierr=1
        goto 9998
      endif
      call cmo_newlen(cmo,ierr)
      if (ierr.ne.0) then
        ierr=2
        goto 9998
      endif
      call cmo_get_info('xic',cmo,ipxic,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=3
        goto 9998
      endif
      call cmo_get_info('yic',cmo,ipyic,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=4
        goto 9998
      endif
      call cmo_get_info('zic',cmo,ipzic,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=5
        goto 9998
      endif
 
C ******************************************************************
C TRANSFORM POINTS AND VELOCITIES TO LOCAL COORD. SYSTEM
C (copied from RZ)
 
      if (normflgc .gt. 0) call chglocl(1,icount,1)
 
C ******************************************************************
C assign the new node positions
c inside is offset from outside by edgedist
c add points on the 8 corners + the "insides" of the 12 lines + 6 faces + volume
c need to use "insides" in order to not screw up lagrit's connect algorithm
c (see notes in counting section above)
c Note that for x=(y*((1+r)^p-r^p)+r^p)^(1/p)-r
c with r=r0/r1 and y uniformly distributed between 0 and 1,
c then x is between 0 and 1 and (r0+x*r1)^p is
c uniformly distributed between r0^p and (r0+r1)^p
c Note that for x=(1/th1)*cos^(-1)(cos(th0)+y*(cos(th0+th1)-cos(th0)))-th0/th1
c with y uniformly distributed between 0 and 1,
c then x is between 0 and 1 and cos(th0+x*th1) is
c uniformly distributed between cos(th0) and cos(th0+th1)
 
      call mmverify()
 
c     ! corners, all geomtries  .........
      do i1=1,abs(maxends(1))
      do i2=1,maxends(2)
      do i3=1,maxends(3)
         icount=icount+1
         xic(icount)=rext(i1,1)
         yic(icount)=rext(i2,2)
         zic(icount)=rext(i3,3)
      enddo
      enddo
      enddo
 
      if (cgeom.eq.'rtp') then ! ........................................
c (see notes in counting section above)
 
         ! maxends(1)=-2 if rmin=0, rmax>0
         !            =-1 if rmin=rmax=0
         !            =1 if rmin=rmax>0
         !            =2 if rmin>0, rmax>rmin
         ! maxends(2)=1 if thetamin=thetamax
         !            =2 if thetamin<thetamax
         ! maxends(4)=2 if 0<thetamin
         !            =1 if 0=thetamin
         ! maxends(5)=2 if thetamax<pi
         !            =1 if thetamax=pi
         ! maxends(3)=1 if phimax-phimin=2*pi or phimax=phimin
         !            =2 if 0<phimax-phimin<2*pi
 
         phi=rext(2,3)-rext(1,3)
         if (maxends(3).ne.1) phi=zero
 
c        ! r edges ...........
         ! use phi maxends 3->4,5 for thetamin,max
         do i1=1,nadd(1)
         do i2=1,maxends(2)
         i4=3+i2
         do i3=1,min(maxends(i4),maxends(3))
            icount=icount+1
            rrr=r0(1)+r1(1)*rand_lg()
            xic(icount)=rrr
            if (phi.gt.pi.and.maxends(i4).eq.1.and.rrr.gt.zero) then
               xxx=edgedist/rrr
               yic(icount)=rext(i2,2)-xxx+rand_lg()*two*xxx
               zic(icount)=rext(i3,3)
            else
               xxx=rext(i2,2)
               yic(icount)=xxx
               rrr=rrr*sin(xxx)
               if (phi.gt.pi.and.rrr.gt.0) then
                  xxx=edgedist/rrr
                  zic(icount)=rext(i3,3)-xxx+rand_lg()*two*xxx
               else
                  zic(icount)=rext(i3,3)
               endif
            endif
         enddo
         enddo
         enddo
c        ! t edges ...........
         if (maxends(1).lt.0) then
            j=abs(maxends(1))
         else
            j=1
         endif
         do i1=j,abs(maxends(1))
         i2=1+i1
         do i4=1,nadd(i2)
         do i3=1,maxends(3)
            icount=icount+1
            rrr=rext(i1,1)
            xic(icount)=rrr
            xxx=r0(i2)+r1(i2)*rand_lg()
            yic(icount)=xxx
            rrr=rrr*sin(xxx)
            if (phi.gt.pi.and.rrr.gt.0) then
               xxx=edgedist/rrr
               zic(icount)=rext(i3,3)-xxx+rand_lg()*two*xxx
            else
               zic(icount)=rext(i3,3)
            endif
         enddo
         enddo
         enddo
c        ! p edges ...........
         do i1=1,abs(maxends(1))
         do i2=1,maxends(2)
         i3=4+i1+(i2-1)*2
         do i4=1,nadd(i3)
            icount=icount+1
            xic(icount)=rext(i1,1)
            yic(icount)=rext(i2,2)
            zic(icount)=r0(i3)+r1(i3)*rand_lg()
         enddo
         enddo
         enddo
 
c        ! rt faces ...........
         theta=rext(2,2)-rext(1,2)
         do i4=1,nadd(1)*nadd(4)
         do i3=1,maxends(3)
            icount=icount+1
            rrr=r0(1)/r1(1)
            rrr=sqrt(rrr*rrr+rand_lg()*(one+two*rrr))-rrr
            rrr=r0(1)+r1(1)*rrr
            xic(icount)=rrr
            if (rrr*theta.gt.two*edgedist) then
               xxx=edgedist/rrr
               xxx=rext(1,2)+xxx+rand_lg()*(theta-two*xxx)
            else
               xxx=half*(rext(2,2)+rext(1,2))
            endif
            yic(icount)=xxx
            rrr=rrr*sin(xxx)
            if (phi.gt.pi.and.rrr.gt.0) then
               xxx=edgedist/rrr
               zic(icount)=rext(i3,3)-xxx+rand_lg()*two*xxx
            else
               zic(icount)=rext(i3,3)
            endif
         enddo
         enddo
c        ! rp faces ...........
         do i2=1,maxends(2)
         i3=8+i2
         do i4=1,nadd(1)*nadd(i3)
            icount=icount+1
            rrr=r0(1)/r1(1)
            rrr=sqrt(rrr*rrr+rand_lg()*(one+two*rrr))-rrr
            rrr=r0(1)+r1(1)*rrr
            xic(icount)=rrr
            theta=rext(i2,2)
            yic(icount)=theta
            rrr=rrr*sin(theta)
            phi=rext(2,3)-rext(1,3)
            if (rrr*phi.gt.two*edgedist) then
               xxx=edgedist/rrr
               zic(icount)=rext(1,3)+xxx+rand_lg()*(phi-two*xxx)
            else
               zic(icount)=half*(rext(2,3)+rext(1,3))
            endif
         enddo
         enddo
c        ! tp faces ...........
         do i1=1,abs(maxends(1))
         i2=1+i1
         i3=10+i1
         do i4=1,nadd(i2)*nadd(i3)
            icount=icount+1
            rrr=rext(i1,1)
            xic(icount)=rrr
            theta=r0(i2)
            phi=r1(i2)
            xxx=cos(theta)
            xxx=acos(  (cos(theta+phi)-xxx)*rand_lg() + xxx )
     &                           /phi - theta/phi
            theta=r0(i2)+xxx*r1(i2)
            yic(icount)=theta
            rrr=rrr*sin(theta)
            phi=rext(2,3)-rext(1,3)
            if (rrr*phi.gt.two*edgedist) then
               xxx=edgedist/rrr
               zic(icount)=rext(1,3)+xxx+rand_lg()*(phi-two*xxx)
            else
               zic(icount)=half*(rext(2,3)+rext(1,3))
            endif
         enddo
         enddo
 
c        ! rtp volume ..........
         do i4=1,nadd(1)*nadd(4)*nadd(13)
            icount=icount+1
 
            rrr=r0(1)/r1(1)
            rrr=( (one+3.d0*rrr+3.d0*rrr*rrr)*rand_lg()
     &              +rrr*rrr*rrr )**one3rd - rrr
            rrr=r0(1)+r1(1)*rrr
            xic(icount)=rrr
 
            theta=rext(2,2)-rext(1,2)
            if (rrr*theta.gt.two*edgedist) then
               xxx=edgedist/rrr
               phi=theta-two*xxx
               theta=rext(1,2)+xxx
               xxx=cos(theta)
               xxx=acos( (cos(theta+phi)-xxx)*rand_lg() + xxx )
     &                         /phi - theta/phi
               theta=theta+xxx*phi
            else
               theta=half*(rext(2,2)+rext(1,2))
            endif
            yic(icount)=theta
 
            rrr=rrr*sin(theta)
            phi=rext(2,3)-rext(1,3)
            if (rrr*phi.gt.two*edgedist) then
               xxx=edgedist/rrr
               zic(icount)=rext(1,3)+xxx+rand_lg()*(phi-two*xxx)
            else
               zic(icount)=half*(rext(2,3)+rext(1,3))
            endif
 
         enddo
 
         if (icount.ne.icntin+nnodes_new) goto 9998
 
c        ! convert spherical to xyz
         do ind=icntin+1,icntin+nnodes_new
            rrr=xic(ind)
            theta=yic(ind)
            phi=zic(ind)
            xic(ind)=rrr*sin(theta)*cos(phi)
            yic(ind)=rrr*sin(theta)*sin(phi)
            zic(ind)=rrr*cos(theta)
         enddo
 
      else if (cgeom.eq.'rtz') then ! ...................................
c (see notes in counting section above)
 
         ! maxends(1)=-2 if rmin=0, rmax>0
         !            =-1 if rmin=rmax=0
         !            =1 if rmin=rmax>0
         !            =2 if rmin>0, rmax>rmin
         ! maxends(2)=1 if thetamax-thetamin=2*pi or thetamax=thetamin
         !            =2 if 0<thetamax-thetamin<2*pi
         ! maxends(3)=1 if zmin=zmax
         !            =2 if zmin<zmax
 
c theta range = 360: should not even print theta faces or
c edges and should extend r faces,edges over whole
c side (so only 4 edges, 4 faces)
 
c        ! r edges ...........
         theta=rext(2,2)-rext(1,2)
         if (maxends(2).ne.1) theta=zero
         do i1=1,nadd(1)
         do i2=1,maxends(2)
         do i3=1,maxends(3)
            icount=icount+1
            rrr=r0(1)+r1(1)*rand_lg()
            xic(icount)=rrr
            if (theta.gt.pi.and.rrr.gt.zero) then
               phi=edgedist/rrr
               yic(icount)=rext(i2,2)-phi+rand_lg()*two*phi
            else
               yic(icount)=rext(i2,2)
            endif
            zic(icount)=rext(i3,3)
         enddo
         enddo
         enddo
c        ! z edges ...........
         do i3=1,nadd(3)
         do i1=1,abs(maxends(1))
            if (maxends(1).lt.0) then
               i4=min(i1,maxends(2))
            else
               i4=maxends(2)
            endif
            do i2=1,i4
               icount=icount+1
               rrr=rext(i1,1)
               xic(icount)=rrr
               if (theta.gt.pi.and.rrr.gt.zero) then
                  phi=edgedist/rrr
                  yic(icount)=rext(i2,2)-phi+rand_lg()*two*phi
               else
                  yic(icount)=rext(i2,2)
               endif
               zic(icount)=r0(3)+r1(3)*rand_lg()
            enddo
         enddo
         enddo
c        ! t edges ...........
         do i1=1,abs(maxends(1))
         i2=2*i1
         do i4=1,nadd(i2)
         do i3=1,maxends(3)
            icount=icount+1
            xic(icount)=rext(i1,1)
            yic(icount)=r0(i2)+r1(i2)*rand_lg()
            zic(icount)=rext(i3,3)
         enddo
         enddo
         enddo
 
c        ! rz faces ...........
         do i4=1,nadd(1)*nadd(3)
         do i2=1,maxends(2)
            icount=icount+1
            rrr=r0(1)+r1(1)*rand_lg()
            xic(icount)=rrr
            if (theta.gt.pi.and.rrr.gt.zero) then
               phi=edgedist/rrr
               yic(icount)=rext(i2,2)-phi+rand_lg()*two*phi
            else
               yic(icount)=rext(i2,2)
            endif
            zic(icount)=r0(3)+r1(3)*rand_lg()
         enddo
         enddo
 
c        ! tz faces ...........
         do i1=1,abs(maxends(1))
         i2=2*i1
         do i4=1,nadd(i2)*nadd(3)
            icount=icount+1
            xic(icount)=rext(i1,1)
            yic(icount)=r0(i2)+r1(i2)*rand_lg()
            zic(icount)=r0(3)+r1(3)*rand_lg()
         enddo
         enddo
 
c        ! rt faces ...........
         do i4=1,nadd(1)*nadd(5)
         do i3=1,maxends(3)
            icount=icount+1
            rrr=r0(1)/r1(1)
            rrr=sqrt(rrr*rrr+rand_lg()*(one+two*rrr))-rrr
            rrr=r0(1)+r1(1)*rrr
            xic(icount)=rrr
            theta=rext(2,2)-rext(1,2)
            if (rrr*theta.gt.two*edgedist) then
               phi=edgedist/rrr
               yic(icount)=rext(1,2)+phi+rand_lg()*(theta-two*phi)
            else
               yic(icount)=half*(rext(2,2)+rext(1,2))
            endif
            zic(icount)=rext(i3,3)
         enddo
         enddo
 
c        ! rtz volume ..........
         do i4=1,nadd(1)*nadd(5)*nadd(3)
            icount=icount+1
            rrr=r0(1)/r1(1)
            rrr=sqrt(rrr*rrr+rand_lg()*(one+two*rrr))-rrr
            rrr=r0(1)+r1(1)*rrr
            xic(icount)=rrr
            theta=rext(2,2)-rext(1,2)
            if (rrr*theta.gt.two*edgedist) then
               phi=edgedist/rrr
               yic(icount)=rext(1,2)+phi+rand_lg()*(theta-two*phi)
            else
               yic(icount)=half*(rext(2,2)+rext(1,2))
            endif
            zic(icount)=r0(3)+r1(3)*rand_lg()
         enddo
 
         if (icount.ne.icntin+nnodes_new) goto 9998
 
c        ! convert cylindrical to xyz
         do ind=icntin+1,icntin+nnodes_new
            rrr=xic(ind)
            theta=yic(ind)
            xic(ind)=rrr*cos(theta)
            yic(ind)=rrr*sin(theta)
            ! zic(ind)=zic(ind)
         enddo
 
      else ! if (geom.eq.'xyz') then ! .................................
c (see notes in counting section above)
 
         ! maxends(j)=1 if rmin=rmax
         !            =2 if rmin<rmax
 
c        ! xyz edges ...........
         do i1=1,nadd(1)
         do i2=1,maxends(2)
         do i3=1,maxends(3)
            icount=icount+1
            xic(icount)=r0(1)+r1(1)*rand_lg()
            yic(icount)=rext(i2,2)
            zic(icount)=rext(i3,3)
         enddo
         enddo
         enddo
         do i2=1,nadd(2)
         do i1=1,maxends(1)
         do i3=1,maxends(3)
            icount=icount+1
            xic(icount)=rext(i1,1)
            yic(icount)=r0(2)+r1(2)*rand_lg()
            zic(icount)=rext(i3,3)
         enddo
         enddo
         enddo
         do i3=1,nadd(3)
         do i1=1,maxends(1)
         do i2=1,maxends(2)
            icount=icount+1
            xic(icount)=rext(i1,1)
            yic(icount)=rext(i2,2)
            zic(icount)=r0(3)+r1(3)*rand_lg()
         enddo
         enddo
         enddo
 
c        ! xyz faces ...........
         do i4=1,nadd(4)
         do i1=1,maxends(1)
            icount=icount+1
            xic(icount)=rext(i1,1)
            yic(icount)=r0(2)+r1(2)*rand_lg()
            zic(icount)=r0(3)+r1(3)*rand_lg()
         enddo
         enddo
         do i4=1,nadd(5)
         do i2=1,maxends(2)
            icount=icount+1
            xic(icount)=r0(1)+r1(1)*rand_lg()
            yic(icount)=rext(i2,2)
            zic(icount)=r0(3)+r1(3)*rand_lg()
         enddo
         enddo
         do i4=1,nadd(6)
         do i3=1,maxends(3)
            icount=icount+1
            xic(icount)=r0(1)+r1(1)*rand_lg()
            yic(icount)=r0(2)+r1(2)*rand_lg()
            zic(icount)=rext(i3,3)
         enddo
         enddo
 
c        ! xyz volume ..........
         do i4=1,nadd(7)
            icount=icount+1
            xic(icount)=r0(1)+r1(1)*rand_lg()
            yic(icount)=r0(2)+r1(2)*rand_lg()
            zic(icount)=r0(3)+r1(3)*rand_lg()
         enddo
 
         if (icount.ne.icntin+nnodes_new) goto 9998
 
      endif !...........................................................
 
      call mmverify()
 
c     ! add xyz shift
      do ind=icntin+1,icntin+nnodes_new
         xic(ind)=roff(1)+xic(ind)
         yic(ind)=roff(2)+yic(ind)
         zic(ind)=roff(3)+zic(ind)
      enddo
 
C ******************************************************************
C TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C (copied from RZ)
 
      if (normflgc .gt. 0) call chgnorm(1,icount,1)
 
C ******************************************************************
C PRINT OUT THE POINT NUMBERS GENERATED
C also come here if error after start (error resets to no nodes added)
 
 9998 write(logmess,6000) icntin+1,icount
 6000 format('  RANPTS GENERATED POINTS ',i11,' TO ',i11)
      call writloga('default',0,logmess,0,icscode)
 
C ******************************************************************
C SET UP THE USUAL CFT IMMUNE STATEMENT IN CASE DDT IS NEEDED.
C (copied from RZ)
 
      ipointi=nnodes_save+1
      ipointj=icount
 
      nnodes=ipointj
      call cmo_set_info('nnodes',cmo,nnodes,1,1,icscode)
      if (icscode.ne.0) ierr=ierr+10
 
      call cmo_set_info('ipointi',cmo
     &                ,ipointi,1,1,icscode)
      if (icscode.ne.0) ierr=ierr+10
      call cmo_set_info('ipointj',cmo
     &                ,ipointj,1,1,icscode)
      if (icscode.ne.0) ierr=ierr+10
 
C ******************************************************************
C NORMAL RETURN
c (and error return if error after start adding)
 
      if (icntin+1.gt.icount.and.ierr.eq.0) ierr=-1
 
      return
 
C ******************************************************************
C ERROR RETURN (at least for errors before anything done)
 
 9999 continue
      write(logmess,*) '  RANPTS ERROR - no points added'
      call writloga('default',0,logmess,0,icscode)
 
      return
 
C#######################################################################
      end
 
C#######################################################################
 
 
 
