*dk,rzv_lg
      subroutine rzv_lg(imsgin,xmsgin,cmsgin,msgtype,nwds,ierr)
 
C#######################################################################
C
C     PURPOSE -
C       this routine is used to ratio zone the region of space
C       spanned by the specified number of copies of the input vectors
C       (no attempt is made to insure that the 3 vectors are orthogonal)
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
C           NOTE: angles are in degrees
C        * NUMBER OF VECTOR COPIES IN THE 3 DIRECTIONS
C           nv(iv)=(n1,n2,n3)
C        * 3 DIRECTION VECTORS IN WHICH TO SPREAD POINTS
C           dv(j,iv)=((v11,v12,v13),(v21,v22,v23),(v31,v32,v33))^T
C             note: dv read in as (dv(j,iv),j=1,3),iv=1,3),
C                   so dv is the transpose of v in manual description
C        * STARTING POINT
C           v0(j)=(v01,v02,v03)
C        * RATIO ZONING VALUE FOR THE 3 VECTORS
C           (dv added modifed by this ratio after each addition)
C           rv(j,iv)=((r11,r12,r13),(r21,r22,r23),(r31,r32,r33))^T
C             note: rv read in as (rv(j,iv),j=1,3),iv=1,3),
C                   so rv is the transpose of r in manual description
C           ratio_method=[*component|vector]
C           ratio_flag=(f1,f2,f3)
C           (development note: could make ratio_flag a scalar or matrix...)
C                ratio_method(1:1)='c' (default):
C                    (scale COMPONENTS)
C                    The j-th component of the iv-th vector dv(j,iv)
C                    is reduced by rv(j,iv) at each step
C                    in the iv-th direction away from the initial point.
C                ratio_method(1:1)=['v'|'V'] and ratio_flag(j)=1 (default):
C                    (scale VECTORS)
C                    All components of the j-th vector dv(1:3,j)
C                    are reduced by rv(j,iv) at each step
C                    in the iv-th direction away from the initial point.
C                ratio_method(1:1)=['v'|'V'] and ratio_flag(j)=0:
C                    (scale VECTORS)
C                    All components of the j-th vector dv(1:3,j)
C                    are reduced by [1-(1-rv(j,iv))*2/(i_iv+1)] after the i_iv-th step
C                    in the iv-th direction away from the initial point,
C                    except that for i_iv=0 the vector is not reduced.
C                    Note that after the first step in the iv-th direction
C                    (i_iv=1), the j-th vector is reduced by rv(j,iv).
C                    Note also if rv(j,iv)=0.5, then the amount the j-th vector
C                    is reduced by after the i_iv-th step is [(i_iv)/(i_iv+1)]
C                    (so for a step in the j-th direction at i_iv=1 of 1,
C                          the step in the j-th direction at i_iv=2 is 1/2,
C                          the step in the j-th direction at i_iv=2 is 1/3,
C                          the step in the j-th direction at i_iv=4 is 1/4,
C                     etc, as the reductions after the current step
C                     act on the current step).
C
C     FORMAT -
C       RZV/[ |xyz|XYZ|rtz|RTZ|rtp|RTP]                 &
C                  /n1,n2,n3                            &
C                  /v11,v12,v13/v21,v22,v23/v31,v32,v33 &
C                  /v01,v02,v03                         &
C                  /r11,r12,r13/r21,r22,r23/r31,r32,r33
C                  /[ |*component|vector]
C                  /f1,f2,f3
C
C     DEFAULTS -
C       "rzv" without any arguments will add a point at the origin
C       cgeom is assumed 'xyz' if not present
C         allowed values are [xyz|XYZ|rtz|RTZ|rtp|RTP]
C         (anything else returns error)
C       n1 through v03 are assumed zero if not present
C       r11 through r33 are assumed 1 if not present
C
C     SAMPLES -
C       a) spiral of points
C             rzv/rtz/n1,0,0/.1,10.,1/ , , / , , / , , /1.1,1,.9/
C       b) sc (simple cubic) point distribution
C             rzv/xyz/n1,n2,n3/1,0,0/0,1,0/0,0,1/
C          same as
C             rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
C       c) bcc (body centered cubic) point distribution
C             rzv/xyz/n1,n2,n3/.5,.5,.5/.5,.5,-.5/.5,-.5,-.5/
C          compare the two command sequence (different bounding box)
C             rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
C             rz/xyz/n1  ,n2  ,n3  /0,0,0/n1,n2,n3/0,0,0
C       d) fcc (face centered cubic) point distribution
C             rzv/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/
C          compare the four command sequence (different bounding box)
C             rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
C             rz/xyz/n1  ,n2  ,n3+1/0,0,0/n1,n2,n3/0,0,1
C             rz/xyz/n1  ,n2+1,n3  /0,0,0/n1,n2,n3/0,1,0
C             rz/xyz/n1+1,n2  ,n3  /0,0,0/n1,n2,n3/1,0,0
C       e) hexagonal lattice of points in x,y plane, repeated in z direction
C             rzv/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/
C       f) (two command sequence)
C          diamond point distribution
C             rzv/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/
C             rzv/xyz/n1,n2,n3/.5,.5,0/0,.5,.5/.5,0,.5/.25,.25,.25
C          compare the eight command sequence (different bounding box)
C             rz/xyz/n1+1,n2+1,n3+1/0,0,0/n1,n2,n3/1,1,1
C             rz/xyz/n1  ,n2  ,n3+1/0,0,0/n1,n2,n3/0,0,1
C             rz/xyz/n1  ,n2+1,n3  /0,0,0/n1,n2,n3/0,1,0
C             rz/xyz/n1+1,n2  ,n3  /0,0,0/n1,n2,n3/1,0,0
C             rz/xyz/n1+1,n2+1,n3+1/0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/1,1,1
C             rz/xyz/n1  ,n2  ,n3+1/0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/0,0,1
C             rz/xyz/n1  ,n2+1,n3  /0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/0,1,0
C             rz/xyz/n1+1,n2  ,n3  /0.25,0.25,0.25/n1+.25,n2+.25,n3+.25/1,0,0
C       g) (two command sequence)
C          hcp (hexagonal close pack) point distribution
C            rzv/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/
C            rzv/xyz/n1,n2,n3/1,0,0/.5,0.866,0/0,0,1/.5,0.289,.5/
C
C     CAVEATS -
C       * filter should be used afterwards to remove possibly duplicate points
C       * this can create some really bizzare point distributions
C       * mistyped input after "rzv/[cgeom]" always returns successful point addition,
C         but may be very different than desired
C
C     CHANGE HISTORY -
C
C        $Log: rzv_lg.f,v $
C        Revision 2.00  2007/11/09 20:04:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   22 Aug 2001 12:46:58   jtg
CPVCS    warning rather that error generated if ndimension_geom not 3
CPVCS    
CPVCS       Rev 1.5   20 Aug 2001 22:00:12   jtg
CPVCS    fixed Log line
CPVCS    rev 1.4 was to add alternate method of ratio zoning
CPVCS    (see description of ratio_method and ratio_flag)
CPVCS    
CPVCS       Rev 1.2   Thu Apr 06 14:19:52 2000   dcg
CPVCS    replace get_info_i and set_info_i calls
CPVCS
CPVCS       Rev 1.1   Fri Jun 25 09:35:08 1999   dcg
CPVCS    set itp, imt, icr for new nodes
CPVCS
CPVCS       Rev 1.0   Wed Jun 23 23:46:54 1999   jtg
CPVCS    Initial revision.
C
C#######################################################################
 
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
      pointer (ipitp1, itp1)
      pointer (ipimt1, imt1)
      pointer (ipicr1, icr1)
      integer itp1(*),imt1(*),icr1(*)
 
      character*32 cgeom,ratio_method
      character*32 cmo, isubname
 
      integer ierr,icscode,ipointi,ipointj,ilen,ityp
     &  ,icount,nnodes,nnodes_new,nnodes_save,icntin
     &  ,iv,j,iwd,i1,i2,i3,nv(3),ind,geom_dim,ratio_flag(3)
 
      real*8 rrr,theta,phi,degtorad,pi
     &  ,dv(3,3),rv(3,3),v0(3),v1(3),v2(3),v3(3)
     &  ,d1(3),d2(3),d3(3)
 
C#######################################################################
C get some initial info
 
      isubname='rzv_lg'
 
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
         ! ierr=6
         ! goto 9999
         write(logmess,*) '  RZV WARNING - ndimensions_geom.ne.3'
         call writloga('default',0,logmess,0,icscode)
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
 
C assign the number of copies for each vector
      do iv=1,3
         iwd=iv+2
         if (iwd.gt.nwds) then
            nv(iv)=0
         elseif (msgtype(iwd) .eq. 1) then
            nv(iv)=imsgin(iwd)
         elseif (msgtype(iwd) .eq. 2) then
            nv(iv)=xmsgin(iwd)
         else
            nv(iv)=0
         endif
      enddo
 
C assign the vectors
      do iv=1,3
         rrr=zero
         do j=1,3
            iwd=iv*3+j+2
            if (iwd.gt.nwds) then
               dv(j,iv)=zero
            elseif (msgtype(iwd) .eq. 1) then
               dv(j,iv)=imsgin(iwd)
            elseif (msgtype(iwd) .eq. 2) then
               dv(j,iv)=xmsgin(iwd)
            else
               dv(j,iv)=zero
            endif
            rrr=rrr+abs(dv(j,iv))
         enddo
         ! if null vector, don't add multiple copies
         if (rrr.eq.zero) nv(j)=0
         if (nv(iv).lt.0) then
            nv(iv)=-nv(iv)
            do j=1,3
               dv(j,iv)=-dv(j,iv)
            enddo
         endif
      enddo
 
C assign the starting point
      do j=1,3
         iwd=j+14
         if (iwd.gt.nwds) then
            v0(j)=zero
         elseif (msgtype(iwd) .eq. 1) then
            v0(j)=imsgin(iwd)
         elseif (msgtype(iwd) .eq. 2) then
            v0(j)=xmsgin(iwd)
         else
            v0(j)=zero
         endif
      enddo
 
C assign the step ratios
      do iv=1,3
         rrr=zero
         do j=1,3
            iwd=iv*3+j+14
            if (iwd.gt.nwds) then
               rv(j,iv)=one
            elseif (msgtype(iwd) .eq. 1) then
               rv(j,iv)=imsgin(iwd)
            elseif (msgtype(iwd) .eq. 2) then
               rv(j,iv)=xmsgin(iwd)
            else
               rv(j,iv)=one
            endif
            rrr=rrr+abs(rv(j,iv)*dv(j,iv))
         enddo
         ! if ratio*step is zero, don't add multiple copies
         if (rrr.eq.zero.and.nv(iv).gt.1) nv(iv)=1
      enddo

C assign the ratio method and set ratio_flag

      iwd=iwd+1
      if ( nwds.ge.iwd .and. msgtype(iwd).eq.3 ) then
         ratio_method=cmsgin(iwd)
         if (ratio_method(1:1).eq.'V') ratio_method='v'
      else 
         ratio_method='c'
      endif

      do iv=1,3
         iwd=iwd+1
         if (iwd.gt.nwds) then
            ratio_flag(iv)=1
         elseif (msgtype(iwd) .eq. 1) then
            ratio_flag(iv)=imsgin(iwd)
         elseif (msgtype(iwd) .eq. 2) then
            ratio_flag(iv)=xmsgin(iwd)
         else
            ratio_flag(iv)=1
         endif
      enddo
 
C ******************************************************************
C increment storage
 
      icntin=icount
      nnodes_new=(1+nv(1))*(1+nv(2))*(1+nv(3))
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
      call cmo_get_info('itp1',cmo,ipitp1,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=5
        goto 9998
      endif
      call cmo_get_info('imt1',cmo,ipimt1,ilen,ityp,ierr)
      if (ierr.ne.0) then
        ierr=5
        goto 9998
      endif
      call cmo_get_info('icr',cmo,ipicr1,ilen,ityp,ierr)
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
 
      if (ratio_method(1:1).ne.'v') then
         ! (COMPONENTS scaled)
         ! jth component of iv-th vec reduced by rv(j,iv)
         ! at each step in iv-th direction

         do j=1,3
            v3(j)=zero
            d3(j)=dv(j,3)
         enddo
         do i3=0,nv(3)
            do j=1,3
               v2(j)=zero
               d2(j)=dv(j,2)
            enddo
            do i2=0,nv(2)
               do j=1,3
                  v1(j)=zero
                  d1(j)=dv(j,1)
               enddo
               do i1=0,nv(1)
                  icount=icount+1
                  xic(icount)=v0(1)+v1(1)+v2(1)+v3(1)
                  yic(icount)=v0(2)+v1(2)+v2(2)+v3(2)
                  zic(icount)=v0(3)+v1(3)+v2(3)+v3(3)
                  do j=1,3
                     v1(j)=v1(j)+d1(j)
                     d1(j)=d1(j)*rv(j,1)
                  enddo
               enddo
               do j=1,3
                  v2(j)=v2(j)+d2(j)
                  d2(j)=d2(j)*rv(j,2)
               enddo
            enddo
            do j=1,3
               v3(j)=v3(j)+d3(j)
               d3(j)=d3(j)*rv(j,3)
            enddo
         enddo

      else 
         ! (VECTORS scaled)
         ! all components of jth vec reduced by rv(j,iv)
         ! at each step in iv-th direction

         do j=1,3
            d3(j)=one
            v3(j)=zero
         enddo
         do i3=0,nv(3)
            do j=1,3
               d2(j)=one
               v2(j)=zero
            enddo
            do i2=0,nv(2)
               do j=1,3
                  d1(j)=one
                  v1(j)=zero
               enddo
               do i1=0,nv(1)
                  icount=icount+1
                  xic(icount)=v0(1)+v1(1)*d1(1)*d2(1)*d3(1)
     &                             +v2(1)*d1(2)*d2(2)*d3(2)
     &                             +v3(1)*d1(3)*d2(3)*d3(3)
                  yic(icount)=v0(2)+v1(2)*d1(1)*d2(1)*d3(1)
     &                             +v2(2)*d1(2)*d2(2)*d3(2)
     &                             +v3(2)*d1(3)*d2(3)*d3(3)
                  zic(icount)=v0(3)+v1(3)*d1(1)*d2(1)*d3(1)
     &                             +v2(3)*d1(2)*d2(2)*d3(2)
     &                             +v3(3)*d1(3)*d2(3)*d3(3)
                  do j=1,3
                     if (i1.ne.0) then
                        if (ratio_flag(j).eq.0) then
                           d1(j)=d1(j)
     &                          *(1.d0-(1.d0-rv(j,1))*2.d0/dble(i1+1))
                        else
                           d1(j)=d1(j)*rv(j,1)
                        endif
                     endif
                     v1(j)=v1(j)+dv(j,1)
                  enddo
               enddo
               do j=1,3
                  if (i2.ne.0) then
                     if (ratio_flag(j).eq.0) then
                        d2(j)=d2(j)
     &                       *(1.d0-(1.d0-rv(j,2))*2.d0/dble(i2+1))
                     else
                        d2(j)=d2(j)*rv(j,2)
                     endif
                  endif
                  v2(j)=v2(j)+dv(j,2)
               enddo
            enddo
            do j=1,3
               if (i3.ne.0) then
                  if (ratio_flag(j).eq.0) then
                     d3(j)=d3(j)
     &                    *(1.d0-(1.d0-rv(j,3))*2.d0/dble(i3+1))
                  else
                     d3(j)=d3(j)*rv(j,3)
                  endif
               endif
               v3(j)=v3(j)+dv(j,3)
            enddo
         enddo

      endif
 
C ******************************************************************
C convert new coordinates to xyz if not already
 
      if (cgeom.eq.'rtz') then
         ! convert cylindrical to xyz
         do ind=icntin+1,icntin+nnodes_new
            rrr=xic(ind)
            theta=yic(ind)*degtorad
            xic(ind)=rrr*cos(theta)
            yic(ind)=rrr*sin(theta)
         enddo
      elseif (cgeom.eq.'rtp') then
         ! convert spherical to xyz
         do ind=icntin+1,icntin+nnodes_new
            rrr=xic(ind)
            theta=yic(ind)*degtorad
            phi=zic(ind)*degtorad
            xic(ind)=rrr*sin(theta)*cos(phi)
            yic(ind)=rrr*sin(theta)*sin(phi)
            zic(ind)=rrr*cos(theta)
         enddo
      endif
 
C ******************************************************************
C TRANSFORM POINTS AND VELOCITIES TO NORMAL COORD. SYSTEM
C (copied from RZ)
 
      if (normflgc .gt. 0) call chgnorm(1,icount,1)
 
C ******************************************************************
C PRINT OUT THE POINT NUMBERS GENERATED
C also come here if error after start (error resets to no nodes added)
 
 9998 write(logmess,6000) icntin+1,icount
 6000 format('  RZV GENERATED POINTS ',i6,' TO ',i6)
      call writloga('default',0,logmess,0,icscode)
c
c     Initialize ipt1, imt1 and icr1 to zero for new nodes.
C
      do ind=icntin+1,icount
         itp1(ind)=0
         imt1(ind)=0
         icr1(ind)=0
      enddo
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
      write(logmess,*) '  RZV ERROR - no points added'
      call writloga('default',0,logmess,0,icscode)
 
      return
 
C#######################################################################
      end
 
C#######################################################################
 
