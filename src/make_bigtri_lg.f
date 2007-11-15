      subroutine make_bigtri_lg(npoints,ntets,xcoords,nwds,ierror)
C#######################################################################
C
C     PURPOSE -
C  Find dimensions of a triangle that will enclose the point set
C
C     INPUT ARGUMENTS -
C
c  npoints - number of points in the mesh
C  ntets  - number of elements in the mesh
c
C     OUTPUT
C  ibigtet and xbigtet,ybigtet,zbigtet
C  boxsize etc will be set in common
C
C  CHANGE CONTROL
C
C  $Log: make_bigtri_lg.f,v $
C  Revision 2.00  2007/11/05 19:46:00  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.13   30 Jan 2001 10:06:42   dcg
CPVCS    change test on number of arguments to command to accept coord of
CPVCS    big tri from input
CPVCS
CPVCS       Rev 1.12   30 Jan 2001 08:18:40   dcg
CPVCS    accept input triangle coordinates
CPVCS
CPVCS       Rev 1.11   26 Oct 2000 11:35:00   dcg
CPVCS    change calculation of coordinate of big triangle
CPVCS
CPVCS       Rev 1.10   Wed Apr 05 13:34:36 2000   nnc
CPVCS    Minor source modifications required by the Absoft compiler.
CPVCS
CPVCS       Rev 1.9   Thu Feb 17 14:50:50 2000   dcg
CPVCS    get rid of debug output
CPVCS
CPVCS       Rev 1.8   17 Feb 2000 12:06:26   jan
CPVCS
CPVCS       Rev 1.6   15 Feb 2000 13:59:20   jan
CPVCS    fixed error for collinear point check
CPVCS
CPVCS       Rev 1.5   08 Feb 2000 10:28:58   dcg
CPVCS    comment out bad tests (collinear, in plane) for now
CPVCS
CPVCS       Rev 1.4   Fri Nov 12 15:30:48 1999   dcg
CPVCS    check for collinear points
CPVCS
CPVCS       Rev 1.3   Thu Oct 07 16:43:52 1999   dcg
CPVCS    fix error in calculating coordinates from equation of plane
CPVCS
CPVCS       Rev 1.2   Wed Sep 01 15:55:30 1999   dcg
CPVCS    repair typo caused by previous fix
CPVCS
CPVCS       Rev 1.1   Wed Sep 01 12:43:20 1999   dcg
CPVCS    get mbndry from mesh object
CPVCS
CPVCS       Rev 1.0   Fri Mar 19 13:14:52 1999   dcg
CPVCS    Initial revision.
C#######################################################################
C
      implicit none
      include 'search.h'
      include 'chydro.h'
      include 'consts.h'
      real*8 depthx,depthy,depthz,xmeanl,a,b,c,d,s1,s2,
     *  ymeanl,zmeanl,xminl,yminl,zminl,xmaxl,ymaxl,zmaxl
      integer npoints,ntets,i,mbndry
      integer ipt1,ipt2,ierror,
     * leni,icmotype,ier,nwds
      pointer (ipitet,itet)
      pointer (ipjtet,jtet)
      integer itet(3,*),jtet(3,*)
	real*8 voltri,absofd
      pointer (ipitp1,itp1)
      integer itp1(*)
      pointer (ipimt1,imt1)
      integer imt1(*)
      pointer (ipxic,xic),(ipyic,yic),(ipzic,zic)
      real*8 xic(*),yic(*),zic(*),xcoords(*)
      character*32 cmo
      character*132 logmess
C
      ierror=0
 
      call cmo_get_name(cmo,ier)
C
      call cmo_get_info('mbndry',cmo,mbndry,leni,icmotype,ier)
      call cmo_get_info('xic',cmo,ipxic,leni,icmotype,ier)
      call cmo_get_info('yic',cmo,ipyic,leni,icmotype,ier)
      call cmo_get_info('zic',cmo,ipzic,leni,icmotype,ier)
      call cmo_get_info('itet',cmo,ipitet,leni,icmotype,ier)
      call cmo_get_info('jtet',cmo,ipjtet,leni,icmotype,ier)
      call cmo_get_info('imt1',cmo,ipimt1,leni,icmotype,ier)
      call cmo_get_info('itp1',cmo,ipitp1,leni,icmotype,ier)
      ibigtet=npoints+1
c
c  see if coordinates are supplied
c
      if(nwds.ge.14) then
         xbigtet(1)=xcoords(1)
         ybigtet(1)=xcoords(2)
         zbigtet(1)=xcoords(3)
         xbigtet(2)=xcoords(4)
         ybigtet(2)=xcoords(5)
         zbigtet(2)=xcoords(6)
         xbigtet(3)=xcoords(7)
         ybigtet(3)=xcoords(8)
         zbigtet(3)=xcoords(9)
         xbigtet(4)=xcoords(10)
         ybigtet(4)=xcoords(11)
         zbigtet(4)=xcoords(12)
         go to 100
      endif
c
c find mins and maxs
c
      xminl=xic(1)
      xmaxl=xic(1)
      yminl=yic(1)
      ymaxl=yic(1)
      zminl=zic(1)
      zmaxl=zic(1)
 
      do i=2,npoints
         if(xic(i).lt.xminl) then
            xminl=xic(i)
 
         endif
         if(xic(i).gt.xmaxl) then
            xmaxl=xic(i)
 
         endif
         if(yic(i).lt.yminl) then
            yminl=yic(i)
 
         endif
         if(yic(i).gt.ymaxl) then
            ymaxl=yic(i)
 
         endif
         if(zic(i).lt.zminl) then
            zminl=zic(i)
 
         endif
         if(zic(i).gt.zmaxl) then
            zmaxl=zic(i)
 
         endif
      enddo
C  find BRICK THAT ENCLOSES ALL THE POINTS and midpoints of edges.
      xmeanl=0.5*(xminl+xmaxl)
      ymeanl=0.5*(yminl+ymaxl)
      zmeanl=0.5*(zminl+zmaxl)
      boxsizex=xmaxl-xminl
      boxsizey=ymaxl-yminl
      boxsizez=zmaxl-zminl
C        SET UP THE EQUATION OF THE PLANE FROM 3 POINTS.
C
      ipt1=1
      ipt2=npoints
c make sure the points are not collinear
c     if(abs(boxsizex).lt.smldistp.or.
c    *   abs(boxsizey).lt.smldistp) then
c        write(logmess,8)
c        call writloga('default',0,logmess,0,ier)
c        ierror=1
c        go to 9999
c     endif
c     do i=1,npoints-1
c       s1=(xmeanl-xic(ipt1))/(ymeanl-yic(ipt1))
c       s2=(xic(ipt2)-xmeanl)/(yic(ipt2)-ymeanl)
c       if(abs(s1-s2).gt.smldistp )go to 10
c       ipt2=npoints-i
c     enddo
c     write(logmess,8)
c8    format( 'All nodes are collinear')
c     call writloga('default',0,logmess,0,ier)
c     ierror=1
c     go to 9999
c
c tests for collinear points
c
c
c      call get_epsilon("epsilon1", eps)
      i = 3
      call volume_tri(xic(1), yic(1), zic(1),
     1                xic(2), yic(2), zic(2),
     1                xic(i), yic(i), zic(i),voltri)
      do while (i .lt. npoints .and. abs(voltri) .lt. epsilonr)
         i = i + 1
         call volume_tri(xic(1), yic(1), zic(1),
     1                   xic(2), yic(2), zic(2),
     1                   xic(i), yic(i), zic(i),voltri)
      end do
      if (i .eq. npoints .and. abs(voltri) .lt. epsilonr) then
         write(logmess,8)
8        format('All nodes are collinear')
         call writloga('default',0,logmess,0,ier)
         ierror=1
         go to 9999
      end if
 
c test to see if in same plane
 
C     SET UP THE EQUATION OF THE PLANE FROM THE 3 POINTS.
C
      a=  (yic(2)-yic(1))*(zic(i)-zic(1)) -
     &    (yic(i)-yic(1))*(zic(2)-zic(1))
      b=-((xic(2)-xic(1))*(zic(i)-zic(1)) -
     &    (xic(i)-xic(1))*(zic(2)-zic(1)))
      c=  (xic(2)-xic(1))*(yic(i)-yic(1)) -
     &    (xic(i)-xic(1))*(yic(2)-yic(1))
      d=a*xic(1)+b*yic(1)+c*zic(1)
      if (abs(d) .gt. epsilonr) then
         absofd=abs(d)
         a=a/absofd
         b=b/absofd
         c=c/absofd
         d=d/absofd
      end if
      do i = 3, npoints
	   if(abs((a*xic(i) + b*yic(i) + c*zic(i) - d)) .gt. epsilonr)
     1 then
         write(logmess,88)
88        format('Points are not in same plane')
         call writloga('default',0,logmess,0,ier)
         ierror=1
           end if
      end do
 
       depthx=3.0*boxsizex
       depthy=3.0*boxsizey
       depthz=3.0*boxsizez
c
       if ((depthx .ge. depthy) .and.
     *     (depthz .ge. depthy)) then
c
c        Bigtri is most parallel to the y=0 plane
c
            xbigtet(1)=xmeanl-0.75*depthx
            zbigtet(1)=zmeanl-0.70*depthz
            ybigtet(1)=-(a*xbigtet(1)+c*zbigtet(1)-d)/b
            xbigtet(2)=xmeanl+0.75*depthx
            zbigtet(2)=zmeanl-0.75*depthz
            ybigtet(2)=-(a*xbigtet(2)+c*zbigtet(2)-d)/b
            xbigtet(3)=1.01*xmeanl
            zbigtet(3)=zmeanl+max(depthx,depthz)
            ybigtet(3)=-(a*xbigtet(3)+c*zbigtet(3)-d)/b
c
      elseif ((depthy .ge. depthx) .and.
     *        (depthz .ge. depthx)) then
c
c        Bigtet is most parallel to the x=0 plane
            ybigtet(1)=ymeanl-0.75*depthy
            zbigtet(1)=zmeanl-0.70*depthz
            xbigtet(1)=-(b*ybigtet(1)+c*zbigtet(1)-d)/a
            ybigtet(2)=ymeanl+0.75*depthy
            zbigtet(2)=zmeanl-0.75*depthz
            xbigtet(2)=-(b*ybigtet(2)+c*zbigtet(2)-d)/a
            ybigtet(3)=1.01*ymeanl
            zbigtet(3)=zmeanl+1.5*max(depthy,depthz)
            xbigtet(3)=-(b*ybigtet(3)+c*zbigtet(3)-d)/a
c
      elseif ((depthy .ge. depthz) .and.
     *        (depthx .ge. depthz)) then
c
c        Bigtri is most parallel to the z=0 plane
c
            xbigtet(1)=xmeanl-0.75*depthx
            ybigtet(1)=ymeanl-0.70*depthy
            zbigtet(1)=-(a*xbigtet(1)+b*ybigtet(1)-d)/c
            xbigtet(2)=xmeanl+0.75*depthx
            ybigtet(2)=ymeanl-0.75*depthy
            zbigtet(2)=-(a*xbigtet(2)+b*ybigtet(2)-d)/c
            xbigtet(3)=1.01*xmeanl
            ybigtet(3)=ymeanl+1.5*max(depthx,depthy)
            zbigtet(3)=-(a*xbigtet(3)+b*ybigtet(3)-d)/c
c
      endif
c
 100  ntets=ntetexcl+1
      itet(1,ntets)=npoints+1
      itet(2,ntets)=npoints+2
      itet(3,ntets)=npoints+3
      jtet(1,ntets)=mbndry
      jtet(2,ntets)=mbndry
      jtet(3,ntets)=mbndry
      xic (ibigtet  )=xbigtet(1)
      yic (ibigtet  )=ybigtet(1)
      zic (ibigtet  )=zbigtet(1)
      itp1(ibigtet  )=ifitpini
      imt1(ibigtet  )=0
      xic (ibigtet+1)=xbigtet(2)
      yic (ibigtet+1)=ybigtet(2)
      zic (ibigtet+1)=zbigtet(2)
      itp1(ibigtet+1)=ifitpini
      imt1(ibigtet+1)=0
      xic (ibigtet+2)=xbigtet(3)
      yic (ibigtet+2)=ybigtet(3)
      zic (ibigtet+2)=zbigtet(3)
      itp1(ibigtet+2)=ifitpini
      imt1(ibigtet+2)=0
c
 9999 continue
      return
      end
