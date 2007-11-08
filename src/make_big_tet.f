      subroutine make_big_tet(npoints,ntets,xcoords,nwds)
C
C#######################################################################
C
C     PURPOSE -
C  Find dimensions of a tetrahedron that will enclose the point set
C
C     INPUT ARGUMENTS -
C
c  npoints - number of points in the mesh
C  ntets  - number of elements in the mesh
C  xcoords - array of user input coordinates for bigtet
C  nwds - must be 12 if user coordinates are to be used
C
C     OUTPUT
C  ibigtet and xbigtet,ybigtet,zbigtet
C  boxsize etc will be set in common
C
C  CHANGE CONTROL
C
C  $Log: make_big_tet.f,v $
C  Revision 2.00  2007/11/05 19:46:00  spchu
C  Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Mon Aug 18 14:55:14 1997   dcg
CPVCS    Initial revision.
C#######################################################################
C
      implicit none
      include 'search.h'
      include 'cmo.h'
      include 'chydro.h'
      real*8 depthx,depthy,depthz,d,sqd,xmeanl,
     *  ymeanl,zmeanl,xminl,yminl,zminl,xmaxl,ymaxl,zmaxl,
     *  xcoords(12)
      integer npoints,ntets,nwds
      integer ismin,ismax
C
      ibigtet=npoints+1
      xminl=xic(ismin(npoints,xic(1),1))
      yminl=yic(ismin(npoints,yic(1),1))
      zminl=zic(ismin(npoints,zic(1),1))
      xmaxl=xic(ismax(npoints,xic(1),1))
      ymaxl=yic(ismax(npoints,yic(1),1))
      zmaxl=zic(ismax(npoints,zic(1),1))
C        *** SIZE OF A BRICK THAT ENCLOSES ALL THE POINTS.
      xmeanl=0.5*(xminl+xmaxl)
      ymeanl=0.5*(yminl+ymaxl)
      zmeanl=0.5*(zminl+zmaxl)
      boxsizex=xmaxl-xminl
      boxsizey=ymaxl-yminl
      boxsizez=zmaxl-zminl
C      ** Check for user supplied coordinates
      if (nwds.ne.12) then
         depthx=5.0*boxsizex
         depthy=5.0*boxsizey
         depthz=5.0*boxsizez
         d=boxsizex**2+boxsizey**2+boxsizez**2
         sqd=sqrt(d)
         xbigtet(1)=xmeanl-0.50*depthx
         ybigtet(1)=ymeanl-0.25*depthy
         zbigtet(1)=zmeanl-0.25*depthz
         xbigtet(2)=xmeanl
         ybigtet(2)=ymeanl-0.25*depthy
         zbigtet(2)=zmeanl+0.75*depthz
         xbigtet(3)=xmeanl+0.50*depthx
         ybigtet(3)=ymeanl-0.25*depthy
         zbigtet(3)=zmeanl-0.25*depthz
         xbigtet(4)=xmeanl
         ybigtet(4)=ymeanl+0.75*depthy
         zbigtet(4)=zmeanl-0.25*depthz
         if ((depthx .ge. depthy) .and.
     *       (depthz .ge. depthy)) then
c
c        Bigtet has a face parallel to the y=0 plane
c
            xbigtet(1)=xmeanl-0.50*depthx
            ybigtet(1)=ymeanl-0.25*depthy
            zbigtet(1)=zmeanl-0.25*depthz
            xbigtet(2)=xmeanl
            ybigtet(2)=ymeanl-0.25*depthy
            zbigtet(2)=zmeanl+0.75*depthz
            xbigtet(3)=xmeanl+0.50*depthx
            ybigtet(3)=ymeanl-0.25*depthy
            zbigtet(3)=zmeanl-0.25*depthz
            xbigtet(4)=xmeanl
            ybigtet(4)=ymeanl+0.75*depthy
            zbigtet(4)=zmeanl
         elseif ((depthy .ge. depthx) .and.
     *           (depthz .ge. depthx)) then
c
c        Bigtet has a face parallel to the x=0 plane
c
            xbigtet(1)=xmeanl-0.25*depthx
            ybigtet(1)=ymeanl-0.50*depthy
            zbigtet(1)=zmeanl-0.25*depthz
            xbigtet(3)=xmeanl-0.25*depthx
            ybigtet(3)=ymeanl
            zbigtet(3)=zmeanl+0.75*depthz
            xbigtet(2)=xmeanl-0.25*depthx
            ybigtet(2)=ymeanl+0.50*depthy
            zbigtet(2)=zmeanl-0.25*depthz
            xbigtet(4)=xmeanl+0.75*depthx
            ybigtet(4)=ymeanl
            zbigtet(4)=zmeanl
         elseif ((depthy .ge. depthz) .and.
     *           (depthx .ge. depthz)) then
c
c        Bigtet has a face parallel to the z=0 plane
c
            xbigtet(1)=xmeanl-0.25*depthx
            ybigtet(1)=ymeanl-0.25*depthy
            zbigtet(1)=zmeanl-0.50*depthz
            xbigtet(2)=xmeanl-0.25*depthx
            ybigtet(2)=ymeanl+0.75*depthy
            zbigtet(2)=zmeanl
            xbigtet(3)=xmeanl-0.25*depthx
            ybigtet(3)=ymeanl-0.25*depthy
            zbigtet(3)=zmeanl+0.50*depthz
            xbigtet(4)=xmeanl+0.75*depthx
            ybigtet(4)=ymeanl
            zbigtet(4)=zmeanl
c
         endif
      else
C
C  use user supplied coordinates if there
C
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
      endif
      ntets=ntetexcl+1
      itet(1,ntets)=npoints+1
      itet(2,ntets)=npoints+2
      itet(3,ntets)=npoints+3
      itet(4,ntets)=npoints+4
      jtet(1,ntets)=mbndry
      jtet(2,ntets)=mbndry
      jtet(3,ntets)=mbndry
      jtet(4,ntets)=mbndry
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
      xic (ibigtet+3)=xbigtet(4)
      yic (ibigtet+3)=ybigtet(4)
      zic (ibigtet+3)=zbigtet(4)
      itp1(ibigtet+3)=ifitpini
      imt1(ibigtet+3)=0
 
C        *** THE BIG-TETRAHEDRON POINT TYPES ARE SET TO INTERFACE TYPES
C        *** BECAUSE THIS ALLOWS US TO EXCLUDE CONNECTIONS ORIGINATING
C        *** FROM THESE POINTS DURING THE FIRST INTERFACE PASS.  ALSO,
      return
      end
