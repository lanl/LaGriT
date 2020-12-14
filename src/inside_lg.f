C     File: inside_lg.f
C
C     This file contains the following:
C     inside_pyr
C     inside_pri
C     inside_hex_planar
C     inside_tet
C     inside_hex
C
C     TAM - Mon Jul 17 15:03:49 MDT 2006
C     Adding comments to code here as things are figured out
C     Comments were not written by author, so these are put
C     together by tam from other code notes or evaluating the
C     code as it is written here. 
C
C     Definition of inside a triangle or a quad exists such that
C     the point must be in the same plane as the triangle in question.
C     So, the first call ensures that the point is in the plane,
C       inside_quad
C       inside_tri
C     Then the following routines are called if point is on the plane
C       inside_quad2d
C       inside_tri2d
C
C     $Log: inside_lg.f,v $
C     Revision 2.00  2007/11/05 19:45:58  spchu
C     Import to CVS
C
CPVCS    
CPVCS       Rev 1.5   03 Jan 2007 12:02:12   tam
CPVCS    pass in itmp instead of iflfag, use itmp for debug flag
CPVCS    add comments and debug and error statements
CPVCS    Set epsilon based on number size and distance from origin
CPVCS    Put a Lower and Upper bound on epsilon adjustment
CPVCS    
CPVCS       Rev 1.3   08 Feb 2006 14:35:40   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    
CPVCS       Rev 1.2   13 Dec 2005 15:16:16   gable
CPVCS    Changed so that change info is included in the file.
CPVCS    Installed new version of inside_hex
C
C ######################################################################
      subroutine inside_pyr(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,
     *                      xa,ya,za,
     *                      iflag)
C
C ######################################################################
C
C     NOTE: I have not checked but I expect this will suffer from the same
C     problems as inside_hex_planar in the case where the elements quad
C     face is highly distorted. Carl Gable Tue Dec 13 14:40:29 MST 2005
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      real*8 x1,y1,z1,
     *       x2,y2,z2,
     *       x3,y3,z3,
     *       x4,y4,z4,
     *       x5,y5,z5
      real*8 xa,ya,za
      integer iflag,idebug,ierr,itmp
C
      integer iflag1432,
     *        iflag125,
     *        iflag235,
     *        iflag345,
     *        iflag415
C
      real*8 ds1a, ds2a, ds3a, ds4a, ds5a, dsmax
C
      real*8 xepsilon
      character*132 logmess
      data xepsilon / 1.0d-10 /
C
C
C ######################################################################
C
      idebug=iflag
      iflag=0
      ds1a=(x1-xa)**2+(y1-ya)**2+(z1-za)**2
      ds2a=(x2-xa)**2+(y2-ya)**2+(z2-za)**2
      ds3a=(x3-xa)**2+(y3-ya)**2+(z3-za)**2
      ds4a=(x4-xa)**2+(y4-ya)**2+(z4-za)**2
      ds5a=(x5-xa)**2+(y5-ya)**2+(z5-za)**2
      dsmax=max(ds1a,ds2a,ds3a,ds4a,ds5a)
      if(ds1a.le.xepsilon*dsmax) then
         iflag=7
         goto 9999
      elseif(ds2a.le.xepsilon*dsmax) then
         iflag=8
         goto 9999
      elseif(ds3a.le.xepsilon*dsmax) then
         iflag=9
         goto 9999
      elseif(ds4a.le.xepsilon*dsmax) then
         iflag=10
         goto 9999
      elseif(ds5a.le.xepsilon*dsmax) then
         iflag=11
         goto 9999
      endif
      iflag1432=0
      iflag125=0
      iflag235=0
      iflag345=0
      iflag415=0
      itmp=idebug
      call inside_quad(x1,y1,z1,x4,y4,z4,x3,y3,z3,x2,y2,z2,
     *                 xa,ya,za,
     *                 itmp)
      iflag1432=itmp
      itmp=idebug
      call inside_tri(x1,y1,z1,x2,y2,z2,x5,y5,z5,
     *                xa,ya,za,
     *                itmp)
      iflag125=itmp
      itmp=idebug
      call inside_tri(x2,y2,z2,x3,y3,z3,x5,y5,z5,
     *                xa,ya,za,
     *                itmp)
      iflag235=itmp
      itmp=idebug
      call inside_tri(x3,y3,z3,x4,y4,z4,x5,y5,z5,
     *                xa,ya,za,
     *                itmp)
      iflag345=itmp
      itmp=idebug
      call inside_tri(x4,y4,z4,x1,y1,z1,x5,y5,z5,
     *                xa,ya,za,
     *                itmp)
      iflag415=itmp

      if(iflag1432.eq.0.and.iflag125.eq.0.and.
     *   iflag235.eq.0.and.iflag345.eq.0.and.
     *   iflag415.eq.0) then
         iflag=0
      elseif(iflag1432.ge.0.and.iflag125.ge.0.and.
     *       iflag235.ge.0.and.iflag345.ge.0.and.
     *       iflag415.ge.0) then
         if(iflag1432.gt.0) then
            iflag=1
         elseif(iflag125.gt.0) then
            iflag=2
         elseif(iflag235.gt.0) then
            iflag=3
         elseif(iflag345.gt.0) then
            iflag=4
         elseif(iflag415.gt.0) then
            iflag=5
         else
            iflag=-1
         endif
      else
         iflag=-1
      endif
      goto 9999
 9999 continue
      if (idebug.gt.1)then
         write(logmess,'(a,i5)')
     *    "Exit inside_pyr iflag: ",iflag
         call writloga('default',0,logmess,0,ierr)
      endif
      return
      end

      subroutine inside_pri(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,x6,y6,z6,
     *                      xa,ya,za,
     *                      iflag)
C
C ######################################################################
C
C     NOTE: I have not checked but I expect this will suffer from the same
C     problems as inside_hex_planar in the case where the elements quad
C     face is highly distorted. Carl Gable Tue Dec 13 14:40:29 MST 2005
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      real*8 x1,y1,z1,
     *       x2,y2,z2,
     *       x3,y3,z3,
     *       x4,y4,z4,
     *       x5,y5,z5,
     *       x6,y6,z6
      real*8 xa,ya,za
      integer iflag,idebug,ierr,itmp
C
      integer iflag132,
     *        iflag456,
     *        iflag1254,
     *        iflag2365,
     *        iflag1463
C
      real*8 ds1a, ds2a, ds3a, ds4a, ds5a, ds6a, dsmax
C
      real*8 xepsilon
      data xepsilon / 1.0d-10 /
      character*132 logmess
C
C
C ######################################################################
C
      idebug=iflag
      iflag=0
      ds1a=(x1-xa)**2+(y1-ya)**2+(z1-za)**2
      ds2a=(x2-xa)**2+(y2-ya)**2+(z2-za)**2
      ds3a=(x3-xa)**2+(y3-ya)**2+(z3-za)**2
      ds4a=(x4-xa)**2+(y4-ya)**2+(z4-za)**2
      ds5a=(x5-xa)**2+(y5-ya)**2+(z5-za)**2
      ds6a=(x6-xa)**2+(y6-ya)**2+(z6-za)**2
      dsmax=max(ds1a,ds2a,ds3a,ds4a,ds5a,ds6a)
      if(ds1a.le.xepsilon*dsmax) then
         iflag=7
         goto 9999
      elseif(ds2a.le.xepsilon*dsmax) then
         iflag=8
         goto 9999
      elseif(ds3a.le.xepsilon*dsmax) then
         iflag=9
         goto 9999
      elseif(ds4a.le.xepsilon*dsmax) then
         iflag=10
         goto 9999
      elseif(ds5a.le.xepsilon*dsmax) then
         iflag=11
         goto 9999
      elseif(ds6a.le.xepsilon*dsmax) then
         iflag=12
         goto 9999
      endif
      iflag132=0
      iflag456=0
      iflag1254=0
      iflag2365=0
      iflag1463=0
      itmp=idebug
      call inside_tri(x1,y1,z1,x3,y3,z3,x2,y2,z2,
     *                xa,ya,za,
     *                itmp)
      iflag132=itmp
      itmp=idebug
      call inside_tri(x4,y4,z4,x5,y5,z5,x6,y6,z6,
     *                xa,ya,za,
     *                itmp)
      iflag456=itmp
      itmp=idebug
      call inside_quad(x1,y1,z1,x2,y2,z2,x5,y5,z5,x4,y4,z4,
     *                 xa,ya,za,
     *                 itmp)
      iflag1254=itmp
      itmp=idebug
      call inside_quad(x2,y2,z2,x3,y3,z3,x6,y6,z6,x5,y5,z5,
     *                 xa,ya,za,
     *                 itmp)
      iflag2365=itmp
      itmp=idebug
      call inside_quad(x1,y1,z1,x4,y4,z4,x6,y6,z6,x3,y3,z3,
     *                 xa,ya,za,
     *                 itmp)
      iflag1463=itmp
      itmp=idebug

      if(iflag132.eq.0.and.iflag456.eq.0.and.
     *   iflag1254.eq.0.and.iflag2365.eq.0.and.
     *   iflag1463.eq.0) then
         iflag=0
      elseif(iflag132.ge.0.and.iflag456.ge.0.and.
     *       iflag1254.ge.0.and.iflag2365.ge.0.and.
     *       iflag1463.ge.0) then
         if(iflag132.gt.0) then
            iflag=1
         elseif(iflag456.gt.0) then
            iflag=2
         elseif(iflag1254.gt.0) then
            iflag=3
         elseif(iflag2365.gt.0) then
            iflag=4
         elseif(iflag1463.gt.0) then
            iflag=5
         else
            iflag=-1
         endif
      else
         iflag=-1
      endif
      goto 9999
 9999 continue
      if (idebug.gt.0) then
         write(logmess,'(a,i5)')
     *    "Exit inside_pri iflag: ",iflag
         call writloga('default',0,logmess,0,ierr)
      endif
      return
      end

      subroutine inside_hex_planar(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                      xa,ya,za,
     *                      iflag)
C
C ######################################################################
C     NOTE: This version will not work correctly if the faces of the hex
C     are non-planar. For example given the hex:
C
C   1  0.000   0.000   1.000
C   2  1.000   0.000   1.000
C   3  1.000   1.000   11.000
C   4  0.000   1.000   1.000
C   5  0.000   0.000   0.000
C   6  1.000   0.000   0.000
C   7  1.000   1.000   10.000
C   8  0.000   1.000   0.000
C
C     the method implemented in this subroutine will not give correct results.
C     I believe if the faces of the hex are planar this will give correct results
C     but I did not do exaustive testing. Carl Gable Tue Dec 13 14:40:29 MST 2005
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      real*8 x1,y1,z1,
     *       x2,y2,z2,
     *       x3,y3,z3,
     *       x4,y4,z4,
     *       x5,y5,z5,
     *       x6,y6,z6,
     *       x7,y7,z7,
     *       x8,y8,z8
      real*8 xa,ya,za
      integer iflag,idebug,ierr,itmp
C
      integer iflag1432,
     *        iflag5678,
     *        iflag1265,
     *        iflag2376,
     *        iflag3487,
     *        iflag1584
C
      real*8 ds1a, ds2a, ds3a, ds4a, ds5a, ds6a, ds7a, ds8a, dsmax
C
      real*8 xepsilon
      data xepsilon / 1.0d-10 /
      character*132 logmess
C
C
C ######################################################################
C
      idebug=iflag
      iflag=0
      ds1a=(x1-xa)**2+(y1-ya)**2+(z1-za)**2
      ds2a=(x2-xa)**2+(y2-ya)**2+(z2-za)**2
      ds3a=(x3-xa)**2+(y3-ya)**2+(z3-za)**2
      ds4a=(x4-xa)**2+(y4-ya)**2+(z4-za)**2
      ds5a=(x5-xa)**2+(y5-ya)**2+(z5-za)**2
      ds6a=(x6-xa)**2+(y6-ya)**2+(z6-za)**2
      ds7a=(x7-xa)**2+(y7-ya)**2+(z7-za)**2
      ds8a=(x8-xa)**2+(y8-ya)**2+(z8-za)**2
      dsmax=max(ds1a,ds2a,ds3a,ds4a,ds5a,ds6a,ds7a,ds8a)
      if(ds1a.le.xepsilon*dsmax) then
         iflag=7
         goto 9999
      elseif(ds2a.le.xepsilon*dsmax) then
         iflag=8
         goto 9999
      elseif(ds3a.le.xepsilon*dsmax) then
         iflag=9
         goto 9999
      elseif(ds4a.le.xepsilon*dsmax) then
         iflag=10
         goto 9999
      elseif(ds5a.le.xepsilon*dsmax) then
         iflag=11
         goto 9999
      elseif(ds6a.le.xepsilon*dsmax) then
         iflag=12
         goto 9999
      elseif(ds7a.le.xepsilon*dsmax) then
         iflag=13
         goto 9999
      elseif(ds8a.le.xepsilon*dsmax) then
         iflag=14
         goto 9999
      endif
      iflag1432=0
      iflag5678=0
      iflag1265=0
      iflag2376=0
      iflag3487=0
      iflag1584=0
      itmp=idebug
      call inside_quad(x1,y1,z1,x4,y4,z4,x3,y3,z3,x2,y2,z2,
     *                 xa,ya,za,
     *                 itmp)
      iflag1432=itmp
      itmp=idebug
      call inside_quad(x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                 xa,ya,za,
     *                 itmp)
      iflag5678=itmp
      itmp=idebug
      call inside_quad(x1,y1,z1,x2,y2,z2,x6,y6,z6,x5,y5,z5,
     *                 xa,ya,za,
     *                 itmp)
      iflag1265=itmp
      itmp=idebug
      call inside_quad(x2,y2,z2,x3,y3,z3,x7,y7,z7,x6,y6,z6,
     *                 xa,ya,za,
     *                 itmp)
      iflag2376=itmp
      itmp=idebug
      call inside_quad(x3,y3,z3,x4,y4,z4,x8,y8,z8,x7,y7,z7,
     *                 xa,ya,za,
     *                 itmp)
      iflag3487=itmp
      itmp=idebug
      call inside_quad(x1,y1,z1,x5,y5,z5,x8,y8,z8,x4,y4,z4,
     *                 xa,ya,za,
     *                 itmp)
      iflag1584=itmp

      if(iflag2376.eq.0.and.iflag1584.eq.0.and.
     *   iflag5678.eq.0.and.iflag1432.eq.0.and.
     *   iflag1265.eq.0.and.iflag3487.eq.0) then
         iflag=0
      elseif(iflag2376.ge.0.and.iflag1584.ge.0.and.
     *       iflag5678.ge.0.and.iflag1432.ge.0.and.
     *       iflag1265.ge.0.and.iflag3487.ge.0) then
         if(iflag1432.gt.0) then
            iflag=1
         elseif(iflag5678.gt.0) then
            iflag=2
         elseif(iflag1265.gt.0) then
            iflag=3
         elseif(iflag2376.gt.0) then
            iflag=4
         elseif(iflag3487.gt.0) then
            iflag=5
         elseif(iflag1584.gt.0) then
            iflag=6
         else
            iflag=-1
         endif
      else
         iflag=-1
      endif
      goto 9999
 9999 continue
      if (idebug.gt.0) then
         write(logmess,'(a,i5)')
     *    "Exit inside_hex_planer iflag: ",iflag
         call writloga('default',0,logmess,0,ierr)
      endif
      return
      end

      subroutine inside_tet(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      xa,ya,za,
     *                      iflag)
     &    BIND(C, name="INSIDE_TET")
C
C ######################################################################
C
      use, intrinsic :: iso_c_binding
      implicit none
C
C ######################################################################
C
      real*8 x1,y1,z1,
     *       x2,y2,z2,
     *       x3,y3,z3,
     *       x4,y4,z4
      real*8 xa,ya,za
      integer iflag,idebug,ierr,itmp
C
      integer iflag234,
     *        iflag143,
     *        iflag124,
     *        iflag132
C
       character*132 logmess
C
C ######################################################################
C
      idebug=iflag
      iflag=0
      iflag234=0
      iflag143=0
      iflag124=0
      iflag132=0
      itmp=idebug
      call inside_tri(x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                xa,ya,za,
     *                itmp)
      iflag234=itmp
      itmp=idebug
      call inside_tri(x1,y1,z1,x4,y4,z4,x3,y3,z3,
     *                xa,ya,za,
     *                itmp)
      iflag143=itmp
      itmp=idebug
      call inside_tri(x1,y1,z1,x2,y2,z2,x4,y4,z4,
     *                xa,ya,za,
     *                itmp)
      iflag124=itmp
      itmp=idebug
      call inside_tri(x1,y1,z1,x3,y3,z3,x2,y2,z2,
     *                xa,ya,za,
     *                itmp)
      iflag132=itmp

      if(iflag234.eq.0.and.iflag143.eq.0.and.
     *   iflag124.eq.0.and.iflag132.eq.0) then
         iflag=0
C*****   ==> POINT MUST BE INSIDE ELEMENT
      elseif(iflag234.ge.0.and.iflag143.ge.0 .and.
     *       iflag124.ge.0.and.iflag132.ge.0) then
C*****       ==> POINT MUST BE INSIDE ELEMENT AND ON AT LEAST
C*****           ONE SURFACE.
         if(iflag234.gt.0) then
            iflag=1
         elseif(iflag143.gt.0) then
            iflag=2
         elseif(iflag124.gt.0) then
            iflag=3
         elseif(iflag132.gt.0) then
            iflag=4
         else
            iflag=-1
         endif
      else
         iflag=-1
C*****   ==> POINT MUST BE OUTSIDE ELEMENT.
      endif
      goto 9999
 9999 continue
      if (idebug.gt.0) then
         write(logmess,'(a,i5)')
     *    "Exit inside_tet iflags: ",iflag
         call writloga('default',0,logmess,0,ierr)
      endif
      return
      end

      subroutine inside_quad(x1,y1,z1,x4,y4,z4,x3,y3,z3,x2,y2,z2,
     *                      xa,ya,za,
     *                      iflag)
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      real*8 x1,y1,z1,
     *       x2,y2,z2,
     *       x3,y3,z3,
     *       x4,y4,z4
      real*8 xa,ya,za
      real*8 xavg, yavg, zavg
      integer iflag, idebug, ierr, itmp
C
      integer iflag14,
     *        iflag43,
     *        iflag32,
     *        iflag21

      character*132 logmess
C
C ######################################################################
C Begin inside quad
C
      idebug=iflag
      iflag=0
      xavg=0.25d+00 * (x1+x2+x3+x4)
      yavg=0.25d+00 * (y1+y2+y3+y4)
      zavg=0.25d+00 * (z1+z2+z3+z4)
      iflag14=0
      iflag43=0
      iflag32=0
      iflag21=0
      itmp=idebug
      call inside_tri(x1,y1,z1,x4,y4,z4,xavg,yavg,zavg,
     *                xa,ya,za,
     *                itmp)
      iflag14=itmp
      itmp=idebug
      call inside_tri(x4,y4,z4,x3,y3,z3,xavg,yavg,zavg,
     *                xa,ya,za,
     *                itmp)
      iflag43=itmp
      itmp=idebug
      call inside_tri(x3,y3,z3,x2,y2,z2,xavg,yavg,zavg,
     *                xa,ya,za,
     *                itmp)
      iflag32=itmp
      itmp=idebug
      call inside_tri(x2,y2,z2,x1,y1,z1,xavg,yavg,zavg,
     *                xa,ya,za,
     *                itmp)
      iflag21=itmp
C
C     ******************************************************************
C
C     IFLAG == +1 ==> POINT MUST BE ON SURFACE.
C           ==  0 ==> POINT MUST BE BELOW SURFACE.
C           == -1 ==> POINT MUST BE ABOVE SURFACE.
C
      if(iflag14.eq.0.and.iflag43.eq.0.and.
     *   iflag32.eq.0.and.iflag21.eq.0) then
         iflag=0
      elseif(iflag14.lt.0.and.iflag43.lt.0.and.
     *       iflag32.lt.0.and.iflag21.lt.0) then
         iflag=-1
      elseif(iflag14.gt.0.or.iflag43.gt.0.or.
     *       iflag32.gt.0.or.iflag21.gt.0) then
         iflag=1
      else
         iflag=-1
      endif

      goto 9999
 9999 continue
      if (idebug.gt.0) then
         write(logmess,'(a,1pe14.7e2,1x,1pe14.7e2,1x,1pe14.7e2)')
     *   " Compute quad median pnt: ",xavg,yavg,zavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a,i5)')                              
     *   "Exit inside_quad iflag: ",iflag
         call writloga('default',0,logmess,0,ierr)
      endif 
      return
      end

      subroutine inside_tri(xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,
     *                      xa,ya,za,
     *                      iflag)
C
C    This routine detirmines if the queary point is on the same
C    plane as the triangle.
C    A call to inside_tri2d returns the location of the query point.
C
C    This routine assumes the query point has been chosen as a
C    possible candidate with this element. It does not test for
C    a query point a long distance away.
C    The difficulty is in finding an epsilon volume to test
C    that the point is on the same plane. This volume can get
C    larger or smaller than precision assumed by 1e-10 epsilon.
C    Here we use the concept that epsilon needs to be related
C    to the size of the bounding box, and the distance away from
C    origin. As this distance grows over 1e+6, epsilon needs to
C    also increase in size to allow less precision in finding
C    a point exactly on the plane. 
C
C ######################################################################
C
      implicit none
      include 'consts.h'
C     this include file has epsilon and finds epsilonr based on machine
C ######################################################################
C
      real*8 xl1,yl1,zl1,
     *       xl2,yl2,zl2,
     *       xl3,yl3,zl3
      real*8 xa,ya,za
      real*8 ax4,ay4,az4
      real*8 eps_pos,eps_size, epstest
      real*8 xminbox, yminbox, zminbox
      real*8 xmaxbox, ymaxbox, zmaxbox
      real*8 maxdist,maxboxarea,maxboxvol,maxboxlen,
     *       xfac,xdist
      integer iflag,idebug,ierr,eps_adjust 
C
      real*8 voltet
      real*8 xepsilon, epsilonl, epsilonv, epsilona
C
      character*132 logmess
C
      data xepsilon / 1.0d-10 /
C
C ######################################################################
C Begin inside_tri
C
      idebug=iflag

      if(idebug .gt. 0)then
        write(logmess,'(a)')
     *  "--------   inside_tri "
        call writloga('default',0,logmess,0,ierr)
      endif

      iflag=0
      eps_adjust=0
      xminbox=min(xl1,xl2,xl3)
      yminbox=min(yl1,yl2,yl3)
      zminbox=min(zl1,zl2,zl3)
      xmaxbox=max(xl1,xl2,xl3)
      ymaxbox=max(yl1,yl2,yl3)
      zmaxbox=max(zl1,zl2,zl3)

C     xfac name changed to represent maxboxlen
C     which is the diagonal of the bounding box
      maxboxlen=sqrt((xmaxbox-xminbox)**2 +
     *                  (ymaxbox-yminbox)**2 +
     *                  (zmaxbox-zminbox)**2 )
      maxboxarea=abs(maxboxlen*maxboxlen)*.5d0
      maxboxvol=(maxboxlen*maxboxlen*maxboxlen)/3.d0


C-----------------------------------------------------------------
C     Define epsilon for zero volume test
C     Method Notes
C     find distance from origin to nearest point
C     find an epsilon multiple of 10 that increases
C     epsilon relative to the distance away from origin
C     We know that vol test 1.0d-11 works for xyz near origin
C     For 32bit machine epsilonr will be near 1.0d-16
C     note: This is really an adjustment of epsilon to allow 
C     for big numbers with a number of digits being used
C     Should we check for number of significant digits 
C     instead of distance?

C     Define object size and number size
C     Use bounding box size and position from origin

      eps_size = maxboxvol 
      xdist=sqrt(xminbox**2+yminbox**2+zminbox**2)
      eps_pos=1.0d0
      if (xdist.gt.eps_pos) then
        do while (eps_pos.lt.xdist*1.0d-2)
           eps_pos = eps_pos * 1.0d+1
        enddo
      endif
      epstest = eps_size*(epsilonr*eps_pos)

C     Put a lower bound on epsilon adjustment
      if (epstest.lt. epsilonr) then
         eps_adjust = -1
         epstest=epsilonr
      endif

C     Put an upper bound on epsilon adjustment
      if (epstest.gt.one) then
         eps_adjust = 1
        epstest=one 
      endif 
           
C-----------------------------------------------------------------

C     calculate the volume formed by source tri and query point
      ax4=  (yl3-yl1)*(zl2-zl1)-(zl3-zl1)*(yl2-yl1)
      ay4=-((xl3-xl1)*(zl2-zl1)-(zl3-zl1)*(xl2-xl1))
      az4=  (xl3-xl1)*(yl2-yl1)-(yl3-yl1)*(xl2-xl1)
      voltet=-((xa-xl1)*ax4+(ya-yl1)*ay4+(za-zl1)*az4)

C     ******************************************************************
C
C     IFLAG == +1 ==> POINT MUST BE ON SURFACE.
C           ==  0 ==> POINT MUST BE BELOW SURFACE.
C           == -1 (and less) ==> POINT MUST BE ABOVE SURFACE.
C

      if(abs(voltet).le.epstest) then
         iflag=+1
      elseif(voltet.gt. epstest) then
         iflag=-1
      elseif(voltet.lt.-epstest) then
         iflag= 0
      endif

C     epsilon checks eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeepsilon
      if (idebug.gt.0) then
        xfac=epsilonr
        ierr=1
        do while ((xfac.lt.10.d0))
           xfac = xfac * 2.0d0
           if (abs(voltet).le.(xfac).and.ierr.ne.0) then
         write(logmess,'(a,1pe20.12e2,1pe20.12e2,1pe20.12e2)')
     *   "vol,eps,eps_find: ",voltet,epstest,xfac
         call writloga('default',0,logmess,0,ierr)
              ierr=0
           endif
        enddo
        if (ierr.ne.0) then
          write(logmess,'(a,1pe20.12e2)')
     *    "NOT WITHIN EPSILON: ",xfac
          call writloga('default',0,logmess,0,ierr)
        endif
      endif
C     epsilon checks eeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeepsilon

      goto 9999
 9999 continue
      if (idebug.gt.0) then

C       get cmo epsilons for debug checks
C       get epsilon settings for current cmo object
        if (idebug.gt.4) then
          call get_epsilon("epsilonl",epsilonl)
          call get_epsilon("epsilona",epsilona)
          call get_epsilon("epsilonv",epsilonv)
        endif

        if (eps_adjust .gt. 0) then
         write(logmess,'(a)')
     *   " Warning upper bound: volume epsilon adjusted to one."
         call writloga('default',0,logmess,0,ierr)
        endif
        if (eps_adjust .lt. 0) then
         write(logmess,'(a)')
     *   " Warning lower bound: volume epsilon adjusted to epsilonr."
         call writloga('default',0,logmess,0,ierr)
        endif

        write(logmess,'(a,i5,1pe20.12e2,1pe20.12e2)')
     *  "Exit inside_tri iflag, vol, eps: ",
     *         iflag,voltet,epstest
        call writloga('default',0,logmess,0,ierr)
      endif 

      return
      end
      subroutine inside_quad2d(x1,y1,z1,x4,y4,z4,x3,y3,z3,x2,y2,z2,
     *                         xa,ya,za,
     *                         iflag)
C
C ######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE DETERMINES IF A TEST POINT (xa,ya,za) IS INSIDE
C            OR OUTSIDE A QUAD ELEMENT. THE DEFINITION OF THE QUAD IS A LINEAR
C            POLYHEDRA FORMED BY SUBDIVIDING IT INTO 4 TRIANGLE ELEMENTS.
C
C            THIS VERSION USES THE MEDIAN POINT OF THE ELEMENT AS THE 
C            VIRTUAL POINT TO FORM THE 4 TRIS.
C
C
C     ******************************************************************
C
C     DEFINE THE STRUCTURE OF A GENERIC HEXRAHEDRON.
C
C     5 is the median point
C     Triangles are named for their outside edge (quad edge)
C     All other edges are virtual edges connected to the median point
C
C     Triangle 21: 1 2 5 (edge 1)
C              32: 2 3 5 (edge 2)
C              43: 3 4 5 (edge 3)
C              14: 1 4 5 (edge 4)
C    
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x4,y4,z4) - THE COORDINATES OF THE QUAD 
C        (xa,ya,za)                - THE POINT TO BE TESTED
C
C
C     OUTPUT ARGUMENTS -
C
C        iflag = -1  TEST POINT IS OUTSIDE
C              =  0  TEST POINT IS INSIDE
C              >  0  TEST POINT IS ON AN EXTERIOR BOUNDARY
C              =  1  TEST IS ON EDGE 1
C              =  2  TEST IS ON EDGE 2
C              =  3  TEST IS ON EDGE 3
C              =  4  TEST IS ON EDGE 4
C
C        To be done:
C        The iflag definitions have been expanded to include
C        additional negative and positive values for better evaluations
C        Not yet robust, may be assigned associated edge rather than point
C        iflag < -1  TEST POINT IS OUTSIDE (iflag indicates failed edge test)
C              =  11 TEST IS WITHIN EPSILON OF NODE 1
C              =  12 TEST IS WITHIN EPSILON OF NODE 2
C              =  13 TEST IS WITHIN EPSILON OF NODE 3
C              =  14 TEST IS WITHIN EPSILON OF NODE 4
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      real*8 x1,y1,z1,
     *       x2,y2,z2,
     *       x3,y3,z3,
     *       x4,y4,z4
      real*8 xa,ya,za
      real*8 xavg, yavg, zavg
      integer iflag,idebug, ierr,itmp
C
      integer iflag14,
     *        iflag43,
     *        iflag32,
     *        iflag21

       character*132 logmess
C
C ######################################################################
C     Begin indside_quad2d
C
      idebug=iflag
      iflag=0
      xavg=0.25d+00 * (x1+x2+x3+x4)
      yavg=0.25d+00 * (y1+y2+y3+y4)
      zavg=0.25d+00 * (z1+z2+z3+z4)

C     use flags to pass debug value to lower routines
      iflag14=0
      iflag43=0
      iflag32=0
      iflag21=0
      itmp=idebug
      call inside_tri2d(x1,y1,z1,x4,y4,z4,xavg,yavg,zavg,
     *                  xa,ya,za,
     *                  itmp)
      iflag14=itmp
      itmp=idebug
      call inside_tri2d(x4,y4,z4,x3,y3,z3,xavg,yavg,zavg,
     *                  xa,ya,za,
     *                  itmp)
      iflag43=itmp
      itmp=idebug
      call inside_tri2d(x3,y3,z3,x2,y2,z2,xavg,yavg,zavg,
     *                  xa,ya,za,
     *                  itmp)
      iflag32=itmp
      itmp=idebug
      call inside_tri2d(x2,y2,z2,x1,y1,z1,xavg,yavg,zavg,
     *                  xa,ya,za,
     *                  itmp)
      iflag21=itmp

C     INSIDE - all dot products are less than negative epsilon
      if(iflag14.eq.0.and.iflag43.eq.0.and.
     *   iflag32.eq.0.and.iflag21.eq.0) then
         iflag=0

C     ON QUAD EDGE  - values of 3 are quad edges
C     ON QUAD POINT - values of 11 and 12 are edge points 
      elseif(iflag14.gt.0.or.iflag43.gt.0.or.
     *       iflag32.gt.0.or.iflag21.gt.0) then
         if(iflag14.eq.3) then
            iflag=4
         elseif(iflag43.eq.3) then
            iflag=3
         elseif(iflag32.eq.3) then
            iflag=2
         elseif(iflag21.eq.3) then
            iflag=1
         elseif(iflag14.eq.11) then
            iflag=11
         elseif(iflag14.eq.12) then
            iflag=12
         elseif(iflag43.eq.11) then
            iflag=12
         elseif(iflag43.eq.12) then
            iflag=13
         elseif(iflag32.eq.11) then
            iflag=13
         elseif(iflag32.eq.12) then
            iflag=14
         elseif(iflag21.eq.11) then
            iflag=14
         elseif(iflag21.eq.12) then
            iflag=11
         else
            iflag=0
         endif
C     ON TRI FACE
      elseif(iflag14.eq.0.or.iflag43.eq.0.or.
     *       iflag32.eq.0.or.iflag21.eq.0) then
         iflag=0
C     OUTSIDE - if all other tests fail
      else
         iflag=-1
      endif

      goto 9999
 9999 continue

      if (idebug.gt.0) then
         write(logmess,'(a,1pe14.7e2,1x,1pe14.7e2,1x,1pe14.7e2)')
     *   " Compute quad2d median pnt: ",xavg,yavg,zavg
         call writloga('default',0,logmess,0,ierr)
         write(logmess,'(a,i5)') 
     *   "Exit inside_quad2d iflag: ",iflag
         call writloga('default',0,logmess,0,ierr)
      endif
      return
      end

      subroutine inside_tri2d(xl1,yl1,zl1,xl2,yl2,zl2,xl3,yl3,zl3,
     *                        xa,ya,za,
     *                        iflag)
C
C ######################################################################
C      PURPOSE -
C
C         THIS ROUTINE DETERMINES IF A TEST POINT (xa,ya,za) IS INSIDE
C            OR OUTSIDE A TRI ELEMENT. 
C
C     ******************************************************************
C
C     if called from inside_quad2d pnt 3 is the median point and
C     the outside edge represents the quad edge and points (1 and 2)
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x3,y3,z3) - THE COORDINATES OF THE TRI
C        (xa,ya,za)                - THE POINT TO BE TESTED
C
C
C     OUTPUT ARGUMENTS -
C
C        iflag = -1  TEST POINT IS OUTSIDE
C              =  0  TEST POINT IS INSIDE
C              >  0  TEST POINT IS ON AN EXTERIOR BOUNDARY
C              =  1  TEST IS ON EDGE 1
C              =  2  TEST IS ON EDGE 2
C              =  3  TEST IS ON EDGE 3 (quad edge and points)
C
C        The iflag definitions have been expanded to include
C        additional negative and positive values for better evaluations
C        In general, the larger the positive number
C        The more lenient the test that was satisifed
C        So that numbers closest to 0 have highest confidence
C
C        iflag < -1  TEST POINT IS OUTSIDE (iflag indicates failed edge test)
C              =  -6 Fails planar test with epsilon volume
C              =  -7 Fails planar test with epsilon area
C
C        test area formed from edge to query point
C              =  11 TEST IS EDGE or WITHIN EPSILON OF NODE 1
C              =  12 TEST IS EDGE or WITHIN EPSILON OF NODE 2
C              =  13 TEST IS EDGE or WITHIN EPSILON OF NODE 3 
C
C        test for vertice points, strict test does not often succeed 
C              =  21 TEST IS WITHIN EPSILON OF NODE 1
C              =  22 TEST IS WITHIN EPSILON OF NODE 2
C              =  23 TEST IS WITHIN EPSILON OF NODE 3 
C
C        test for vertice points, may find success where 
C        it is not intended (very large or small numbers) 
C        very often succeeds
C              =  31 TEST IS WITHIN EPSILON OF NODE 1
C              =  32 TEST IS WITHIN EPSILON OF NODE 2
C              =  33 TEST IS WITHIN EPSILON OF NODE 3 
C              >  33 TEST SUCCEEDS MULTIPLE POINTS
C
C     NOTE: Flags changed for better condition reporting
C     iflag < 0 flags remain the same, point is outside
C     iflag = 0 the point is inside, fist test succeeded
C     iflag > 0 indicate which of the tests succeeded
C               results closest to 0 have highest confidence
C
C     the elseif conditions after 0 flag
C     have been removed so all tests
C     are made instead of returning with first success 
C
C     CHANGES:
C
C     September 2010 tam
C     Extensive changes to allow flags to be used
C     for evaluation of confidence in result
C     when numbers are very small or large within
C     the tolerances of epsilon tests
C
C     August 2010 cwg
C     Add tests and relax possibility of success
C
C     July 2006 tam epsilon changes 
C     replace xepsilon using epsfac and esptest  
C     which are adjusted for distance from origin
C
C ######################################################################

C
      implicit none
C
      include 'consts.h'
C     this include file has epsilon and finds epsilonr based on machine
C ######################################################################
C
      real*8 xl1,yl1,zl1,
     *       xl2,yl2,zl2,
     *       xl3,yl3,zl3
      real*8 xa,ya,za
      integer iflag,idebug,eps_adjust,ierr,itmp
      integer iflag_area, iflag_set, iflag_save
C
      real*8 xdot1, xdot2, xdot3, xarea, voltet
      real*8 ax1,ay1,az1,ax2,ay2,az2,ax3,ay3,az3,ax4,ay4,az4
      real*8 xfac, area1, area2, area3, ds23, ds2a, ds3a, ds13,
     *       ds1a, ds12, dtest

      real*8 xminbox, yminbox, zminbox
      real*8 xmaxbox, ymaxbox, zmaxbox
      real*8 xmedbox, ymedbox, zmedbox
      real*8 maxboxlen, maxboxarea, maxboxvol
      real*8 xfacbox, epsilonv, epsilona, epsfac
      real*8 eps_len,eps_size,eps_pos,epstest,xdist
     
      real*8  xsum,ra,rb,rc
      real*8 extemp,extemp2,exa,exb,exc,exsum,exerr,exerr_tot
C
      character*132 logmess
C
      real*8 xepsilon
      data xepsilon / 1.0d-10 /
C
C ######################################################################
C Begin inside_tri2d
C
      idebug=iflag

      if (idebug .gt. 0) then
         write(logmess,'(a)')
     *   "--------   inside_tri2d "
         call writloga('default',0,logmess,0,ierr)
      endif

      iflag=0
      iflag_set=0
      iflag_save=0
      eps_adjust=0
      xminbox=min(xl1,xl2,xl3)
      yminbox=min(yl1,yl2,yl3)
      zminbox=min(zl1,zl2,zl3)
      xmaxbox=max(xl1,xl2,xl3)
      ymaxbox=max(yl1,yl2,yl3)
      zmaxbox=max(zl1,zl2,zl3)
      xmedbox=(xl1+xl2+xl3)/3.0d0
      ymedbox=(yl1+yl2+yl3)/3.0d0
      zmedbox=(zl1+zl2+zl3)/3.0d0

C     old xfacbox,eps_len was diagonal mult by xepsilon
C     maxboxlen is the diagonal of bounding box 
      maxboxlen=sqrt((xmaxbox-xminbox)**2 +
     *                  (ymaxbox-yminbox)**2 +
     *                  (zmaxbox-zminbox)**2 )
      maxboxarea=abs(maxboxlen*maxboxlen)*.5d0
      maxboxvol=(maxboxlen*maxboxlen*maxboxlen)/3.0d0
C
C     Compute the volume of a tet formed by the three points
C     of the triangle and the query point. A small volume implies
C     the query point is in the plane of the triangle but says nothing
C     about if the point is inside or outside the triangle.
C
      ax4=  (yl3-yl1)*(zl2-zl1)-(zl3-zl1)*(yl2-yl1)
      ay4=-((xl3-xl1)*(zl2-zl1)-(zl3-zl1)*(xl2-xl1))
      az4=  (xl3-xl1)*(yl2-yl1)-(yl3-yl1)*(xl2-xl1)
      voltet=-((xa-xl1)*ax4+(ya-yl1)*ay4+(za-zl1)*az4)

C     epstest and epsfac should reflect same values found
C     in inside_tri subroutine testing for planer points
C
C     find distance from origin to nearest point
C     find an epsilon multiple of 10 that increases
C     epsilon relative to the distance away from origin
C     We know that vol test 1e-11 works for coordinates near origin
C     For 32bit machine epsilonr will be near 1e-16
C     
C     maxboxvol = 10,   eps_pos = 1
C                 100             10
C                 1000            100
C
C     Use the median point instead of the minbox. This will only effect
C     problems where the bounding box covers multiple orders of magnitude.
C

C     EPSILON COMPUTATON and ADJUSTMENTS
      eps_size = maxboxvol
c cwg      xdist=sqrt(xminbox**2+yminbox**2+zminbox**2)
      xdist=sqrt(xmedbox**2+ymedbox**2+zmedbox**2)
      eps_pos=1.0d0
      if (xdist.gt.eps_pos) then
        do while (eps_pos.lt.xdist*1.0d-2)
           eps_pos = eps_pos * 1.0d+1
        enddo
      endif

      epstest = eps_size*(epsilonr*eps_pos)

C     Put a lower bound on epsilon adjustment
      if (epstest.lt. epsilonr) then
        eps_adjust= -1
        epstest=epsilonr
      endif

C     Put an upper bound on epsilon adjustment
      if (epstest.gt.one) then
        eps_adjust= 1
        epstest=one
      endif
C     epsfac = max(epsilonr*eps_pos,epsilonr*1000.0d0)
      epsfac = max(epsilonr*eps_pos,epsilonr)


CEPS  This test is also used in inside_tri to detirmine if point on plane
C     and would normally be checked before this routine
C     Do this check here just in case it has not been done
      if(abs(voltet).le.epstest) then

C        compute mag of a4
         xarea=sqrt(ax4**2+ay4**2+az4**2)

C        FAIL -7 :  OUTSIDE TEST
         if(xarea.le.epstest) then
            iflag=-7
            goto 9999
         endif

C        INSIDE TEST
         ax3=  (yl2-yl1)*(za-zl1)-(zl2-zl1)*(ya-yl1)
         ay3=-((xl2-xl1)*(za-zl1)-(zl2-zl1)*(xa-xl1))
         az3=  (xl2-xl1)*(ya-yl1)-(yl2-yl1)*(xa-xl1)
         ax2=  (yl1-yl3)*(za-zl3)-(zl1-zl3)*(ya-yl3)
         ay2=-((xl1-xl3)*(za-zl3)-(zl1-zl3)*(xa-xl3))
         az2=  (xl1-xl3)*(ya-yl3)-(yl1-yl3)*(xa-xl3)
         ax1=  (yl3-yl2)*(za-zl2)-(zl3-zl2)*(ya-yl2)
         ay1=-((xl3-xl2)*(za-zl2)-(zl3-zl2)*(xa-xl2))
         az1=  (xl3-xl2)*(ya-yl2)-(yl3-yl2)*(xa-xl2)
         xdot1=ax4*ax1+ay4*ay1+az4*az1
         xdot2=ax4*ax2+ay4*ay2+az4*az2
         xdot3=ax4*ax3+ay4*ay3+az4*az3

c cwg       xfac=epsfac*abs(xarea)
c tam       xfac=epsfac*abs(xarea)*1000.0
c commented out cwg change as it was inconsistent 
c with test in inside_tri and caused some failure
c where there was not a failure previously

         xfac=epsfac*abs(xarea)

C        FLAG 0 : FULLY INSIDE
         if(xdot1.lt.-xfac .and.
     *         xdot2.lt.-xfac .and.
     *         xdot3.lt.-xfac) then
               iflag=0
               goto 9999
         endif

C        INSIDE ON EDGE OR POINT
C        The following 2 tests are accurate for numbers within epsilon values
C        Multiple tests are evaluated in attempt to find success
C        when numbers get too large or small for epsilon to resolve

C        Check for being on one of three edges or one of three vertices
C        check tri areas formed from edge points 1-2-3 and query point a 
C
C        A small area implies the query point is on that edge
C        or is on the end point of that edge.
C
         area1=sqrt(ax1**2+ay1**2+az1**2)
         area2=sqrt(ax2**2+ay2**2+az2**2)
         area3=sqrt(ax3**2+ay3**2+az3**2)
C
C        easy checks for query point on triangle vertice 
C        If two areas are small, that implies the query point
C        is on their common end point.
C
C CWG    The tests below do not seem valid to me. If the query point
C        is colinear then the area of the two triangles can be zero
C        but the query point may not be coincident with one of the 
C        end points. Maybe previous tests have already determined that
C        the point is not clearly outside, so the fact that it gets
C        here implies other tests have been passed.
C

C        FLAG 21,22,23 : END POINT through zero distance
C        has shown infrequent success
C        Can be used to confirm vertice point success
         iflag_set = 0
         if     ((abs(xa-xl1).lt. epsfac)
     *            .and.(abs(ya-yl1).lt. epsfac)
     *            .and.(abs(za-zl1).lt. epsfac)) then
                    iflag_set = iflag_set+1
                    iflag=21
         endif
         if ((abs(xa-xl2).lt. epsfac)
     *            .and.(abs(ya-yl2).lt. epsfac)
     *            .and.(abs(za-zl2).lt. epsfac)) then
                    iflag_set = iflag_set+1
                    iflag=22
         endif
         if ((abs(xa-xl3).lt. epsfac)
     *            .and.(abs(ya-yl3).lt. epsfac)
     *            .and.(abs(za-zl3).lt. epsfac)) then
                    iflag_set = iflag_set+1
                    iflag=23
         endif
         if (iflag_set.gt.1) then
            itmp= iflag_set
            iflag_save = iflag + itmp 
            if (idebug.gt.0 ) then
              write(logmess,'(a,i5,1pe20.12e2)')
     *     "Multiple points using distance test 20 within epsfac: ",
     *        iflag_save,epsfac
              call writloga('default',0,logmess,0,ierr)
            endif
         else if (iflag_set.eq.1) then
            iflag_save = iflag 
         endif


C        FLAG 31,32,33 : END POINT through shared zero area
C        these tests often succeed at very small numbers
C        because all points are within the epsilon used
         itmp = iflag_set
         iflag_set = 0
         if ((abs(area3).lt.xfac).and.
     *                 (abs(area2).lt.xfac)) then

               iflag=31
               iflag_set = iflag_set+1
         endif
         if ((abs(area1).lt.xfac).and.
     *                 (abs(area3).lt.xfac)) then
               iflag_set = iflag_set+1
               iflag=32
         endif
         if ((abs(area1).lt.xfac).and.
     *                 (abs(area2).lt.xfac)) then
               iflag_set = iflag_set+1
               iflag=33
          endif
          if (iflag_set.gt.1) then
            if (idebug.gt.0 ) then
               iflag = iflag + iflag_set
               write(logmess,'(a,i5,1pe20.12e2)')
     *     "Multiple points using area test 30 within epsfac: ",
     *         iflag,epsfac
               call writloga('default',0,logmess,0,ierr)
            endif
          endif

C         set flags so far
C         let flags in 20's win over 30's
C         add number of success to success in 30's
          iflag_set = itmp + iflag_set 
          if (iflag_save.gt.20 .and. iflag_save.lt.30) then
             iflag = 0
          else 
             iflag_save = iflag_save + iflag
             if (iflag_set.gt.1) iflag_save = iflag_save+iflag_set
             iflag = 0
          endif
          itmp = 0


C         CONTINUE tests with point and edge evaluations

C         Check for zero area formed between each of the
C         two vertices and the query point
C         if edgelen1-edgelen2-edgelen3 is zero, then on edge 
C         Note these tag are in teens and high confidence
C
C  cwg    Added additional check to see if an edge length is zero
C         and therefore on the vertice point
C         This is needed when signifigant digits are lost 
C         causing the first length check to fail the epsilon test
C         Note this tag is set to 70's and has least confidence
C
C         ON EDGE 1 or ON pnt 2 or 3
C         area formed from edge 23 to query point 
          if(abs(area1).lt.xfac) then

C                 FLAG 1 : ON EDGE 2 to 3
C                 FLAG 12 : ON POINT 2 
C                 FLAG 13 : ON POINT 3 
C                 FAIL -1 : NOT ON EDGE 2 to 3 
                  ds23=sqrt((xl3-xl2)**2+(yl3-yl2)**2+(zl3-zl2)**2)
                  ds2a=sqrt((xl2-xa)**2+(yl2-ya)**2+(zl2-za)**2)
                  ds3a=sqrt((xl3-xa)**2+(yl3-ya)**2+(zl3-za)**2)
                  dtest = abs(ds23-ds2a-ds3a)
                  if(dtest .lt. xepsilon*eps_pos*ds23) then
                    iflag=1
                    itmp = itmp+1
                  else 
                     if (ds2a.lt.xfac) then
                       iflag=12
                       itmp = itmp+1
                     else if (ds3a.lt.xfac) then
                       iflag=13
                       itmp = itmp+1
                     else
                       iflag=-1
                     endif
                  endif
          endif

C         ON EDGE 2 or ON pnt 1 or 3
C                 FLAG 2 : ON EDGE 1 to 3
C                 FLAG 11 : ON POINT 1 
C                 FLAG 13 : ON POINT 3 
C                 FAIL -2 : NOT ON EDGE 1 to 3 
C         area formed from edge 13 to query point 
          if(abs(area2).lt.xfac) then
                  ds13=sqrt((xl3-xl1)**2+(yl3-yl1)**2+(zl3-zl1)**2)
                  ds1a=sqrt((xl1-xa)**2+(yl1-ya)**2+(zl1-za)**2)
                  ds3a=sqrt((xl3-xa)**2+(yl3-ya)**2+(zl3-za)**2)
                  dtest=abs(ds13-ds1a-ds3a)
                  if(dtest .lt. xepsilon*eps_pos*ds13) then
                     iflag=2
                     itmp = itmp+1
                  else 
                     if (ds1a.lt.xfac) then
                        iflag=11
                        itmp = itmp+1
                     elseif (ds3a.lt.xfac) then
                        iflag=13
                        itmp = itmp+1
                     else
                        iflag=-2
                     endif
                  endif
          endif

C         ON EDGE 3 or ON pnt 1 or 2
C                 FLAG 3 : ON EDGE 1 to 2
C                 FLAG 11 : ON POINT 1 
C                 FLAG 12 : ON POINT 2 
C                 FAIL -3 : NOT ON EDGE 1 to 3 
C         area formed from edge 12 to query point 
          if(abs(area3).lt.xfac) then
                  ds12=sqrt((xl2-xl1)**2+(yl2-yl1)**2+(zl2-zl1)**2)
                  ds1a=sqrt((xl1-xa)**2+(yl1-ya)**2+(zl1-za)**2)
                  ds2a=sqrt((xl2-xa)**2+(yl2-ya)**2+(zl2-za)**2)
                  dtest=abs(ds12-ds1a-ds2a)
                  if(dtest .lt. xepsilon*eps_pos*ds12) then
                     iflag=3
                     itmp = itmp+1
                  else 
                     if (ds1a.lt.xfac) then
                        iflag=11
                        itmp = itmp+1
                     elseif (ds2a.lt.xfac) then
                        iflag=12
                        itmp = itmp+1
                     else
                        iflag=-3
                     endif
                  endif
          endif

          if (idebug.gt.0 .and. itmp.gt.1) then
             write(logmess,'(i5,a,i5,1pe20.12e2)')
     *       itmp, " Multiple edge tests: last flag and epsilon: ",
     *       iflag,xfac
             call writloga('default',0,logmess,0,ierr)
          endif
          iflag_set = iflag_set + itmp


C         NOT FOUND on edge or vertice
C
C         Arriving here means the outside test failed and inside tests
C         failed. The result will be the point will be reported to be
C         outside, but not due to a any positive result from a test. One
C         just arrives here because no tests worked.
C
C         check for failure - make sure iflag is to something other than 0 
C         FAIL -5 : fell through all EDGE cases
          if (iflag.eq.0) then
C            should not get here
             iflag=-5
          endif

C     epstest inside_tri routine finds not on plane
C     should not get here if planar test used before this routine
C     FAIL -6 : NOT ON PLANE 
      else

           iflag=-6

      endif
CEPS  outer if-then case for point on plane

      goto 9999
 9999 continue


      if (idebug .gt. 0) then

C       epsilon adjustments
        if (eps_adjust .gt. 0) then
         write(logmess,'(a)')
     *   " Warning upper bound: volume epsilon adjusted to one."
         call writloga('default',0,logmess,0,ierr)
        endif
        if (eps_adjust .lt. 0) then
         write(logmess,'(a)')
     *   " Warning lower bound: volume epsilon adjusted to epsilonr."
         call writloga('default',0,logmess,0,ierr)
        endif

C       Failures finding test success
        if (iflag.eq.-6) then
         write(logmess,'(a,1pe20.12e2)')
     *   "Exit inside_tri2d iflag -6, vol eps: ",epsilonv
         call writloga('default',0,logmess,0,ierr)

        else if (iflag.eq.-7) then
         write(logmess,'(a,2pe20.12e2)')
     *   "Exit inside_tri2d iflag -7, area epslen: ",epstest,xarea
         call writloga('default',0,logmess,0,ierr)

        else if (iflag.eq.-8) then
         write(logmess,'(a,1pe20.12e2)')
     *   "Exit inside_tri2d iflag -8, point or edge xfac: ",xfac
         call writloga('default',0,logmess,0,ierr)

        else if (iflag.eq.0) then
         write(logmess,'(a,1pe14.7e2,1x,1pe14.7e2,1x,1pe14.7e2)')
     *   "Exit inside_tri2d iflag 0, DOT: ",xdot1,xdot2,xdot3
         call writloga('default',0,logmess,0,ierr)

        else if (iflag.eq.-1 .or. iflag.eq.-2 
     *                       .or. iflag.eq.-3) then
         write(logmess,'(a,i5,1pe20.12e2)')
     *   "Exit inside_tri2d iflag, edge epslen: ",iflag,xfac
         call writloga('default',0,logmess,0,ierr)

        else if (iflag.eq.-5) then
         write(logmess,'(a,1pe20.12e2)')
     *   "Exit inside_tri2d iflag -5, edge not found within ",xfac
         call writloga('default',0,logmess,0,ierr)

        endif 


C       Report successful multiple tests
C       that may indicate tests can not resolve within epsilon
        if (iflag_set.gt.1 ) then
         write(logmess,'(a,i5)')
     *   "Multiple point and edge tests succeeded: ",iflag_set
         call writloga('default',0,logmess,0,ierr)
        endif

C     Assign higher return value for test 30 success plus other success
      if (iflag_save.gt.33 .and. iflag.gt.0) iflag = iflag_save

         write(logmess,'(a,i5,1pe20.12e2)')
     *   "FOUND INSIDE: Exit inside_tri2d iflag, len eps: ",
     *    iflag,xfac
         call writloga('default',0,logmess,1,ierr)
        endif


      return
      end

      subroutine inside_hex(x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *                      x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *                      xa,ya,za,iflag)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE DETERMINES IF A TEST POINT (xa,ya,za) IS INSIDE
C            OR OUTSIDE A HEX ELEMENT. THE DEFINITION OF THE HEX IS A LINEAR
C            POLYHEDRA FORMED BY SUBDIVIDING IT INTO 24 TETRAHEDRAL ELEMENTS.
C
C            THIS VERSION USES THE MEDIAN POINT OF THE FACES AND THE MEDIAN
C            POINT OF THE ELEMENT AS THE VIRTUAL POINTS TO FORM THE 24 TETS.
C
C
C     ******************************************************************
C
C     DEFINE THE STRUCTURE OF A GENERIC HEXRAHEDRON.
C
C 
C          8$ * * * * * * $7             Face 1: 1 2 3 4  (bottom)
C           * *           - *                 2: 5 8 7 6  (top)
C           *   *         -   *               3: 1 5 6 2  (front)
C           *     *       -     *             4: 2 6 7 3  (back)
C           *       *     -       *           5: 3 7 8 4  (right)
C           *        5$ * * * * * * $6        6: 1 4 8 5  (left)
C           *         *   -         *    
C          4$ - - - - - - $3        *    Edge 1: 1 2
C             *       *     -       *            2 3 
C               *     *       -     *            3 4
C                 *   *         -   *            4 1
C                   * *           - *            5 6
C                    1$ * * * * * * $2           6 7 
C                                                7 8 
C                                                8 5
C                                                1 5
C                                                2 6
C                                                3 7 
C                                                4 8
C
C     ******************************************************************
C
C      INPUT ARGUMENTS -
C
C        (x1,y1,z1),...,(x8,y8,z8) - THE COORDINATES OF THE HEX WHERE
C                                    1,2,3,4 FORM THE BOTTOM FACE AND 
C                                    5,6,7,8 FORM THE TOP FACE.
C        (xa,ya,za)                - THE POINT TO BE TESTED
C
C
C     OUTPUT ARGUMENTS -
C
C        iflag = -1  (or less) TEST POINT IS OUTSIDE
C              =  0  TEST POINT IS INSIDE
C              >  0  TEST POINT IS ON AN EXTERIOR BOUNDARY
C              =  1  TEST IS ON FACE 1
C              =  2  TEST IS ON FACE 2
C              =  3  TEST IS ON FACE 3
C              =  4  TEST IS ON FACE 4
C              =  5  TEST IS ON FACE 5
C              =  6  TEST IS ON FACE 6
C              =  7  TEST IS WITHIN EPSILON OF NODE 1
C              =  8  TEST IS WITHIN EPSILON OF NODE 2
C              =  9  TEST IS WITHIN EPSILON OF NODE 3
C              =  10 TEST IS WITHIN EPSILON OF NODE 4
C              =  11 TEST IS WITHIN EPSILON OF NODE 5
C              =  12 TEST IS WITHIN EPSILON OF NODE 6
C              =  13 TEST IS WITHIN EPSILON OF NODE 7
C              =  14 TEST IS WITHIN EPSILON OF NODE 8
C
C        NOTE: IF THE TEST POINT IS ON AN EXTERIOR EDGE iflag = 0
C
C        THIS IS WRITTEN SO THAT AS SOON AS A POINT IS 'WITHIN' OR 'ON'
C        THE CALCULATION IS FINISHED (goto 9999)
C
C#######################################################################
C
      implicit none
      integer iflag, i,idebug,ierr,itmp
      integer iflag_tet(24)
C
      real*8 ds1a, ds2a, ds3a, ds4a, ds5a, ds6a, ds7a, ds8a, dsmax
      real*8 x1,y1,z1,x2,y2,z2,x3,y3,z3,x4,y4,z4,
     *       x5,y5,z5,x6,y6,z6,x7,y7,z7,x8,y8,z8,
     *       xa,ya,za
      real*8 x1432, y1432, z1432, x5678, y5678, z5678,
     *       x2376, y2376, z2376, x1584, y1584, z1584,
     *       x1265, y1265, z1265, x4873, y4873, z4873,
     *       x12345678, y12345678, z12345678
C
      character*132 logmess
C
      real*8 xepsilon
      data xepsilon / 1.0d-10 /
C
C ######################################################################
C Begin inside_hex
C
      idebug=iflag
      iflag=0
      do i = 1, 24
         iflag_tet(i) = -2
      enddo

C     Start by figuring out if the test point is within epsilon
C     of one of the nodes of the hex element.
C
      ds1a=(x1-xa)**2+(y1-ya)**2+(z1-za)**2
      ds2a=(x2-xa)**2+(y2-ya)**2+(z2-za)**2
      ds3a=(x3-xa)**2+(y3-ya)**2+(z3-za)**2
      ds4a=(x4-xa)**2+(y4-ya)**2+(z4-za)**2
      ds5a=(x5-xa)**2+(y5-ya)**2+(z5-za)**2
      ds6a=(x6-xa)**2+(y6-ya)**2+(z6-za)**2
      ds7a=(x7-xa)**2+(y7-ya)**2+(z7-za)**2
      ds8a=(x8-xa)**2+(y8-ya)**2+(z8-za)**2
      dsmax=max(ds1a,ds2a,ds3a,ds4a,ds5a,ds6a,ds7a,ds8a)
      if(ds1a.le.xepsilon*dsmax) then
         iflag=7
         goto 9999
      elseif(ds2a.le.xepsilon*dsmax) then
         iflag=8
         goto 9999
      elseif(ds3a.le.xepsilon*dsmax) then
         iflag=9
         goto 9999
      elseif(ds4a.le.xepsilon*dsmax) then
         iflag=10
         goto 9999
      elseif(ds5a.le.xepsilon*dsmax) then
         iflag=11
         goto 9999
      elseif(ds6a.le.xepsilon*dsmax) then
         iflag=12
         goto 9999
      elseif(ds7a.le.xepsilon*dsmax) then
         iflag=13
         goto 9999
      elseif(ds8a.le.xepsilon*dsmax) then
         iflag=14
         goto 9999
      endif
C
C ######################################################################
C     Now make 24 virtual tets and test if the the point is inside
C     or on the face of one of the virtual tets.
C ######################################################################
C     FIND THE MEDIAN POINT FOR EACH OF THE SIX FACES OF THE HEX.
C
      x1432=0.25d+00*(x1+x4+x3+x2)
      y1432=0.25d+00*(y1+y4+y3+y2)
      z1432=0.25d+00*(z1+z4+z3+z2)
      x5678=0.25d+00*(x5+x6+x7+x8)
      y5678=0.25d+00*(y5+y6+y7+y8)
      z5678=0.25d+00*(z5+z6+z7+z8)
      x2376=0.25d+00*(x2+x3+x7+x6)
      y2376=0.25d+00*(y2+y3+y7+y6)
      z2376=0.25d+00*(z2+z3+z7+z6)
      x1584=0.25d+00*(x1+x5+x8+x4)
      y1584=0.25d+00*(y1+y5+y8+y4)
      z1584=0.25d+00*(z1+z5+z8+z4)
      x1265=0.25d+00*(x1+x2+x6+x5)
      y1265=0.25d+00*(y1+y2+y6+y5)
      z1265=0.25d+00*(z1+z2+z6+z5)
      x4873=0.25d+00*(x4+x8+x7+x3)
      y4873=0.25d+00*(y4+y8+y7+y3)
      z4873=0.25d+00*(z4+z8+z7+z3)
      x12345678=(x1+x2+x3+x4+x5+x6+x7+x8)/8.0d+00
      y12345678=(y1+y2+y3+y4+y5+y6+y7+y8)/8.0d+00
      z12345678=(z1+z2+z3+z4+z5+z6+z7+z8)/8.0d+00
C
C     ..................................................................
C
C     The way these are ordered, it is local face 1 & 2 of the virtual
C     tet that is the 'outside' of the hex. That is the face opposite
C     the virtual face point (local node number 1) and the virtual
C     interior point (local node number 2).
C
C     tam - pass in itmp value instead of iflag
C     since it is supposed to be set by iflag_tet results
C     set itmp to idebug to pass in debug flag
      iflag = -1
C
C     Face 1 (bottom)
C
      itmp=idebug
      call inside_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x1,y1,z1,xa,ya,za,itmp)
      iflag_tet(1) = itmp
      itmp=idebug
      call inside_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x4,y4,z4,xa,ya,za,itmp)
      iflag_tet(2) = itmp
      itmp=idebug
      call inside_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x3,y3,z3,xa,ya,za,itmp)
      iflag_tet(3) = itmp
      itmp=idebug
      call inside_tet(x1432,y1432,z1432,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x2,y2,z2,xa,ya,za,itmp)
      iflag_tet(4) = itmp
      do i = 1, 4
         if(iflag_tet(i) .eq. 2) then
         iflag = 1
         goto 9999
         endif
      enddo
      do i = 1, 4
         if(iflag_tet(i) .ge. 0) then
         iflag = 0
         goto 9999
         endif
      enddo
C
C     Face 2 (top)
C
      itmp=idebug
      call inside_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x5,y5,z5,xa,ya,za,itmp)
      iflag_tet(5) = itmp
      itmp=idebug
      call inside_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x6,y6,z6,xa,ya,za,itmp)
      iflag_tet(6) = itmp
      itmp=idebug
      call inside_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x7,y7,z7,xa,ya,za,itmp)
      iflag_tet(7) = itmp
      itmp=idebug
      call inside_tet(x5678,y5678,z5678,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x8,y8,z8,xa,ya,za,itmp)
      iflag_tet(8) = itmp
      do i = 5, 8
         if(iflag_tet(i) .eq. 2) then
         iflag = 2
         goto 9999
         endif
      enddo
      do i = 5, 8
         if(iflag_tet(i) .ge. 0) then
         iflag = 0
         goto 9999
         endif
      enddo
C
C     Face 4 (back)
C
      itmp=idebug
      call inside_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x2,y2,z2,xa,ya,za,itmp)
      iflag_tet(9) = itmp
      itmp=idebug
      call inside_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x3,y3,z3,xa,ya,za,itmp)
      iflag_tet(10) = itmp
      itmp=idebug
      call inside_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x7,y7,z7,xa,ya,za,itmp)
      iflag_tet(11) = itmp
      itmp=idebug
      call inside_tet(x2376,y2376,z2376,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x6,y6,z6,xa,ya,za,itmp)
      iflag_tet(12) = itmp
      do i = 9, 12
         if(iflag_tet(i) .eq. 2) then
         iflag = 4
         goto 9999
         endif
      enddo
      do i = 9, 12
         if(iflag_tet(i) .ge. 0) then
         iflag = 0
         goto 9999
         endif
      enddo
C
C     Face 6 (left)
C
      itmp=idebug
      call inside_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x1,y1,z1,xa,ya,za,itmp)
      iflag_tet(13) = itmp
      itmp=idebug
      call inside_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x5,y5,z5,xa,ya,za,itmp)
      iflag_tet(14) = itmp
      itmp=idebug
      call inside_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x8,y8,z8,xa,ya,za,itmp)
      iflag_tet(15) = itmp
      itmp=idebug
      call inside_tet(x1584,y1584,z1584,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x4,y4,z4,xa,ya,za,itmp)
      iflag_tet(16) = itmp
      do i = 13, 16
         if(iflag_tet(i) .eq. 2) then
         iflag = 6
         goto 9999
         endif
      enddo
      do i = 13, 16
         if(iflag_tet(i) .ge. 0) then
         iflag = 0
         goto 9999
         endif
      enddo
C
C     Face 3 (front)
C
      itmp=idebug
      call inside_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x2,y2,z2,
     *            x1,y1,z1,xa,ya,za,itmp)
      iflag_tet(17) = itmp
      itmp=idebug
      call inside_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x6,y6,z6,
     *            x2,y2,z2,xa,ya,za,itmp)
      iflag_tet(18) = itmp
      itmp=idebug
      call inside_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x5,y5,z5,
     *            x6,y6,z6,xa,ya,za,itmp)
      iflag_tet(19) = itmp
      itmp=idebug
      call inside_tet(x1265,y1265,z1265,
     *            x12345678,y12345678,z12345678,
     *            x1,y1,z1,
     *            x5,y5,z5,xa,ya,za,itmp)
      iflag_tet(20) = itmp
      do i = 17, 20
         if(iflag_tet(i) .eq. 2) then
         iflag = 3
         goto 9999
         endif
      enddo
      do i = 17, 20
         if(iflag_tet(i) .ge. 0) then
         iflag = 0
         goto 9999
         endif
      enddo
C
C     Face 5 (right)
C
      itmp=idebug
      call inside_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x8,y8,z8,
     *            x4,y4,z4,xa,ya,za,itmp)
      iflag_tet(21) = itmp
      itmp=idebug
      call inside_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x7,y7,z7,
     *            x8,y8,z8,xa,ya,za,itmp)
      iflag_tet(22) = itmp
      itmp=idebug
      call inside_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x3,y3,z3,
     *            x7,y7,z7,xa,ya,za,itmp)
      iflag_tet(23) = itmp
      itmp=idebug
      call inside_tet(x4873,y4873,z4873,
     *            x12345678,y12345678,z12345678,
     *            x4,y4,z4,
     *            x3,y3,z3,xa,ya,za,itmp)
      iflag_tet(24) = itmp
      do i = 21, 24
         if(iflag_tet(i) .eq. 2) then
         iflag = 5
         goto 9999
         endif
      enddo
      do i = 21, 24
         if(iflag_tet(i) .ge. 0) then
         iflag = 0
         goto 9999
         endif
      enddo

 9999 continue
C     Note here that iflag is first set to 0, then set to -1 before
C     the calls to inside_tet, iflag_tet is checked for iflag value
      if (idebug.gt.0) then
         write(logmess,'(a,i5,1pe20.12e2)')
     *    "Exit inside_hex iflag, len eps: ",iflag,xepsilon*dsmax
         call writloga('default',0,logmess,0,ierr)
      endif
      return
      end
