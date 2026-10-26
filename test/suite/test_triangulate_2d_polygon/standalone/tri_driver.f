      program tri_driver
C     Sandbox driver: does what triangulate_lg does around the call to
C     maketriangles_lg (drop a repeated last vertex, try the requested
C     orientation, on failure retry the other one), without LaGriT.
C     usage: tri_driver polygon.txt ccw|cw  > cells.txt
C     polygon.txt: n, then n lines "x y".   cells.txt: status lines,
C     then one cell per line (three 1-based vertex numbers).
      implicit none
      integer n,nlast,nelts,ierror,i,iarg
      real*8, allocatable :: x(:),y(:)
      integer, allocatable :: ielts(:,:)
      logical isccw
      character*256 fname,orient
      call get_command_argument(1,fname)
      call get_command_argument(2,orient)
      isccw = orient(1:3).eq.'ccw'
      open(10,file=fname,status='old')
      read(10,*) n
      allocate(x(n),y(n),ielts(3,max(n,3)))
      do i=1,n
        read(10,*) x(i),y(i)
      enddo
      close(10)
      nlast=n
      if(x(1).eq.x(nlast).and.y(1).eq.y(nlast)) then
        nlast=nlast-1
        print '(a)','dropped_last 1'
      endif
      nelts=0
      call maketriangles_lg(1,nlast,x,y,nelts,ielts,isccw,ierror)
      print '(a,i3,a,i10)','attempt1 ierror ',ierror,' cells ',nelts
      if(ierror.ne.0) then
        nelts=0
        ierror=0
        isccw=.not.isccw
        call maketriangles_lg(1,nlast,x,y,nelts,ielts,isccw,ierror)
        print '(a,i3,a,i10)','attempt2 ierror ',ierror,' cells ',nelts
      endif
      if(ierror.ne.0) then
        print '(a)','result FAIL'
        stop
      endif
      print '(a,i10)','result OK cells ',nelts
      do i=1,nelts
        print '(3i10)',ielts(1,i),ielts(2,i),ielts(3,i)
      enddo
      end
