      subroutine connect_check_imt(imt1,itp1,npoints,nmregs,ierror)
C
C #####################################################################
C
C     PURPOSE -
C
C        Check the vertex material numbers (imt) before connect
C        (2D and 3D) uses them, and refuse values that would crash.
C
C        connect keeps one bit per material for every vertex:
C           matindex = ((imt-1)/32)*lenmatmx
C        and an array of npoints * (largest imt) integers. So
C        - an imt below 0 writes before the start of matlst
C          (heap corruption, then a segmentation fault), and
C        - a very large imt (uninitialized or garbage values)
C          asks for more memory than exists; the allocation fails
C          and the run ends in a segmentation fault.
C        Both are reported here, before connect changes anything,
C        so the mesh object is left as it was. imt = 0 is allowed
C        (connect reports those vertices as having no material).
C
C        See LaGriT issue #240.
C
C     INPUT ARGUMENTS -
C
C        imt1    - vertex material numbers
C        itp1    - vertex types; dudded vertices (ifitpdud) are not
C                  connected, so their imt is not checked for < 0
C        npoints - number of vertices
C        nmregs  - number of material regions in the geometry
C
C     OUTPUT ARGUMENTS -
C
C        ierror  - 0 if every imt is usable, 1 otherwise
C
C #####################################################################
C
      implicit none
      include 'chydro.h'
C
      integer npoints,nmregs,ierror
      integer imt1(npoints),itp1(npoints)
C
      integer i,nneg,ifirst,imtmax,ierrw
      real*8  xneed
      character*132 logmess
C
C     Largest material array connect will try to make, in integers
C     (npoints * largest imt): 2**31 integers, 16 GB.
C
      real*8  XNEED_MAX
      parameter (XNEED_MAX = 2147483648.0d0)
C
      ierror = 0
      nneg = 0
      ifirst = 0
      imtmax = 1
      do i = 1, npoints
C        connect sizes its arrays by the largest imt of ALL vertices.
         imtmax = max(imtmax,imt1(i))
         if(imt1(i).lt.0 .and. itp1(i).ne.ifitpdud)then
            nneg = nneg + 1
            if(ifirst .eq. 0) ifirst = i
         endif
      enddo
C
      if(nneg .gt. 0)then
         ierror = 1
         write(logmess,'(a,i12,a)')
     &     'ERROR: connect: ',nneg,' vertices have imt < 0'
         call writloga('default',1,logmess,0,ierrw)
         write(logmess,'(a,i12,a,i12)')
     &     '  the first is vertex ',ifirst,' with imt ',imt1(ifirst)
         call writloga('default',0,logmess,0,ierrw)
      endif
C
      xneed = dble(npoints)*dble(max(imtmax,nmregs))
      if(xneed .gt. XNEED_MAX)then
         ierror = 1
         write(logmess,'(a,i20)')
     &     'ERROR: connect: largest imt is ',imtmax
         call writloga('default',1,logmess,0,ierrw)
         write(logmess,'(a,1pe10.3,a)')
     &     '  npoints * largest imt = ',xneed,
     &     ' is more than connect can hold'
         call writloga('default',0,logmess,0,ierrw)
      endif
C
      if(ierror .ne. 0)then
         write(logmess,'(a)')
     &     '  imt must be a material number, 0 or greater; set it'
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a)')
     &     '  first, for example  cmo / setatt / mo / imt / 1 0 0 / 1'
         call writloga('default',0,logmess,0,ierrw)
         write(logmess,'(a)')
     &     '  connect not done; the mesh object is unchanged'
         call writloga('default',0,logmess,1,ierrw)
      endif
C
      return
      end
