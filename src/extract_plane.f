*dk,extract_plane
      subroutine extract_plane(cmoout,cmoin,npoints,ntets,a,b,c,d,ierr1)
C
C #####################################################################
C
C     PURPOSE -
C
C        Extracts a plane defined by the parameters a,b,c, and d.
C        The plane has an outward unit normal (a,b,c).
C
C
C     INPUT ARGUMENTS -
C
C        cmoout    - THE MESH OBJECT TO HOLD THE RESULT
C        cmoin     - THE INCOMING MESH OBJECT
C        npoints   - NUMBER OF POINTS
C        ntets     - NUMBER OF TETS
C        a,b,c,d   - COEFFICIENTS IN THE PLANE EQUATION
C                      ax + by + cz + d = 0
C
C     OUTPUT ARGUMENTS -
C
C        ierr1   - ERROR RETURNED (ZERO IF NO ERRORS)
C
C     CHANGE HISTORY -
C
C        $Log: extract_plane.f,v $
C        Revision 2.00  2007/11/05 19:45:54  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
      implicit real*8 (a-h, o-z)
C
C   Definitions for incoming (existing) cmo
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      dimension xic(1000000), yic(1000000), zic(1000000)
C
      pointer (ipdist, dist)
      dimension dist(1000000)
C
      character*132 logmess
      character*(*) cmoin, cmoout
      character*32 isubname
C
      isubname = 'extract_plane'
      ierr1 = 0
C
C   Get the existing cmo
C
C
      call mmgetblk('dist',isubname,ipdist,npoints,2,icscode)
C
      call cmo_get_info('xic',cmoin,ipxic,lenxic,icmotype,ierror)
      call cmo_get_info('yic',cmoin,ipyic,lenyic,icmotype,ierror)
      call cmo_get_info('zic',cmoin,ipzic,lenzic,icmotype,ierror)
C
C   dist is the signed distance of each node from the plane
C
      epsilone = 1.0e-10
      do i=1,npoints
         dist(i) = a*xic(i) + b*yic(i) + c*zic(i) + d
         if (abs(dist(i)) .lt. epsilone) dist(i) = 0.0
      enddo
C
C   Do an isosurface on dist = 0.0.
C
      value = 0.0
      call isosurface(ipdist,value,npoints,ntets,cmoin,cmoout,ierr1)
C
      call mmrelprt(isubname,icscode)
C
      return
      end
