      subroutine set_epsilon()
C
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE SETS THE SEARCH RANGE TO BE USED IN DETERMINING
C        POINT TYPES, MATERIAL TYPES, AND INTERFACE LOCATIONS.
C
C     INPUT ARGUMENTS -
C
C        NONE
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C     CHANGE HISTORY -
C
C$Log:   /pvcs.config/t3d/src/set_epsilon_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.2   03 May 2001 15:22:42   kuprat
CPVCS    Removed obsolete IF clause.
CPVCS    
CPVCS       Rev 1.1   25 Jan 2000 13:35:12   dcg
CPVCS    
CPVCS       Rev 1.1   Tue Oct 05 14:24:02 1999   kuprat
CPVCS    We allow EPSILONL to be arbitrarily small, given 
CPVCS    arbitrarily small bounding boxes.
CPVCS    
CPVCS       Rev 1.0   Tue Sep 29 15:04:38 1998   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.16   Fri Jan 09 17:13:56 1998   dcg
CPVCS    use problem size and epsilonr to set epsilonl if
CPVCS    setsize has been called otherwise use
CPVCS    problem size and fixed factor of 10**-8
CPVCS
CPVCS       Rev 1.15   Fri Jan 09 16:42:06 1998   dcg
CPVCS    change set_epsilon to use problem size as
CPVCS    returned by getsize
CPVCS
CPVCS       Rev 1.14   Mon Apr 14 17:03:20 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.13   08/29/95 12:11:16   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.12   08/24/95 08:39:22   dcg
CPVCS    separated get_info and set_info
CPVCS
CPVCS       Rev 1.11   08/23/95 15:50:48   dcg
CPVCS    cmo storage block changes
CPVCS
CPVCS       Rev 1.10   08/02/95 10:09:38   ejl
CPVCS    Installed IEEE Floating Point exception handler
CPVCS
CPVCS       Rev 1.9   07/17/95 16:10:04   dcg
CPVCS
CPVCS
CPVCS       Rev 1.8   07/14/95 10:16:48   het
CPVCS    Correct errors with point types
CPVCS
CPVCS       Rev 1.7   06/19/95 16:39:44   dcg
CPVCS    IBM platform changes
CPVCS
CPVCS       Rev 1.6   06/16/95 16:49:12   dcg
CPVCS    add calls to hsb2getx for real and integer data replacing call to hsb2geta
CPVCS
CPVCS       Rev 1.5   06/13/95 09:21:54   ejl
CPVCS    added get_info.f
CPVCS
CPVCS       Rev 1.4   06/13/95 09:03:44   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
CPVCS
CPVCS       Rev 1.3   05/31/95 10:42:30   ejl
CPVCS    Added function ICHARLN.F
CPVCS
CPVCS       Rev 1.2   05/23/95 06:50:34   het
CPVCS    Change dictionary so that they are CMO specific
CPVCS
CPVCS       Rev 1.1   05/05/95 14:36:06   ejl
CPVCS    Epsilon Routines
C
C#######################################################################
C
      implicit none
      include 'consts.h'
C
C#######################################################################
C
      integer npoints
C
      pointer (ipxic, xic)
      pointer (ipyic, yic)
      pointer (ipzic, zic)
      REAL*8 xic(1000000), yic(1000000), zic(1000000)
C
C#######################################################################
C
      integer ilen, itype, ierr, idata 
      REAL*8 xmin1, xmax1
      REAL*8 ymin1, ymax1
      REAL*8 zmin1, zmax1
C
      REAL*8 epsilonl,epa,epv
C
      integer icscode
C
      character*32 isubname,cmo, cdata
C
C#######################################################################
C
C
      isubname='set_epsilon'
C
C     ******************************************************************
C     Get mesh object name
C
      call cmo_get_name(cmo,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_name')
C
      call cmo_get_info('nnodes',cmo,npoints,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call cmo_get_info('xic',cmo,ipxic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('yic',cmo,ipyic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
      call cmo_get_info('zic',cmo,ipzic,ilen,itype,ierr)
      if(ierr.ne.0) call x3d_error(isubname,'cmo_get_info')
C
      call getsize(xmin1,xmax1,ymin1,ymax1,zmin1,zmax1,epa,epv)
      epsilonl=sqrt(((xmax1-xmin1)**2+(ymax1-ymin1)**2+
     *   (zmax1-zmin1)**2))*epsilonr*1000

C     PUT THE VALUE OF EPSILON LENGTH IN THE MESH OBJECT
C
      call cmo_set_attinfo('epsilonl',cmo,idata,epsilonl,cdata,2,icscode
     &   )
      if(icscode.ne.0) call x3d_error(isubname,'cmo_set_info')
C
      return
      end
