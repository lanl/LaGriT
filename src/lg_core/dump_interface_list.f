      subroutine dump_interface_list(ifile)
C
C #####################################################################
C
C     PURPOSE -
C
C        Output lists of nodes for each material interface.
C
C     INPUT ARGUMENTS - 
C
C        Character string for naming output file
C
C     OUTPUT ARGUMENTS -
C
C        None
C
C     CHANGE HISTORY -
C
C        $Log: dump_interface_list.f,v $
C        Revision 2.00  2007/11/05 19:45:52  spchu
C        Import to CVS
C   
CPVCS    
CPVCS       Rev 1.2   Wed Jul 02 09:16:58 1997   gable
CPVCS    Added calculation or min/max material numbers. This
CPVCS    had been forgotten.
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:43:42 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Wed May 08 12:36:38 1996   gable
CPVCS    Initial revision.
C
C ######################################################################
C
	IMPLICIT NONE
C
      character*(*) ifile
C
      include "chydro.h"
      integer icharlnf
c
      integer
     >   ierror
     > , n
     > , icount
     > , imaterial
     > , itotal
     > , iunit
      integer
     >   min_material
     > , max_material
c
      character log_io*132
c
c     cmo variables
c
      character*32 cmo_name, ifilename
      integer
     >   ilenimt1
     > , itypimt1
      integer
     >   nnodes
     > , ilennnodes
     > , itypnnodes
      integer
     >   ilenicr1
     > , itypicr1
      integer
     >   ilenitp1
     > , itypitp1
      pointer (ipitp1, itp1)
      integer  itp1(10000000)
      pointer (ipimt1, imt1)
      integer  imt1(10000000)
      pointer (ipiwork, iwork)
      integer iwork(10000000)
      pointer (ipicr1, icr1)
      integer  icr1(10000000)
C
      character*32 isubname
C
      integer if_debug
      data if_debug / 0 /
c
*--------------------------------------------------------
c
      isubname='dump_interface_list'
c
      write(log_io,100)
  100 format('*********dump_interface_list********')
      call writloga('default',0,log_io,0,ierror)
c
      call cmo_get_name
     >  (cmo_name,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value cmo_name'
 
      call cmo_get_info
     >  ('nnodes',cmo_name,nnodes,ilennnodes,itypnnodes,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value imt1'
 
      call cmo_get_info
     >  ('imt1',cmo_name,ipimt1,ilenimt1,itypimt1,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value imt1'
 
      call cmo_get_info
     >  ('icr1',cmo_name,ipicr1,ilenicr1,itypicr1,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value itetclr'
 
      call cmo_get_info
     >  ('itp1',cmo_name,ipitp1,ilenitp1,itypitp1,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value itp1'
 
 
      if(ierror .ne. 0)write(6,*)ierror,' ierror value icr1'
c
c     set up work vector nnodes long
c
      call mmgetblk("iwork",isubname,ipiwork,nnodes,2,ierror)
c

c
c     Find and output interface points using flags in itp1 array.
c     itp1 =  0 for interior points
c     itp1 =  2 for interior interface points
c     itp1 = 10 for outside (NOT interface) points
c     itp1 = 12 for outside interface points
c
      ifilename=ifile(1:icharlnf(ifile)) // '_interface.zone'
      iunit=-1
      call hassign(iunit,ifilename,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        goto 9999
      endif

c
      min_material =  1e9
      max_material = -1e9
      do n = 1, nnodes
        if(imt1(n) .lt. min_material)min_material = imt1(n)
        if(imt1(n) .gt. max_material)max_material = imt1(n)
      enddo
c
      do imaterial = min_material, max_material
         icount = 0
         do n = 1, nnodes
            if((imt1(n) .eq. imaterial) .and.
     >        (itp1(n) .eq. 2) .or.(itp1(n) .eq. 12))then
               icount = icount + 1
               iwork(icount) = n
            endif
         enddo
         if(icount .ne. 0)then
            itotal = itotal + icount
            write(log_io,900) imaterial, icount
  900       format('Interface material ',i9, ' has ',i9, ' nodes.')
            call writloga('default',0,log_io,0,ierror)
            write(iunit,265)
     >              imaterial,
     >              ' material interface points'
c cwg     >              max_material-imaterial+1,
c cwg     >              ' interface points'
c cwg      >              imaterial
C*****     >              material_name(imaterial)
C*****     >  (1:icharlnf(material_name(imaterial))), ' interface'
 265        format(i5.5,2x,a)
            write(iunit,270)'nnum'
 270        format(a4)
            write(iunit,*)icount
            write(iunit,*)(iwork(n),n=1,icount)
         endif
      enddo
c
      close(iunit)
c
c     release memory of work array
c
 9999 continue
      call mmrelprt(isubname,ierror)
c
      return
      end
