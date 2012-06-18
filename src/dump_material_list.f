      subroutine dump_material_list(ifile,iselect)
C
*****************************************************************
*
*	FUNCTION:
*		Output lists of nodes that have same material (imt1) value
*
*****************************************************************
C
C #####################################################################
C
C     PURPOSE -
C
C        Output lists of nodes that have same material (imt1) value.
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
C        $Log: dump_material_list.f,v $
C        Revision 2.00  2007/11/05 19:45:52  spchu
C        Import to CVS
C   
CPVCS    
CPVCS       Rev 1.8   07 Dec 2005 07:19:56   gable
CPVCS    Modified format of screen output in case of no nodes found in a
CPVCS    particular material number. Removed extra blank lines above and
CPVCS    below screen output.
CPVCS    
CPVCS       Rev 1.7   23 Feb 2005 08:24:06   tam
CPVCS    added argument iselect to subroutine call that allows the
CPVCS    user to choose a single selected zone value and corrected
CPVCS    code to finish writing zone file if no materials are found
CPVCS    
CPVCS       Rev 1.6   04 Jun 2003 10:19:54   gable
CPVCS    Fixed format statement in screen output.
CPVCS    
CPVCS       Rev 1.5   Tue Aug 24 09:26:16 1999   gable
CPVCS     Changed format of output from * to 10(i10,1x). This
CPVCS     should speed up the IO.
CPVCS    
CPVCS       Rev 1.4   Mon Aug 02 11:15:06 1999   gable
CPVCS    Modified how min max materials are counted.
CPVCS    Now the code will only clamp negative material (imt) values
CPVCS    and will only loop throught nodes looking for imt's that
CPVCS    exist. Old version would have been very slow if imt values
CPVCS    were not in a contiguous block of values. max imt value
CPVCS    is not limited to 100 as was the case in the old version
CPVCS    of the code.
CPVCS    
CPVCS       Rev 1.3   Tue Dec 01 14:02:52 1998   llt
CPVCS    added #nodes/nnodes line to output
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:43:44 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Fri Oct 11 13:39:44 1996   gable
CPVCS    Add zone header and put a blank line and stop at the end of the output file.
CPVCS    
CPVCS       Rev 1.0   Wed May 08 12:37:22 1996   gable
CPVCS    Initial revision.
C
C ######################################################################
C
	IMPLICIT NONE
C
      character*(*) ifile
      integer iselect
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
     > , material_total
c
      character log_io*132
      character logmess*132
      character ifilename*132
c
c     cmo variables
c
      character*32 cmo_name
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
      pointer (ipimt1, imt1)
      integer  imt1(10000000)
      pointer (ipiwork, iwork)
      integer iwork(10000000)
      pointer (ipjwork, jwork)
      integer jwork(10000000)
      pointer (ipicr1, icr1)
      integer  icr1(10000000)

c 
      character*32 isubname
c
      integer if_debug
      data if_debug / 0 /
c
*--------------------------------------------------------
c
      isubname='dump_material_lists'
c
      write(log_io,100)
  100 format('*********dump_material_lists********')
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
 
      if(ierror .ne. 0)write(6,*)ierror,' ierror value icr1'
c
c     set up work vector nnodes long
c
      call mmgetblk("iwork",isubname,ipiwork,nnodes,1,ierror)
      call mmgetblk("jwork",isubname,ipjwork,nnodes,1,ierror)
      do n = 1, nnodes
        iwork(n) = 0
        jwork(n) = 0
      enddo
c
c     open file to output zone lists
c
      ifilename=ifile(1:icharlnf(ifile)) // '_material.zone'
      iunit=-1
      call hassign(iunit,ifilename,ierror)
      if (iunit.lt.0 .or. ierror.lt.0) then
        call x3d_error(isubname,'hassign bad file unit')
        goto 9999
      endif

c
C
c    write the 'zone' header at the top of the file
c
      logmess = 'zone'
      write(iunit, 9072)logmess

      min_material =  1e9
      max_material = -1e9
      do n = 1, nnodes
        if(imt1(n) .lt. min_material)min_material = imt1(n)
        if(imt1(n) .gt. max_material)max_material = imt1(n)
        if(imt1(n) .gt. 0)then
           jwork(imt1(n))=jwork(imt1(n)) + 1
        endif
      enddo
      
      if(min_material .lt. 0)then
      write(log_io,800)min_material
  800 format('******Warning: Minimum material ID value = ',i6)
      call writloga('default',0,log_io,0,ierror)
      call writloga('default',0,log_io,0,ierror)
      call writloga('default',0,log_io,0,ierror)
      call writloga('default',0,log_io,0,ierror)
      call writloga('default',0,log_io,0,ierror)
      endif
c
c     clamp the possible range to positive material numbers
c
      if(min_material .lt.  0  )min_material =  0
c
c     The total possible material zones is then...
c
      material_total = max_material - min_material + 1
c
      write(log_io,900)min_material
  900 format('Minimum material ID value = ',i6)
      call writloga('default',0,log_io,0,ierror)
      write(log_io,910)max_material
  910 format('Maximum material ID value = ',i6)
      call writloga('default',0,log_io,0,ierror)
      write(log_io,920)material_total
  920 format('Total possible materials  = ',i6)
      call writloga('default',0,log_io,0,ierror)

C     User may select just one material
      if (iselect.gt.0) then
        write(log_io,925)iselect
  925   format('One selected material  = ',i6)
        call writloga('default',0,log_io,0,ierror)
        min_material=iselect
        max_material=iselect
      endif

      itotal = 0
      do imaterial = min_material, max_material
         icount = 0
         if(jwork(imaterial) .ne. 0)then
         do n = 1, nnodes
            if(imt1(n) .eq. imaterial)then
               icount = icount + 1
               iwork(icount) = n
            endif
         enddo
         endif
         if(icount .ne. 0)then
            itotal = itotal + icount
            write(log_io,930) imaterial, icount, 
     >                        real(icount)/real(nnodes)
  930       format('Material   ',i9, ' has ',i9, ' nodes.',
     >              ' #nodes/nnodes is ', g20.12)
            call writloga('default',0,log_io,0,ierror)
            write(iunit,65)
     >              imaterial
C*****     >  (1:icharlnf(material_name(imaterial)))
  65        format(i5.5,2x,a)
            write(iunit,70)'nnum'
  70        format(a4)
            write(iunit,*)icount
            write(iunit,940)(iwork(n),n=1,icount)
 940        format(10(i10,1x))
         endif

C        finish writing to file even if no materials found
         if (icount.eq.0) then
            write(iunit,65)imaterial
            write(iunit,70)'nnum'
            write(iunit,*)icount
            write(log_io,950)imaterial
 950        format('No nodes found in material  = ',i6)
            call writloga('default',0,log_io,0,ierror)
         endif
      enddo
c
      write(iunit,9074)
      logmess = 'stop'
      write(iunit,9075)logmess
 9072 format(a4)
 9074 format(' ')
 9075 format(a4)
 
      close(iunit)

 9999 continue
c
c     release memory of work array
c
      call mmrelprt(isubname,ierror)
c
      return
      end
