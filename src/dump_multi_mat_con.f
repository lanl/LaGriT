      subroutine dump_multi_mat_con(ifile)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                         Copyright, 1996                              C
C This program was prepared by the Regents of the University of        C
C California at Los Alamos National Laboratory (the University) under  C
C Contract No. W-7405-ENG-36 with the U.S. Department of Energy (DOE). C
C The University has certain rights in the program pursuant to the     C
C contract and the program should not be copied or distributed outside C
C your organization. All rights in the program are reserved by the DOE C
C and the University. Neither the U.S. Government nor the University   C
C makes any warranty, express or implied, or assumes and liability or  C
C responsibility for the use of this software.                         C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C #####################################################################
C
C     PURPOSE -
C
C        Output lists of nodes for each material that are multi-material 
C        connections.
C        Each list consists of a header followed by list entries.
C        The header consists of the material number followed by
C        "multi-material connections" on the same line, followed by
C        nnum on the next line, followed by the number of entries in
C        the list. (The notation nnum is a holdback from the earlier version
C        of this command, and really should be nedges.)
C        The list entries consist of two nodes. The first node is the one
C        inside the material in question, thee second is the node in the other 
C        material. The lists are sorted by the first node, but are not 
C        sorted by the second. Each list entry is on its own separate line, 
C        and the nodes in each entry are separated by a comma.
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
C        $Lognfig/t3d/src/dump_multi_mat_con.f_a $ 
CPVCS    
CPVCS       Rev 1.0   Wed Jul 02 09:19:16 1997   gable
CPVCS    Initial revision.
C
C ######################################################################
C
	IMPLICIT NONE
C
      character*(*) ifile
C
      include "chydro.h"
      include 'local_element.h'
      integer icharlnf
c
      integer
     >   ierror
     > , n
     > , i
     > , i1
     > , i2
     > , i12mat
     > , i21mat
     > , it
     > , ie
     > , icount
     > , imaterial
C     > , itotal
     > , iunit
      integer
     >   min_material
     > , max_material
C     Sort Routine (Binary insertion Sort...)
      integer
     >   minposition
     > , maxposition
     > , currposition
     > , maxedges
      logical gotit
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
     >   nelements
     > , nnodes
     > , ilennnodes
     > , itypnnodes
      integer
     >   ilenicr1
     > , itypicr1
      integer
     >   ilen
     > , ityp
      integer
     >   ilenitp1
     > , itypitp1
      pointer (ipitp1, itp1)
      integer  itp1(10000000)
      pointer (ipimt1, imt1)
      integer  imt1(10000000)
      pointer (ipiwork, iwork)
      integer iwork(10000000)
C      pointer (ipindex, index)
C      integer index(10000000)
      pointer (ipicr1, icr1)
      integer  icr1(10000000)
      pointer (ipitet, itet1)
      integer itet1(1000000)
      pointer (ipitettyp, itettyp)
      integer itettyp(1000000)
      pointer (ipitetoff, itetoff)
      integer itetoff(1000000)
C
      character*32 isubname
C
      integer if_debug
      data if_debug / 0 /
c
*--------------------------------------------------------
c
      isubname='dump_mult_mat_con'
c
      write(log_io,100)
  100 format('*********dump_multi_material_list********')
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
 
      call cmo_get_info
     >  ('nelements',cmo_name,nelements,ilen,ityp,ierror)
      call cmo_get_info
     >  ('itet',cmo_name,ipitet,ilen,ityp,ierror)
      call cmo_get_info
     >  ('itettyp',cmo_name,ipitettyp,ilen,ityp,ierror)
      call cmo_get_info
     >  ('itetoff',cmo_name,ipitetoff,ilen,ityp,ierror)
 
      if(ierror .ne. 0)write(6,*)ierror,' ierror value icr1'
c
c     set up work vector nnodes long
c
      call mmgetblk("iwork",isubname,ipiwork,nelements*12*2,1,ierror)
c
      ifilename=ifile(1:icharlnf(ifile)) // '_multi_mat.zone'
      iunit=-1
      call hassign(iunit,ifilename,ierror)
c
      min_material =  1e9
      max_material = -1e9
      do n = 1, nnodes
        if(imt1(n) .lt. min_material)min_material = imt1(n)
        if(imt1(n) .gt. max_material)max_material = imt1(n)
      enddo
c
      do imaterial = min_material, max_material
         do i = 1, nelements*12
            iwork(i) = 0
            iwork(i+nelements*12) = 0 
         enddo
         maxedges = 0
         currposition = 1
         do it = 1, nelements
            do ie = 1, nelmnee(itettyp(it))
               minposition = 1
               maxposition = maxedges
               i1 = itet1(itetoff(it)+ielmedge1(1,ie,itettyp(it)))
               i2 = itet1(itetoff(it)+ielmedge1(2,ie,itettyp(it)))
               i12mat = 0
               if((imt1(i1) .eq. imaterial) .and.
     >           (imt1(i2) .ne. imaterial)) then
                  i12mat = i1
                  i21mat = i2
               elseif((imt1(i1) .ne. imaterial) .and.
     >           (imt1(i2) .eq. imaterial)) then
                  i12mat = i2
                  i21mat = i1
               endif
               if(i12mat .ne. 0) then
                  if(maxedges .eq. 0) then
                     iwork(1) = i12mat
                     iwork(1+nelements*12) = i21mat
                     maxedges = 1
                  else
                     do while ((maxposition-minposition).gt.1)
                        currposition = (maxposition+minposition)/2
                        if(i12mat.gt.iwork(currposition)) then
                           minposition = currposition
                        elseif(i12mat.lt.iwork(currposition)) then
                           maxposition = currposition
                        else
                           maxposition = currposition
                           minposition = currposition
                        endif
                     enddo
                     if((iwork(maxposition) .gt. i12mat) .AND.
     $                    iwork(minposition) .lt. i12mat) then
C                        * Shift up from maxposition (i.e., insert
C                        * before)
                        do i = maxposition,maxedges
                           iwork(maxedges+maxposition-i+1) =
     $                     iwork(maxedges+maxposition-i)
                           iwork(maxedges+maxposition-i+1+
     $                           nelements*12) =
     $                     iwork(maxedges+maxposition-i+
     $                           nelements*12)
                        enddo
                        iwork(maxposition) = i12mat
                        iwork(maxposition+nelements*12) = i21mat
                        maxedges = maxedges+1
                     elseif(iwork(maxposition) .lt. i12mat) then
C                        * Shift up from minposition+1 (i.e., insert
C                        * after)
                        do i = maxposition+1,maxedges
                           iwork(maxedges+maxposition+1-i+1) =
     $                     iwork(maxedges+maxposition+1-i)
                           iwork(maxedges+maxposition+1-i+1+
     $                           nelements*12) =
     $                     iwork(maxedges+maxposition+1-i+
     $                           nelements*12)
                        enddo
                        iwork(maxposition+1) = i12mat
                        iwork(maxposition+1+nelements*12) = i21mat
                        maxedges = maxedges+1
                     elseif(iwork(minposition) .gt. i12mat) then
C                        * Shift up from minposition (i.e., insert
C                        * before)
                        do i = minposition,maxedges
                           iwork(maxedges+minposition-i+1) =
     $                     iwork(maxedges+minposition-i)
                           iwork(maxedges+minposition-i+1+
     $                           nelements*12) =
     $                     iwork(maxedges+minposition-i+
     $                           nelements*12)
                        enddo
                        iwork(minposition) = i12mat
                        iwork(minposition+nelements*12) = i21mat
                        maxedges = maxedges+1
                     else 
C                       * we are even on one front... work harder.
C                       * Find the range in the array corresponding to
C                       * the node in the material.
C                       * min & max
                        do while ((iwork(minposition).ge.i12mat) .AND.
     $                       (minposition.ge.1))
                           minposition = minposition-1
                        enddo
                        minposition = minposition + 1
                        do while ((iwork(maxposition).le.i12mat) .AND.
     $                       (maxposition.le.maxedges))
                           maxposition = maxposition+1
                        enddo
                        maxposition = maxposition-1
                        gotit = .FALSE.
                        do i = minposition,maxposition
                           if(iwork(i+nelements*12) .eq. i21mat) then
                              gotit= .TRUE.
                           endif
                        enddo
                        if(.NOT.gotit) then
C                       * Shift & insert before maxposition
                           do i = maxposition,maxedges
                              iwork(maxedges+maxposition-i+1) =
     $                        iwork(maxedges+maxposition-i)
                              iwork(maxedges+maxposition-i+1+
     $                             nelements*12) =
     $                        iwork(maxedges+maxposition-i+
     $                             nelements*12)
                           enddo
                           iwork(maxposition) = i12mat
                           iwork(maxposition+nelements*12) = i21mat
                           maxedges=maxedges+1
                        endif
                     endif
                  endif
               endif
            enddo
         enddo
         icount = maxedges
C         do i = 1, nnodes
C            if(iwork(i) .ne. 0)then
C               icount = icount + 1
C               index(icount) = i
C            endif
C         enddo
         if(icount .ne. 0)then
C            itotal = itotal + icount
            write(log_io,900) imaterial, icount
  900       format
     >      ('Material ',i9, ' has ',i9, ' multi-material connections.')
            call writloga('default',0,log_io,0,ierror)
            write(iunit,265)
     >              imaterial,
     >              ' multi-material connections'
 265        format(i5.5,2x,a)
            write(iunit,270)'nnum'
 270        format(a4)
            write(iunit,*)icount
            do i = 1,icount
               write(iunit,275) iwork(i), ',', iwork(i+nelements*12)
 275           format(I7,A,I7)
            enddo
         endif
      enddo
c
      close(iunit)
c
c     release memory of work array
c
      call mmrelprt(isubname,ierror)
c
      return
      end
