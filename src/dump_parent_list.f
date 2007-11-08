      subroutine dump_parent_list(ifile)
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
*	 Output lists of parent child node relationship
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
C        $Log:   /pvcs.config/t3d/src/dump_parent_list.f_a  $
CPVCS    
CPVCS       Rev 1.4   29 Jun 2001 07:03:54   gable
CPVCS    Changes to mdnode file header and default to method 2 for
CPVCS    split control volumes. Changes by Rao Garimella.
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:43:48 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   Tue May 14 14:47:30 1996   dcg
CPVCS    fix type iwork(4) to iwork4(i)
CPVCS
CPVCS       Rev 1.1   Tue May 14 08:06:00 1996   gable
CPVCS    Modified header of output file. Include the maximum
CPVCS    number of children as part of the header.
CPVCS
CPVCS       Rev 1.0   Wed May 08 12:38:22 1996   gable
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
     > , i
     > , icount
     > , iunit
     > , max_child
c
      character log_io*132
c
c     cmo variables
c
      character*32 cmo_name, ifilename
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
      integer
     >   ilenisn1
     > , itypisn1
      integer
     >   ntets
     > , ilenntets
     > , itypntets
      integer
     >   nen
     > , ilennen
     > , itypnen
      integer
     >   ilenitet
     > , itypitet
      integer
     >   ilenitetp1
      pointer (ipitp1, itp1)
      integer  itp1(10000000)
      pointer (ipicr1, icr1)
      integer  icr1(10000000)
      pointer (ipisn1, isn1)
      integer  isn1(10000000)
      pointer (ipiparent, iparent)
      integer  iparent(10000000)
      pointer (ipitetp1, itetp1)
      integer  itetp1(4*1000000)
      pointer (ipitet, itet)
      integer  itet(4,10000000)
      pointer (ipitet, itet1)
      integer  itet1(4*10000000)
c
      pointer (ipiwork1, iwork1)
      integer  iwork1(10000000)
      pointer (ipiwork2, iwork2)
      integer  iwork2(10000000)
      pointer (ipiwork3, iwork3)
      integer  iwork3(10000000)
      pointer (ipiwork4, iwork4)
      integer  iwork4(10000000)
c
      integer
     >    ifparent
C
      character*32 isubname
C
      integer icscode
      integer i_elim, meth
      real*8  mult
C
      integer if_debug
      data if_debug / 0 /
c
*--------------------------------------------------------
c
      isubname='dump_parent_list'
c
      write(log_io,100)
  100 format('*********dump_parent_list********')
      call writloga('default',0,log_io,0,ierror)
c
      call cmo_get_name
     >  (cmo_name,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value cmo_name'
 
      call cmo_get_info
     >  ('nnodes',cmo_name,nnodes,ilennnodes,itypnnodes,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value nnodes'
 
      call cmo_get_info
     >  ('icr1',cmo_name,ipicr1,ilenicr1,itypicr1,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value itetclr'
 
      call cmo_get_info
     >  ('itp1',cmo_name,ipitp1,ilenitp1,itypitp1,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value itp1'
 
 
      if(ierror .ne. 0)write(6,*)ierror,' ierror value icr1'
c
c
C     FIND THE PARENTS OF EACH NODE AND OUTPUT PARENT CHILD RELATIONSHIP.
C
      call cmo_get_info
     >  ('nelements',cmo_name,ntets,ilenntets,itypntets,ierror)
      if(ierror .ne. 0)write(6,*)ierror,' ierror value ntets'
 
      call cmo_get_info
     >  ('nodes_per_element',cmo_name,nen,ilennen,itypnen,ierror)
      call cmo_get_info
     >  ('isn1',cmo_name,ipisn1,ilenisn1,itypisn1,ierror)
      call cmo_get_info
     >  ('itet',cmo_name,ipitet,ilenitet,itypitet,ierror)
 
      call mmgetblk("iparent",isubname,ipiparent,nnodes,1,icscode)
      ilenitetp1=nen*ntets
      call mmgetblk("itetp1",isubname,ipitetp1,ilenitetp1,1,icscode)
c
      call unpackpc(nnodes,itp1,isn1,iparent)
C
      do n=1,ntets
         icount=nen*(n-1)
         do i=1,nen
            itetp1(icount+i)=iparent(itet1(icount+i))
         enddo
      enddo
c
c     The flag ifitpcup is passed in via chydro.h and is set in blockcom.f
c
c     First time through put the information in work arrays so that
c     when it goes out to a file, the file header will include the
c     number of entries in the list.
c
      icount = 0
      call mmgetblk("iwork1",isubname,ipiwork1,nnodes,1,icscode)
      call mmgetblk("iwork2",isubname,ipiwork2,nnodes,1,icscode)
      call mmgetblk("iwork3",isubname,ipiwork3,nnodes,1,icscode)
      call mmgetblk("iwork4",isubname,ipiwork4,nnodes,1,icscode)
      do i = 1, nnodes
         ifparent = 0
         if(itp1(i) .eq. ifitpcup)ifparent = 1
         if((ifparent .eq. 1) .or. (i .ne. iparent(i)))then
            icount = icount + 1
            iwork1(icount) = i
            iwork2(icount) = ifparent
            iwork3(icount) = iparent(i)
            if(ifparent .eq. 0)then
               iwork4(iparent(i)) = iwork4(iparent(i)) + 1
            endif
         endif
      enddo
c
c     open file and output parent child lists (if double defined nodes exist)
c     This list is not nnodes long. It only outputs the nodes that are parent
c     or child.
c
      if(icount .ne. 0)then
c
c       Find the maximum number of children.
c
        max_child = 0
        do i = 1, nnodes
           max_child = max(max_child, iwork4(i))
        enddo
c
        ifilename=ifile(1:icharlnf(ifile))//'_parent_child.list'
        iunit=-1
        call hassign(iunit,ifilename,ierror)
        write(iunit,310)'mdnode'
c	Don't invoke elimination of parents
        i_elim = 0
c       Multiplier for tying multiply defined nodes in old method
        mult = 1.0
c       Make the new split control volumes method the default
        meth = 2
        write(iunit,311) icount, max_child, i_elim, mult, meth
        do i = 1, icount
           write(iunit, 320)iwork1(i), iwork2(i), iwork3(i)
        enddo
  310   format(a6,3x,i10,3x,i10)
  311   format(i10,3x,i5,3x,i5,3x,f5.3,3x,i3)
  320   format(i10,2x,i2,2x,i10)
        close(iunit)
 
      endif
c
      call mmrelprt(isubname,icscode)
C     ..................................................................
      return
      end
