      subroutine hmemadjb(inodes,numnodes)
       implicit none
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE SETS UP AND MAINTAINS DYNAMIC MEMORY.
C
C
C     INPUT ARGUMENTS -
C
C        inodes   - NODES TO BE SET UP OR MAINTAINED
C
C        nummodes - NUMBER OF NODES TO BE SET UP OR MAINTAINED
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/hmemadjb_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.1   08 Jan 2002 08:33:10   dcg
CPVCS    remove warning messages about increasing arrays
CPVCS    
CPVCS       Rev 1.0   28 Jan 2000 14:59:28   dcg
CPVCS    Initial revision.
CPVCS
CPVCS       Rev 1.32   Wed Nov 10 15:34:52 1999   dcg
CPVCS    remove blockini.h
CPVCS
CPVCS       Rev 1.31   Wed Nov 10 09:19:06 1999   dcg
CPVCS    remove references to icdname
CPVCS
CPVCS       Rev 1.30   Wed Nov 10 08:38:52 1999   dcg
CPVCS    delete unused sections of code
CPVCS
CPVCS       Rev 1.29   Fri Jan 22 13:53:58 1999   dcg
CPVCS    put initialization of nwpz outside if
CPVCS
CPVCS       Rev 1.28   Fri Oct 23 13:06:00 1998   dcg
CPVCS    move declaration of numnodes before use - DEC compiler complaint
CPVCS
CPVCS       Rev 1.27   Wed Sep 30 11:28:48 1998   dcg
CPVCS    implicit none
CPVCS    removed unused common statements
CPVCS
CPVCS       Rev 1.26   Thu Dec 18 16:38:54 1997   dcg
CPVCS    replace calls to mminplac with calls to mmnewlen
CPVCS
CPVCS       Rev 1.25   Wed Nov 19 10:08:26 1997   dcg
CPVCS    remove obsolete code and declarations
CPVCS
CPVCS       Rev 1.24   Fri Sep 26 13:54:50 1997   dcg
CPVCS    use cmo_memory to increase size of cmo
CPVCS
CPVCS       Rev 1.23   Mon Apr 14 16:51:00 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.22   Tue Nov 05 10:05:48 1996   dcg
CPVCS    separate integer,real and charater variable in common
CPVCS
CPVCS       Rev 1.21   Tue Apr 30 11:25:18 1996   dcg
CPVCS    replace literal in argument lists with consts variables
CPVCS
CPVCS       Rev 1.20   Tue Mar 05 12:49:48 1996   dcg
CPVCS    remove icn1, int1
CPVCS
CPVCS       Rev 1.19   Mon Feb 26 09:55:20 1996   dcg
CPVCS    fix typo in sbcmodef section
CPVCS
CPVCS       Rev 1.18   Fri Feb 23 13:52:36 1996   dcg
CPVCS    changes for new cmo.h
CPVCS
CPVCS       Rev 1.17   Thu Feb 15 15:31:24 1996   dcg
CPVCS    remove references to uic, vic, wic
CPVCS    note that nodmesh option is really a no-op
CPVCS
CPVCS       Rev 1.16   Thu Feb 01 01:45:52 1996   het
CPVCS    Fix a problem for UNICOS
CPVCS
CPVCS       Rev 1.15   Tue Jan 09 16:54:26 1996   dcg
CPVCS    comment out resetting of ninter -- to fix merge bug
CPVCS
CPVCS       Rev 1.14   11/09/95 14:48:00   dcg
CPVCS    remove obsolete memory management calls
CPVCS
CPVCS       Rev 1.13   11/08/95 09:33:40   dcg
CPVCS    replace calls to mmgasblk with mmgetblk
CPVCS
CPVCS       Rev 1.12   11/07/95 17:19:02   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.11   10/12/95 21:50:12   het
CPVCS    Add the cmowrk storage block
CPVCS
CPVCS       Rev 1.10   08/30/95 21:07:54   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.9   06/28/95 11:07:30   het
CPVCS    Replace an explicit common with blockini.h
CPVCS
CPVCS       Rev 1.7   04/28/95 14:25:56   dcg
CPVCS    change inodes array to variable length character
CPVCS
CPVCS       Rev 1.6   03/23/95 22:58:32   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.5   02/03/95 20:07:50   het
CPVCS    Change the inodes character comparison to include a length
CPVCS
CPVCS       Rev 1.4   01/09/95 17:32:18   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.3   12/01/94 18:57:08   het
CPVCS    Added a variable type to the "cmo" calles
CPVCS       and include the "cmo.h" include file.
CPVCS
CPVCS
CPVCS       Rev 1.2   11/23/94 10:15:10   dcg
CPVCS     fixed IBM specific compiler errors
CPVCS
CPVCS       Rev 1.1   11/17/94 22:06:34   het
CPVCS    Major modifications associated with the changes to the reconnec
CPVCS    routines. Getting ready for the change to current_mesh_object w
CPVCS    of managing memeory.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:15:02   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
      include "consts.h"
      include "cmerge.h"
C
C ######################################################################
C
      character*32 iblknam, iprtnam
C
C ######################################################################
C
 
C
C     THIS COMDECK CONTAINS THE DOUBLE-BUFFER MEMORY AND PARAMETERS FOR
C        THE SSD-FILES.
C
 
      pointer (ipeic,eic)
      pointer (ipuic,uic)
      pointer (ipvic,vic)
      pointer (ipwic,wic)
      pointer (ippic,pic)
      pointer (ipric,ric)
      real*8 eic(*),uic(*),vic(*),wic(*),pic(*),ric(*)
C
C#######################################################################
C
      character*132 logmess
      integer iholesize,lenitp1,
     *  ier,nlen,ntetmaxo,lenkfix,
     *  lenitet,nptcnt,inodnbrp,
     *  i1,lennnodes,inodmesh,ierror,npoints,
     *  length,icmotype,len11,nwpz,ics,i,ierr,nlen11,ninc,
     *  numnodes,istart
      character*(*) inodes(numnodes)
      integer icharlnf
C
C#######################################################################
C
C
C
C     PROCESS THE MEMORY MANAGER NODES REQUESTED IN THE inodes ARRAY.
C
      do 100 i1=1,numnodes
         lennnodes=icharlnf(inodes(i1))
C
C        _______________________________________________________________
C
C        PROCESS THE NODE nodmesh.
C
C        (INITIALIZE OR ADJUST MEMORY FOR CELL-CENTERED MESH VARIABLES.)
C
         nwpz=21
         if(inodes(i1)(1:lennnodes).eq.'nodmesh') then
            inodmesh=1
C              .........................................................
C
C              ADJUST MEMORY SPACE FOR MESH ARRAYS THAT ARE ALWAYS
C              DUMPED TO THE RESTART FILE.
C
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('nnodes',cmo,npoints,length,icmotype,
     *                           ierror)
               call mmblklen('xic',cmo,ipxic,len11,ics)
               nlen11=npoints
               if(nlen11+1.gt.len11) then
                  ninc=nlen11-len11
                  call mmincblk('isetwd',cmo,ipisetwd,ninc,ics)
                  call mmincblk('imt1',cmo,ipimt1,ninc,ics)
                  call mmincblk('itp1',cmo,ipitp1,ninc,ics)
                  call mmincblk('icr1',cmo,ipicr1,ninc,ics)
                  call mmincblk('isn1',cmo,ipisn1,ninc,ics)
                  call mmincblk('xic',cmo,ipxic,ninc,ics)
                  call mmincblk('yic',cmo,ipyic,ninc,ics)
                  call mmincblk('zic',cmo,ipzic,ninc,ics)
                  call mmincblk('uic',cmo,ipuic,ninc,ics)
                  call mmincblk('vic',cmo,ipvic,ninc,ics)
                  call mmincblk('wic',cmo,ipwic,ninc,ics)
                  call mmincblk('pic',cmo,ippic,ninc,ics)
                  call mmincblk('ric',cmo,ipric,ninc,ics)
                  call mmincblk('eic',cmo,ipeic,ninc,ics)
 
                  istart=len11+1
                  do 40 i=istart,istart+ninc-1
 
                       isetwd(i)=0.0
                       imt1(i)=0
                       itp1(i)=0
                       icr1(i)=0
                       isn1(i)=0
                          xic(i)=0.0
                          yic(i)=0.0
                          zic(i)=0.0
 40               continue
C
C                 ......................................................
C
C                 ADJUST MEMORY SPACE FOR MESH ARRAYS THAT MAY NOT BE
C                 DUMPED TO THE RESTART FILE.
C
                  iholesize=0
                  call mmcomprs(iholesize, ics)
C                    **COMPRESS OUT FREE SPACE IN MEMORY
                  write(logmess,9010) npoints
                  call writloga('default',0,logmess,0,ierr)
 9010             format(a8,': expanded mesh storage to  ',i8)
 
C              .........................................................
C
            endif
            goto 100
         endif
C
C
C        _______________________________________________________________
C
C        PROCESS NODE nodnbrp.
C
C        (INITIALIZE OR ADJUST MEMORY FOR TETRAHEDRAL CONNECTIVITY
C        POINTER ARRAYS.)
C
         if(inodes(i1)(1:lennnodes).eq.'nodnbrp') then
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nelements',cmo,itetcnt,length,icmotype,
     *                        ierror)
            inodnbrp=1
C
C              .........................................................
C
C              ADJUST MEMORY SPACE.
C
            call cmo_get_name(cmo,ierror)
            call cmo_get_info('nelements',cmo,itetcnt,length,
     *                           icmotype,ierror)
            call cmo_get_info('nnodes',cmo,nptcnt,length,
     *                          icmotype,ierror)
            call mmgetnam(ipitet,iblknam,iprtnam,ics)
            call mmblklen(iblknam,iprtnam,ipitet,lenitet,ics)
            call mmgetnam(ipkfix,iblknam,iprtnam,ics)
            call mmblklen(iblknam,iprtnam,ipkfix,lenkfix,ics)
            ntetmax=1+(lenitet-1)/4
            nfixmax=ntetmax
            ninc = max0(itetcnt+2-ntetmax,10000)
            if(4*(ntetmax+ninc).gt.lenitet.or.
     *            4*(nfixmax+ninc).gt.lenkfix
     *           ) then
               if(4*(ntetmax+ninc).gt.lenitet) then
                  ntetmaxo = ntetmax
                  ntetmax = ntetmax+ninc
                  nlen = 4*ntetmax
                  call cmo_memory(cmo,nptcnt,ntetmax,ierror)
                  call cmo_get_info('itetclr',cmo,ipitetclr,length,
     *                          icmotype,ierror)
                  call cmo_get_info('itet',cmo,ipitet,length,
     *                          icmotype,ier)
                  call cmo_get_info('jtet',cmo,ipjtet,length,
     *                          icmotype,ier)
c                 write(logmess,9070) cmo,ntetmax
c                 call writloga('default',0,logmess,0,ierr)
c9070             format(a8,'arrays max set to  ',i8)
                  istart=lenitet+1
                  do 74 i=istart,nlen
                         itet1(i)=0
                         jtet1(i)=0
 74               continue
                   do 76 i=ntetmaxo+1,ntetmax
                        itetclr(i)=0
 76               continue
               endif
               if(4*(nfixmax+ninc).gt.lenkfix) then
                  nfixmax=max0(nfixmax+ninc,ntetmax)
                  nlen = 4*nfixmax
                  call mmgetnam(ipkfix,iblknam,iprtnam,ics)
                  call mmnewlen(iblknam,iprtnam,ipkfix,nlen,ics)
                  call mmgetnam(ipxfix,iblknam,iprtnam,ics)
                  call mmnewlen(iblknam,iprtnam,ipxfix,nlen,ics)
c                 write(logmess,9070) 'fix',nlen
c                 call writloga('default',0,logmess,0,ierr)
               endif
C              .........................................................
C
            endif
            goto 100
         endif
 
 
 
C        _______________________________________________________________
C
C        PROCESS NODE nodphy.
C
C        (INITIALIZE OR ADJUST MEMORY FOR NON-MESH ARRAYS NEEDED IN THE
C        PHYSICS CODE ONLY.)
C
         if(inodes(i1)(1:lennnodes).eq.'nodphy') then
            inodphy=1
            if(inodphy.eq.0) then
C
C              .........................................................
C
C              INITIALIZE MEMORY SPACE.
C
               inodphy=1
               call mmgetblk('inter','nodphy',ipinter,6,2,ics)
C
C              .........................................................
C
            else
C
C              .........................................................
C
C              ADJUST MEMORY SPACE.
C
               call mmgetnam(ipinter,iblknam,iprtnam,ics)
               call mmblklen(iblknam,iprtnam,ipinter,len11,ics)
               call cmo_get_name(cmo,ierror)
               call cmo_get_info('nnodes',cmo,npoints,length,icmotype,
     *                           ierror)
               call cmo_get_info('itp1',cmo,ipitp1,lenitp1,icmotype,ier)
               ninter=0
               do 75 i=1,npoints
                  if(itp1(i).eq.ifitpcup) ninter=ninter+1
   75          continue
               if(6*ninter.gt.len11) then
                  ninc=6*ninter-len11+600
                  call mmincblk(iblknam,iprtnam,ipinter,ninc,ics)
               endif
C
C              .........................................................
C
            endif
            goto 100
         endif
C
C
 100  continue
 
C
C     ******************************************************************
C
      return
      end
