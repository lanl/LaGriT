      subroutine getpset(name,ipiset,ibitpos,num,ierr,
     *                   npoints,isetwd,itp1)
C
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine extracts the points for a given set.
C
C     INPUT ARGUMENTS -
C
C        name    - the name of the set.
C        ipiset  - pointer to the array in which to put the set.
C
C     OUTPUT ARGUMENTS -
C
C        ibitpos - the position of the bit.
C        num     - the number of elements in the set.
C        ierr    - 0 => no error
C                  1 => error
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/getpset_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.6   08 Feb 2006 14:34:32   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.5   30 Sep 2004 09:38:42   dcg

CPVCS    use iand inplace of .and. with integers

CPVCS    

CPVCS       Rev 1.4   Wed Apr 05 13:34:26 2000   nnc

CPVCS    Minor source modifications required by the Absoft compiler.

CPVCS    

CPVCS       Rev 1.3   Fri Feb 04 16:35:26 2000   dcg

CPVCS    

CPVCS       Rev 1.2   Mon Jan 31 17:21:12 2000   dcg

CPVCS    

CPVCS       Rev 1.1   24 Jan 2000 17:31:32   dcg

CPVCS
CPVCS       Rev 1.13   Wed Dec 17 12:18:52 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.12   Mon Apr 14 16:49:54 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.11   12/05/95 08:20:22   het
CPVCS    Make changes for UNICOS
CPVCS
CPVCS       Rev 1.10   11/07/95 17:18:12   dcg
CPVCS    change flag to 2 in mmgetblk calls
CPVCS
CPVCS       Rev 1.9   08/30/95 21:07:18   het
CPVCS    Change the name of the storage block id from pset to psetnames
CPVCS
CPVCS       Rev 1.8   08/29/95 11:39:04   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.7   08/23/95 06:56:18   het
CPVCS    Remove the CMO prefix from SB-ids
CPVCS
CPVCS       Rev 1.6   08/22/95 06:49:58   het
CPVCS    Split the storage block for CMO variables.
CPVCS
CPVCS       Rev 1.5   06/07/95 15:29:22   het
CPVCS    Change character*32 idsb to character*132 idsb
CPVCS
CPVCS       Rev 1.4   05/01/95 08:37:04   het
CPVCS    Modifiy all the storage block calles for long names
CPVCS
CPVCS       Rev 1.3   03/28/95 12:35:52   het
CPVCS    Add the binary dumpx3d/readx3d commands and correct associated mm-errors.
CPVCS
CPVCS       Rev 1.2   01/09/95 17:32:38   het
CPVCS    Unicos changes.
CPVCS
CPVCS
CPVCS       Rev 1.1   11/15/94 14:54:18   dcg
CPVCS    check lengths of strings when matching names
CPVCS    change variable mask from type logical to type binary
CPVCS
CPVCS       Rev 1.0   11/10/94 12:14:28   pvcs
CPVCS    Original version.
C
C ######################################################################
C
      implicit none
      include 'cmo_lg.h'
C
      character*132 logmess
C
      include "chydro.h"
C
      integer icscode,mask,i1,itp,ndup,len,len1,i,icharlnf,len2,isetchg,
     *  ibitpos,num,ierr,npoints,ierror,ilen,ics
      integer isetwd(npoints), itp1(npoints)
C
C ######################################################################
C
C
      integer nbitsmax
      parameter (nbitsmax=64)
C
      pointer( ipiset  , iset(1) )
      integer iset
 
      integer itest(nbitsmax)
C
      character*32 name, iword
      character*32 cmo, isubname
      pointer (ippsetnames,psetnames)
      character*32 psetnames(*)
C
      external shiftr
      integer shiftr
      data mask/ 1 /
C
      isubname='getpset'
      ierr=0
      num=0
c
C     ******************************************************************
C
C     get mesh object
C
      call cmo_get_name(cmo,ierror)
C
      isetchg=0
      call mmfindbk('psetnames',cmo,ippsetnames,ilen,ics)
C
      len2=icharlnf(name)
C
      do 10 i=1,nbitsmax
         iword=psetnames(i)
         len1=icharlnf(iword)
         len=max(len1,len2)
         itest(i)=0
         if(name(1:len).eq.iword(1:len)) itest(i)=i
 10   continue
      call kmprsn(nbitsmax,itest,1,itest,1,itest,1,ndup)
      if(ndup.eq.0) then
         ierr=1
         write(logmess,1000) name
         call writloga('default',1,logmess,1,ierr)
 1000    format(' ERROR - THE NAME ',2x,a8,2x,' DOES NOT EXIST !')
         goto 9999
      endif
      ibitpos=itest(1)
      do 20 i=1,npoints
         itp=itp1(i)
         i1=iand(mask,shiftr(isetwd(i),ibitpos-1))
         if(i1.eq.mask.and.(itp.lt.ifitpst3.or.itp.gt.ifitpen3)) then
            iset(i)=i
         else
            iset(i)=0
         endif
 20   continue
      call kmprsn(npoints,iset,1,iset,1,iset,1,num)
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
 
 
