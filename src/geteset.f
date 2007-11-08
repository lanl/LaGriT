      subroutine geteset(name,ipxtet,ibitpos,num,ierr,
     *                   ntets,xtetwd)
C
C
C ######################################################################
C
C     PURPOSE -
C
C        This routine extracts the elements for a given set.
C
C     INPUT ARGUMENTS -
C
C        name    - the name of the set.
C        ipxtet  - pointer to the array in which to put the set.
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
C$Log: geteset.f,v $
CRevision 2.00  2007/11/05 19:45:57  spchu
CImport to CVS
C
CPVCS    
CPVCS       Rev 1.5   08 Feb 2006 14:35:30   dcg
CPVCS    "enforce lower case - add external statements for shift routines
CPVCS    these changes needed to compile with absoft pro fortran"
CPVCS    

CPVCS       Rev 1.4   30 Sep 2004 09:39:42   dcg

CPVCS     use iand inplace of .and. with integers

CPVCS    

CPVCS       Rev 1.3   Wed Apr 05 13:34:24 2000   nnc

CPVCS    Minor source modifications required by the Absoft compiler.

CPVCS    

CPVCS       Rev 1.2   Fri Feb 04 16:35:36 2000   dcg

CPVCS    

CPVCS       Rev 1.1   Mon Jan 31 17:33:18 2000   dcg

CPVCS    

CPVCS       Rev 1.0   28 Jan 2000 12:13:48   dcg

CPVCS    Initial revision.

CPVCS
CPVCS       Rev 1.2   Fri Oct 31 10:47:00 1997   dcg
CPVCS    declare ipcmoprm as a pointer
CPVCS
CPVCS       Rev 1.1   Mon Apr 14 16:49:46 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.0   Tue Jan 30 14:24:44 1996   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      implicit none
C
      character*132 logmess
C
      include 'cmo_lg.h'
C
C
C ######################################################################
C
C
      integer nbitsmax,ibitpos,num,ierr,ndup,i1,icharlnf
      parameter (nbitsmax=32)
C
 
      pointer( ipxtet  , xtet )
      integer xtet(1000000)
C
      integer itest(nbitsmax)
C
      character*32 name, iword
      character*32 cmo, isubname
      integer ierror,ilen,ics,isetchg,len2,i,len1,len,ntets,
     *  icscode,mask
      integer xtetwd(ntets)
      pointer (ipeltsetnames,eltsetnames)
      character*32 eltsetnames(*)
C
C
      external shiftr
      integer shiftr
      data mask/ 1 /
C
      isubname='geteset'
      ierr=0
      num=0
C
C
C     ******************************************************************
C
C     get mesh object
C
      call cmo_get_name(cmo,ierror)
C
      isetchg=0
      call mmfindbk('eltsetnames',cmo,ipeltsetnames,ilen,ics)
      isetchg=0
C
      len2=icharlnf(name)
C
      do 10 i=1,nbitsmax
         iword=eltsetnames(i)
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
      do 20 i=1,ntets
         i1=iand(mask,shiftr(xtetwd(i),ibitpos-1))
         if(i1.eq.mask)then
            xtet(i)=i
         else
            xtet(i)=0
         endif
 20   continue
      call kmprsn(ntets,xtet,1,xtet,1,xtet,1,num)
C
      goto 9999
 9999 continue
      call mmrelprt(isubname,icscode)
      return
      end
 
 
