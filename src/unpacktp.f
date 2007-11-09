*dk,unpacktp
      subroutine unpacktp(ioptitp,iopt2,inum,ipitp1,ipitp2,icscode)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FLAGS POINTS THAT SATISFY THE ioptitp
C         OPTION.
C
C      INPUT ARGUMENTS -
C
C         ioptitp - (character) The Option.
C         iopt2   - (character) The Suboption.
C         ipitp1  - (pointer) Pointer to Point Type Array.
C
C      OUTPUT ARGUMENTS -
C
C         ipitp2  - (pointer) Pointer to Output Array..
C         icscode - (integer) Error flag.
C
C      CHANGE HISTORY -
C
C        $Log: unpacktp.f,v $
C        Revision 2.00  2007/11/09 20:04:05  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.12   30 Sep 2004 10:03:44   dcg
CPVCS    use iand, ior in place of .and., .or. with integer variables
CPVCS    
CPVCS       Rev 1.11   Mon Apr 14 17:05:28 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.10   Mon Jun 03 13:58:24 1996   dcg
CPVCS    fix typo with virtual type
CPVCS
CPVCS       Rev 1.9   Mon May 06 10:38:12 1996   dcg
CPVCS    add interior interface mode
CPVCS    'inteintf'
CPVCS
CPVCS       Rev 1.8   Fri May 03 17:02:44 1996   dcg
CPVCS    add matlintr category
CPVCS
CPVCS       Rev 1.7   Fri May 03 10:46:24 1996   dcg
CPVCS    changes for new point type --virtual interfaces
CPVCS
CPVCS       Rev 1.6   07/14/95 10:16:10   het
CPVCS    Correct errors with point types
CPVCS
CPVCS       Rev 1.5   05/19/95 10:58:20   ejl
CPVCS    Fixed more bugs. Cleaned up. Implicit none.
CPVCS
CPVCS       Rev 1.4   05/19/95 08:23:34   ejl
CPVCS    Fixed more bugs with new point types
CPVCS
CPVCS       Rev 1.3   05/11/95 13:14:10   het
CPVCS    Add new point types for combining free, reflective and interface boundaries
CPVCS
CPVCS       Rev 1.2   03/23/95 22:59:38   het
CPVCS    Add the model routines and add the cmo name into the idsbs
CPVCS
CPVCS       Rev 1.1   01/04/95 22:06:34   llt
CPVCS    unicos changes (made by het)
CPVCS
CPVCS       Rev 1.0   11/10/94 12:20:12   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "chydro.h"
C
C#######################################################################
C
      character*(*) ioptitp, iopt2
C
      integer inum
C
      pointer (ipitp1, itp1 )
      integer itp1(inum)
      pointer (ipitp2, itp2 )
      integer itp2(inum)
C
      integer icscode
C
C#######################################################################
C
      integer i1, len1, iset, iset1, iset2, itp, ioptitp2
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
C
C
      icscode = -1
C
      if (iopt2(1:3) .eq. 'set') then
         do i1=1,inum
            itp2(i1)=0
         enddo
      endif
C
      len1=icharlnf(ioptitp)
C
      if (ioptitp(1:len1) .eq. 'allreal') then
C
C****    ALLREAL Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset1=0
               if (itp.ge.ifitpst1.and.itp.le.ifitpen1) iset1=1
               iset2=0
               if (itp.ge.ifitpst2.and.itp.le.ifitpen2) iset2=1
               itp2(i1) = ior(iset1,iset2)
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset1=0
               if (itp.ge.ifitpst1.and.itp.le.ifitpen1) iset1=1
               iset2=0
               if (itp.ge.ifitpst2.and.itp.le.ifitpen2) iset2=1
               itp2(i1) = iand(ior(iset1,iset2),itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset1=0
               if (itp.ge.ifitpst1.and.itp.le.ifitpen1) iset1=1
               iset2=0
               if (itp.ge.ifitpst2.and.itp.le.ifitpen2) iset2=1
               itp2(i1) = ior(ior(iset1,iset2),itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'interior') then
C
C****    INTERIOR Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst1.and.itp1(i1).le.ifitpen1) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst1.and.itp1(i1).le.ifitpen1) iset=1
               itp2(i1) = iand(iset,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst1.and.itp1(i1).le.ifitpen1) iset=1
               itp2(i1) = ior(iset,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'inteintf') then
C
C    INTERIOR INTERFACE Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpini.or.
     *             itp1(i1).eq.ifitpvin.or.
     *             itp1(i1).eq.ifitpvrt) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpini.or.
     *             itp1(i1).eq.ifitpvin.or.
     *             itp1(i1).eq.ifitpvrt) iset=1
               itp2(i1) = iand(iset,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpini.or.
     *             itp1(i1).eq.ifitpvin.or.
     *             itp1(i1).eq.ifitpvrt) iset=1
               itp2(i1) = ior(iset,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'boundary') then
C
C****    BOUNDARY Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst2.and.itp1(i1).le.ifitpen2) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst2.and.itp1(i1).le.ifitpen2) iset=1
               itp2(i1) = iand(iset,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst2.and.itp1(i1).le.ifitpen2) iset=1
               itp2(i1) = ior(iset,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'matlintr') then
C
C****    MATLINTR Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = iand(iset,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = ior(iset,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'intrface') then
C
C****    INTRFACE Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpvrt) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitpvrb) .or.
     *             (itp.eq.ifitpvfb) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpvrt) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitpvrb) .or.
     *             (itp.eq.ifitpvfb) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = iand(iset,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpvrt) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitpvrb) .or.
     *             (itp.eq.ifitpvfb) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = ior(iset,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'reflect') then
C
C****    REFLECT Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitprfl) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitprfb) .or.
     *             (itp.eq.ifitpvrb) .or.
     *             (itp.eq.ifitpvrf) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitprfl) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitprfb) .or.
     *             (itp.eq.ifitpvrb) .or.
     *             (itp.eq.ifitpvrf) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = iand(iset,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               itp=itp1(i1)
               iset=0
               if ((itp.eq.ifitprfl) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitprfb) .or.
     *             (itp.eq.ifitpvrb) .or.
     *             (itp.eq.ifitpvrf) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) iset=1
               itp2(i1) = ior(iset,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'free') then
C
C****    FREE Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if ((itp1(i1).eq.ifitpfre) .or.
     *             (itp1(i1).eq.ifitpifb) .or.
     *             (itp1(i1).eq.ifitprfb) .or.
     *             (itp1(i1).eq.ifitpvfb) .or.
     *             (itp1(i1).eq.ifitpvrf) .or.
     *             (itp1(i1).eq.ifitpvif) .or.
     *             (itp1(i1).eq.ifitpalb) .or.
     *             (itp1(i1).eq.ifitpirf)) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if ((itp1(i1).eq.ifitpfre) .or.
     *             (itp1(i1).eq.ifitpifb) .or.
     *             (itp1(i1).eq.ifitprfb) .or.
     *             (itp1(i1).eq.ifitpvfb) .or.
     *             (itp1(i1).eq.ifitpvrf) .or.
     *             (itp1(i1).eq.ifitpvif) .or.
     *             (itp1(i1).eq.ifitpalb) .or.
     *             (itp1(i1).eq.ifitpirf)) iset=1
               itp2(i1) = iand(iset ,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if ((itp1(i1).eq.ifitpfre) .or.
     *             (itp1(i1).eq.ifitpifb) .or.
     *             (itp1(i1).eq.ifitprfb) .or.
     *             (itp1(i1).eq.ifitpvfb) .or.
     *             (itp1(i1).eq.ifitpvrf) .or.
     *             (itp1(i1).eq.ifitpvif) .or.
     *             (itp1(i1).eq.ifitpalb) .or.
     *             (itp1(i1).eq.ifitpirf)) iset=1
               itp2(i1) = ior(iset ,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'virtual') then
C
C****    VIRTUAL Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if ((itp1(i1).eq.ifitpvrt) .or.
     *             (itp1(i1).eq.ifitpvin) .or.
     *             (itp1(i1).eq.ifitpvir) .or.
     *             (itp1(i1).eq.ifitpvfb) .or.
     *             (itp1(i1).eq.ifitpvrf) .or.
     *             (itp1(i1).eq.ifitpvif) .or.
     *             (itp1(i1).eq.ifitpalb) .or.
     *             (itp1(i1).eq.ifitpvrb)) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if ((itp1(i1).eq.ifitpvrt) .or.
     *             (itp1(i1).eq.ifitpvin) .or.
     *             (itp1(i1).eq.ifitpvir) .or.
     *             (itp1(i1).eq.ifitpvfb) .or.
     *             (itp1(i1).eq.ifitpvrf) .or.
     *             (itp1(i1).eq.ifitpvif) .or.
     *             (itp1(i1).eq.ifitpalb) .or.
     *             (itp1(i1).eq.ifitpvfb)) iset=1
               itp2(i1) = iand(iset ,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if ((itp1(i1).eq.ifitpvrt) .or.
     *             (itp1(i1).eq.ifitpvin) .or.
     *             (itp1(i1).eq.ifitpvir) .or.
     *             (itp1(i1).eq.ifitpvfb) .or.
     *             (itp1(i1).eq.ifitpvrf) .or.
     *             (itp1(i1).eq.ifitpvif) .or.
     *             (itp1(i1).eq.ifitpalb) .or.
     *             (itp1(i1).eq.ifitpvfb)) iset=1
               itp2(i1) = ior(iset ,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'removed') then
C
C****    REMOVED Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst3.and.itp1(i1).le.ifitpen3) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst3.and.itp1(i1).le.ifitpen3) iset=1
               itp2(i1) = iand(iset ,itp2(i1))
            enddo
C
            elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).ge.ifitpst3.and.itp1(i1).le.ifitpen3) iset=1
               itp2(i1) = ior(iset ,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'merged') then
C
C****    MERGED Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpmrg) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpmrg) iset=1
               itp2(i1) = iand(iset ,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpmrg) iset=1
               itp2(i1) = ior(iset ,itp2(i1))
            enddo
C
         endif
C
      elseif (ioptitp(1:len1) .eq. 'dudded') then
C
C****    DUDDED Option.
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpdud) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpdud) iset=1
               itp2(i1) = iand(iset ,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ifitpdud) iset=1
               itp2(i1) = ior(iset ,itp2(i1))
            enddo
C
         endif
C
      else
C
C****    Specific Point Type Option.
C
         read(ioptitp,'(i10)') ioptitp2
C
         if (iopt2(1:3) .eq. 'set') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ioptitp2) iset=1
               itp2(i1) = iset
            enddo
C
         elseif (iopt2(1:3) .eq. 'and') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ioptitp2) iset=1
               itp2(i1) = iand(iset ,itp2(i1))
            enddo
C
         elseif (iopt2(1:2) .eq. 'or') then
C
            icscode=0
            do i1=1,inum
               iset=0
               if (itp1(i1).eq.ioptitp2) iset=1
               itp2(i1) = ior(iset ,itp2(i1))
            enddo
C
         endif
C
      endif
C
      return
      end
*dk,itsttp
      function itsttp(ioptitp,itp)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE FRETURNS TRUE IF ITP IS IN THE CLASS ioptitp
C         IT RETURNS FALSE OTHERWISE
C
C      INPUT ARGUMENTS -
C
C         ioptitp - (character) The Option.
C               classes are interior, boundary, matlintr, reflect, free,
C                   intrface,virtual, removed, dudded, merged, allreal
C         itp     - (integer) point type to be tested.
C
C      OUTPUT ARGUMENTS -
C
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include "chydro.h"
C
C#######################################################################
C
      character*(*) ioptitp
      character*132 logmess
      integer itp
      logical itsttp
C
      integer icharlnf,ierror,len1
C
      len1=icharlnf(ioptitp)
      itsttp=.false.
C
      if (ioptitp(1:len1) .eq. 'allreal') then
C
C****    ALLREAL Option.
C
         if (itp.ge.ifitpst1.and.itp.le.ifitpen1) itsttp=.true.
         if (itp.ge.ifitpst2.and.itp.le.ifitpen2) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'interior') then
C
C****    INTERIOR Option.
C
         if (itp.ge.ifitpst1.and.itp.le.ifitpen1) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'inteintf') then
C
C****    INTERIOR INTERFACE Option.
C
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvrt)) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'boundary') then
C
C****    BOUNDARY Option.
C
         if (itp.ge.ifitpst2.and.itp.le.ifitpen2) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'matlintr') then
C
C****    MATLINTR Option.
C
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'intrface') then
C
C****    INTRFACE Option.
C
               if ((itp.eq.ifitpini) .or.
     *             (itp.eq.ifitpvrt) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitpvrb) .or.
     *             (itp.eq.ifitpvfb) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'reflect') then
C
C****    REFLECT Option.
C
               if ((itp.eq.ifitprfl) .or.
     *             (itp.eq.ifitpirb) .or.
     *             (itp.eq.ifitprfb) .or.
     *             (itp.eq.ifitpvrb) .or.
     *             (itp.eq.ifitpvrf) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'free') then
C
C****    FREE Option.
C
               if ((itp.eq.ifitpfre) .or.
     *             (itp.eq.ifitpifb) .or.
     *             (itp.eq.ifitprfb) .or.
     *             (itp.eq.ifitpvfb) .or.
     *             (itp.eq.ifitpvrf) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpirf)) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'virtual') then
C
C****    FREE Option.
C
               if ((itp.eq.ifitpvrt) .or.
     *             (itp.eq.ifitpvin) .or.
     *             (itp.eq.ifitpvir) .or.
     *             (itp.eq.ifitpvfb) .or.
     *             (itp.eq.ifitpvrf) .or.
     *             (itp.eq.ifitpvif) .or.
     *             (itp.eq.ifitpalb) .or.
     *             (itp.eq.ifitpvrb)) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'removed') then
C
C****    REMOVED Option.
C
         if (itp.ge.ifitpst3.and.itp.le.ifitpen3) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'merged') then
C
C****    MERGED Option.
C
         if (itp.eq.ifitpmrg) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'dudded') then
C
C****    DUDDED Option.
C
         if (itp.eq.ifitpdud) itsttp=.true.
C
      elseif (ioptitp(1:len1) .eq. 'parent') then
C
C****    PARENT Option.
C
         if (itp.eq.ifitpcup) itsttp=.true.
C
      else
         write(logmess,"('Illegal option to itsttp',a8)") ioptitp
         call writloga('default',0,logmess,0,ierror)
      endif
      return
      end
