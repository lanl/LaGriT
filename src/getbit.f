      subroutine getbit(nbitnum,ibitnum,iword,istate)
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
C***********************************************************************
CD1
CD1  PURPOSE
CD1
CD1  This subroutine returns the state of the specified bit.  The
CD1  bits are arranged from least-significant-bit (0 which is on the
CD1  right) to the most-significant-bit (nbitnum which is on the left).
CD1
C***********************************************************************
CD2 $Log: getbit.f,v $
CD2 Revision 2.00  2007/11/05 19:45:57  spchu
CD2 Import to CVS
CD2
CPVCS    
CPVCS       Rev 1.4   Mon Jan 25 17:54:58 1999   nnc
CPVCS    Changed declaration of ISHFT as EXTERNAL to INTRINSIC.
CPVCS    
CPVCS       Rev 1.3   Mon Jan 25 17:11:22 1999   nnc
CPVCS    Fixed error in external statement.
CPVCS    
CPVCS       Rev 1.2   Mon Apr 14 16:49:40 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.1   Mon Jun 03 15:12:28 1996   dcg
CPVCS    hp changes
CPVCS
CPVCS       Rev 1.0   Wed May 08 12:38:40 1996   gable
CPVCS    Initial revision.
CD2
C      INPUT ARGUMENTS -
C
C         nbitnum - TOTAL NUMBER OF BITS IN THE INPUT WORD. THIS
C                      CAN BE ANY LENGTH.
C         ibitnum - THE BIT THAT IS TO BE CHANGED.
C         iword() - THE STARTING LOCATION OF THE MOST-SIGNIFICANT-BIT.
C         istate  - THE STATE OF THE BIT TO BE GOTTEN.
C
C      OUTPUT ARGUMENTS -
C
C         iword() - THE STARTING LOCATION OF THE MOST-SIGNIFICANT-BIT.
C
      implicit none
      integer nbitnum,ibitnum,istate,iword(*)
      integer itotal, ichange, j, jchange, idum, imask
      integer ishft
      intrinsic ishft
C
C#######################################################################
C
C     DEFINE A STATEMENT FUNCTION FOR CREATING A MASK BY SHIFTING
C        BY A SPECIFIED NUMBER OF BITS.
C
      imask(idum)=2**(idum-1)
C
C#######################################################################
C
C     *** CALCULATE THE TOTAL NUMBER OF 32-BIT ELEMENTS IN THE INPUT
C            VECTOR.
      itotal=1+(nbitnum-1)/32
C
C     *** CALCULATE THE INDEX NUMBER OF THE 32-BIT ELEMENT TO CHANGE.
      ichange=1+(ibitnum-1)/32
C
C     *** CALCULATE THE BIT NUMBER OF THE 32-BIT ELEMENT TO CHANGE.
      j=ibitnum-32*(ichange-1)+1
C
C     *** CALCULATE THE INDEX NUMBER OF THE INPUT VECTOR TO CHANGE.
      jchange=itotal-ichange+1
C
C     *** FIND THE CURRENT VALUE OF THE BIT IN THE INPUT VECTOR.
      istate=ishft(iand(iword(jchange),imask(j)),-(j-1))
C
C     ******************************************************************
      return
      end
