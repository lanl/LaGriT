      logical function bit(nbitnum,ibitnum,iword)
C
C#######################################################################
C
C      PURPOSE -
C
C         THIS ROUTINE SETS THE SPECIFIED BIT TO THE INPUT STATE.
C            THE BITS ARE ARRANGED FROM LEAST-SIGNIFICANT-BIT (0 WHICH
C            IS ON THE RIGHT) TO THE MOST-SIGNIFICANT-BIT (nbitnum
C            WHICH IS ON THE LEFT).
C
C      INPUT ARGUMENTS -
C
C         nbitnum - TOTAL NUMBER OF BITS IN THE INPUT WORD. THIS
C                      CAN BE ANY LENGTH.
C         ibitnum - THE BIT THAT IS TO BE CHANGED.
C         iword() - THE STARTING LOCATION OF THE MOST-SIGNIFICANT-BIT.
C         istate  - THE STATE OF THE BIT TO BE SET.
C
C      OUTPUT ARGUMENTS -
C
C         iword() - THE STARTING LOCATION OF THE MOST-SIGNIFICANT-BIT.
C
C      CHANGE HISTORY -
C
C        $Log: bit.f,v $
C        Revision 2.00  2007/11/03 00:49:10  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Mon Jan 25 17:59:26 1999   nnc
CPVCS    Changed declaration of ISHFT as EXTERNAL to INTRINSIC.
CPVCS    
CPVCS       Rev 1.2   Mon Jan 25 17:18:02 1999   nnc
CPVCS    Fixed error in external statement.
CPVCS    
CPVCS       Rev 1.1   Wed Jan 03 12:09:52 1996   het
CPVCS    Replace and() with iand()
CPVCS    
CPVCS       Rev 1.0   04/12/95 18:10:14   llt
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
      integer nbitnum,ibitnum,iword(*)
      integer itotal, ichange, j, jchange, nwd1, idum, imask
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
      nwd1=ishft(iand(iword(jchange),imask(j)),-(j-1))
C
C     ******************************************************************
C     TEST THE CURRENT BIT SETTING. 
C         =  0 ==> FALSE
C        < > 0 ==> TRUE
C
      if(nwd1.eq.0) then
         bit=.false.
      else
         bit=.true.
      endif
C
C     ******************************************************************
C
      goto 9999
 9999 continue
      return
      end
