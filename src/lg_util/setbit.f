      subroutine setbit(nbitnum,ibitnum,iword,istate)
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
C        $Log: setbit.f,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.6   Mon Jan 25 17:58:58 1999   nnc
CPVCS    Changed declaration of ISHFT as EXTERNAL to INTRINSIC.
CPVCS    
CPVCS       Rev 1.5   Mon Jan 25 17:17:48 1999   nnc
CPVCS    Fixed error in external statement.
CPVCS    
CPVCS       Rev 1.4   Tue Jun 18 09:21:14 1996   dcg
CPVCS    use ior and iand in place of or and and
CPVCS
CPVCS       Rev 1.3   12/05/95 08:32:24   het
CPVCS    Make UNICOS changes
CPVCS
CPVCS       Rev 1.2   11/21/95 11:52:20   dcg
CPVCS    add arbitrary length or, and functions
CPVCS
CPVCS       Rev 1.0   04/12/95 18:10:16   llt
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
      integer nbitnum,ibitnum,istate,iword(*)
      integer itotal, ichange, j, jchange, nwd1, jstate, idum, imask
      integer maskv1, maskv2
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
      maskv1=iword(jchange)
      maskv2=imask(j)
      nwd1=ishft(iand(maskv1,maskv2),-(j-1))
C
C     ******************************************************************
C     SET THE BIT TO THE INPUT STATE BY DOING A TRUTH TABLE WITH THE
C        CURRENT BIT SETTING AND THE STATE VALUE.
C
C        CURRENT BIT  STATE VALUE  OUTPUT BIT
C        ***********  ***********  **********
C                  0            1           1
C                  1            0           0
C                  1            1           1
C                  0            0           0
C
      if(nwd1.eq.0.and.istate.ne.0) then
         jstate=ishft(1,(j-1))
         iword(jchange)=ior(iword(jchange),jstate)
      elseif(nwd1.eq.1.and.istate.eq.0) then
         jstate=not(ishft(1,(j-1)))
         iword(jchange)=iand(iword(jchange),jstate)
      elseif(nwd1.eq.1.and.istate.ne.0) then
      elseif(nwd1.eq.0.and.istate.eq.0) then
      endif
C
C     ******************************************************************
C
      goto 9999
 9999 continue
      return
      end
      subroutine fpor(numbits,x,y,z)
C
C #######################################################
C
C   PURPOSE
C      perform a logical or operation on an
C      arbitrary number of bits
C
C   INPUT ARGUMENTS
C      numbits -- number of bits to 'or'
C      x          first argument of 'or'
C      y          second argument of 'or'
C
C   OUTPUT ARGUMENTS
C      z    result of 'or' on x or y
C
C #######################################################
      implicit none
      logical bit
      real*8 x(*),y(*),z(*)
      integer i1,i2,i3,i
      integer numbits
      do i=1,numbits
      i1=0
      i2=0
      if(bit(numbits,i-1,x) ) i1=1
      if(bit(numbits,i-1,y) ) i2=1
      i3 = ior(i1,i2)

C 32 bit compiler warning
C Warning: Type mismatch in argument 'iword' 
C at (1); passed REAL(8) to INTEGER(4)
C 64 bit; passed REAL(8) to INTEGER(8)
      call setbit(numbits,i-1,z,i3)
      enddo
      return
      end
      subroutine fpand(numbits,x,y,z)
C
C #######################################################
C
C   PURPOSE
C      perfandm a logical and operation on an
C      arbitrary number of bits
C
C   INPUT ARGUMENTS
C      numbits -- number of bits to 'and'
C      x          first argument of 'and'
C      y          second argument of 'and'
C
C   OUTPUT ARGUMENTS
C      z    result of 'and' on x and y
C
C #######################################################
      implicit none
      logical bit
      real*8 x(*),y(*),z(*)
      integer i1,i2,i3,i
      integer numbits
      do i=1,numbits
      i1=0
      i2=0
      if(bit(numbits,i-1,x) ) i1=1
      if(bit(numbits,i-1,y) ) i2=1
      i3 = iand(i1,i2)

C 32 bit compiler warning
C Warning: Type mismatch in argument 'iword' at (1); 
C passed REAL(8) to INTEGER(4)
C 64 bit; passed REAL(8) to INTEGER(8)
      call setbit(numbits,i-1,z,i3)
      enddo
      return
      end
