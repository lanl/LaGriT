*dk,primestep
      subroutine primestep(m,n)
C #####################################################################
C
C     PURPOSE -
C
C        Given a positive integer M, repeated calls to PRIMESTEP
C        produce a complete permutation of the integers {1,...,M}.
C        Then, when that permutation is finished, a different
C        permutation is generated, and so on.
C        The permutations are derived by taking jumps by
C        a prime number that doesn't divide into M.  The
C        purpose of this is to produce 'good' sequences of
C        nodes for Gauss-Seidel relaxation.
C
C     INPUT ARGUMENTS -
C
C         M         - The length of the sequence to be permuted
C
C     INPUT/OUTPUT ARGUMENTS -
C
C         N         - The next element in the permutation.
C
C     CHANGE HISTORY -
C
C        $Log: primestep.f,v $
C        Revision 2.00  2007/11/09 20:03:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.2   Fri Dec 18 17:43:14 1998   kuprat
CPVCS    Corrected documentation.
CPVCS    
CPVCS       Rev 1.1   Fri Nov 13 15:43:00 1998   kuprat
CPVCS    We now start afresh if we notice the user has changed the 
CPVCS    length of the array to be permuted.
CPVCS    
CPVCS       Rev 1.0   Wed Nov 04 02:43:34 1998   kuprat
CPVCS    Initial revision.
C#######################################################################
 
      integer m,n
 
      integer mp
      parameter (mp=10)
 
      integer iprime(10),np
 
c.... IPRIME contains the choices of primes for stepping through
c.... the sequence.
 
      data iprime / 17,19,23,29,31,37,41,43,47,53 /
 
      data np    / 0 /
      data mlast / 0 /
      save iprime,np,inext,mlast
 
c.... Check if M is different from MLAST.  If this is so, then this 
c.... is either the initial invocation, or the user has changed the
c.... length of the array we are permuting.  In either case, set
c.... INEXT to 1, meaning we want the first element of a fresh
c.... permutation.  If M is the same as MLAST, increment INEXT to 
c.... get the next element of the permutation.

      if (m.ne.mlast) then
         inext=1
         mlast=m
      else
         inext=mod(inext,m)+1   
      endif

c.... If INEXT is 1, we need to find a new prime that doesn't divide M
c.... to generate a fresh permutation.

      if (inext.eq.1) then
         np=mod(np,mp)+1
         itried=0
         do while (mod(m,iprime(np)).eq.0)
            np=mod(np,mp)+1
            itried=itried+1
            if (itried.eq.mp) then
               print*,'PRIMESTEP: All primes divide ',m
               stop
            endif
         enddo
      endif

c.... Generate the next element of the permutation.
 
      n=mod(inext*iprime(np),m)+1
 
      return
      end
 
