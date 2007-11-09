*dk,ran2_lg
      function ran2_lg(idum)
C
C #####################################################################
C
C     PURPOSE -
C
C     RAN2_LG returns a uniform random deviate between 0.0 and 1.0 
C     (exclusive of endpoint values).  Initialize with IDUM equal
C     to a negative integer and do not change thereafter.
C
C     INPUT ARGUMENTS -
C
C         idum - Initialize this seed with a negative integer
C                and do not change thereafter.
C
C     OUTPUT ARGUMENTS -
C
C         ran2_lg - uniform random deviate between 0.0 and 1.0
C
C     CHANGE HISTORY -
C
C        $Log: ran2_lg.f,v $
C        Revision 2.00  2007/11/09 20:03:59  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   Wed Jul 07 13:46:32 1999   kuprat
CPVCS    Initial revision.

      implicit none
      integer idum,im1,im2,imm1,ia1,ia2,iq1,iq2,ir1,ir2,ntab,ndiv
      real*8 ran2_lg,am,eps,rnmx
c
c... Note:  RNMX should approximate the largest floating point
c... value that is less than one.
c
      parameter (im1=2147483563,im2=2147483399,am=1.d0/im1,imm1=im1-1,
     &   ia1=40014,ia2=40692,iq1=53668,iq2=52774,ir1=12211,
     &   ir2=3791,ntab=32,ndiv=1+imm1/ntab,eps=1.e-14,rnmx=1.d0-eps)
      integer idum2,j,k,iv(ntab),iy
      save iv,iy,idum2
      data idum2/123456789/, iv/ntab*0/, iy/0/
      if (idum.le.0) then
         idum=max(-idum,1)
         idum2=idum
         do j=ntab+8,1,-1
            k=idum/iq1
            idum=ia1*(idum-k*iq1)-k*ir1
            if (idum.lt.0) idum=idum+im1
            if (j.le.ntab) iv(j)=idum
         enddo
         iy=iv(1)
      endif
      k=idum/iq1
      idum=ia1*(idum-k*iq1)-k*ir1
      if (idum.lt.0) idum=idum+im1
      k=idum2/iq2
      idum2=ia2*(idum2-k*iq2)-k*ir2
      if (idum2.lt.0) idum2=idum2+im2
      j=1+iy/ndiv
      iy=iv(j)-idum2
      iv(j)=idum
      if (iy.lt.1) iy=iy+imm1
      ran2_lg=min(am*iy,rnmx)
      return
      end
