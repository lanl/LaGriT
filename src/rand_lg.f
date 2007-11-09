*dk rand_lg
C***********************************************************************
C
C  PURPOSE -
C
C   Machine independent general-purpose real*8
C   random number generator for unix workstations.
C
C ............................................................
C
C   TO USE the random number generator:
C
C   *  x=rand_lg()
C       - puts the next random real*8 number on (0,1) in
C         variable x.
C
C   *  i=irand_lg(j)
C       - puts the next random integer on [0,j] in
C         variable i.
C
C   * * * NOTE INITIALIZATION IS *NOT* REQUIRED BEFORE USE * * *
C    (as the defaults are in a block data statement)
C
C ............................................................
C
C   TO MODIFY the random number generator parameters:
C
C   *  call seed_rand_lg(seed1,seed2)
C       - reset the random number seeds using the integers seed1 and
C         seed2, which should be "large, but not too large" positive
C         integers, and seed2 should be odd and smaller than seed1.
C
C   *  call adv_rand_lg()
C       - advances the random number parameters
C
C   *  call reset_rand_lg(rnmultin,nstridin,rijkin,nadvin)
C       - If the integer arguments are all zero or smaller reset
C         the random number generator to the initial defaults
C         (initialized at compile time with a block data statement).
C       - Otherwise re-initialize the random number generator parameters
C         using the integers rnmultin,nstridin,rijkin,nadvin
C         which should be "large, but not too large" positive integers
C         (see code for meaning).
C
C ............................................................
C
C$Log:   /pvcs.config/t3d/src/rand_lg.f_a  
CPVCS    
CPVCS       Rev 1.3   Wed Jul 21 15:25:44 1999   jtg
CPVCS    gave block data statement a name
CPVCS    
CPVCS       Rev 1.2   Fri Jul 09 22:38:06 1999   jtg
CPVCS    fixed typo
CPVCS    
CPVCS       Rev 1.1   Fri Jul 09 21:21:32 1999   jtg
CPVCS    fixed typo
CPVCS    
CPVCS       Rev 1.0   Fri Jul 09 20:59:30 1999   jtg
CPVCS    Initial revision.
C
C PRIOR HISTORY:
C
C replaces t3d/potts_random_numbers.f and grain3d/potts_random_numbers.f
C (jtg@lanl.gov got this from Charlie Snell at LANL who got it from
C Hendricks at LANL, and I don't know where they have it from.
C It is "well tested"....)
C
C "_lg" hass been added to all subroutine and function names
C to avoid package conflicts
C
*********************************************************************
c returns the next random integer in [0,j]

      integer function irand_lg(j)
c ........................................................................
      implicit none
      real*8 rand_lg
      integer j,absj
c     --------------------------------------------------------------

      absj = abs(j)

c     ! find random integer in [0,|j|]
      irand_lg = rand_lg() * dble(absj+1)

c     ! check for error due to numerical roundoff
      if (irand_lg.gt.absj)  irand_lg=absj

      if (j.lt.0)   irand_lg = -irand_lg

      return
      end
c ########################################################################
c returns the next pseudo-random real number in (0,1)

      real*8 function rand_lg()
c ........................................................................
      implicit none
      real*8  rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      common /com_rand_lg/ rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      real*8 p, q, a, b, r
      parameter (p=2d0**24,q=2d0**(-24),r=2d0**(-48))
c     --------------------------------------------------------------

c        rand_lg()=mod(2**48*rand_lg()*rnmult,2**48)
c        split rand_lg() and rnmult into upper and lower 24-bit halves,
c        ranb,rans,rngb,rngs, respectively, to achieve 96-bit precision.
c        this expression for b is invalid unless rngb+rngs < 2**24
c        or unless the more elaborate expression of adv_rand_lg is used.

      a = rngs * rans
      b = rngb*rans + rngs*ranb + int(a*q)
      rans = a - int(a*q)*p
      ranb = b - int(b*q)*p
      rand_lg = ( ranb*p + rans ) * r

      return
      end
c ########################################################################
c resets the random number seed

      subroutine seed_rand_lg(seed1,seed2)
c ........................................................................
      implicit none
      real*8  rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      common /com_rand_lg/ rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      integer seed1, seed2
      real*8 large
      parameter (large=1.d16)
c     --------------------------------------------------------------

      ranb = abs(seed1)
      rans = abs(seed2)

c check for legal values of the two components of the random seed
      if (rans.gt.ranb) then
         ranb = rans
         rans = abs(seed1)
      endif
      if (mod(int(rans),2).eq.0) rans = rans+1.
      if (ranb.gt.large) ranb = large
      if (rans.gt.large) rans = large - 101234567.

      return
      end
c ########################################################################
c pre-initialize the pseudo-random number sequence at compile time

      block data rand_data_lg
c ........................................................................
      implicit none
      real*8  rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      common /com_rand_lg/ rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb

      data rnfb /13008944.d0/, rnfs /170125.d0/, rngb /1136868.d0/
     &   , rngs /6328637.d0/, rijk /19073486328125.d0/
     &   , rani /1136868.d0/, ranj /6328637.d0/
     &   , ranb /1136868.d0/, rans /6328637.d0/

c     --------------------------------------------------------------

      end
c ########################################################################
c re-initialize the pseudo-random number sequence
c to defaults if (rnmultin,nstridin,rijkin,nadvin) = (0,0,0,0),
c otherwise use those inputs>0 to startup the random number generator.

      subroutine reset_rand_lg(rnmultin,nstridin,rijkin,nadvin)
c ........................................................................
      implicit none
      integer rijkin,nstridin,rnmultin,nadvin
      real*8  rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      common /com_rand_lg/ rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      real*8 p,q
      parameter (p=2d0**24,q=2d0**(-24))
      real*8 a,b,rnmult,nstrid
      integer istrid,iadv
c     --------------------------------------------------------------

      if ( (nstridin.gt.0) .or. (rnmultin.gt.0) ) then

c        ! Set the random number multiplier rnmult and the random number
c        ! stride nstrid. The random number multiplier must be odd.
c        ! The random number modulus sum must be .lt. p
c        ! rnmultin should be greater than p

10       if (rnmultin.gt.0) then
            if (mod(rnmultin,2).eq.0) then
               rnmult = rnmultin+1
            else
               rnmult = rnmultin
            endif
         else
            rnmult=19073486328125.d0
         endif

c        ! rngb and rngs are the upper and lower 24 bits of rnmult.
         rngb = int(rnmult*q)
         rngs = rnmult-rngb*p
         if (rngb.eq.0) rngb = 1
         if (rngs.eq.0) rngs = 1
         if (rngb+rngs.ge.p)  then
            rnmultin = rnmultin/7
            goto 10
         endif

c        ! get rnfb (upper 24 bits) and rnfs (lower 24 bits) of
c        ! rnmult**nstrid which is used in adv_rand_lg to advance the random
c        ! number by nstrid random numbers for each history.

         if (nstridin.gt.0) then
            nstrid = nstridin
         else
            nstrid = 152917
         endif

         rnfb = rngb
         rnfs = rngs
         nstrid = nstrid - 1
         do istrid=1,nstrid
            a = rngs*rnfs
            b = rngb*rnfs - int(rngb*rnfs*q)*p
     &        + rngs*rnfb - int(rngs*rnfb*q)*p
     &        + int(a*q)
            rnfs = a - int(a*q)*p
            rnfb = b - int(b*q)*p
         enddo

      else

         ! rnmult = 5**19
         rnmult = 19073486328125.d0
         rngb = 1136868.d0
         rngs = 6328637.d0
         rnfb = 13008944.d0
         rnfs = 170125.d0

      endif

c     ! if desired, insert a starting value of the random number rijk
c     ! otherwise use the default rijk=rnmult
      if (rijkin.gt.0) then
         rijk = rijkin
      else
         rijk = rnmult
      endif

c     ! rijk is composed of ranj (top 24 bits) and rani (bottom 24 bits).
      rani = int(rijk*q)
      ranj = rijk-rani*p

c     ! if desired, advance the initial random number
      do iadv=1,nadvin-1
         call adv_rand_lg()
      enddo

      ranb = rani
      rans = ranj

      return
      end
c ########################################################################
c advance source random number, rijk, for the next history.
c ranj is the lower 24 bits of rijk, rani is the upper 24 bits.
c (currently not used: see comments above)

      subroutine adv_rand_lg()
c ........................................................................
      implicit none
      real*8  rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      common /com_rand_lg/ rnfb,rnfs,rngb,rngs,rijk,ranj,rani,rans,ranb
      real*8 p, q, a, b
      parameter (p=2d0**24,q=2d0**(-24))
c     --------------------------------------------------------------

      a = rnfs * ranj
      b = rnfb*ranj - int(rnfb*ranj*q)*p
     &  + rnfs*rani - int(rnfs*rani*q)*p
     &  + int(a*q)
      ranj = a - int(a*q)*p
      rani = b - int(b*q)*p
      rijk = rani*p + ranj

      return
      end
c ########################################################################
