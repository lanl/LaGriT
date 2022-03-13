!!
!!  Header file for MM2000.f
!!
!!  Values set for a 32 bit architecture with 4-byte default integers
!!  and 8-byte real*8.
!!
!!  Array descriptor format:
!!
!!    ad_name(1,j) - array name
!!    ad_name(2,j) - partition name
!!
!!    ad_addr(1,j) - prolog (also block) address
!!    ad_addr(2,j) - array address
!!    ad_addr(3,j) - epilog address
!!    ad_addr(4,j) - array type (0=inactive, 1=int, 2=real*8, 3=char*32)
!!    ad_addr(5,j) - array size
!!    ad_addr(6,j) - block size in bytes
!!
!!  CHANGE HISTORY
!!
!!  $Log: mm2000.h,v $
!!  Revision 2.00  2007/11/03 00:49:12  spchu
!!  Import to CVS
!!
CPVCS    
CPVCS       Rev 1.0   Thu Apr 06 10:50:48 2000   nnc
CPVCS    Initial revision.
!!

!!    define SIZEOF_VOIDP for operating system
#include "opsys.h" 

      real*8 RMAX_ALLOCATE
      parameter (RMAX_ALLOCATE = MAX_UINT)

      integer BYTES_PER_INT, BYTES_PER_REAL
      parameter (BYTES_PER_INT = SIZEOF_VOIDP, BYTES_PER_REAL = 8)

      integer BYTES_PER_CHAR
      parameter (BYTES_PER_CHAR = 32)

      integer BYTES_PER_PTR
      parameter (BYTES_PER_PTR = SIZEOF_VOIDP) ! pointer-sized integer

      integer AD_SIZE
      parameter (AD_SIZE = 6)

      integer PROLOG_BYTES, EPILOG_BYTES
      parameter (PROLOG_BYTES = AD_SIZE*BYTES_PER_PTR)
      parameter (EPILOG_BYTES = AD_SIZE*BYTES_PER_PTR)

      pointer (adnptr, ad_name)
      character*32 ad_name(2,*)

      pointer (adaptr, ad_addr)
      integer ad_addr(AD_SIZE,*) ! pointer-sized integer

      pointer (lptr, link)
      integer link(*)

      integer max_ad, num_ad, first, next
      common /mm2000/ max_ad, num_ad, first, next, adnptr, adaptr, lptr

      save /mm2000/

