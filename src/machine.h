 
C$Log:   /pvcs.config/t3d/src/machine.h_a  $
CPVCS    
CPVCS       Rev 1.9   Tue Oct 26 14:08:04 1999   dcg
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.8   Thu Dec 18 10:44:40 1997   dcg
CPVCS    restore value of kveclen
CPVCS
CPVCS       Rev 1.7   Wed Dec 17 15:06:22 1997   dcg
CPVCS    add nwadr for number of machine words in an address
CPVCS
CPVCS       Rev 1.6   Mon Apr 14 16:37:28 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.5   Mon Jul 08 15:04:02 1996   kuprat
CPVCS    Added KVECLEN parameter.  This parameter specifies optimal
CPVCS    vector length for vectorized computation of quantities.
CPVCS
CPVCS       Rev 1.4   11/28/95 11:26:14   dcg
CPVCS    add nbytes_real and nbytes_int for workstation/cray
CPVCS    implementations
CPVCS
CPVCS       Rev 1.3   11/27/95 11:36:38   het
CPVCS    Add the NPTRFAC_INT parameter to indicate the length of integers.
CPVCS
CPVCS       Rev 1.4   11/27/95 11:32:56   het
CPVCS    Add the NPTRFAC_INT parameter to indicate the length of integers
CPVCS
CPVCS       Rev 1.3   06/06/95 16:08:32   dcg
CPVCS
CPVCS       Rev 1.2   06/06/95 16:01:54   dcg
CPVCS    add type statments
C                           comdeck machine
 
 
C                 PARAMETERize Cray-Machine  and CTSS
C                     System-Dependent Quantities.
 
 
C                       CRAY Machine Quantities
 
C     KNCPW    The number of characters per computer (REAL*8) word.
      INTEGER KNCPW
               PARAMETER (KNCPW = 8)
 
C     KNWPN    The number of computer (REAL*8) words per character name.
      INTEGER KNWPN
               PARAMETER (KNWPN = 4)
 
C     KNCPN    The number of characters per character name.
      INTEGER KNCPN
               PARAMETER (KNCPN = KNCPW * KNWPN)
 
C     KNBPW    The number of bits per computer (REAL*8) word.
      INTEGER KNBPW
               PARAMETER (KNBPW = 64)
 
 
C                       CTSS System  Quantities
 
C     KWLSECT  The length in computer words of a disk sector.
      INTEGER KWLSECT
               PARAMETER (KWLSECT = 512)
 
C
C     nptrfac The multiplier for pointer arithmetic.  i.e for:
C             WORKSTATION set NPTRFAC=8, NPTRFAC_INT=4
C             CRAY        set NPTRFAC=1, NPTRFAC_INT=1
      INTEGER NPTRFAC, NPTRFAC_INT
              parameter (NPTRFAC=8, NPTRFAC_INT=4)
C
C     NBYTES_REAL number of bytes in a real variable
C     NBYTES_INT number of bytes in a integer variable
C             WORKSTATION set NBYTES_REAL = 8, NBYTES_INT = 4
C             CRAY set NBYTES_REAL = 8, NBYTES_INT = 8
      integer NBYTES_REAL, NBYTES_INT
              parameter (NBYTES_REAL = 8, NBYTES_INT=4)
 
 
C                     Optimal Machine Quantities
 
C     KVECLEN --- Optimal vector length
      INTEGER KVECLEN
              PARAMETER (KVECLEN = 256)
C  nwadr is number of words in a machine address
C
      integer nwadr
      parameter (nwadr=1)
 
C                         ( end  of  machine )
