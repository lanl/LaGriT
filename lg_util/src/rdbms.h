C
C ######################################################################
C
C        $Log: rdbms.h,v $
C        Revision 2.00  2007/11/03 00:49:12  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C ######################################################################
C
*CD sbmssc
 
 
C                           comdeck  sbmssc
 
 
C                    Storage Block Manager System.
C                       Completion-Status Codes.
 
 
C     KSBMSOK  The nominal completion-status  code.  It  indicates  that
C              the  SUBROUTINE  performed  its  function   as   normally
C              expected.
               PARAMETER (KSBMSOK = 0)
 
C     KSBMSET  The storage block of interest has not been set up.
               PARAMETER (KSBMSET = 2)
 
C     KSBMVAF  The length of the value field is less than one (1).
               PARAMETER (KSBMVAF = 3)
 
C     KSBMIDF  Data are already stored under the indicated id.
               PARAMETER (KSBMIDF = 5)
 
C     KSBMNOD  No data are stored under the id.
               PARAMETER (KSBMNOD = 6)
 
C     KSBMEXP  The storage block could not be expanded.
               PARAMETER (KSBMEXP = 7)
 
C     KSBMEMP  The storage block is empty.
               PARAMETER (KSBMEMP = 9)
 
C                         ( end  of  sbmssc )
*CD fdvdsd
 
 
C
C                                  PURPOSE:
 
C          PARAMETERize the  data-structure definition  of all file dope
C     vectors.
 
 
C                                DEFINITION:
 
C          A file  dope vector  is a  small data structure that contains
C     information about a file.
 
 
C                          PROGRAMMING CONVENTIONS:
 
C          To keep  open long-term maintenance options, always index for
C     entries in  file dope  vectors with  PARAMETERs.   Never use  hard
C     constants or  assume any  absolute or  relative  ordering  of  its
C     entries.  (The order is subject to change without notice.)
 
C          To ensure notational uniformity, only these PARAMETERs should
C     be used to index for entries in file dope vectors.
 
C     ..................................................................
 
 
C     KFDVLW   The length in computer words of the file dope vectors.
               PARAMETER (KFDVLW = 4)
 
C     KFDVNMI  The index into the file dope vector to the file's name.
               PARAMETER (KFDVNMI = 1)
 
C     KFDVUNI  The index into the file dope vector to the file's logical
C              unit number.  (A value of -1 means that no unit number is
C              used.)
               PARAMETER (KFDVUNI = 2)
 
C     KFDVBSI  The index  into the  file dope  vector  to  the  size  in
C              computer words  of the  I/O buffer.   (A value of 0 means
C              that no buffer is used.)
               PARAMETER (KFDVBSI = 3)
 
C     KFDVMSI  The index  into the  file dope  vector  to  the  size  in
C              computer words of each family member size.
               PARAMETER (KFDVMSI = 4)
 
C        ( end  of  structure  definition  for  file  dope  vectors )
*CD advdsd
 
 
 
C                                  PURPOSE:
 
C          PARAMETERize the  data-structure definition of all array dope
C     vectors.
 
 
C                                DEFINITION:
 
C          An array  dope vector is a small data structure that contains
C     information about an array.
 
 
C                          PROGRAMMING CONVENTIONS:
 
C          To keep  open long-term maintenance options, always index for
C     entries in  array dope  vectors with  PARAMETERs.   Never use hard
C     constants or  assume any  absolute or  relative  ordering  of  its
C     entries.  (The order is subject to change without notice.)
 
C          To ensure notational uniformity, only these PARAMETERs should
C     be used to index for entries in array dope vectors.
 
C          To ensure  logic uniformity,  all arrays  that are  allocated
C     memory by  the Memory Manager, or contain data that are externally
C     read or written must have array dope vectors.
 
C     ..................................................................
 
 
C     KADVLW   The length in computer words of the array dope vectors.
               PARAMETER (KADVLW = 15)
 
C     KADVPTI  The index  into the  array dope  vector  to  the  array's
C              POINTER.   If the array is allocated memory by the Memory
C              Manager, this  POINTER is  used as  the permanent memory-
C              managed POINTER.
               PARAMETER (KADVPTI = 1)
 
C     KADVNMI  The index into the array dope vector to the array's name.
C              This is  the name  used in the Memory Manager to refer to
C              the array.   It can also be attached to the array when it
C              it written to a file for a NAMELIST-type  of access.   It
C              is a one-computer-word Hollerith string.
               PARAMETER (KADVNMI = 2)
 
C     KADVPRI  The index  into the  array dope  vector  to  the  array's
C              Memory-Manager partition.    This  is  the  name  of  the
C              partition the Memory Manager puts the array in.   It is a
C              one-computer-word Hollerith string.
               PARAMETER (KADVPRI = 3)
 
C     KADVACI  The index  into the  array dope  vector  to  the  array's
C              allocation code.   This code indicates how the allocation
C              length  is  set.    It  is  a one-computer-word Hollerith
C              string.
               PARAMETER (KADVACI = 4)
 
C     KADVALI  The index  into the  array dope  vector  to  the  array's
C              allocation length  in units  of array width.  This is the
C              length  the  array  should  have  the  next  time  it  is
C              allocated memory.   (This  value times  the width  is the
C              size in  computer words  the array  will be  when  it  is
C              allocated.)
               PARAMETER (KADVALI = 5)
 
C     KADVAFI  The index  into the  array dope  vector  to  the  array's
C              allocation flag.   The  array is  allocated memory by the
C              Memory Manager if  its allocation flag is equal to one of
C              the allocation-flag  values set  by the host environment.
C              Arrays that  are allocated  memory by  the   loader  have
C              8Hloader   as the value of the allocation flag.   It is a
C              one-computer-word Hollerith string.
               PARAMETER (KADVAFI = 6)
 
C     KADVAVI  The index  into the  array dope  vector  to  the  array's
C              allocation value.   This  is the  value that is stored in
C              each word of the array at the time the array is allocated
C              memory.
               PARAMETER (KADVAVI = 7)
 
C     KADVMPI  The index  into the array dope vector to the array's move
C              permission.   This value tells whether the Memory Manager
C              has permission  to move an array when it needs to or does
C              not have that permission.  It is an INTEGER quantity.
               PARAMETER (KADVMPI = 8)
 
C     KADVICI  The index  into the  array   dope vector  to the  array's
C              increment counter.   This  is a counter for the number of
C              times the  array is  incremented  in  size  after  it  is
C              originally allocated memory.
               PARAMETER (KADVICI = 9)
 
C     KADVIAI  The index  into the  array dope  vector  to  the  array's
C              increment amount.   This  is the  length in  units of the
C              array's width  that is  added to  the array  after it  is
C              originally allocated memory.
               PARAMETER (KADVIAI = 10)
 
C     KADVWDI  The index into the array dope vector to the array's width
C              in computer  words.  If the array is one DIMENSIONal, the
C              width is  one (1).   If the array is two DIMENSIONal, the
C              width is  the first  DIMENSION.   If the  array is  three
C              DIMENSIONal, the  width is  the product  of the first two
C              DIMENSIONs.
               PARAMETER (KADVWDI = 11)
 
C     KADVNXI  The index into the array dope vector to the array's next-
C              row index.   This  is the index to the next-available row
C              in the array.
               PARAMETER (KADVNXI = 12)
 
C     KADVMXI  The index into the array dope vector to the maximum value
C              the next-row index minus one (1) can have.
               PARAMETER (KADVMXI = 13)
 
C     KADVQII  The index  into the  array dope  vector  to  the  array's
C              quantum increment  in units  of array width.  An integral
C              multiple of this value times the array's width is used as
C              the amount to increase the array when it needs to be made
C              larger.
               PARAMETER (KADVQII = 14)
               PARAMETER (KADVBFI = 15)
 
C       ( end  of  structure  definition  for  array  dope  vectors )
*CD pcb
 
 
C                   Parsing Storage and Control PARAMETERs
 
 
 
C     DBHDPCB  The parsing control block  for parsing dictionary  header
C              lines  and  the  mnemonic  definitions  in  key-formatted
C              dictionaries.
               COMMON /PCB/ DBHDPCB
               INTEGER DBHDPCB(10)
 
C     DBTBPCB  The parsing control block  for  parsing  the  body  of  a
C              dictionary.
               COMMON /PCB/ DBTBPCB
               INTEGER DBTBPCB(10)
 
C     KDBDPRL  The default  rule for parsing  a key-formatted dictionary
C              entry.
               PARAMETER (KDBDPRL = 1)
 
C     KDBNMMN  The  maximum  number  of mnemonics that can be defined in
C              any key-formatted dictionary.
               PARAMETER (KDBNMMN = 1001)
 
C     NUMMN    The number  of  mnemonics  that  have  descriptions  in a
C              key-formatted dictionary.
               COMMON /PCB/ NUMMN
               INTEGER NUMMN
 
C     MNEML    The  list  of  mnemonics  that  have  been  defined  in a
C              dictionary.
               COMMON /PCB/ MNEML
               INTEGER MNEML(KDBNMMN)
 
C     RULEL    The parsing rule for each mnemonic in  MNEML .
               COMMON /PCB/ RULEL
               INTEGER RULEL(KDBNMMN)
 
C     FLAGL    The processing-state flag for each mnemonic in  MNEML .
               COMMON /PCB/ FLAGL
               INTEGER FLAGL(KDBNMMN)
 
C     KDBIHS   The length  in  computer words of the array that contains
C              the unparsed version of each line in the dictionary.
               PARAMETER (KDBIHS = 1001)
 
C     KDBTKN   The maximum number of tokens that can be processed in any
C              one dictionary header.
               PARAMETER (KDBTKN = 20)
 
C     KDBNDEL  The  number  of spacer delineators used in the parsing of
C              the dictionaries.
               PARAMETER (KDBNDEL = 2)
 
C     SPDEL    The spacer  delineators  used  during  the parsing of the
C              dictionaries.
               COMMON /PCB/ SPDEL
               INTEGER SPDEL(KDBNDEL)
               DATA SPDEL /1H , 1H,/
 
C     KDBLONG  The  length  in  computer  words of the longest allowable
C              parsed token.
               PARAMETER (KDBLONG = 1001)
 
C     KDBIDLW  The length in computer words  of the longest allowable id
C              field.
               PARAMETER (KDBIDLW = 10)
 
C                            ( end  of  pcb )
*CD kssdb
 
C      KSSDB    The identification  index  for the  Relational Data Base
C               Management System.
                PARAMETER (KSSDB = 04)
*CD rdbsc
 
 
C                               comdeck  rdbsc
 
 
C                   Relational Data Base Management System
C                          Completion-Status Codes
*CD kdbnoml
 
C     KDBNOML  The SUBROUTINE completed nominally.
               PARAMETER (KDBNOML = 0)
*CD kdbnosk
 
C     KDBNOSK  The  stack  for  the  completion-status  codes  cannot be
C              setup.  (DB1COMP)
               PARAMETER (KDBNOSK = 5)
*CD kdbnphl
 
C     KDBNPHL  The header  line  on  the  dictionary  cannot  be parsed.
C              (DB3HEAD)
               PARAMETER (KDBNPHL = 10)
*CD kdbncdt
 
C     KDBNCDT  There  is  no  "comdeck"  token  in the dictionary header
C              line.  (DB3HEAD)
               PARAMETER (KDBNCDT = 20)
*CD kdbnbyt
 
C     KDBNBYT  There is no "by"  token  in  the  dictionary header line.
C              (DB3HEAD)
               PARAMETER (KDBNBYT = 30)
*CD kdbmdpe
 
C     KDBMDPE  A  mnemonic  description  in  a  key-formatted dictionary
C              cannot be parsed.  (DB3KEYD)
               PARAMETER (KDBMDPE = 40)
*CD kdbudmn
 
C     KDBUDMN  A mnemonic is used in a key-formatted dictionary that has
C              not been defined.  (DB3KEYD)
               PARAMETER (KDBUDMN = 50)
*CD kdbmset
 
C     KDBMSET  A storage block has not been set up.  (DB3KEYD, DB3TBLD)
               PARAMETER (KDBMSET = 60)
*CD kdbmvaf
 
C     KDBMVAF  The  value field given to a storage block has a length of
C              less than one.  (DB3KEYD, DB3TBLD)
               PARAMETER (KDBMVAF = 70)
*CD kdbmidf
 
C     KDBMIDF  The id used  to  store data in a storage block is in use.
C              (DB3KEYD, DB3TBLD)
               PARAMETER (KDBMIDF = 80)
*CD kdbmexp
 
C     KDBMEXP  A block cannot be expanded.  (DB3KEYD, DB3TBLD)
               PARAMETER (KDBMEXP = 90)
*CD kdbnidv
 
C     KDBNIDV  The id mnemonic has not been assigned a value.  (DB3BULD,
C              DB3TBLD)
               PARAMETER (KDBNIDV = 100)
*CD kdbbidh
 
C     KDBBIDH  The value of  an id mnemonic cannot be retrieved from the
C              holding storage block.  (DB3BULD)
               PARAMETER (KDBBIDH = 110)
*CD kdblidv
 
C     KDBLIDV  The  value  of  an  id mnemonic is more than one computer
C              word.  (DB3BULD)
               PARAMETER (KDBLIDV = 120)
*CD kdbnmnv
 
C     KDBNMNV  The value of  a  mnemonic  cannot  be  retrieved from the
C              holding storage block.  (DB3BULD)
               PARAMETER (KDBNMNV = 130)
*CD kdbmidd
 
C     KDBMIDD  The id used to store data in the working storage block is
C              used.
               PARAMETER (KDBMIDD = 140)
*CD kdbapnd
 
C     KDBAPND  An element  in  a  dictionary entry cannot be appended to
C              the entry.  (DB3BULD)
               PARAMETER (KDBAPND = 150)
*CD kdbnpvt
 
C     KDBNPVT  The  value of a Fortran PARAMETER cannot be translated to
C              the binary form.  (DB3PSPA)
               PARAMETER (KDBNPVT = 160)
*CD kdbnolp
 
C     KDBNOLP  There is no left  parenthesis  in  a balanced-parentheses
C              delineated string.  (DB3PSPR)
               PARAMETER (KDBNOLP = 170)
*CD kdbpimp
 
C     KDBPIMP  Parenthesis do not balance.  (DB3PSPR)
               PARAMETER (KDBPIMP = 180)
*CD kdbepml
 
C     KDBEPML  The list of mnemonics in a tabulated dictionary cannot be
C              parsed.  (DB3TBLD)
               PARAMETER (KDBEPML = 190)
*CD kdbnipv
 
C     KDBNIPV  The  id mnemonic  is  not  in  the  mnemonic  list  in  a
C              dictionary.
               PARAMETER (KDBNIPV = 200)
*CD kdbeptr
 
C     KDBEPTR  A  row  in  a  tabulated  dictionary  cannot  be  parsed.
C              (DB3TBLD)
               PARAMETER (KDBEPTR = 210)
*CD kdbbnis
 
C     KDBBNIS  The dictionary name is invalid.  (DB4NSUR)
               PARAMETER (KDBBNIS = 220)
*CD kdbsltl
 
C     KDBSLTL  A dictionary line is too long for the array.  (DB2LINE)
               PARAMETER (KDBSLTL = 230)
*CD kdbndcf
 
C     KDBNDCF  A dictionary form is invalid.
               PARAMETER (KDBNDCF = 240)
*CD kdbieof
 
C     KDBIEOF  An  end of file  was encountered while reading the source
C              file.  (DB2LINE)
               PARAMETER (KDBIEOF = 250)
*CD kdbbpro
 
C     KDBBPRO  The directory protocol has not been set up.  (DB3RDIC)
               PARAMETER (KDBBPRO = 270)
*CD kdbnewd
 
C     KDBNEWD  The dictionary name is new.  (DB3RDIC)
               PARAMETER (KDBNEWD = 280)
*CD kdbsize
 
C     KDBSIZE  The working storage block cannot be sized to the compiled
C              dictionary.
               PARAMETER (KDBSIZE = 290)
*CD kdbnodl
 
C     KDBNODL  The memory for a storage block cannot be deallocated.
               PARAMETER (KDBNODL = 295)
*CD kdbsbmf
 
C     KDBSBMF  Storage Block Manager error.  (DB2GETD, DB2SETP)
               PARAMETER (KDBSBMF = 300)
*CD kdbsbms
 
C     KDBSBMS  Storage Block Manager error.  (DB3TBLD)
               PARAMETER (KDBSBMS = 310)
*CD kdbnatt
 
C     KDBNATT  An attribute  is  not  available  for  the indicated  id.
C              (DB2GETD, DB2SETP)
               PARAMETER (KDBNATT = 320)
*CD kdblndr
 
C     KDBLNDR  The directory is  longer  that the space  allowed  on the
C              file for it.
               PARAMETER (KDBLNDR = 330)
*CD kdbbdsb
 
C     KDBBDSB  The  indicated  storage  block   cannot  be  initialized.
C              (DB4NSUR)
               PARAMETER (KDBBDSB = 340)
*CD kdbrule
 
C     KDBRULE  The indicated parsing rule for a key-formatted dictionary
C              is not recognized.  (DB3KEYD)
               PARAMETER (KDBRULE = 350)
*CD kdbnkey
 
C     KDBNKEY  No key has the indicated attribute.  (DB2GETK)
               PARAMETER (KDBNKEY = 360)
*CD kdbnoen
 
C     KDBNOEN  There is  no  entry stored under the  indicated attribute
C              mnemonic (DB2GETD) or id index (DB2GTDX).
               PARAMETER (KDBNOEN = 380)
*CD kdbnuld
 
C     KDBNULD  The dictionary of interest is empty.  (DB2GTDX)
               PARAMETER (KDBNULD = 390)
*CD kdbnulm
 
C     KDBNULM  The attribute map is missing.
               PARAMETER (KDBNULM = 400)
*CD kdboobs
 
C     KDBOOBS  The entry index is out of bounds.
               PARAMETER (KDBOOBS = 410)
*CD kdbidtl
 
C     KDBIDTL  The id field is too long for the id scratch array.
               PARAMETER (KDBIDTL = 420)
*CD kdbmdel
 
C     KDBMDEL  The indicated storage block's memory cannot be released.
               PARAMETER (KDBMDEL = 430)
*CD rdbscx
C                             ( end  of  rdbsc )
*CD machine
 
 
C                           comdeck machine
 
 
C                 PARAMETERize Cray-Machine  and CTSS
C                     System-Dependent Quantities.
 
 
C                       CRAY Machine Quantities
 
C     KNCPW    The number of characters per computer word.
               PARAMETER (KNCPW = 8)
 
C     KNBPW    The number of bits per computer word.
               PARAMETER (KNBPW = 64)
 
 
C                       CTSS System  Quantities
 
C     KWLSECT  The length in computer words of a disk sector.
               PARAMETER (KWLSECT = 512)
 
C                         ( end  of  machine )
*CD xsbdv
 
C     HSBDV    The array dope vector for the holding storage block.
               COMMON /XSBDV/ HSBDV
               real*8 HSBDV(KADVLW)
 
C     HSBPT    The POINTER to the holding storage block.
               EQUIVALENCE (HSBPT, HSBDV(KADVPTI))
               POINTER (HSBPT, HSB)
 
C     WSBDV    The array dope vector for the working storage block.
               COMMON /XSBDV/ WSBDV
               real*8 WSBDV(KADVLW)
 
C     WSBPT    The POINTER to the working storage block.
               EQUIVALENCE (WSBPT, WSBDV(KADVPTI))
               POINTER (WSBPT, WSB)
