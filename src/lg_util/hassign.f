
      subroutine hassign(iunit,ifile,ibuffer)

C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE ASSIGNS (OPENS) A FILE.
C
C     INPUT ARGUMENTS -
C
C        iunit    - UNIT NUMBER FLAG:  =  -1 ==> ROUTINE SHOULD PICK AN
C                                                UNUSED UNIT NUMBER.
C                                      <> -1 ==> TEST THIS SPECIFIC UNIT
C                                                   TO SEE IF IT HAS
C                                                   BEEN USED.
C        ifile    - FILE NAME TO BE ASSIGNED (OPENED).
C        ibuffer  - BUFFER SIZE (=0 ALMOST ALWAYS).
C                 - allow ibuffer to hold ierror on return
C                   since this is how this argument is used
C                   for most code using this subroutine
C                   ibuffer seems to be ignored most times
C
C     OUTPUT ARGUMENTS -
C
C        iunit - THE UNIT NUMBER ASSIGED FOR THIS FILE.
C
C     CHANGE HISTORY -
C
C        $Log: hassign.f,v $
C        Revision 2.00  2007/11/03 00:49:10  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   01/04/95 21:55:08   llt
CPVCS    unicos changes (made by het)
CPVCS    
CPVCS       Rev 1.0   11/10/94 12:41:52   pvcs
CPVCS    Original version.
C
C#######################################################################
C
C args
      character ifile*(*)
      integer iunit
      integer ibuffer

c     The I/O library does not have entry points for handling
c     type INTEGER*8 I/O control list specifiers
c     but keep arguments as integer

      integer*4 iunit4, kunit
      integer ierror
      integer lenmax
      integer icharlnf

      logical opend
C
C#######################################################################
C begin

      ierror=-1
      iunit4 = iunit

      if(len(ifile).eq.0) then
         lenmax=8
C        *** IF THIS HAPPENS, THIS IS "PROABALY" AN INTEGER FIELD WITH A
C               WITH A CHARACTER CONTAINED WITHIN, MAKE AN ASSUMPTION.
      else
         lenmax=icharlnf(ifile)
      endif
      if(iunit4.le.0) then
         ierror=0
         iunit4=nextlun()
         open(unit=iunit4,file=ifile(1:lenmax))
      else
         inquire(iunit4,opened=opend,err=9999)
         if(opend.eqv..false.) then
            ierror=0
            kunit=iunit4
            open(unit=iunit4,file=ifile(1:lenmax))
         else
            ierror=0
            iunit4=nextlun()
            open(unit=iunit4,file=ifile(1:lenmax))
         endif
      endif
      goto 9999
 9999 continue

C     iunit is returned with the file number
C     ibuffer is returned with error value

      iunit = iunit4
      ibuffer = ierror
      return
      end
