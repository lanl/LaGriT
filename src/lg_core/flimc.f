      subroutine flimc(ich1,ich2,ich3,ipmpary,mpno,
     *                   flen,ftet,mode,ivalue)
      implicit none
C
C
C
C#######################################################################
C
C      PURPOSE - RETURN FSET MEMBERS TO LAGRIT
C
C         CHECK AND INTERPETS THE ESET LIMITS THAT ARE INPUT.
C
C      INPUT ARGUMENTS -
C
C         ich1 - must be 'fset'
C         ich2 - must be 'get'
C         ich3 - NAME OF FSET
C         iptmpftet - POINTER TO ARRAY OF ELEMENT FACE INDICES
C         mpno - number of faces in the set
C
C      CHANGE HISTORY -
C
C        $Log: flimc.f,v $
C        Revision 2.00  2007/11/05 19:45:55  spchu
C        Import to CVS
C
CPVCS
CPVCS       Rev 1.21   02 Oct 2007 12:40:28   spchu
CPVCS    original version
C
C#######################################################################
C
C
C
      pointer (ipmpary, mpary(10000000))
      integer mpary,flen,nptsmax,i,num,ibitpos,ierreset,mpno
      integer ftet(flen)
      integer ivalue
C
C
      character*(*) ich1,ich2,ich3
      character*32 name,mode
 
C
C     ******************************************************************
C     INITIALIZE
C
      nptsmax=flen
      do 10 i=1,flen
         mpary(i)=0
   10 continue
C
C     ******************************************************************
C     HANDLE PSET GROUP
C
      if (ich1(1:4) .eq. 'fset') then
         num=flen
         name=ich3
         call getfset(name,ipmpary,ibitpos,num,ierreset,
     *                flen,ftet,mode,ivalue)
         mpno=num
         go to 9999
      endif
9999  return
      end
