      integer function popcnt(inum)
C
C #####################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE PERFORMS A POP COUNT ON AN INTEGER NUMBER. THE
C           SUM OF BITS IN ARE SUMMED TO RETURN THE TOTAL NUMBER OF
C           BITS TURNED ON.
C
C     INPUT ARGUMENTS -
C
C        inum - THE NUMBER TO BE SCANNED.
C
C     OUTPUT ARGUMENTS -
C
C        popcnt - THE (INTEGER) COUNT OF BITS TURNED ON.
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/popcnt.f_a  $
CPVCS    
CPVCS       Rev 1.1   30 Sep 2004 09:46:20   dcg
CPVCS    use iand in place of .and. for integers
CPVCS    
CPVCS       Rev 1.0   Thu Dec 18 16:17:54 1997   dcg
CPVCS    Initial revision.
C
C ######################################################################
C
      integer inum, inum1
      popcnt=0
      do ipos1=1,32
         inum1=iand(inum,(2**(ipos1-1)))
         nwd1=ishft(inum1,-(ipos1-1))
         popcnt=popcnt+nwd1
      enddo
      return
      end
