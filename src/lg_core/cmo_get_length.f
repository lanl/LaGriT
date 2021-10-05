      subroutine cmo_get_length(att_name,cmo_name,length,irank,
     *                          ierror_return)
C
C
C#######################################################################
C
C      PURPOSE -
C
C         This Routine Computes the new length of Memory Managed Arrays.
C
C      INPUT ARGUMENTS -
C
C         cmo_name - (character) Name of CMO.
C         att_name - (character) Name of the CMO attribute.
C
C      OUTPUT ARGUMENTS -
C
C         length        - (integer) Array Length.
C         irank         - (integer) Array Rank.
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C         $Log: cmo_get_length.f,v $
C         Revision 2.00  2007/11/05 19:45:49  spchu
C         Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:03:06   dcg
CPVCS     
CPVCS    
CPVCS       Rev 1.7   Mon Apr 14 16:41:14 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.6   Mon Nov 18 10:31:54 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS    
CPVCS       Rev 1.5   09/14/95 12:09:42   dcg
CPVCS    replace character literals in call argument lists
CPVCS
CPVCS       Rev 1.4   09/11/95 14:44:04   het
CPVCS    Change to the storage block based CMO stuff.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      include 'cmo_lg.h'
C
C#######################################################################
C
      character*(*) att_name
C
      integer length, irank
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer icscode,il,it,index
C
      character*32 cmo_name, clength, crank,cinter,cpers,cio,ctype
 
C
C#######################################################################
C
C
C
      length=0
      irank=0
c
C.... Get attribute parameters
C
      call cmo_get_attparam(att_name,cmo_name,index,ctype,crank,
     *    clength,cinter,cpers,cio,ierror_return)

      call cmo_get_info(clength,cmo_name,length,il,it,icscode)
      call cmo_get_info(crank,cmo_name,irank,il,it,icscode)
C
      length=max(1,length)
      irank=max(1,irank)
C
      ierror_return=0
C
      return
      end
