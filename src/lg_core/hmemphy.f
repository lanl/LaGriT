*dk,hmemphy
      subroutine hmemphy
       implicit real*8 (a-h,o-z)
C
C#######################################################################
C
C     PURPOSE -
C
C        THIS ROUTINE DRIVES THE GENERAL MEMORY MANAGER ROUTINE TO
C        INITIALIZE AND ADJUST THE MEMORY OF THIS CODE.
C
C
C     INPUT ARGUMENTS -
C
C        NONE
C
C
C     OUTPUT ARGUMENTS -
C
C        NONE
C
C
C     CHANGE HISTORY -
C
C        $Log: hmemphy.f,v $
C        Revision 2.00  2007/11/05 19:45:58  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.3   Mon Apr 14 16:51:14 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.2   12/01/94 18:57:20   het
CPVCS    Added a variable type to the "cmo" calles
CPVCS       and include the "cmo.h" include file.
CPVCS    
CPVCS
CPVCS       Rev 1.1   11/17/94 22:06:48   het
CPVCS    Major modifications associated with the changes to the reconnec
CPVCS    routines. Getting ready for the change to current_mesh_object w
CPVCS    of managing memeory.
CPVCS
CPVCS
CPVCS       Rev 1.0   11/10/94 12:15:16   pvcs
CPVCS    Original version.
C
C#######################################################################
C
      include "chydro.h"
C
C#######################################################################
C
      character*8 inodes(20)
C
C#######################################################################
C
C
C     ******************************************************************
C
C     DEFINE NODES TO BE SET UP AND INITIALIZE MEMORY.
C
      inode=0
      inode=inode+1
      inodes(inode)='nodmesh'
C*****      inode=inode+1
C*****      inodes(inode)='nodeos'
      inode=inode+1
      inodes(inode)='nodphy'
      call hmemadjb(inodes,inode)
C
C     ******************************************************************
C
C     SET UP THE USUAL CFT IMMUNE STATEMENT 9999 IN CASE DDT IS NEEDED.
C
      goto 9999
 9999 continue
C
C     ******************************************************************
C
      return
      end
