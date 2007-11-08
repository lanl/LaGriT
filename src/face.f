*dk,face
      subroutine face(num,i1,i2,j1,j2,j3)
C
       implicit real*8 (a-h,o-z)
C #####################################################################
C
C     PURPOSE -
C
C        This subroutine returns the face variables
C
C     INPUT ARGUMENTS -
C
C        num      - the pointer to the face in the tetlist
C
C     OUTPUT ARGUMENTS -
C
C        i1       - the place value of the face (ranges from 1 to 4)
C        i2       - the tet number
C        j1       - the first value in the face
C        j2       - the second value in the face
C        j3       - the third value in the face
C                       in the face (ranges from 1 to 4)
C
C     CHANGE HISTORY -
C
C        $Log:   /pvcs.config/t3d/src/face.f_a  $
CPVCS    
CPVCS       Rev 1.6   30 Sep 2004 09:53:58   dcg
CPVCS    use iand in place of .and. with integers
CPVCS    
CPVCS       Rev 1.5   05 Jan 2001 12:55:54   dcg
CPVCS    use dble in place of float so as not to lose digits
CPVCS
CPVCS       Rev 1.4   Mon Apr 14 16:45:10 1997   pvcs
CPVCS    No change.
CPVCS
CPVCS       Rev 1.3   12/02/94 15:04:50   het
CPVCS    Added an option for the "cmo" access functions
CPVCS
CPVCS
CPVCS       Rev 1.2   12/01/94 18:46:24   het
CPVCS    Added a data type to the "cmo" calles
CPVCS       and added the "cmo.h" include file.
CPVCS
CPVCS       Rev 1.1   11/17/94 21:50:30   het
CPVCS    Added include files for chydro, neibor, cmerge, comdict. Added calles and
CPVCS    pointer statements for current_mesh_object database access.
CPVCS
CPVCS       Rev 1.0   11/10/94 12:11:42   pvcs
CPVCS    Original version.
C
C #####################################################################
C
      include "cmo.h"
      include "chydro.h"
      include "neibor.h"
C
C ######################################################################
C
C
C
C     ******************************************************************
C     FETCH MESH OBJECT NAME AND POINTER INFORMATION.
C
      if(icmoget.eq.1) then
C
      call cmo_get_name(cmo,ierror)
C
      call cmo_get_info('itet',cmo,ipitet,lenitet,icmotype,ierror)
      endif
C
C     ******************************************************************
C
C
      i1=iand((num-1),maskface)+1
      i2=0.25*dble(num)+0.9
      j1=itet(iflist(3*i1-2),i2)
      j2=itet(iflist(3*i1-1),i2)
      j3=itet(iflist(3*i1  ),i2)
      goto 9999
 9999 continue
      return
      end
