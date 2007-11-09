      subroutine set_info_i(attribute, cmo, sblock, option, ivalue,
     &                      icscode)
C
C
C ######################################################################
C
C     PURPOSE -
C
C        Sets the integer value of ATTRIBUTE in storage block SBLOCK
C        for mesh object CMO to VALUE.
c        call cmo_set_info since storage block do not exist
C
C     INPUT ARGUMENTS -
C
C        attribute - (character) Attribute Name.
C        cmo       - (character) Mesh Object Name.
C        sblock    - (character) Storage Block Name.
C        option    - (character) Optional value to return.
C        ivalue    - (integer) Value input.
C
C     OUTPUT ARGUMENTS -
C
C        icscode - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: set_info_i.f,v $
C        Revision 2.00  2007/11/09 20:04:02  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 16:21:16   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.7   Fri Oct 29 16:56:50 1999   dcg
CPVCS    requests for storage block variables that are really cmo
CPVCS    variables sometimes come in as global requests (historic reasons)
CPVCS    so if the global request fails try as sbcmoprm, cmoname
CPVCS    
CPVCS       Rev 1.6   Mon Apr 14 17:01:08 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.5   09/12/95 06:55:22   het
CPVCS    Correct errors with sb_get_info
CPVCS    
CPVCS       Rev 1.4   09/11/95 14:42:58   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS    
CPVCS       Rev 1.3   08/30/95 21:08:56   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS    
CPVCS       Rev 1.2   08/29/95 12:10:44   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.1   08/24/95 08:39:54   dcg
CPVCS    separated get_info and set_info
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
      character*(*) attribute, cmo, sblock, option
      integer ivalue,ilen,itype,icscode
C
C ######################################################################
      call cmo_set_info(attribute,cmo,ivalue,ilen,itype,icscode)
      if(icscode.ne.0) 
     *           call x3d_error('set_info','cmo_set_info')
      return
      end
