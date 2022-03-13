      subroutine get_info_i(attribute, cmo, sblock, option, ivalue,
     &                      icscode)
C
C
C ######################################################################
C
C     PURPOSE -
C
C        Gets the integer value of ATTRIBUTE from storage block SBLOCK
C        for mesh object CMO.  
c        call cmo_get_info or global_get_info since storage blocks
c        no longer exist
C
C     INPUT ARGUMENTS -
C
C        attribute - (character) Attribute Name.
C        cmo       - (character) Mesh Object Name.
C        sblock    - (character) Storage Block Name.
C        option    - (character) Optional value to return.
C
C     OUTPUT ARGUMENTS -
C
C        ivalue  - (integer) Value returned.
C        icscode - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: get_info_i.f,v $
C        Revision 2.00  2007/11/05 19:45:56  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 16:21:00   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.10   Fri Oct 29 16:55:08 1999   dcg
CPVCS    requests for storage block variables that are really cmo
CPVCS    variables sometimes come in as global requests (historic reasons)
CPVCS    so if the global request fails try as sbcmoprm, cmoname
CPVCS    
CPVCS       Rev 1.9   Mon Apr 14 16:49:34 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.8   Fri Apr 26 14:31:52 1996   dcg
CPVCS    check debug flag before printing error messages
CPVCS
CPVCS       Rev 1.7   Wed Mar 06 16:44:18 1996   dcg
CPVCS    print error messages if idebug=1
CPVCS
CPVCS       Rev 1.6   Tue Jan 09 16:34:28 1996   dcg
CPVCS    return number of attributes if requested
CPVCS
CPVCS       Rev 1.5   09/12/95 06:55:18   het
CPVCS    Correct errors with sb_get_info
CPVCS
CPVCS       Rev 1.4   09/11/95 14:42:54   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS
CPVCS       Rev 1.3   08/30/95 21:08:52   het
CPVCS    Put cmo table data into the cmoatt storage block
CPVCS
CPVCS       Rev 1.2   08/29/95 11:38:52   dcg
CPVCS    set length for names to 40 characters
CPVCS
CPVCS       Rev 1.1   08/24/95 08:39:50   dcg
CPVCS    separated get_info and set_info
CPVCS
CPVCS       Rev 1.0   06/13/95 09:01:48   ejl
CPVCS    Cleaned up msgtty, calling arguments.
CPVCS
C
C ######################################################################
C
      implicit none
C
C ######################################################################
C
C
      include 'chydro.h'
C
      character*(*) attribute, cmo, sblock, option
C
      integer ivalue,ilen,itype
      integer icscode

C
C ######################################################################
C
      call cmo_get_info(attribute,cmo,ivalue,ilen,itype,icscode)
      if(icscode.ne.0) then

      endif
C
      return
      end
