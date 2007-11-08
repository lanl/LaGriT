      subroutine cmo_get_attribute_name(cmo,attribute_index,
     *                     attribute_name,icscode)
C
C
C ###################################################################
C
C   PURPOSE -
C     return attribute name of the attribute in cmo which is number
C        attribute_index
C
C   INPUT ARGUMENTS -
C     cmo   name of mesh object
C     attribute_index   attribute number
C   OUTPUT ARGUMENTS -
C
C     attribute_name  name of attribute
C     icscode   completion code (0 for good completion)
C   CHANGE HISTORY
C$Log:   /pvcs.config/t3d/src/cmo_get_attribute_name_nosb.f_a  $
CPVCS    
CPVCS       Rev 1.2   Mon Feb 07 11:16:32 2000   dcg
CPVCS    
CPVCS       Rev 1.1   Mon Apr 14 16:41:08 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.0   Thu Jan 25 16:25:32 1996   dcg
CPVCS    Initial revision.
C
C##########################################################
C
      implicit none
      include 'cmo_lg.h'
      character *32 cmo, attribute_name, partname,name
      integer icscode,attribute_index, i,natts,ilen,ierror,
     *  index
      character *132 logmess

C
      partname='define_cmo_lg'
      name=cmo
      if(cmo.eq.'-default-') name=partname
      call cmo_get_index(cmo,index,icscode)
      call mmfindbk('cmo_attlist',name,ipcmo_attlist,ilen,icscode)
      call mmfindbk('cmo_natts',partname,ipcmo_natts,ilen,icscode)
      attribute_name=cmo_attlist((attribute_index-1)*
     *     number_of_params_per_att+1)
   
9999  return
      end
