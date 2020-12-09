      subroutine cmo_get_attribute_name(cmo,att_index,
     *                     att_name,icscode)
C
C
C ###################################################################
C
C   PURPOSE -
C     return attribute name of the attribute in cmo which is number
C        att_index
C
C   INPUT ARGUMENTS -
C     cmo   name of mesh object
C     att_index   att number
C   OUTPUT ARGUMENTS -
C
C     att_name  name of attribute
C     icscode   completion code (0 for good completion)
C   CHANGE HISTORY
C$Log: cmo_get_attribute_name.f,v $
CRevision 2.00  2007/11/05 19:45:48  spchu
CImport to CVS
C
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

      character *32 cmo, att_name
      integer att_index, icscode

      integer i,natts,ilen,ierror, index, ierr
      character *32 partname,name,isubname
      character *132 logmess

C
      isubname='cmo_get_attribute_name'
      icscode=0
      partname='define_cmo_lg'
      name=cmo
      if(cmo.eq.'-default-') name=partname

      call cmo_exist(cmo,icscode)
      if(icscode.ne.0) then
         write(logmess,'(a,a)')
     *   'CMO_GET_ATTRIBUTE: Mesh Object does not exist: ',cmo
         call writloga('default',0,logmess,0,ierr)
         goto 9999
      endif

C     index returns as -1 if not found
      call cmo_get_index(cmo,index,icscode)
      if(index.lt.0 .or. icscode.ne.0) then

         if(index.lt.0) then
         write(logmess,'(a,a)')
     *   'CMO_GET_ATTRIBUTE: Mesh Object not found: ',cmo
         call writloga('default',0,logmess,0,ierr)
         icscode = index
         endif

C        this should fail silently
C        and just return that index was not found
         if(icscode.ne.0) then
         write(logmess,'(a,i3)')
     *   'CMO_GET_ATTRIBUTE: Index not found ',att_index
         call writloga('default',0,logmess,0,ierr)
         icscode = index
         endif
         goto 9999

      endif

      call mmfindbk('cmo_attlist',name,ipcmo_attlist,ilen,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmfindbk')

      call mmfindbk('cmo_natts',partname,ipcmo_natts,ilen,icscode)
      if (icscode .ne. 0) call x3d_error(isubname,'mmfindbk')

      att_name=cmo_attlist((att_index-1)* number_of_params_per_att+1)
   
9999  return
      end
