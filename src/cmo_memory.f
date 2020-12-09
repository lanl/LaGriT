      subroutine cmo_memory(cmo_name,number_nodes,number_elements,
     *                      ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine preallocates memory for the Memory Managed
C        arrays for a Mesh Object.
C
C     INPUT ARGUMENTS -
C
C        cmo_name        - (character) Name of the Mesh Object.
C        number_nodes    - (integer) the number of nodes.
C        number_elements - (integer) the number of elements.
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: cmo_memory.f,v $
C        Revision 2.00  2007/11/05 19:45:49  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   21 Jan 2000 17:03:10   dcg
CPVCS     
CPVCS    
CPVCS       Rev 1.4   Mon Apr 14 16:41:28 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.3   Mon Nov 18 10:29:12 1996   dcg
CPVCS    remove charater literals from argument lists
CPVCS    
CPVCS       Rev 1.2   09/11/95 14:44:18   het
CPVCS    Change to the storage block based CMO stuff.
CPVCS    
CPVCS       Rev 1.1   03/15/95 15:23:18   ejl
CPVCS    Finished installing the defaults.
CPVCS    
CPVCS       Rev 1.0   02/16/95 13:53:30   dcg
CPVCS    Original Version
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
      character*(*) cmo_name
C
      integer number_nodes, number_elements
C
      integer ierror_return
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
      integer len, lentype, i,  ierr, ier, icscode, k
      integer irank, length, mmlength,ierror,j,itype,ilen,index
C
      character*32 cname, ctype, clength, crank, cinter,cpers,
     *  cio
C
      integer number_edges, number_faces,natts
C
      character*132 logmess
C
      pointer (ipcmo_pointer, icmo_pointer)
      integer icmo_pointer(10000000)
      character*8 sbname,cattfld
C
C#######################################################################
C
      integer icharlnf
C
C#######################################################################
C
      number_edges=0
      number_faces=0
C
C
      len=icharlnf(cmo_name)
C
      if((cmo_name(1:len).eq.'-cmo-') .or.
     *   (cmo_name(1:len).eq.'-def-')) then
C
C....    Use the Current Mesh Object.
C
         call cmo_get_name(cmo_name,ierror_return)
C
      endif
C
      if(number_of_mesh_objects.gt.1) then
C
C....    Search table for Mesh Object.
C
         call cmo_exist(cmo_name,icscode)
C
      endif
C
      if(icscode.ne.0) then
C
         ierror_return=-1
C
         write(logmess,'(a,a)')
     *            'CMO_MEMORY: Mesh Object does not exist: ', cmo_name
         call writloga('default',0,logmess,0,ierr)
C
      else
C
         ierror_return=0
C
C....    Set the new Length of the Mesh Object Memory Managed Arrays.
C
C
         call cmo_get_info('number_of_attributes',cmo_name,natts,
     *            ilen,itype,icscode)
 
         do j=1,natts
            call cmo_get_attribute_name(cmo_name,j,cname,ier)
            call cmo_get_attparam(cname,cmo_name,index,ctype,crank
     *              ,clength,cinter,cpers,cio,ierror)

            lentype=icharlnf(ctype)
C
            if(ctype(1:1).eq.'V') then
C
C....          Memory Managed Attribute.
C
               call cmo_get_length(cname,cmo_name,length,irank,
     *                             ierror_return)
C
               len=icharlnf(clength)
               if(clength(1:len).eq.'nnodes') then
                  length=max(length,number_nodes)
               elseif(clength(1:len).eq.'nedges') then
                  length=max(length,number_edges)
               elseif(clength(1:len).eq.'nfaces') then
                  length=max(length,number_faces)
               elseif(clength(1:len).eq.'nelements') then
                  length=max(length,number_elements)
               endif
C
               len=icharlnf(crank)
               if(crank(1:len).eq.'nnodes') then
                  irank=max(irank,number_nodes)
               elseif(crank(1:len).eq.'nelements') then
                  irank=max(irank,number_elements)
               endif
C
               mmlength=irank*length
C
               call mmnewlen(cname,
     *                       cmo_name,
     *                       ipcmo_pointer,mmlength,
     *                       ier)
C
               if(ier.ne.0) call cmo_mm_error('cmo_memory')
C
            endif
C
         enddo
C
         call cmo_select(cmo_name,ierror_return)
C
      endif
C
      return
      end
