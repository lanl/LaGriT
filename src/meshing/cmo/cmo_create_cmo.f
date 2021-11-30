      subroutine cmo_create_cmo(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                          ierror_return)
C
C
C#######################################################################
C
C     PURPOSE -
C
C        This routine creates a Mesh Object.
C
C     INPUT ARGUMENTS -
C
C        imsgin()  - Integer array of command input tokens
C        xmsgin()  - Real array of command input tokens
C        cmsgin()  - Character array of command input tokens
C        msgtype() - Integer array of command input token types
C        nwds      - Number of command input tokens
C
C     OUTPUT ARGUMENTS -
C
C        ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C     CHANGE HISTORY -
C
C        $Log: cmo_create_cmo.f,v $
C        Revision 2.00  2007/11/05 19:45:48  spchu
C        Import to CVS
C
CPVCS    
CPVCS       Rev 1.1   27 Jul 2001 11:27:40   tam
CPVCS    changed call to cmo_mesh_type to cmo_set_mesh_type
CPVCS    
CPVCS       Rev 1.0   24 Jan 2000 13:24:38   dcg
CPVCS    Initial revision.
CPVCS    
CPVCS       Rev 1.6   Mon Apr 14 16:40:10 1997   pvcs
CPVCS    No change.
CPVCS    
CPVCS       Rev 1.5   03/15/95 15:22:36   ejl
CPVCS    Finished installing the defaults.
CPVCS
CPVCS       Rev 1.4   02/23/95 14:52:14   ejl
CPVCS    Fixed so defaults will work:
CPVCS    COMMAND////input//.
CPVCS
CPVCS       Rev 1.3   02/22/95 09:31:00   ejl
CPVCS
CPVCS
CPVCS       Rev 1.2   02/16/95 16:11:32   ejl
CPVCS
CPVCS
CPVCS       Rev 1.1   02/16/95 14:40:16   ejl
CPVCS    Fixed error in if statement in defaults.
CPVCS
CPVCS
CPVCS       Rev 1.0   02/16/95 13:54:44   dcg
CPVCS    Original Version
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      integer ierror_return
      REAL*8 xmsgin(nwds)
      character*32 cmsgin(nwds)
C
C
C#######################################################################
C
C     LOCAL VARIABLE DEFINITION
C
C
      integer len, ierror, number_nodes, number_elements

      integer icharlnf

      character*32 cmo_name
      character*32 isubname
C
C
C#######################################################################
C BEGIN begin
C
C
C.... Mesh Object Name.
C
      isubname='cmo_create'
      cmo_name=cmsgin(3)
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
C.... Create the Mesh Object from the Default Mesh Object.
C
      number_elements=0
      number_nodes=0
      call cmo_create(cmo_name,ierror_return)
C
      if((ierror_return.eq.0) .and. (nwds.gt.3)) then
C
         if((nwds.ge.4) .and. (cmsgin(4)(1:5).ne.'-def-') .and.
     *                        (cmsgin(4)(1:9).ne.'-default-')) then
            number_nodes=imsgin(4)
         else
            number_nodes=0
         endif
C
         if((nwds.ge.5) .and. (cmsgin(5)(1:5).ne.'-def-') .and.
     *                        (cmsgin(5)(1:9).ne.'-default-')) then
            number_elements=imsgin(5)
         else
            number_elements=0
         endif
C
         if(msgtype(6).eq.3) then
C
C....       Mesh Type Specified.
C
            call cmo_set_mesh_type(cmo_name, cmsgin(6), ierror_return)
C
         else
C
C....       Store data in the Mesh Object.
C
            if((nwds.ge.6) .and. (cmsgin(6)(1:5).ne.'-def-') .and.
     *                           (cmsgin(6)(1:9).ne.'-default-')) then
               call cmo_set_info('ndimensions_geom',cmo_name,imsgin(6),
     *                           1,1,ierror)
            endif
C
            if((nwds.ge.7) .and. (cmsgin(7)(1:5).ne.'-def-') .and.
     *                           (cmsgin(7)(1:9).ne.'-default-')) then
               call cmo_set_info('ndimensions_topo',cmo_name,imsgin(7),
     *                           1,1,ierror)
            endif
C
            if((nwds.ge.8) .and. (cmsgin(8)(1:5).ne.'-def-') .and.
     *                           (cmsgin(8)(1:9).ne.'-default-')) then
               call cmo_set_info('nodes_per_element',cmo_name,imsgin(8),
     *                           1,1,ierror)
            endif
C
            if((nwds.ge.9) .and. (cmsgin(9)(1:5).ne.'-def-') .and.
     *                           (cmsgin(9)(1:9).ne.'-default-')) then
               call cmo_set_info('faces_per_element',cmo_name,imsgin(9),
     *                           1,1,ierror)
            endif
C
            if((nwds.ge.10) .and. (cmsgin(10)(1:5).ne.'-def-') .and.
     *                            (cmsgin(10)(1:9).ne.'-default-')) then
               call cmo_set_info('edges_per_element',cmo_name,
     *                           imsgin(10),1,1,ierror)
            endif
C
         endif
C
C....    Set all Memory Managed array lengths.
C

         call cmo_memory(cmo_name,number_nodes,number_elements,
     *                   ierror_return)
C
      endif

      if (ierror_return.eq.0) then
      call cmo_set_info('nelements',cmo_name,number_elements,1,1,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_set_info nelements')

      call cmo_set_info('nnodes',cmo_name,number_nodes,1,1,ierror)
      if(ierror.ne.0) call x3d_error(isubname,'cmo_set_info nodes')

      endif 
C
      return
      end
