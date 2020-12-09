      subroutine create_graph(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                ierror_return)
C
C
C#######################################################################
C
C      PURPOSE - Create the adjacency graph for a mesh.
C      If node option is selected, the graph of node adjacency
C      is created, if dual hption is selected, the graph of
C      element adjacency (dual graph) is created. See METIS
C      documentation for description of graph format.
C
C     Command syntax
C
C    create_graph / method / graph_type / offset_vector / adjancy_vector / action
C
C    method = metis - use METIS functions to create graph
C           = lagrit - use LAGriT function to create graph (not implemented)
C
C    graph_type = node - node adjacency graph is created
C               = dual - dual (element) adjacency graph is created
C
C    offset_vector = name of integer vector which holds offset pointers into adjancy vector
C                    default name = nxadj
C
C    adjancy_vector = name of integer vector which holds adjancy information
C                     default name = nadjncy
C
C    action = create - create graph arrays
C           = delete - delete graph arrays
C
C    create_graph / {metis,lagrit]  / [node,dual] / nxadj / nadjncy / create,delete
CC
C    lagrit option not implemented. It is a place holder for future development.
C    The graph for a hybrid dual graph can be created from jtet array.
C
C   NOTE: create_graph options will not work on a hybrid mesh
C         and the only element types supported are tri, tet, quad, hex
C
C      INPUT ARGUMENTS -
C
C         imsgin()  - Integer array of command input tokens
C         xmsgin()  - Real array of command input tokens
C         cmsgin()  - Character array of command input tokens
C         msgtype() - Integer array of command input token types
C         nwds      - Number of command input tokens
C
C      OUTPUT ARGUMENTS -
C
C         ierror_return - Error Return Code (==0 ==> OK, <>0 ==> Error)
C
C      CHANGE HISTORY -
C
C      $Log: create_graph.f,v $
C      Revision 2.00  2007/11/05 19:45:51  spchu
C      Import to CVS
C
CPVCS    
CPVCS       Rev 1.0   01 Nov 2002 13:04:32   gable
CPVCS    Initial revision.
C
C#######################################################################
C
      implicit none
C
C#######################################################################
C
C  Subroutine argument variables
C
      integer nwds, imsgin(nwds), msgtype(nwds)
      REAL*8 xmsgin(nwds)
      character*(*) cmsgin(nwds)
      integer ierror_return
C
C#######################################################################
C
      character*132 logmess
      character*256 cmdmess
      character*12   char_number, char_number2
      integer ierr, len, itype, iflag, indx
      integer ifactor
      integer if_node_1_dual_2, n_vertices_graph,numflag
      integer if_create_1_delete_2
      integer isize_nxadj,     isize_nadjncy,     irank_nadjncy
      integer isize_nxadj_set, isize_nadjncy_set, irank_nadjncy_set
C
C#######################################################################
C
      integer icharlnf
C
      character*32 isubname
C
C     Character variables for adjacency arrays
C
      character*32 ch_nadjncy,ch_nxadj,ch_isize_nxadj
      character*32 ch_isize_nadjncy, ch_irank_nadjncy
C
C     Metis  Element Types triangle=1 tetrahedra=2 hexahedra=3 quadrilateral=4
C     LaGriT Element Types triangle=3 tetrahedra=5 hexahedra=8 quadrilateral=4
C
C     Make a mapping between LaGriT element type and Metis element type.
C
      integer metis_element_type(8)
C
C#######################################################################
C
C     Mesh Object Arrays and Variables
C
      character*32 cmo_name
C
      integer nnode, nelem
C
      pointer (ipitet, itet)
      integer itet(10000000)
      pointer (ipitetoff, itetoff)
      integer itetoff(10000000)
      pointer (ipnxadj, nxadj)
      integer nxadj(10000000)
      pointer (ipnadjncy, nadjncy)
      integer nadjncy(10000000)
      pointer (ipitettyp, itettyp)
      integer itettyp(10000000)
C
C#######################################################################
C
C     Set some defaults and constants
C
C     Mapping between LAGriT element types and element types.
C
      data metis_element_type / 0, 0, 1, 4, 2, 0, 0, 3 /
C
C     METIS flag to indicate Fortran-style arrays start at 1
C
      data numflag / 1 /
C
C#######################################################################
C
C     Begin create_graph
C
C#######################################################################
C
      isubname = 'create_graph'
C
C     Get name of current MO
C
      call cmo_get_name(cmo_name,ierr)
      if(ierr .ne. 0)then
         logmess = 'CREATE_GRAPH ERROR: No active MO found'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = -1
         go to 9999
      endif
C
C#######################################################################
C
C     Parse and check the second argument
C
      indx = 2
      if(cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. 'metis')then

      else
        logmess='CREATE_GRAPH ERROR: 2th argument requires /metis/'
        call writloga('default',0,logmess,0,ierr)
        ierror_return = -2
        go to 9999
      endif
C
C#######################################################################
C     Parse and check the third argument
C
      indx = 3
      if(cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. 'node')then
        if_node_1_dual_2 = 1
      elseif(cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. 'dual')then
        if_node_1_dual_2 = 2
      else
        logmess='CREATE_GRAPH ERROR: '
        call writloga('default',0,logmess,0,ierr)
        logmess=' ERROR: 3rd argument options /node/ or /dual/'
        call writloga('default',0,logmess,0,ierr)
        ierror_return = -2
        go to 9999
      endif
C
C#######################################################################
C
C    Parse and check the fourth and fifth argument
C
C     Create the appropriate attributes names for output variables.
C
C***********************************************************************
C     create_graph needs nxadj, nadjncy
C
      if(if_node_1_dual_2 .eq. 1)then
          ch_isize_nxadj   = 'isize_nxadj_n'
	  ch_irank_nadjncy = 'irank_nadjncy_n'
	  ch_isize_nadjncy = 'nnodes'
      else
         ch_isize_nxadj   = 'isize_nxadj_e'
	 ch_irank_nadjncy = 'irank_nadjncy_e'
	 ch_isize_nadjncy = 'nelements'
      endif
C
      indx = 4
      if((nwds .ge. indx) .and. (msgtype(indx) .ne. 3))then
         logmess = 'CREATE_GRAPH ERROR: Invalid 4th argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'CREATE_GRAPH ERROR: Requires character variable'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
      endif
      if((nwds .lt. indx) .or.
     1   (cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. '-def-')) then
         if(if_node_1_dual_2 .eq. 1)then
           ch_nxadj = 'nxadj_n'
	 else
	   ch_nxadj = 'nxadj_e'
	 endif
      else
         ch_nxadj = cmsgin(indx)(1:icharlnf(cmsgin(indx)))
      endif
C
      indx = 5
       if((nwds .ge. indx) .and. (msgtype(indx) .ne. 3))then
         logmess = 'CREATE_GRAPH ERROR: Invalid 5th argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'CREATE_GRAPH ERROR: Requires character variable'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
      endif
      if((nwds .lt. indx) .or.
     1   (cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. '-def-')) then
         if(if_node_1_dual_2 .eq. 1)then
           ch_nadjncy = 'nadjncy_n'
         else
           ch_nadjncy = 'nadjncy_e'
         endif
      else
         ch_nadjncy = cmsgin(indx)(1:icharlnf(cmsgin(indx)))
      endif
C
C#######################################################################
C
C    Parse and check the sixth argument
C
      indx = 6
      if((nwds .ge. indx) .and. (msgtype(indx) .ne. 3))then
         logmess = 'CREATE_GRAPH ERROR: Invalid 6th argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'CREATE_GRAPH ERROR: options are create or delete'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
      endif
      if((nwds .lt. indx) .or.
     1   (cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. '-def-')) then
	   if_create_1_delete_2 = 1
      elseif(cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. 'create')then
	   if_create_1_delete_2 = 1
      elseif(cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. 'delete')then
	   if_create_1_delete_2 = 2
      endif
C
C
      if(if_create_1_delete_2 .eq. 2)then
       cmdmess = 'cmo/delatt/ /'//ch_nxadj//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess = 'cmo/delatt/ /'//ch_nadjncy//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess =
     1   'cmo/delatt/'//cmo_name//'/'//ch_isize_nxadj//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess =
     1   'cmo/delatt/'//cmo_name//'/'//ch_irank_nadjncy//' ; finish'
       call dotask(cmdmess,ierr)
C
C      Exit
C
       go to 9999
       endif
C
C#######################################################################
C
C     Get MO pointers
C
      call cmo_get_info('nnodes',cmo_name,nnode,len,itype,ierr)
      call cmo_get_info('nelements',cmo_name,nelem,len,itype,ierr)
      call cmo_get_info('itettyp',cmo_name,ipitettyp,len,itype,ierr)
      call cmo_get_info('itetoff',cmo_name,ipitetoff,len,itype,ierr)
      call cmo_get_info('itet',cmo_name,ipitet,len,itype,ierr)
C
C#######################################################################
C
C    Make some checks that MO is of a type METIS can handle.
C
C     Check that it is an element type Metis supports.
C
      if(metis_element_type(itettyp(1)) .eq. 0)then
        logmess = 'CREATE_GRAPH ERROR: Element type not supported'
        call writloga('default',0,logmess,0,ierr)
        return
      endif
C
C     Check that mesh is not hybrid by checking that all values of itettyp
C     are equal.
C
      call check_int_vector_equal(nelem,itettyp,iflag)
      if(iflag .ne. 0)then
C
C       If iflag is not zero then it must be a hybrid mesh.
C       METIS will not handle a hybrid mesh.
C
        logmess = 'CREATE_GRAPH ERROR: Hybrid mesh not supported'
        call writloga('default',0,logmess,0,ierr)
        return
      endif

C
C#######################################################################
C
C     Check for existance of and allocate if necessary the nxadj, nadjncy arrays.
C
C     Figure out the size various arrays will require.
C
        if(if_node_1_dual_2 .eq. 1)then
          isize_nxadj_set = nnode + 1
	  write(char_number2,'(i12)')isize_nxadj_set
       elseif(if_node_1_dual_2 .eq. 2)then
          isize_nxadj_set = nelem + 1
	  write(char_number2,'(i12)')isize_nxadj_set
       endif
C
C     Figure out the size of the nadjncy array based on
C     element type (tri, quad, tet, hex)
C     graph type (node, dual)
C
      ifactor = 2
C
      if    (itettyp(1) .eq. 3 .and. if_node_1_dual_2 .eq. 1)then
         isize_nadjncy_set = ifactor*6*nnode  ! triangles, node mesh
	 irank_nadjncy_set = ifactor*6
	 write(char_number,'(f6.2)')6.0
      elseif(itettyp(1) .eq. 3 .and. if_node_1_dual_2 .eq. 2)then
         isize_nadjncy_set = 3*nelem  ! triangles, dual mesh
	 irank_nadjncy_set = 3
	 write(char_number,'(f6.2)')3.0

      elseif(itettyp(1) .eq. 4 .and. if_node_1_dual_2 .eq. 1)then
         isize_nadjncy_set = ifactor*4*nnode  ! quads, node mesh
	 irank_nadjncy_set = ifactor*4
	 write(char_number,'(f6.2)')4.0
      elseif(itettyp(1) .eq. 4 .and. if_node_1_dual_2 .eq. 2)then
         isize_nadjncy_set = 4*nelem  ! quads, dual mesh
	 irank_nadjncy_set = 4
	 write(char_number,'(f6.2)')4.0

      elseif(itettyp(1) .eq. 5 .and. if_node_1_dual_2 .eq. 1)then
         isize_nadjncy_set = ifactor*15*nnode  ! tets, node mesh
	 irank_nadjncy_set = ifactor*15
	 write(char_number,'(f6.2)')15.0
      elseif(itettyp(1) .eq. 5 .and. if_node_1_dual_2 .eq. 2)then
         isize_nadjncy_set = 4*nelem  ! tets, dual mesh
	 irank_nadjncy_set = 4
	 write(char_number,'(f6.2)')4.0

      elseif(itettyp(1) .eq. 8 .and. if_node_1_dual_2 .eq. 1)then
         isize_nadjncy_set = ifactor*6*nnode  ! hex, node mesh
	 irank_nadjncy_set = ifactor*6
	 write(char_number,'(f6.2)')12.0
      elseif(itettyp(1) .eq. 8 .and. if_node_1_dual_2 .eq. 2)then
         isize_nadjncy_set = 6*nelem  ! hex, dual mesh
	 irank_nadjncy_set = 6
	 write(char_number,'(f6.2)')12.0
      endif
C
C     Right now the 'node mesh' case is just a guess. There are clearly
C     cases where the sizes set above are too small, such as a case where
C     you have N triangles incident on a single point and N is a large number.
C
C     Check to see if the adjacency array exists,
C     if it doesn't, create it.
C
        call cmo_get_info
     1    (ch_isize_nxadj,cmo_name,isize_nxadj,len,itype,ierr)
C
C     NOTE: Since there are dotask calls below, one can no longer use
C           the subroutine argument variables. They are changed by the
C           dotask calls.
C
C     If the isize_nxadj is different than isize_nxadj_set then deallocate
C     and reallocate the nxadj array.
C
      if((isize_nxadj .ne. isize_nxadj_set) .and. (ierr .eq. 0))then
C
C        If you are here, it means isize_nxadj already exists but is the
C        wrong size. Just deallocate arrays and reallocate arrays of the
C        correct size below.
C
C       NOTE: should be able to delete with two calls to DELATT
C             the rank and size parameters, but that appears to
C             not be working. Use lower case delatt
C
       cmdmess = 'cmo/delatt/ /'//ch_nxadj//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess = 'cmo/delatt/ /'//ch_nadjncy//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess =
     1   'cmo/delatt/'//cmo_name//'/'//ch_isize_nxadj//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess =
     1   'cmo/delatt/'//cmo_name//'/'//ch_irank_nadjncy//' ; finish'
       call dotask(cmdmess,ierr)
C
C        Set ierr to non-zero value so allocation will occur below.
C
         ierr = -1
      endif
C
        call cmo_get_info
     1    (ch_irank_nadjncy,cmo_name,irank_nadjncy,len,itype,ierr)
C
C     If the irank_nadjncy is different than irank_nadjncy_set then deallocate
C     and reallocate the nxadj array.
C
      if((irank_nadjncy .ne. irank_nadjncy_set) .and. (ierr .eq. 0))then
C
C        If you are here, it means irank_nxadj already exists but is the
C        wrong size. Just deallocate arrays and reallocate arrays of the
C        correct size below.
C
       cmdmess = 'cmo/delatt/ /'//ch_nxadj//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess = 'cmo/delatt/ /'//ch_nadjncy//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess =
     1   'cmo/delatt/'//cmo_name//'/'//ch_isize_nxadj//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess =
     1   'cmo/delatt/'//cmo_name//'/'//ch_irank_nadjncy//' ; finish'
       call dotask(cmdmess,ierr)
C
C        Set ierr to non-zero value so allocation will occur below.
C
         ierr = -1
      endif

      if (ierr.ne.0) then
         cmdmess ='cmo/addatt//'//ch_isize_nxadj//'/INT' //
     &      '/scalar/scalar/constant/temporary/l/'// char_number2 //
     &      ' ; finish'
         call dotask(cmdmess,ierr)
         call cmo_get_info
     1    (ch_isize_nxadj,cmo_name,isize_nxadj,len,itype,ierr)
       endif
      
      call cmo_get_info(ch_nxadj,cmo_name,ipnxadj,len,itype,ierr)
      if(ierr .ne. 0)then
          cmdmess='cmo/addatt/' //
     &      '/' //
     &      ch_nxadj(1:icharlnf(ch_nxadj)) //
     &      '/VINT' //
     &      '/scalar/'//ch_isize_nxadj//'//temporary/l/0.0' //
     &      ' ; finish'
       ierr = 0
       call dotask(cmdmess,ierr)
       call cmo_get_info(ch_nxadj,cmo_name,ipnxadj,len,itype,ierr)
      endif

      call cmo_get_info
     1    (ch_irank_nadjncy,cmo_name,irank_nadjncy,len,itype,ierr)
      if (ierr.ne.0) then
         cmdmess ='cmo/addatt//'//ch_irank_nadjncy//'/INT' //
     &      '/scalar/scalar/constant/temporary/l/'// char_number //
     &      ' ; finish'
         call dotask(cmdmess,ierr)
         call cmo_get_info
     1    (ch_irank_nadjncy,cmo_name,irank_nadjncy,len,itype,ierr)
       endif

      call cmo_get_info(ch_nadjncy,cmo_name,ipnadjncy,len,itype,ierr)
      if (ierr.ne.0) then
         cmdmess='cmo/addatt//' //
     &      ch_nadjncy(1:icharlnf(ch_nadjncy)) //
     &      '/VINT/'//ch_irank_nadjncy//'/'//ch_isize_nadjncy//
     &      '//temporary/l/0.0'//
     &      ' ; finish'

       call dotask(cmdmess,ierr)
       call cmo_get_info(ch_nadjncy,cmo_name,ipnadjncy,len,itype,ierr)
      endif
C
C     *****************************************************************
C
        if(if_node_1_dual_2 .eq. 1)then
C
C METIS_MeshToNodal
	  call metis_meshtonodal(nelem,nnode,itet,
     1         metis_element_type(itettyp(1)),numflag,nxadj,nadjncy)
          n_vertices_graph = nnode
C
	elseif(if_node_1_dual_2 .eq. 2)then
C METIS_MeshToDual
	  call metis_meshtodual(nelem,nnode,itet,
     1         metis_element_type(itettyp(1)),numflag,nxadj,nadjncy)
          n_vertices_graph = nelem
	endif
C     *****************************************************************
C
C     Release any temporary memory associated with partition isubname
C
 9999 continue
      call mmrelprt(isubname,ierr)

      return
      end
