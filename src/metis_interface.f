      subroutine metis_interface(imsgin,xmsgin,cmsgin,msgtype,nwds,
     *                ierror_return)
C
C#######################################################################
C
C      PURPOSE -
C
C         Interface METIS with LAGriT data structures.
C
C         For details see:
C         http://www-users.cs.umn.edu/~karypis/metis
C
C     Command syntax
C
C For PARTITION
C
C    metis / partition / metis_method / [node, dual] / npartitions / inodeprt / ielemprt
C    default name for node partition vector = inodeprt
C    default name for element partition vector = ielemprt
C
C    Where metis_method can be:
C                metis_partmeshnodal
C                metis_partmeshdual
C
C                metis_partgraphrecursive (not implemented)
C                metis_partgraphkway (not implemented)
C                metis_partgraphvkway (not implemented)
C                metis_mcpartgraphrecursive (not implemented)
C                metis_mcpartgraphkway (not implemented)
C                metis_wpartgraphrecursive (not implemented)
C                metis_wpartgraphkway (not implemented)
C                metis_wpartgraphvkway (not implemented)
C
C    Example:
C    metis / partition / metis_partmeshnodal / node / npartitions / inodeprt / ielemprt
C    metis / partition / metis_partmeshdual  / dual / npartitions / inodeprt / ielemprt
C    metis / partition / metis_partmeshnodal / node / npartitions / -def- / -def-
C    metis / partition / metis_partmeshnodal / node / npartitions
C
C    What it does: Adds arrays inodeprt and ielemprt to MO. These are integer
C                  arrays with the partition number (starting at 1) of each
C                  node and element in MO.
C
C For REORDER without weights
C
C   metis / reorder / [metis_edgend, mmetis_nodend]  / [node, dual] / [iperm] / [invperm] /
C
C For REORDER with weights
C
C   metis / reorder / mmetis_nodewnd  / [node, dual] / [iperm] / [invperm] / ivert_weight
C
C   What it does: Adds arrays iperm, invperm to MO.
C                 iperm and invperm are permutation vectors
C                 to be used to reorder the nodes and/or elements of
C                 the mesh using the LAGriT reorder command,
C                 reorder / iperm
C
C     Argument 9:
C      ivert_weight is a user supplied integer array that specifies the
C                   weight of the verticies
C
C      If metis_job = partition argument 5 is integer npartition
C      If metis_job = reorder argument 5 is character [node,dual]
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
C      $Log:   /pvcs.config/t3d/src/metis_interface.f_a  $
CPVCS    
CPVCS       Rev 1.0   01 Nov 2002 13:05:36   gable
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
C
      integer ierror_return
C
C#######################################################################
C
      character*132 logmess
      character*256 cmdmess
      character*12   char_number, char_number2
      integer i, ierr, ifound, len, itype, iflag, indx
      integer if_node_1_dual_2, n_vertices_graph,numflag
      integer if_create_graph
      integer isize_nxadj,     isize_nadjncy,     irank_nadjncy
      integer isize_nxadj_set, isize_nadjncy_set, irank_nadjncy_set
C
C#######################################################################
C
      integer icharlnf
C
      character*32 isubname
      character*32 metis_option(16)
      character*32 metis_job
C
C     Character variables for partition, permutation and adjacency arrays
C
      character*32 ch_inodeprt, ch_ielemprt
      Character*32 ch_iperm,    ch_invperm
      character*32 ch_nadjncy,  ch_nxadj
      character*32 ch_ivert_weight
C
      integer metis_option_id, metis_job_id
      integer nmetis_options
      integer npartitions, nedge_cut
C
C     Metis  Element Types triangle=1 tetrahedra=2 hexahedra=3 quadrilateral=4
C     LaGriT Element Types triangle=3 tetrahedra=5 hexahedra=8 quadrilateral=4
C
C     Make a mapping between LaGriT element type and Metis element type.
C
      integer metis_element_type(8)
C
C     Pass option flags using array. Only some METIS routines use this.
C
      integer metis_option_flag(16,5)
C
C#######################################################################
C
C     Mesh Object Arrays and Variables
C
      character*32 cmo_name
      integer nnode, nelem
C
      pointer (ipitet, itet)
      integer itet(10000000)
      pointer (ipitetoff, itetoff)
      integer itetoff(10000000)
      pointer (ipinode_part, inode_part)
      integer inode_part(10000000)
      pointer (ipielem_part, ielem_part)
      integer ielem_part(10000000)
      pointer (ipnxadj, nxadj)
      integer nxadj(10000000)
      pointer (ipnadjncy, nadjncy)
      integer nadjncy(10000000)
      pointer (ipitettyp, itettyp)
      integer itettyp(10000000)
      pointer (ipiperm, iperm)
      integer iperm(10000000)
      pointer (ipinvperm, invperm)
      integer invperm(10000000)
      pointer (ipivert_weight, ivert_weight)
      integer ivert_weight(10000000)
C
C#######################################################################
C
C     Set defaults and constants
C
      data ch_nadjncy  / 'nadjncy' /
      data ch_nxadj    / 'nxadj' /
      data ch_iperm    / 'iperm' /
      data ch_invperm  / 'invperm' /
      data ch_inodeprt / 'inodeprt' /
      data ch_ielemprt / 'ielemprt' /
C
C     List of valid metis options
C
      data nmetis_options / 16 /
      data metis_option (1) /'metis_partgraphrecursive'/
      data metis_option (2) /'metis_partgraphkway'/
      data metis_option (3) /'metis_partgraphvkway'/
      data metis_option (4) /'metis_mcpartgraphrecursive'/
      data metis_option (5) /'metis_mcpartgraphkway'/
      data metis_option (6) /'metis_wpartgraphrecursive'/
      data metis_option (7) /'metis_wpartgraphkway'/
      data metis_option (8) /'metis_wpartgraphvkway'/
      data metis_option (9) /'metis_partmeshnodal'/
      data metis_option(10) /'metis_partmeshdual'/
      data metis_option(11) /'metis_edgend'/
      data metis_option(12) /'metis_nodend'/
      data metis_option(13) /'metis_nodewnd'/
      data metis_option(14) /'metis_meshtonodal'/
      data metis_option(15) /'metis_meshtodual'/
      data metis_option(16) /'metis_estimatememory'/
C
C     Mapping between LAGriT element types and METIS element types.
C
      data metis_element_type / 0, 0, 1, 4, 2, 0, 0, 3 /
C
C     METIS flag to indicate Fortran-style arrays start at 1
C
      data numflag / 1 /
C
      data if_create_graph / 0 /
C
C     This array is used to pass options to METIS. Setting the first
C     entry to zero will result in METIS using defaults. Only some
C     options actually use this array.
C
      data metis_option_flag(1,1) / 0 /
      data metis_option_flag(2,1) / 0 /
      data metis_option_flag(3,1) / 0 /
      data metis_option_flag(4,1) / 0 /
      data metis_option_flag(5,1) / 0 /
      data metis_option_flag(6,1) / 0 /
      data metis_option_flag(7,1) / 0 /
      data metis_option_flag(8,1) / 0 /
      data metis_option_flag(9,1) / 0 /
      data metis_option_flag(10,1) / 0 /
      data metis_option_flag(11,1) / 0 /
      data metis_option_flag(12,1) / 0 /
      data metis_option_flag(13,1) / 0 /
      data metis_option_flag(14,1) / 0 /
      data metis_option_flag(15,1) / 0 /
      data metis_option_flag(16,1) / 0 /
C
C#######################################################################
C
C     Begin metis_interface
C
C#######################################################################
C
      isubname = 'metis_interface'
C
C     Get name of current MO
C
      call cmo_get_name(cmo_name,ierr)
      if(ierr .ne. 0)then
         logmess = 'METIS ERROR: No active MO found'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 1
         go to 9999
      endif
C
C#######################################################################
C     Parse and check the second and fourth argument
C
      if(msgtype(2) .eq. 3)then
        if(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'partition')then
           metis_job = cmsgin(2)(1:icharlnf(cmsgin(2)))
	   metis_job_id = 1
C
C     Get the number of partitions
C
          if(msgtype(5) .eq. 1)then
	    npartitions = imsgin(5)
          else
            logmess = 'METIS ERROR: Invalid 5th argument'
            call writloga('default',0,logmess,0,ierr)
            logmess = 'METIS ERROR: With partition option 5th argument'
            call writloga('default',0,logmess,0,ierr)
            logmess = 'METIS ERROR: must be integer number of partions'
            call writloga('default',0,logmess,0,ierr)
	    ierror_return = 1
	    go to 9999
          endif
        elseif(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'reorder')then
           metis_job = cmsgin(2)(1:icharlnf(cmsgin(2)))
	   metis_job_id = 2
	elseif(cmsgin(2)(1:icharlnf(cmsgin(2))) .eq. 'create_graph')then
	   metis_job = cmsgin(2)(1:icharlnf(cmsgin(2)))
	   metis_job_id = 3
	else
	   logmess = 'METIS ERROR: Invalid second argument '
	   call writloga('default',0,logmess,0,ierr)
	   logmess =
     1     'Valid METIS OPTIONS: partition, reorder, create_graph'
	   call writloga('default',0,logmess,0,ierr)
           ierror_return = 1
	   go to 9999
        endif
      else
        logmess = 'METIS ERROR: Second argument must be character'
        call writloga('default',0,logmess,0,ierr)
	logmess =
     1  'Valid METIS OPTIONS: partition, reorder, create_graph'
	call writloga('default',0,logmess,0,ierr)
        ierror_return = 1
	go to 9999
      endif
C
C#######################################################################
C     Parse and check the third argument
C
      i = 1
      ifound = 0
      if(msgtype(3) .eq. 3)then
      dowhile ((ifound .eq. 0) .and. (i .lt. nmetis_options))
      
        if(cmsgin(3)(1:icharlnf(cmsgin(3))) .eq.
     1     metis_option(i)(1:icharlnf(metis_option(i))))then
           ifound = 1
	   metis_option_id = i
        endif
	i = i + 1
      enddo
      endif
      
      if(ifound .eq. 0)then
         logmess = 'METIS ERROR: Invalid third argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'METIS ERROR: Valid METIS option not found'
         call writloga('default',0,logmess,0,ierr)
	 do i = 1, nmetis_options
	   write(logmess,'(a,a32)')'Valid Options: ', metis_option(i)
	   call writloga('default',0,logmess,0,ierr)
	 enddo
         ierror_return = 2
         go to 9999
      endif
C
C#######################################################################
C     Parse and check the fourth argument
C
      if(cmsgin(4)(1:icharlnf(cmsgin(4))) .eq. 'node')then
         if_node_1_dual_2 = 1
      elseif(cmsgin(4)(1:icharlnf(cmsgin(4))) .eq. 'dual')then
         if_node_1_dual_2 = 2
      else
         logmess = 'METIS ERROR: 4th argument must be node or dual'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
      endif
C
C#######################################################################
C
C     Create the appropriate attributes names for METIS output variables.
C
      if(metis_job_id .eq. 1)then
C
C***********************************************************************
C
C     'partition' option need variables inodeprt, ielemprt
C
      indx = 6
      if((nwds .ge. indx) .and. (msgtype(indx) .ne. 3))then
         logmess = 'METIS ERROR: Invalid sixth argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'METIS ERROR: Argument must be a character string'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
      endif
      if((nwds .lt. indx) .or.
     1   (cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. '-def-')) then
         ch_inodeprt = 'inodeprt'
      else
         ch_inodeprt = cmsgin(indx)(1:icharlnf(cmsgin(indx)))
      endif
C
      indx = 7
      if((nwds .ge. indx) .and. (msgtype(indx) .ne. 3))then
         logmess = 'METIS ERROR: Invalid seventh argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'METIS ERROR: Argument must be a character string'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
      endif
      if((nwds .lt. indx) .or.
     1   (cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. '-def-')) then
         ch_ielemprt = 'ielemprt'
      else
         ch_ielemprt = cmsgin(indx)(1:icharlnf(cmsgin(indx)))
      endif
C
      elseif(metis_job_id .eq. 2)then
C
C***********************************************************************
C     'reorder' option needs iperm, invperm, nxadj, nadjncy
C
C     When using metis_nodewnd the user must provide weights for vertices
C     in argument 9
C
      indx = 5
      if((nwds .ge. indx) .and. (msgtype(indx) .ne. 3))then
         logmess = 'METIS ERROR: Invalid 5th argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'METIS ERROR: Argument must be a character string'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
      endif
      if((nwds .lt. indx) .or.
     1   (cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. '-def-')) then
         ch_iperm = 'iperm'
      else
         ch_iperm = cmsgin(indx)(1:icharlnf(cmsgin(indx)))
      endif
C
      indx = 6
       if((nwds .ge. indx) .and. (msgtype(indx) .ne. 3))then
         logmess = 'METIS ERROR: Invalid 6th argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'METIS ERROR: Argument must be a character string'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
      endif
      if((nwds .lt. indx) .or.
     1   (cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. '-def-')) then
         ch_invperm = 'invperm'
      else
         ch_invperm = cmsgin(indx)(1:icharlnf(cmsgin(indx)))
      endif
C
C     Check that a weight vector is specified.
C
      if(metis_option_id .eq. 13)then
      indx = 7
       if((nwds .ge. indx) .and. (msgtype(indx) .ne. 3))then
         logmess = 'METIS ERROR: Invalid 7th argument'
         call writloga('default',0,logmess,0,ierr)
         logmess = 'METIS ERROR: Argument must be a character string'
         call writloga('default',0,logmess,0,ierr)
         ierror_return = 3
         go to 9999
       elseif((nwds .lt. indx) .or.
     1   (cmsgin(indx)(1:icharlnf(cmsgin(indx))) .eq. '-def-')) then
         logmess = 'METIS ERROR: 7th argument must be specified by user'
         call writloga('default',0,logmess,0,ierr)
	 ierror_return = 3
	 go to 9999
       else
         ch_ivert_weight = cmsgin(indx)(1:icharlnf(cmsgin(indx)))
       endif
      endif
C
      endif
C#######################################################################
C
C    All options except 9 and 10 require graph creation
C    Create the graph and get pointers to graph arrays.
C    The graph information is deleted after the partition or reorder
C    arrays are computed.
C
      if((metis_option_id .le. 8) .or.
     1   (metis_option_id .ge. 11))then
	  if_create_graph = 1
	  if(if_node_1_dual_2 .eq. 1)then
	     cmdmess =
     1     'create_graph/metis/node/nx_def/nadj_def; finish'
	  else
	     cmdmess =
     1     'create_graph/metis/dual/nx_def/nadj_def; finish'
	  endif
	  call dotask(cmdmess,ierr)
	  if(ierr .ne. 0)then
	     logmess = 'METIS ERROR: Problem creating graph'
             call writloga('default',0,logmess,0,ierr)
             ierror_return = 3
             go to 9999
          endif
      call cmo_get_info('nx_def',  cmo_name,ipnxadj,  len,itype,ierr)
      call cmo_get_info('nadj_def',cmo_name,ipnadjncy,len,itype,ierr)

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
      
      if(metis_option_id .eq. 13)then
      call cmo_get_info
     1    (ch_ivert_weight,cmo_name,ipivert_weight,len,itype,ierr)
      if(ierr .ne. 0)then
        logmess = 'METIS ERROR: Problem getting weight vector'
        call writloga('default',0,logmess,0,ierr)
        go to 9999
      endif

      if(if_node_1_dual_2 .eq. 1)then
        if(len .ne. nnode)then
          logmess =
     1  'METIS ERROR:Node graph, Weight vector is not the same lenght'
          call writloga('default',0,logmess,0,ierr)
          go to 9999
        endif
      elseif(if_node_1_dual_2 .eq. 1)then
        if(len .ne. nelem)then
          logmess =
     1  'METIS ERROR:Dual graph, Weight vector is not the same lenght'
          call writloga('default',0,logmess,0,ierr)
          go to 9999
        endif
      endif
      endif
C
C#######################################################################
C
C    Make some checks that MO is of a type METIS can handle.
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
        logmess = 'METIS ERROR: Hybrid mesh not supported'
        call writloga('default',0,logmess,0,ierr)
        return
      endif
C
C     Check that it is an element type Metis supports.
C
      if(metis_element_type(itettyp(1)) .eq. 0)then
        logmess = 'METIS ERROR: Element type not supported'
        call writloga('default',0,logmess,0,ierr)
        return
      endif
C
C     NOTE: First call to dotask is below. Do not use any more subroutine
C     argument variables.
C
C#######################################################################
C
      if(if_node_1_dual_2 .eq. 1)then
         n_vertices_graph = nnode
      else
         n_vertices_graph = nelem
      endif
C
C#######################################################################
C
C     Check if METIS output arrays already exist. If they do, check if
C     they are the correct size. If they do not exist, create them.
C
      if(metis_job_id .eq. 1)then
C
C     Check to see if the node partition array exists,
C     if it doesn't, create it.
C
      call cmo_get_info(ch_inodeprt(1:icharlnf(ch_inodeprt)),
     &                  cmo_name,ipinode_part,
     &                  len,itype,ierr)
      if (ierr.ne.0) then
         cmdmess = 'cmo/addatt/-def-/'
     &             // ch_inodeprt(1:icharlnf(ch_inodeprt))
     &             // '/VINT/scalar/nnodes/linear/temporary/agx/0/'
     &             // '; finish'
         ierr = 0
         call dotaskx3d(cmdmess,ierr)
         if(ierr.ne.0) then
            write(logmess,'(a)')'Error in METIS: addatt failed!'
            call writloga('default',0,logmess,0,ierr)
            ierr = 1
            goto 9999
         endif
         call cmo_get_info(ch_inodeprt(1:icharlnf(ch_inodeprt)),
     &                     cmo_name,ipinode_part,
     &                     len,itype,ierr)
      endif
C
C***********************************************************************
C
C     Check to see if the element partition array exists,
C     if it doesn't, create it.
      call cmo_get_info(ch_ielemprt(1:icharlnf(ch_ielemprt)),
     &                  cmo_name,ipielem_part,
     &                  len,itype,ierr)
      if (ierr.ne.0) then
         cmdmess = 'cmo/addatt/-def-/'
     &             // ch_ielemprt(1:icharlnf(ch_ielemprt))
     &             // '/VINT/scalar/nelements/linear/temporary/agx/0/'
     &             // '; finish'
         ierr = 0
         call dotaskx3d(cmdmess,ierr)
         if(ierr.ne.0) then
            write(logmess,'(a)')'Error in METIS: addatt failed!'
            call writloga('default',0,logmess,0,ierr)
            ierr = 1
            goto 9999
         endif
         call cmo_get_info(ch_ielemprt(1:icharlnf(ch_ielemprt)),
     &                     cmo_name,ipielem_part,
     &                     len,itype,ierr)
      endif
C***********************************************************************
      endif
C***********************************************************************

      if(metis_job_id .eq. 1)then
        if(metis_option_id .eq. 9)then
C
C METIS_PartMeshNodal
	 call metis_partmeshnodal(nelem,nnode,itet,
     1        metis_element_type(itettyp(1)),numflag,npartitions,
     2        nedge_cut,ielem_part,inode_part)
        elseif(metis_option_id .eq. 10)then
C
C METIS_PartMeshDual
         call metis_partmeshdual(nelem,nnode,itet,
     1        metis_element_type(itettyp(1)),numflag,npartitions,
     2        nedge_cut,ielem_part,inode_part)
	endif
      endif
CC     *****************************************************************
C
C  NOTE: need to check if itet array is correct order for hex
C
C     *****************************************************************
C     Allocate space for the permutation vectors
C
C   NOTE: No check for existance of iperm and invperm before allocation.
C
      if(metis_job_id .eq. 2)then

      call cmo_get_info(ch_iperm,cmo_name,ipiperm,len,itype,ierr)
      if (ierr.eq.0)then
C
C     Check the length of iperm array, if correct use it, if not
C     correct, deallocate and reallocate below.
C
      if(((if_node_1_dual_2 .eq. 1) .and. (len .ne. nnode)) .or.
     1   ((if_node_1_dual_2 .eq. 2) .and. (len .ne. nelem)))then
       cmdmess = 'cmo/delatt/ /'//ch_iperm//' ; finish'
       call dotask(cmdmess,ierr)
       cmdmess = 'cmo/delatt/ /'//ch_invperm//' ; finish'
       call dotask(cmdmess,ierr)
       ierr = -1
      endif
      endif
      if (ierr.ne.0) then
        if(if_node_1_dual_2 .eq. 1)then
         cmdmess='cmo/addatt//'//ch_iperm//'/VINT' //
     &      '/scalar/nnodes/constant/temporary//0.0' //
     &      ' ; finish'
        elseif(if_node_1_dual_2 .eq. 2)then
         cmdmess='cmo/addatt//'//ch_iperm//'/VINT' //
     &      '/scalar/nelements/constant/temporary//0.0' //
     &      ' ; finish'
	endif
        call dotask(cmdmess,ierr)
      endif
      call cmo_get_info(ch_iperm,cmo_name,ipiperm,len,itype,ierr)

      call cmo_get_info(ch_invperm,cmo_name,ipinvperm,len,itype,ierr)
      if (ierr.ne.0) then
        if(if_node_1_dual_2 .eq. 1)then
         cmdmess='cmo/addatt//'//ch_invperm//'/VINT' //
     &      '/scalar/nnodes/constant/temporary//0.0' //
     &      ' ; finish'
        elseif(if_node_1_dual_2 .eq. 2)then
         cmdmess='cmo/addatt//'//ch_invperm//'/VINT' //
     &      '/scalar/nelements/constant/temporary//0.0' //
     &      ' ; finish'
        endif
        call dotask(cmdmess,ierr)
      endif
      call cmo_get_info(ch_invperm,cmo_name,ipinvperm,len,itype,ierr)

      endif
CC     *****************************************************************
      if(metis_job_id .eq. 1)then
      if(metis_option_id .eq. 1)then
c         call metis_partgraphrecursive(n_vertices_graph,nxadj,nadjncy,
c     1        nvwgt,nadjwgt,nwgtflag,numflag,npartitions,
c     2        metis_option_flag(metis_option_id,1),nedgecut,ipart)
      elseif(metis_option_id .eq. 2)then
      elseif(metis_option_id .eq. 3)then
      elseif(metis_option_id .eq. 4)then
      elseif(metis_option_id .eq. 5)then
      elseif(metis_option_id .eq. 6)then
      elseif(metis_option_id .eq. 7)then
      elseif(metis_option_id .eq. 8)then
      endif
      endif
C
C     Now run the reorder scheme using the nxadj, nadjncy arrays
C     created by the create_graph routines.
C
      if(metis_job_id .eq. 2)then
        if(metis_option_id .eq. 11)then
C
C METIS_EdgeND
          print *, 'call metis_edgend'
          call metis_edgend(n_vertices_graph,nxadj,nadjncy,numflag,
     1         metis_option_flag(metis_option_id,1),iperm,invperm)
	elseif(metis_option_id .eq. 12)then
C
C METIS_NodeND
          print *, 'call metis_nodend'
          call metis_nodend(n_vertices_graph,nxadj,nadjncy,numflag,
     1         metis_option_flag(metis_option_id,1),iperm,invperm)
	elseif(metis_option_id .eq. 13)then
C
C METIS_NodeWND
          print *, 'call metis_nodewnd'
          call metis_nodewnd(n_vertices_graph,nxadj,nadjncy,
     1         ivert_weight,numflag,
     2         metis_option_flag(metis_option_id,1),iperm,invperm)
	endif
      endif
C
C     Remove some arrays from the MO
C
      if(metis_job_id .eq. 2)then
         cmdmess='cmo/delatt/ /nx_def  ; finish'
         call dotask(cmdmess,ierr)
         cmdmess='cmo/delatt/ /nadj_def; finish'
         call dotask(cmdmess,ierr)
         cmdmess='cmo/delatt/ /isize_nxadj   ; finish'
         call dotask(cmdmess,ierr)
         cmdmess='cmo/delatt/ /irank_nadjncy ; finish'
         call dotask(cmdmess,ierr)
        endif
C
C     Release any temporary memory associated with partition isubname
C
 9999 continue
      call mmrelprt(isubname,ierr)

      return
      end

      subroutine check_int_vector_equal(n,ivec,iflag)
C
C     Check to see that all entries of a vector are
C     equal. Returns when the first non-equal value
C     is found. iflag is set to the index of the first
C     non-equal entry if one is found, otherwise it is
C     returned as zero.
C
      implicit none
      integer n, iflag
      integer ivec(n)
      integer itest, i

      itest = ivec(1)
      iflag = 0
      
      do i = 1,n
      if(ivec(i) .ne. itest)then
C
C     The test has failed
C
         iflag = i
	 return
      endif
      enddo
      return
      end
      

