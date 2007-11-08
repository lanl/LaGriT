!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  colormap.f90  -- A component of the LaGriT library
!!
!!  Neil N. Carlson <nnc@lanl.gov>, March 1999
!!  Group T-1, Los Alamos National Laboratory
!!
!!  When a mesh object involves a large number of materials, it is often
!!  useful to have a suitable mapping of the material numbers onto a small set
!!  of "colors" (think positive integers).  A valid colormap is constrained
!!  by the condition that adjacent materials be colored differently.  It is
!!  also often useful to have a single colormap that is valid for a collection
!!  of mesh objects.  This suite of routines provides the functionality for
!!  generating such colormaps, as well as using them to recolor mesh objects.
!!
!!  The user-level routines are colormap_lg (building a colormap), cmo_recolor
!!  (recoloring a mesh object using a colormap), dump_recolor_lg (recoloring
!!  and dumping a mesh object), and dump_colormap_lg (dumping a colormap to a
!!  file).  See the comments below for their use.  These are callable (meaning
!!  directly usable) from a program, and feature a F77-style interface.  The
!!  internal support routines are encapsulated in the F90 modules
!!  ColormapModule, which appears below, and ColoredGraphModule, which is of
!!  general use and is found separately.
!!
!!  CHANGE HISTORY
!!
!!    $Log:   /pvcs.config/t3d/src/colormap.f9a  $
!!PVCS  
!!PVCS     Rev 1.2   Fri Nov 05 13:29:12 1999   dcg
!!PVCS  remove dictionary dependencies
!!PVCS  
!!PVCS     Rev 1.1   Thu Apr 15 13:44:44 1999   nnc
!!PVCS  In CMO_RECOLOR non-real nodes (real nodes have itp1<20 or itp1=41)
!!PVCS  are assigned a `color' 1 greater than the largest in the colormap.
!!PVCS  
!!PVCS     Rev 1.0   Fri Apr 02 09:59:14 1999   nnc
!!PVCS  Initial revision.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  ColormapModule --
!!
!!  This module provides the internal support routines and data structures
!!  for building the material adjacency graph and generating the colormap.
!!
!!  PROGRAMMING INTERFACE
!!
!!    call AccumMatAdjacency (cnbr, cmat, create) accumulates the material
!!        adjacency information from a multi-material mesh, specified by the
!!        arguments CNBR and CMAT, to the current material adjacency graph.
!!
!!        CNBR is a rank-2 integer array giving the neighbors of the cells.
!!            CNBR(:,j) are the neighbors of cell j encoded in the following
!!            manner.  If cells j and j' are adjacent, with face k of cell j
!!            and face k' of cell j' being the shared face then
!!            CNBR(k,j) = k' + n * (j'-1) and CNBR(k',j') = k + n * (j-1)
!!            where n is the number of vertices of the simplicial cell.
!!            If CNBR(k,j) = 0 then this face is a boundary face, and if
!!            CNBR(k,j) < 0 then this is a material interface face, with
!!            |CNBR(k,j)| giving the neighbor encoded as above.  In this
!!            latter case, CMAT(j) and CMAT(j') are marked as adjacent.
!!
!!        CMAT is a rank-1, integer array giving the material numbers for
!!            the cells.  If adjacent cells have different material numbers
!!            then their shared face should be indicated as a material
!!            interface face in CNBR, and conversely.
!!
!!        CREATE is an optional logical variable.  If true, then the current
!!            material adjacency graph (if any) is deleted and the material
!!            adjacency graph is initialized with the material adjacency
!!            characteristics of the specified mesh.  If false, or if not
!!            present, the material adjacency characteristics of the specified
!!            mesh is added to the current material adjacency graph.  If no
!!            material adjacency graph currently exists, execution proceeds
!!            as in the former case.
!!
!!    call DeleteMatAdjacency () deallocates the internal storage associated
!!        with the material adjacency graph.
!!
!!    call GetColormap (colormap) returns a pointer to a rank-2 integer array
!!        containing the current colormap; material COLORMAP(1,j) maps to the
!!        "color" COLORMAP(2,j).  If colormap is not associated, then either
!!        no material adjacency graph exists from which to generate the
!!        colormap, or that graph has no nodes.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module ColormapModule

  use ColoredGraphModule, GetColormap_loc => GetColormap
  implicit none
  private

  public :: AccumMatAdjacency, DeleteMatAdjacency, GetColormap

  type(ColoredGraph), private, save :: g

  !! Because F90 doesn't permit derived data types to be initialized when
  !! declared, in particular nullifying pointers, we have to go through some
  !! gymnastics to ensure that we don't test the association status of
  !! undefined pointers.  This is the reason behind maintaining the following
  !! statement, which can be deleted in F95.
  logical, private, save :: g_is_defined = .false.

  contains

    subroutine AccumMatAdjacency (cnbr, cmat, create)

      integer, dimension(:,:), intent(in) :: cnbr
      integer, dimension(:),   intent(in) :: cmat
      logical, optional, intent(in) :: create

      integer :: nvtx, ncell, jnbr, j, k

      if (g_is_defined) then
        if (present(create)) then
          if (create) then
            call DeleteGraph (g)
            call CreateGraph (g)
          end if
        end if
      else
        call CreateGraph(g)
        g_is_defined = .true.
      end if

      !! In F95 we can replace the preceeding block with the following:
      !if (present(create)) then
      !  if (create) then
      !    call DeleteGraph (g)
      !    call CreateGraph (g)
      !  end if
      !end if

      nvtx  = size(cnbr, dim=1)
      ncell = size(cnbr, dim=2)

      do j = 1, ncell
        do k = 1, nvtx
          if (cnbr(k,j) > 0) cycle
          if (cnbr(k,j) < 0) then
            jnbr = 1 + (abs(cnbr(k,j)) - 1) / nvtx
            if (cmat(jnbr) > cmat(j)) call AddEdge (g, cmat(j), cmat(jnbr))
          end if
        end do
      end do

    end subroutine AccumMatAdjacency

    subroutine DeleteMatAdjacency ()
      if (g_is_defined) call DeleteGraph(g)
    end subroutine DeleteMatAdjacency

    subroutine GetColormap (colormap)

      integer, dimension(:,:), pointer :: colormap
      integer :: ncolors

      if (g_is_defined) then
        call ColorGraph (g, ncolors)
        call GetColormap_loc (g, colormap)
      else
        nullify(colormap)
      end if

      !! In F95 the preceding block of code can simply be replaced by:
      !call ColorGraph (g, ncolors)
      !call GetColormap_loc (g, colormap)

    end subroutine GetColormap

end module ColormapModule


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!                                 !!
!!  F77-CALLABLE EXTERNAL ROUTINES !!
!!                                 !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  COLORMAP_LG -- Build a colormap.
!!
!!      call colormap_lg (cmo, action, ierror)
!!
!!    This routine doesn't actually generate a colormap.  Rather it builds
!!    and maintains the material adjacency graph from which a colormap can
!!    be quickly generated.  For simplicity, the mesh is required to consist
!!    solely of triangles or solely of tetrahedra (this is checked for).
!!
!!  INPUT ARGUMENTS
!!
!!    cmo    -- Mesh object name.
!!    action -- What to do:
!!              "create" ==> Create a new material adjacency graph from the
!!                       specified mesh object.  The existing adjacency graph
!!                       is deleted.
!!              "add"    ==> Add to the existing graph the material adjacency
!!                       of the specified mesh object.  If no graph currently
!!                       exists this action is equivalent to "create".
!!              "delete" ==> Delete the existing adjacency graph (if any).
!!                       The CMO argument is ignored in this case.
!!
!!  OUTPUT ARGUMENTS
!!
!!    ierror -- Error flag: ==0 ==> OK, /= 0 ==> error occurred:
!!              == -1 ==> Bad value for ACTION.
!!              == -2 ==> Mesh object doesn't exist.
!!              == -3 ==> Mesh object isn't purely simplicial (2D or 3D).
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine colormap_lg (cmo, action, ierror)

  use ColormapModule
  implicit none

  character*(*) cmo, action
  integer ierror

  pointer (ipjtet,jtet), (ipjtetoff,jtetoff), (ipitetclr,itetclr), (ipitettyp,itettyp)
  integer jtet(*), jtetoff(*), itetclr(*), itettyp(*)

  integer :: topo_dim, ncells, mbndry, ilen, ityp

  integer, dimension(:),   allocatable :: cmat
  integer, dimension(:,:), allocatable :: cnbr

  integer :: nvtx, j, k
  character(len=128) :: buffer

  external cmo_get_info, writloga

  select case (action)

    case default

       buffer = 'COLORMAP_LG: Unknown action "' // trim(action) // '".'
       call writloga ('default', 0, buffer, 0, ierror)
       ierror = -1

    case ("delete")

      call DeleteMatAdjacency ()
      ierror = 0

    case ("create", "add")

      call cmo_exist (cmo, ierror)
      if (ierror /= 0) then
        buffer = 'COLORMAP_LG: No such mesh object: "' // trim(cmo) // '"'
        call writloga ('default', 0, buffer, 0, ierror)
        ierror = -2
        return
      end if

      call cmo_get_info ('ndimensions_topo', cmo,  topo_dim, ilen, ityp, ierror)
      call cmo_get_info ('nelements', cmo,    ncells, ilen, ityp, ierror)
      call cmo_get_info (   'mbndry', cmo,    mbndry, ilen, ityp, ierror)
      call cmo_get_info (     'jtet', cmo,    ipjtet, ilen, ityp, ierror)
      call cmo_get_info (  'jtetoff', cmo, ipjtetoff, ilen, ityp, ierror)
      call cmo_get_info (  'itetclr', cmo, ipitetclr, ilen, ityp, ierror)
      call cmo_get_info (  'itettyp', cmo, ipitettyp, ilen, ityp, ierror)

     !!!
     !!! Verify that the mesh object is purely simplicial.

      select case (topo_dim)
        case (2)
          if (any(itettyp(1:ncells) /= 3)) then
            buffer = 'COLORMAP_LG: 2D mesh must be purely triangular.  Aborting.'
            call writloga('default', 0, buffer, 0, ierror)
            ierror = -3
            return
          end if
          nvtx = 3
        case (3)
          if (any(itettyp(1:ncells) /= 5)) then
            buffer =  'COLORMAP_LG: 3D mesh must be purely tetrahedral.  Aborting.'
            call writloga('default', 0, buffer, 0, ierror)
            ierror = -3
            return
          end if
          nvtx = 4
        case default
          buffer = 'COLORMAP_LG: Mesh is neither 2D or 3D!  Aborting.'
          call writloga('default', 0, buffer, 0, ierror)
          ierror = -3
          return
      end select

     !!!
     !!! Construct a "clean" cell neighbor structure and call

      allocate (cnbr(nvtx,ncells), cmat(ncells))
      do j = 1, ncells
        cmat(j) = itetclr(j)
        do k = 1, nvtx
          if (jtet(k+jtetoff(j)) < mbndry) then
            cnbr(k,j) = jtet(k+jtetoff(j))
          else
            cnbr(k,j) = mbndry - jtet(k+jtetoff(j))
          end if
        end do
      end do

      select case (action)
        case ('create')
          call AccumMatAdjacency (cnbr, cmat, create=.true.)
        case ('add')
          call AccumMatAdjacency (cnbr, cmat, create=.false.)
      end select

      deallocate (cmat, cnbr)
      ierror = 0

  end select

end subroutine colormap_lg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  CMO_RECOLOR -- Recolor a mesh object using the existing colormap.
!!
!!      cmo_recolor (cmo, mode, ierror)
!!
!!  INPUT ARGUMENTS
!!
!!    cmo  -- Mesh object name.
!!    mode -- Recoloring mode:
!!            "save"    ==> The original ITETCLR and IMT1 values are copied to
!!                          mesh object attributes ITETCLR_SAVE and IMT1_SAVE
!!                          before recoloring;
!!            "nosave"  ==> The original ITETCLR and IMT1 values are discarded;
!!            "restore" ==> The attributes ITETCLR_SAVE and IMT1_SAVE are copied
!!                          back into ITETCLR and IMT1 and then deleted.
!!
!!  OUTPUT ARGUMENT
!!
!!    ierror -- Error flag: == 0 ==> OK, /= 0 ==> Error occured:
!!              == -1 ==> Bad value for the argument MODE.
!!              == -2 ==> Mesh object doesn't exist.
!!              == -3 ==> No colormap exists.
!!              == -4 ==> Bad values in ITETCLR or IMT1.
!!              == -5 ==> Incomplete colormap.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine cmo_recolor (cmo, mode, ierror)

  use ColormapModule
  implicit none

  character*(*) cmo, mode
  integer ierror

  pointer (ipimt1,imt1), (ipitp1,itp1), (ipitetclr,itetclr)
  pointer (ipimt1_save,imt1_save), (ipitetclr_save,itetclr_save)
  integer imt1(*), itp1(*), itetclr(*), imt1_save(*), itetclr_save(*)

  logical, dimension(:), allocatable :: node_is_real
  integer, dimension(:), allocatable :: cmap
  integer, dimension(:,:), pointer :: colormap
  integer :: ncells, nnodes, ilen, ityp, nmat, j

  character(len=128) :: buffer

  external cmo_exist, cmo_get_info, writloga, dotaskx3d

  select case (mode)

  case default

    buffer = 'CMO_RECOLOR: Unknown mode "' // trim(mode) // '".'
    call writloga ('default', 0, buffer, 0, ierror)
    ierror = -1

  case ("restore")

    call cmo_exist (cmo, ierror)
    if (ierror /= 0) then
      buffer = 'CMO_RECOLOR: No such mesh object: "' // trim(cmo) // '"'
      call writloga ('default', 0, buffer, 0, ierror)
      ierror = -2
      return
    end if

    !! Restore IMT1 from IMT1_SAVE, and delete the latter attribute.
    call cmo_get_info ('imt1_save', cmo, ipimt1_save, ilen, ityp, ierror)
    if (ierror == 0) then
      buffer = 'cmo/copyatt/' // trim(cmo) // '/' // trim(cmo) // '/imt1/imt1_save ; ' // &
               'cmo/delatt/' // trim(cmo) // '/imt1_save ; finish'
      call dotaskx3d (buffer, ierror)
    else
      buffer = 'CMO_RECOLOR: Unable to restore IMT1; IMT1_SAVE not found.'
      call writloga ('default', 0, buffer, 0, ierror)
    end if

    !! Restore ITETCLR from ITETCLR_SAVE, and delete the latter attribute.
    call cmo_get_info ('itetclr_save', cmo, ipitetclr_save, ilen, ityp, ierror)
    if (ierror == 0) then
      buffer = 'cmo/copyatt/' // trim(cmo) // '/' // trim(cmo) // '/itetclr/itetclr_save ; ' // &
               'cmo/delatt/' // trim(cmo) // '/itetclr_save ; finish'
      call dotaskx3d (buffer, ierror)
    else
      buffer = 'CMO_RECOLOR: Unable to restore ITETCLR; ITETCLR_SAVE not found.'
      call writloga ('default', 0, buffer, 0, ierror)
    end if

    ierror = 0


  case ("save", "nosave")

   !!!
   !!! Generate the colormap and verify that all materials present are mapped.

    call GetColormap (colormap)  !! Get the compressed colormap.
    if (.not.associated(colormap)) then
      buffer = 'CMO_RECOLOR: No colormap exists.  Aborting.'
      call writloga ('default', 0, buffer, 0, ierror)
      ierror = -3
      return
    end if

    call cmo_exist (cmo, ierror)
    if (ierror /= 0) then
      buffer = 'CMO_RECOLOR: No such mesh object: "' // trim(cmo) // '"'
      call writloga ('default', 0, buffer, 0, ierror)
      ierror = -2
      return
    end if

    call cmo_get_info ('nelements', cmo,    ncells, ilen, ityp, ierror)
    call cmo_get_info (   'nnodes', cmo,    nnodes, ilen, ityp, ierror)
    call cmo_get_info (  'itetclr', cmo, ipitetclr, ilen, ityp, ierror)
    call cmo_get_info (     'imt1', cmo,    ipimt1, ilen, ityp, ierror)
    call cmo_get_info (     'itp1', cmo,    ipitp1, ilen, ityp, ierror)

    !! Set the node mask; these are nodes where the IMT1 value will be modified.
    allocate(node_is_real(nnodes))
    where ((itp1(1:nnodes) >= 0 .and. itp1(1:nnodes) < 20) .or. itp1(1:nnodes) == 41)
      node_is_real = .true.
    elsewhere
      node_is_real = .false.
    end where

    if (any(itetclr(1:ncells) <= 0)) then
      buffer = 'CMO_RECOLOR: Nonpositive material number in the ITETCLR array.  Aborting.'
      call writloga ('default', 0, buffer, 0, ierror)
      deallocate(node_is_real, colormap)
      ierror = -4
      return
    end if

    if (any(imt1(1:nnodes) <= 0 .and. node_is_real)) then
      buffer = 'CMO_RECOLOR: Nonpositive material number in the IMT1 array.  Aborting.'
      call writloga ('default', 0, buffer, 0, ierror)
      deallocate(node_is_real, colormap)
      ierror = -4
      return
    end if

    nmat = max( maxval(colormap(1,:)),                &
                maxval(itetclr(1:ncells)),            &
                maxval(imt1(1:nnodes), mask=node_is_real) )
    allocate(cmap(nmat))

    cmap = 0

    !! Tag materials appearing in ITETCLR.
    do j = 1, ncells
      cmap(itetclr(j)) = -1
    end do

    !! Tag materials appearing in IMT1.
    do j = 1, nnodes
      if (node_is_real(j)) cmap(imt1(j)) = -1
    end do

    !! Initialize the uncompressed colormap.
    do j = 1, size(colormap, dim=2)
      cmap(colormap(1,j)) = colormap(2,j)
    end do

    if (any(cmap < 0)) then  !! Incomplete colormap.
      buffer = 'CMO_RECOLOR: Not all materials present are mapped by the colormap.  Aborting.'
      call writloga ('default', 0, buffer, 0, ierror)
      deallocate(cmap, node_is_real, colormap)
      ierror = -5
      return
    end if

   !!!
   !!! Map material numbers to new values, saving existing values if indicated.

    if (mode == "save") then

      !! Copy existing IMT1 to IMT1_SAVE, creating the latter attribute if necessary.
      call cmo_get_info ('imt1_save', cmo, ipimt1_save, ilen, ityp, ierror)
      if (ierror /= 0) then
        buffer = 'cmo/addatt/' // trim(cmo) // &
                 '/imt1_save/vint/scalar/nnodes/max/temporary/gxa/0 ; finish'
        call dotaskx3d (buffer, ierror)
      end if
      buffer = 'cmo/copyatt/' // trim(cmo) // '/' // trim(cmo) // '/imt1_save/imt1;finish'
      call dotaskx3d (buffer, ierror)

      !! Copy existing ITETCLR to ITETCLR_SAVE, creating the latter attribute if necessary.
      call cmo_get_info ('itetclr_save', cmo, ipitetclr_save, ilen, ityp, ierror)
      if (ierror /= 0) then
        buffer = 'cmo/addatt/' // trim(cmo) // &
                 '/itetclr_save/vint/scalar/nelements/user/temporary/x/0 ; finish'
        call dotaskx3d (buffer, ierror)
      end if
      buffer = 'cmo/copyatt/' // trim(cmo) // '/' // trim(cmo) // '/itetclr_save/itetclr;finish'
      call dotaskx3d (buffer, ierror)

    end if

    do j = 1, ncells
      itetclr(j) = cmap(itetclr(j))
    end do

    do j = 1, nnodes
      if (node_is_real(j)) then
        imt1(j) = cmap(imt1(j))
      else
        imt1(j) = 1 + maxval(colormap(2,:))
      end if
    end do

    deallocate(cmap, node_is_real, colormap)
    ierror = 0

  end select

end subroutine cmo_recolor

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  DUMP_COLORMAP_LG -- Dump the existing colormap to a file
!!
!!      dump_colormap_lg (file, ierror)
!!
!!  INPUT ARGUMENTS
!!
!!    file   -- The dumpfile name.
!!
!!  OUTPUT ARGUMENTS
!!
!!    ierror -- Error flag: == 0 ==> OK, /= 0 ==> error occurred:
!!              == -1 ==> No colormap exists.
!!              == -2 ==> Unable to open file.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine dump_colormap_lg (file_name, ierror)

  use ColormapModule
  implicit none

  character*(*) file_name
  integer ierror

  integer, dimension(:,:), pointer :: colormap
  integer, dimension(2) :: digits
  character(len=128) :: buffer
  integer :: file_unit

  external hassign, writloga

  call GetColormap (colormap)
  if (.not.associated(colormap)) then
    buffer = 'DUMP_COLORMAP_LG: No colormap exists.'
    call writloga ('default', 0, buffer, 0, ierror)
    ierror = -1
    return
  end if

  file_unit = -1
  call hassign (file_unit, file_name, ierror)
  if (ierror /= 0) then
    buffer = 'DUMP_COLORMAP_LG: Unable to open file.'
    call writloga ('default', 0, buffer, 0, ierror)
    ierror = -2
    return
  end if

  digits = 1 + log10(real(max( 1, maxval(abs(colormap),dim=2) )))
  write(unit=buffer, fmt='("(i",i1,",1x,i",i1,")")') digits
  write(unit=file_unit, fmt=buffer) colormap
  close(file_unit)
  ierror = 0

end subroutine dump_colormap_lg

!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  DUMP_RECOLOR -- Recolor and dump a mesh object
!!
!!      dump_recolor_lg (type, file, cmo, restore, create, iomode, ierror)
!!
!!  INPUT ARGUMENTS
!!
!!    type    -- Type of dump: "gmv", "x3d", or "avs".
!!    file    -- Dumpfile name.
!!    cmo     -- Mesh object name.
!!    restore -- Logical flag.  If true, then the original ITETCLR and IMT1
!!               values are restored before exiting.  If false, the mesh is
!!               left recolored.
!!    create  -- Logical flag.  If true, a new colormap is created and used.
!!               If false, the existing colormap is used.
!!    iomode  -- Dump format; e.g. "ascii" or "binary".  Which values are
!!               valid will depend on the type of dump being done.
!!
!!  OUTPUT ARGUMENTS
!!
!!    ierror  -- Error flag; == 0 ==> OK, /= 0 ==> Error occured:
!!               == -1 ==> Mesh object doesn't exist.
!!               == -2 ==> Unable to create a new colormap.
!!               == -3 ==> Existing colormap is invalid.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine dump_recolor_lg (dump_type, dump_file, cmo, restore, create, iomode, ierror)

  implicit none

  character*(*) dump_type, dump_file, cmo, iomode
  logical restore, create
  integer ierror

  character*128 buffer

  external colormap_lg, cmo_recolor, dumpgmv_hybrid, dotaskx3d, cmo_exist, cmo_select

  call cmo_exist (cmo, ierror)
  if (ierror /= 0) then
    buffer = 'DUMP_RECOLOR_LG: No such mesh object: "' // trim(cmo) // '"'
    call writloga ('default', 0, buffer, 0, ierror)
    ierror = -1
    return
  end if

  if (create) then
    call colormap_lg (cmo, 'create', ierror)
    if (ierror /= 0) then
      buffer = 'DUMP_RECOLOR_LG: Unable to create the colormap.  Aborting.'
      call writloga ('default', 0, buffer, 0, ierror)
      ierror = -2
      return
    end if
  end if

  if (restore) then
    call cmo_recolor (cmo, 'save', ierror)
  else
    call cmo_recolor (cmo, 'nosave', ierror)
  end if
  if (ierror /= 0) then
    buffer = 'DUMP_RECOLOR_LG: Invalid colormap.  Aborting.'
    call writloga ('default', 0, buffer, 0, ierror)
    ierror = -3
    return
  end if

  select case (dump_type)

    case ('gmv')
      call cmo_select (cmo, ierror)  !! DUMPGMV_HYBRID gets the cmo name two different ways!
      call dumpgmv_hybrid (dump_file, cmo, iomode)

    case ('avs')
      buffer = 'dump/avs/' // trim(dump_file) // trim(cmo) // ' ; finish'
      call dotaskx3d (buffer, ierror)

    case default
      buffer = 'DUMP_RECOLOR_LG: Unimplemented dump type "' // trim(dump_type) // '".'
      call writloga ('default', 0, buffer, 0, ierror)

  end select

  if (restore) call cmo_recolor (cmo, 'restore', ierror)
  ierror = 0

end subroutine dump_recolor_lg
