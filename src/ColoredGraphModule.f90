!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  ColoredGraphModule --
!!
!!    This module provides a derived type ColoredGraph for representing
!!    colored graphs, and procedures for creating and manipulating them.
!!
!!    The only functionality provided by this module is that which is
!!    required by the module ColormapModule, and as such it is far from
!!    complete.  This module should probably be folded in with the module
!!    GraphModule.
!!
!!  PROGRAMMING INTERFACE
!!
!!    type(ColoredGraph) is an ADT with private components that holds the
!!        representation of a colored graph.
!!
!!    call CreateGraph (g [,directed]) returns a ColoredGraph G with no nodes
!!        and no edges.  DIRECTED is an optional intent(in) logical variable
!!        that specifies whether the graph is a directed graph or an
!!        undirected graph (default).
!!
!!    call DeleteGraph (g) deallocates the internal storage associated
!!        with the ColoredGraph variable G.
!!
!!    call AddEdge (g, from, to) adds to the graph G an edge from the node
!!        FROM to the node TO.  FROM and TO are default integer variables
!!        which serve as labels for the nodes.  If either node isn't found
!!        in G it is added.  Edges that are not consistent with the type of
!!        graph are silently ignored.  In the case of a undirected graph,
!!        the edge from TO to FROM is also added.
!!
!!    call PrintGraph (g) prints the ColoredGraph G to standard output in
!!        a readable format.  Mainly useful for debugging.
!!
!!    call ColorGraph (g, ncolors) colors the nodes in the graph G such
!!        that adjacent nodes have different colors.  The algorithm used
!!        is simple-minded.  Multiple passes through the nodes are made
!!        until all nodes are colored.  On the first pass, a node is
!!        assigned a color of "1" if it has no neighbors of that color.
!!        On the second pass, an uncolored node is assigned a color of "2"
!!        if it has no neighbors of that color.  This proceeds until all
!!        nodes are colored.  The number of colors used is returned in the
!!        intent(out) integer variable NCOLORS.
!!
!!    call GetColormap (g, colormap) returns the colormap for the
!!        ColoredGraph variable G.  COLORMAP is a rank-2 integer pointer
!!        which is allocated with shape (2,N) by the routine, where N
!!        is the number of nodes in G.  COLORMAP(1,:) are the node labels
!!        and COLORMAP(2,:) are the corresponding colors.  If G has no nodes
!!        then COLORMAP is nullified rather than being allocated with
!!        zero size.
!!
!!  CHANGE HISTORY
!!
!!    $Log: ColoredGraphModule.f90,v $
!!    Revision 2.00  2007/11/05 19:45:45  spchu
!!    Import to CVS
!!
!!PVCS  
!!PVCS     Rev 1.1   Fri Apr 07 15:54:44 2000   nnc
!!PVCS  Internal lists reimplemented as binary search trees.
!!PVCS  No external changes.
!!PVCS  
!!PVCS     Rev 1.0   Fri Apr 02 09:59:26 1999   nnc
!!PVCS  Initial revision.
!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module CGNodeType

  implicit none
  private
  
 !!!
 !!! RECURSIVE LIST STRUCTURE FOR STORING GRAPH NODE ADJACENCY

  type, public :: CGNodePointerList
    type(CGNodePointerListItem), pointer :: first ! => null() ! Uncomment for F95
  end type CGNodePointerList

  type, public :: CGNodePointerListItem
    type(CGNode), pointer   :: node
    type(CGNodePointerList) :: rest
  end type CGNodePointerListItem
  
 !!!
 !!! GRAPH NODE DATA TYPE

  type, public :: CGNode
    integer :: label
    integer :: color ! = 0 ! Uncomment for F95
    type(CGNodePointerList) :: nbrs
  end type CGNode

end module CGNodeType

module ColoredGraphModule

  use CGNodeType
  implicit none
  private

  public  :: CreateGraph, DeleteGraph, AddEdge, PrintGraph, ColorGraph, GetColormap

  private :: CreateColoredGraph, DeleteColoredGraph, AddEdgeToColoredGraph, PrintColoredGraph
  private :: AddToCGNodePointerList, DeleteFromCGNodePointerList
  private :: DeleteCGNodePointerList, CGNodePointer
  
  private :: DeleteCGNodeTree

  interface CreateGraph
    module procedure CreateColoredGraph
  end interface

  interface DeleteGraph
    module procedure DeleteColoredGraph
  end interface

  interface AddEdge
    module procedure AddEdgeToColoredGraph
  end interface

  interface PrintGraph
    module procedure PrintColoredGraph
  end interface
  
 !!!
 !!! BINARY SEARCH TREE FOR STORAGE OF GRAPH NODES
 
  type, private :: CGNodeTree
    type(CGNodeTreeNode), pointer :: root ! => null() ! Uncomment for F95
  end type CGNodeTree

  type, private :: CGNodeTreeNode
    type(CGNode)     :: node
    type(CGNodeTree) :: lt, gt
  end type CGNodeTreeNode
  
 !!!
 !!! GRAPH DATA TYPE

  type, public :: ColoredGraph
    private
    logical :: directed  ! = .false. ! Uncomment for F95
    logical :: colored   ! = .false. ! Uncomment for F95
    type(CGNodeTree) :: nodes
  end type ColoredGraph
  
 !!!
 !!! STATE VARIABLES FOR NODE_COLOR PROCEDURE
 
  integer, save, private :: color
  logical, save, private :: uncolored

  contains

    subroutine CreateColoredGraph (g, directed)

      logical, intent(in), optional :: directed
      type(ColoredGraph) :: g

      g % colored   = .false. ! Delete this statement for F95
      g % directed  = .false. ! Delete this statement for F95

      if (present(directed))  g % directed  = directed

      nullify(g % nodes % root)  ! Delete this statement for F95

    end subroutine CreateColoredGraph

    subroutine DeleteColoredGraph (g)

      type(ColoredGraph) :: g

      g % colored  = .false.
      g % directed = .false.
      call DeleteCGNodeTree (g % nodes)

    end subroutine DeleteColoredGraph

    subroutine AddEdgeToColoredGraph (g, from, to)

      type(ColoredGraph), intent(inout) :: g
      integer, intent(in) :: from, to

      type(CGNode), pointer :: from_node, to_node

      if (from == to) return

      call CGNodePointer (g % nodes, from, from_node)
      call CGNodePointer (g % nodes, to,   to_node)

      call AddToCGNodePointerList (from_node % nbrs, to_node)
      if (g % directed) return
      call AddToCGNodePointerList (to_node % nbrs, from_node)

    end subroutine AddEdgeToColoredGraph

    subroutine AddToCGNodePointerList (list, node)

      type(CGNodePointerList), intent(inout), target :: list
      type(CGNode), intent(in), target :: node

      type(CGNodePointerList) :: r
      type(CGNodePointerList), pointer :: l

      l => list
      do while (associated(l % first))
        if (node % label > l % first % node % label) then
          l => l % first % rest
        else
          if (node % label == l % first % node % label) return
          exit
        end if
      end do

      r = l
      allocate(l % first)
      l % first % node => node
      l % first % rest = r

    end subroutine AddToCGNodePointerList

    subroutine DeleteFromCGNodePointerList (list, node)

      type(CGNodePointerList), intent(inout), target :: list
      type(CGNode), intent(in) :: node

      type(CGNodePointerList) :: r
      type(CGNodePointerList), pointer :: l

      l => list
      do while (associated(l % first))
        if (node % label > l % first % node % label) then
          l => l % first % rest
        else
          if (node % label == l % first % node % label) then
            r = l % first % rest
            deallocate (l % first)
            l = r
          end if
          exit
        end if
      end do

    end subroutine DeleteFromCGNodePointerList

    subroutine DeleteCGNodePointerList (list)

      type(CGNodePointerList), intent(inout) :: list

      type(CGNodePointerList) :: rest

      do while (associated(list % first))
        rest = list % first % rest
        deallocate(list % first)
        list = rest
      end do

    end subroutine DeleteCGNodePointerList

    recursive subroutine DeleteCGNodeTree (bst)

      type(CGNodeTree), intent(inout) :: bst

      do while (associated(bst % root))
        call DeleteCGNodeTree (bst % root % lt)
        call DeleteCGNodeTree (bst % root % gt)
        call DeleteCGNodePointerList (bst % root % node % nbrs)
        deallocate(bst % root)
      end do

    end subroutine DeleteCGNodeTree
    
    recursive subroutine CGNodePointer (bst, label, node)

      type(CGNodeTree), intent(inout), target :: bst
      integer, intent(in) :: label
      type(CGNode), pointer :: node

      if (.not.associated(bst % root)) then ! add
        allocate (bst % root)
        bst % root % node % label = label
        bst % root % node % color = 0
        nullify(bst % root % node % nbrs % first) ! Delete this statement for F95
        nullify(bst % root % lt % root)           ! Delete this statement for F95
        nullify(bst % root % gt % root)           ! Delete this statement for F95
        node => bst % root % node
        
      else if (label < bst % root % node % label) then
        call CGNodePointer (bst % root % lt, label, node)
        
      else if (label > bst % root % node % label) then
        call CGNodePointer (bst % root % gt, label, node)
        
      else
        node => bst % root % node
      end if

    end subroutine CGNodePointer

!    recursive function CGNodePointer (bst, label) result (node)
!
!      type(CGNodeTree), intent(inout), target :: bst
!      integer, intent(in) :: label
!      type(CGNode), pointer :: node
!
!      if (.not.associated(bst % root)) then ! add
!        allocate (bst % root)
!        bst % root % node % label = label
!        bst % root % node % color = 0
!        nullify(bst % root % node % nbrs % first) ! Delete this statement for F95
!        nullify(bst % root % lt % root)           ! Delete this statement for F95
!        nullify(bst % root % gt % root)           ! Delete this statement for F95
!
!      else if (label < bst % root % node % label) then
!        node => CGNodePointer (bst % root % lt, label)
!
!      else if (label > bst % root % node % label) then
!        node => CGNodePointer (bst % root % gt, label)
!
!      else
!        node => bst % root % node
!      end if
!
!    end function CGNodePointer

    subroutine PrintColoredGraph (g)

      type(ColoredGraph), intent(in) :: g

      write(unit=*, fmt="(a,l1)") "directed  = ", g % directed
      write(unit=*, fmt="(a)") "node [color] (neighbors):"
      call for_each (g % nodes, print_node)
      
    end subroutine PrintColoredGraph
    
    subroutine print_node (node)
    
      type(CGNode), intent(in) :: node
      
      type(CGNodePointerList) :: nbrs
      
      write(unit=*,fmt="(i6)",advance="no") node % label
      write(unit=*,fmt="(a,i3,a)",advance="no") " [", node % color, "] ("
      nbrs = node % nbrs
      do while (associated(nbrs % first))
        write(unit=*,fmt="(i7)",advance="no") nbrs % first % node % label
        nbrs = nbrs % first % rest
      end do
      write(unit=*,fmt="(a)") ")"
      
    end subroutine print_node
    
    subroutine ColorGraph (g, ncolors)

      type(ColoredGraph), intent(inout) :: g
      integer, intent(out) :: ncolors
      
      !! COLOR and UNCOLORED are state variables for the procedure COLOR_NODE.
      if (associated(g % nodes % root)) then
        call for_each (g % nodes, zero_color)   ! Set all colors to 0
        color = 0
        uncolored = .true.
        do while (uncolored)
          color = color + 1  
          uncolored = .false.
          call for_each (g % nodes, color_node)
        end do
        g % colored = .true.
        ncolors = color
      else
        ncolors = 0
      end if

    end subroutine ColorGraph
    
    subroutine zero_color (node)
      type(CGNode), intent(inout) :: node
      node % color = 0
    end subroutine zero_color

    subroutine color_node (node)
    
      type(CGNode), intent(inout) :: node
      
      type(CGNodePointerList) :: nbrs
      
      if (node % color > 0) return  ! it's already colored.
      
      nbrs = node % nbrs
      do while (associated(nbrs % first))
        if (nbrs % first % node % color == color) then
          uncolored = .true.
          return
        end if
        nbrs = nbrs % first % rest
      end do
      
      node % color = color
      
    end subroutine color_node

    subroutine GetColormap (g, colormap)

      type(ColoredGraph), intent(in) :: g
      integer, dimension(:,:), pointer :: colormap

      integer :: num_nodes, map_size

      if (.not. g % colored) then
        nullify(colormap)
        return
      end if

      num_nodes = tree_size(g % nodes)

      if (num_nodes == 0) then
        nullify(colormap)
	return
      end if

      allocate(colormap(2,num_nodes))
      map_size = 0
      call get_colormap (g % nodes, colormap, map_size)
      !! Assert map_size == num_nodes

    end subroutine GetColormap
    
    recursive subroutine get_colormap (bst, colormap, map_size)
    
      type(CGNodeTree), intent(in) :: bst
      integer, dimension(:,:), intent(out) :: colormap
      integer, intent(inout) :: map_size
      
      if (associated(bst % root)) then
        call get_colormap (bst % root % lt, colormap, map_size)
        map_size = 1 + map_size
        colormap(1,map_size) = bst % root % node % label
        colormap(2,map_size) = bst % root % node % color
        call get_colormap (bst % root % gt, colormap, map_size)
      end if
      
    end subroutine get_colormap
    
    recursive function tree_size (bst) result (n)
      type(CGNodeTree), intent(in) :: bst
      integer :: n
      if (associated(bst % root)) then
        n = 1 + tree_size(bst % root % lt) + tree_size(bst % root % gt)
      else
        n = 0
      end if
    end function tree_size
    
    subroutine map_to_vector (proc, bst, vec, nvec)
    
      type(CGNodeTree), intent(in) :: bst
      integer, dimension(:,:), intent(out) :: vec
      integer, intent(out) :: nvec
      
      interface
        subroutine proc (node, vec)
          use CGNodeType
          type(CGNode), intent(in) :: node
          integer, dimension(:), intent(out) :: vec
        end subroutine proc
      end interface
      
      nvec = 0
      call map_to_vector_aux (proc, bst, vec, nvec)
    
    end subroutine map_to_vector
    
    recursive subroutine map_to_vector_aux (proc, bst, vec, nvec)
    
      type(CGNodeTree), intent(in) :: bst
      integer, dimension(:,:), intent(out) :: vec
      integer, intent(inout) :: nvec
      
      interface
        subroutine proc (node, vec)
          use CGNodeType
          type(CGNode), intent(in) :: node
          integer, dimension(:), intent(out) :: vec
        end subroutine proc
      end interface
      
      if (associated(bst % root)) then
        call map_to_vector_aux (proc, bst % root % lt, vec, nvec)
        nvec = nvec + 1
        if (nvec <= size(vec,dim=1)) then
          call proc (bst % root % node, vec(:,nvec))
        end if
        call map_to_vector_aux (proc, bst % root % gt, vec, nvec)
      end if
    
    end subroutine map_to_vector_aux
    
    recursive subroutine for_each (bst, proc)
    
      type(CGNodeTree) :: bst
      
      interface
        subroutine proc (node)
          use CGNodeType
          type(CGNode) :: node
        end subroutine proc
      end interface
      
      if (associated(bst % root)) then
        call for_each (bst % root % lt, proc)
        call proc (bst % root % node)
        call for_each (bst % root % gt, proc)
      end if
    
    end subroutine for_each
    
end module ColoredGraphModule
