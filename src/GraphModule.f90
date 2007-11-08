!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  GraphModule -- A Fortran 90 module for creating and manipulating graphs.
!!
!!    This module provides a derived type NGraphType for representing
!!    n-node graphs, and procedures for creating and manipulating them.
!!    Graphs may be directed or undirected, nonreflexive, or multigraphs.
!!
!!  PROGRAMMING INTERFACE
!!
!!    type(NGraphType) is an ADT with private components that holds the
!!        representation of an N-node graph.
!!
!!    CreateGraph (n [,directed] [,mult_edge] [,self_edge]) returns an N-node
!!        graph (variable of type NGraphType) with no edges.  The remaining
!!        arguments are optional intent(in) logical variables that specify
!!        the type of n-graph:
!!
!!        DIRECTED specifies whether the graph is a directed graph or an
!!            undirected graph (default).
!!        MULT_EDGE specifies whether the graph is a conventional graph
!!            (default) or a multigraph in which multiple edges linking
!!            a pair of nodes is allowed.
!!        SELF_EDGE specifies whether edges linking a node to itself are
!!            allowed or not allowed (default).
!!
!!    call DeleteGraph (g) deallocates the internal storage associated
!!        with the NGraphType variable G.
!!
!!    call AddEdge (g, from, to) adds to the graph G an edge from the node
!!        FROM to the node TO.  FROM is a default integer variable, and TO
!!        is either a default integer variable or default integer rank-1
!!        array.  In the latter case, a sequence of edges is added to G.
!!        Edges that are not consistent with the size and type of graph
!!        are silently ignored.  In the case of a undirected graph, the
!!        edge from TO to FROM is also added.
!!
!!    call AddClique (g, clique) adds a clique of edges to the graph G.
!!        CLIQUE is a rank-1 default integer array, and an edge from
!!        CLIQUE(j) to CLIQUE(k) is added for all pairs (j,k).  Edges
!!        that are inconsistent with the size and type of graph are
!!        silently ignored.
!!
!!    call DeleteEdge (g, from [,to]) deletes from the graph G the edge from
!!        the node FROM to the node TO.  This edge need not actually exist
!!        in the graph.  If the optional argument TO is missing, then all
!!        edges emanating from FROM are deleted.  In the case of a multigraph,
!!        _all_ edges from FROM to TO are deleted; the edge index is _not_
!!        simply decremented.
!!
!!    call GetNeighborList (g, n, nbr [,index]) returns the neighbor or
!!        adjacency list of node N in the graph G.  NBR is a pointer
!!        to a rank-1, default integer array which contains the neighbors
!!        of node N on return.  The optional argument INDEX is also a pointer
!!        to a rank-1, default integer array of the same size as NBR that
!!        on return contains the indices of the edges from the node N to the
!!        nodes in NBR; this is meaningful only for a multigraph.  If N is
!!        not a valid node then NBR and INDEX are nullified.
!!        
!!        CAUTION!  The pointers NBR and INDEX are allocated within this
!!        procedure.  Thus to avoid a memory leak, it is important that
!!        these pointers should not, on entry to GetNeighborList, point
!!        to memory inaccessible through other means.
!!        
!!    call GetNeighborStructure (g, xnbr, nbr) returns the neighbor or
!!        adjacency structure of the graph G in a compressed F77-style
!!        format.  XNBR and NBR are pointers to rank-1, default integer
!!        arrays.  NBR(XNBR(j):XNBR(j+1)-1) is the neighbor list for
!!        node j.  If G is a graph with N nodes, then XNBR has size N+1
!!        and NBR has size XNBR(N+1)-1.
!!        
!!        CAUTION!  The pointers XNBR and NBR are allocated within this
!!        procedure.  Thus to avoid a memory leak, it is important that
!!        these pointers should not, on entry to GetNeighborStructure,
!!        point to memory inaccessible through other means.
!!
!!
!!  IMPLEMENTATION DETAILS
!!
!!    (Coming soon!)
!!
!!
!!  CHANGE HISTORY
!!
!!    $Log:   /pvcs.config/t3d/src/GraphModule.f9a  $
!PVCS
!PVCS   Rev 1.2   Mon Nov 30 12:47:42 1998   nnc
!PVCSFixed "cycle" bug introduced in last version.
!PVCS
!PVCS   Rev 1.1   Thu Nov 19 14:45:30 1998   nnc
!PVCSAdded documentation and a few error checks.
!PVCS
!PVCS   Rev 1.0   Mon Nov 16 14:14:02 1998   dcg
!PVCSInitial revision.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module GraphModule
 
  implicit none
  private
 
  public  :: CreateGraph, DeleteGraph
  public  :: AddEdge, AddClique, DeleteEdge
  public  :: GetNeighborList, GetNeighborStructure
  private :: AddEdge1
 
  interface AddEdge
    module procedure AddEdge, AddEdge1
  end interface
 
  private :: EmptyList, EmptyListQ, ListLength, DeleteList, AddToList, DeleteFromList, ListToVector
 
  type, public :: NGraphType
    private
    integer :: n                                  ! = 0
    logical :: directed                           ! = .false.
    logical :: mult_edge                          ! = .false.
    logical :: self_edge                          ! = .false.
    type(ListType), dimension(:), pointer :: nbrs ! => null()
  end type NGraphType
 
  type, private :: ListType
    type(ListItemType), pointer :: first
  end type ListType
 
  type, private :: ListItemType
    integer :: node
    integer :: index
    type(ListType) :: rest
  end type ListItemType
 
  contains
 
    function CreateGraph (n, directed, mult_edge, self_edge) result (g)
 
      integer, intent(in)  :: n
      logical, intent(in), optional :: directed
      logical, intent(in), optional :: mult_edge
      logical, intent(in), optional :: self_edge
      type(NGraphType) :: g
 
      integer :: j
 
      g % n = n
      g % directed  = .false.
      g % mult_edge = .false.
      g % self_edge = .false.
 
      if (present(directed))  g % directed  = directed
      if (present(mult_edge)) g % mult_edge = mult_edge
      if (present(self_edge)) g % self_edge = self_edge
 
      allocate(g % nbrs(n))
 
      do j = 1, n
        g % nbrs(j) = EmptyList()
      end do
 
    end function CreateGraph
 
    subroutine DeleteGraph (g)
 
      type(NGraphType) :: g
 
      integer :: j
 
      g % n = 0
      do j = 1, size(g % nbrs)
        call DeleteList (g % nbrs(j))
      end do
 
      deallocate (g % nbrs)
 
    end subroutine DeleteGraph
 
    subroutine AddEdge (g, from, to)
 
      type(NGraphType), intent(inout) :: g
      integer, intent(in) :: from, to
 
      if (from < 1 .or. from > g % n) return
      if (to < 1 .or. to > g % n) return
      if (.not. g % self_edge .and. from == to) return
 
      call AddToList (g % nbrs(from), to)
      if (g % directed) return
      call AddToList (g % nbrs(to), from)
 
    end subroutine AddEdge
 
    subroutine AddEdge1 (g, from, to)
 
      type(NGraphType), intent(inout) :: g
      integer, intent(in) :: from
      integer, dimension(:), intent(in) :: to
 
      integer :: j
 
      if (from < 1 .or. from > g % n) return

      do j = 1, size(to)
        if (to(j) < 1 .or. to(j) > g % n) cycle
        if (.not. g % self_edge .and. from == to(j)) cycle
        call AddToList (g % nbrs(from), to(j))
        if (g % directed) return
        call AddToList (g % nbrs(to(j)), from)
      end do
 
    end subroutine AddEdge1
 
    subroutine AddClique (g, clique)
 
      type(NGraphType), intent(inout) :: g
      integer, dimension(:), intent(in) :: clique
 
      integer :: j, k, from, to
 
      do j = 1, size(clique)
        from = clique(j)
        if (from < 1 .or. from > g % n) cycle
        do k = 1, size(clique)
          to = clique(k)
          if (to == from .and. .not. g % self_edge) cycle
          if (to < 1 .or. to > g % n) cycle
          call AddToList (g % nbrs(from), to)
        end do
      end do
 
    end subroutine AddClique
 
    subroutine DeleteEdge (g, from, to)
 
      type(NGraphType), intent(inout) :: g
      integer, intent(in) :: from
      integer, intent(in), optional :: to
 
      if (from < 1 .or. from > g % n) return
 
      if (present(to)) then
        call DeleteFromList (g % nbrs(from), to)
      else
        call DeleteList (g % nbrs(from))
      end if
 
    end subroutine DeleteEdge
 
    subroutine GetNeighborList (g, n, nbr, index)
 
      type(NGraphType), intent(in) :: g
      integer, intent(in) :: n
      integer, dimension(:), pointer :: nbr
      integer, dimension(:), pointer, optional :: index
 
      integer :: degree, j
      type(ListType) :: l
 
      if (n < 1 .or. n > g % n) then
        nullify(nbr)
        if (present(index)) nullify(index)
        return
      end if
 
      degree = ListLength(g % nbrs(n))
 
      if (present(index)) then
 
        allocate(nbr(degree), index(degree))
        l = g % nbrs(n)
        do j = 1, degree
          nbr(j)   = l % first % node
          index(j) = l % first % index
          l = l % first % rest
        end do
 
      else
 
        allocate(nbr(degree))
        l = g % nbrs(n)
        do j = 1, degree
          nbr(j)   = l % first % node
          l = l % first % rest
        end do
 
      end if
 
    end subroutine GetNeighborList
 
    subroutine GetNeighborStructure (g, xnbr, nbr)
 
      type(NGraphType), intent(in) :: g
      integer, dimension(:), pointer :: xnbr, nbr
 
      integer :: i
 
      allocate(xnbr(g%n+1))
 
      xnbr(1) = 1
      do i = 1, g % n
        xnbr(i+1) = xnbr(i) + ListLength(g % nbrs(i))
      end do
 
      allocate(nbr(xnbr(g%n+1)-1))
 
      do i = 1, g % n
        call ListToVector (nbr(xnbr(i):), g % nbrs(i))
      end do
 
    end subroutine GetNeighborStructure
 
 
    function EmptyList () result (l)
      type(ListType) :: l
      nullify(l % first)
    end function EmptyList
 
    function EmptyListQ (l) result (value)
      type(ListType), intent(in) :: l
      logical :: value
      value = .not. associated(l % first)
    end function EmptyListQ
 
    recursive function ListLength (l) result (value)
      type(ListType), intent(in) :: l
      integer :: value
      if (EmptyListQ(l)) then
        value = 0
      else
        value = ListLength (l % first % rest) + 1
      end if
    end function ListLength
 
    recursive subroutine DeleteList (l)
      type(ListType), intent(inout) :: l
      if (.not. EmptyListQ(l)) then
        call DeleteList(l % first % rest)
        deallocate (l % first)
      end if
    end subroutine DeleteList
 
    recursive subroutine AddToList (l, n)
 
      type(ListType), intent(inout) :: l
      integer, intent(in) :: n
 
      type(ListType) :: r
 
      if(EmptyListQ(l)) then
        allocate(l % first)
        l % first % node  = n
        l % first % index = 1
        l % first % rest  = EmptyList()
      else if (n < l % first % node) then
        r = l
        allocate(l % first)
        l % first % node  = n
        l % first % index = 1
        l % first % rest  = r
      else if (n == l % first % node) then
        l % first % index = 1 + l % first % index
      else
        call AddToList (l % first % rest, n)
      end if
 
    end subroutine AddToList
 
    recursive subroutine DeleteFromList (l, n)
 
      type(ListType), intent(inout) :: l
      integer, intent(in) :: n
 
      type(ListType) :: r
 
      if(EmptyListQ(l)) then
        return
      else if (n < l % first % node) then
        return
      else if (n == l % first % node) then
        r = l % first % rest
        deallocate(l % first)
        l = r
      else
        call DeleteFromList (l % first % rest, n)
      end if
 
    end subroutine DeleteFromList
 
    recursive subroutine ListToVector (v, l)
 
      integer, dimension(:), intent(out) :: v
      type(ListType), intent(in) :: l
 
      integer :: n
      type(ListType) :: r
 
      !if (.not. EmptyListQ(s)) then
      !  v(1) = s % first % node
      !  call ListToVector (v(2:), s % first % rest)
      !end if
 
      r = l
      n = 1
      do
        if (EmptyListQ(r)) exit
        v(n) = r % first % node
        r = r % first % rest
        n = n + 1
      end do
 
    end subroutine ListToVector
 
end module GraphModule
