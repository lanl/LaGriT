!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  LowPassFilterModule -- A Fortran 90 module for low-pass filtering of
!!      discrete graph signals.
!!
!!  This module provides the core procedures that implement the low-pass
!!  filtering of discrete graph signals described in [1,2].  By generalizing
!!  classical discrete fourier analysis to discrete graph signals --
!!  functions defined on the nodes of an undirected graph -- the problem
!!  of smoothing is reduced to that of low-pass filtering.
!!
!!  Classical Fourier analysis decomposes a signal into the eigencomponents
!!  of the Laplacian, so to extend Fourier analysis to discrete graph signals,
!!  one merely needs to define a generalized laplacian on the graph.  The
!!  simplest definition, and the one used here, is to define the "laplacian"
!!  at a node to be the difference between the signal at that node and the
!!  mean of the signals at the neighboring nodes.
!!
!!  In the context of surface and mesh smoothing, the relevent discrete graph
!!  signal is the node coordinates on the graph associated with the simplicial
!!  mesh.  In this instance, we may desire that the filtering preserve
!!  certain intrinsic topological features of mesh as well as some extrinsic
!!  "geometric" features.  This is easily accomplished by modifying the
!!  neighbor structure of the graph (see CreateLaplacian below).
!!
!!  [1] Gabriel Taubin, "A signal processing approach to fair surface design".
!!      Computer Graphics, p351-358, August 1995 (Proceedings SIGGRAPH'95).
!!  [2] Taubin, Zhang, and Golub, "Optimal surface smoothing as filter design".
!!      IBM research report, RC-20404(#90237), Computer Sciences, 3/12/96.
!!
!!  PROGRAMMING INTERFACE
!!
!!    A integer parameter, r8, is provided that gives the kind value of all
!!        reals used in the module.  Typically this should correspond to
!!        8-byte reals.
!!
!!    An ADT, ASetType, is provided to facilitate the construction of the
!!        Laplacian (see CreateLaplacian below).  It has a single component
!!        which is a pointer to a rank-1 integer array.
!!
!!    call HammingFilterCoef (f, n, k_pb [,sigma]) returns the polynomial
!!        coefficients of the Nth degree Hamming filter.  The coefficients
!!        are normalized to sum to 1, which make them, strictly speaking,
!!        not the true Hamming coefficients.  F is an intent(out), rank-1,
!!        real array containing the filter coefficients.  Its length must be
!!        at least N+1.  K_PB is a real intent(in) variable that specifies
!!        the pass-band value, and should lie within the interval (0,2).
!!        The Hamming filter can be regarded as the convolution of the ideal
!!        low-pass filter with a mollification kernel that is a trig series
!!        defined by the filter coefficients.  If the real, intent(in)
!!        variable SIGMA is not present, the pass-band is shifted right by
!!        an amount equal to the radius of this mollification kernel.  This
!!        assures that the filter will be approximately 1 in the entire
!!        interval (0, K_PB).  If SIGMA is present, its value is used as the
!!        shift amount.
!!
!!    PolynomialFilter (x, f) returns the filtered "signal".  F is a rank-1,
!!        real, intent(in) array that contains the coefficients of a polynomial
!!        filter, such as is returned by HammingFilterCoef.  X is a rank-2,
!!        real, intent(in) array that contains the input "signal"; X(:,j) is
!!        the signal at node j.
!!
!!    GaussianFilter (x, lambda) returns the Gaussian filtered "signal".
!!        LAMBDA is a real, intent(in) variable, and X is a rank-2, real,
!!        intent(in) array that contains the input "signal".  The filtered
!!        signal is simply  X - LAMBDA * Laplacian(X).
!!
!!    call CreateLaplacian (mask, xset, aset, cvtx [,cnbr, cmat]) constructs
!!        the Laplacian that is central to the filtering algorithm.
!!
!!        Unless otherwise noted, all arguments are intent(in).
!!
!!        MASK is a rank-1, integer array.  Node j is ignored if MASK(j) == 0.
!!
!!        CVTX is a rank-2 integer array giving the vertices of the
!!            simplicial cells which form the mesh.  CVTX(:,j) are the
!!            vertices of cell j.
!!
!!        CNBR is a rank-2 integer array giving the neighbors of the cells.
!!            CNBR(:,j) are the neighbors of cell j encoded in the following
!!            manner.  If cells j and j' are adjacent, with face k of cell j
!!            and face k' of cell j' being the shared face then
!!            CNBR(k,j) = k' + n * (j'-1) and CNBR(k',j') = k + n * (j-1)
!!            where n is the number of vertices of the simplicial cell.
!!            If CNBR(k,j) = 0 then this face is a boundary face, and if
!!            CNBR(k,j) < 0 then this is a material interface face, with
!!            |CNBR(k,j)| giving the neighbor encoded as above.
!!
!!        CMAT is a rank-1, integer array giving the material numbers for
!!            the cells.  If adjacent cells have different material numbers
!!            then their shared face should be indicated as a material
!!            interface face in CNBR, and conversely.
!!
!!        The graph that defines the laplacian is derived from the simplicial
!!        mesh described by CVTX, CNBR, and CMAT.  There are two possible
!!        cases.  If CNBR and CMAT are present, then the mesh is a multi-
!!        material mesh and there is a distinguished hierarchy of topological
!!        features.  For a 3D mesh, there are the 3D material volumes, the
!!        2D material interface network, the 1D network of interface
!!        boundaries/singularities, and the isolated point set of singularity
!!        points of the 1D network.  The neighbors of a node in one class are
!!        restricted to those in the same or subsequent classes.  By
!!        modifying the graph in this way, the resulting filtering will respect
!!        these topological features; a material interface, for example, is
!!        smoothed as a unit without regard to connected volume nodes.  For
!!        a 2D mesh the situation is similar.
!!
!!        If CNBR and CMAT are absent, then the mesh is complex of simplicial
!!        cells described completely by CVTX.  It may be a simple 2D surface,
!!        for example, or a 2D surface network such as the material interface
!!        network of a multi-material 3D mesh.  In any case, there is also
!!        a (shorter) hierarchy of topological features, and the graph
!!        associated with the mesh is modified as above.
!!
!!        The remaining arguments describe the extrinsic "geometric" features
!!        of the mesh that the filtering should respect.  This is modeled in
!!        the following way.  Associated with each node is an integer set of
!!        "attributes" (possibly empty).  The neighbors of one node are
!!        restricted to those nodes whose attribute sets contain the attribute
!!        set of the first.
!!
!!        XSET is a rank-1 integer array giving the attribute set index for
!!            each node.  If XSET(j) == 0 the the attribute set for node j
!!            is empty.
!!
!!        ASET is a rank-1 array of ASetType values.  ASET(j) is the jth
!!            attribute set.
!!
!!    call DeleteLaplacian () deallocates all storage associated with the
!!        Laplacian.
!!
!!
!!    CHANGE HISTORY
!!
!!        $Log:   /pvcs.config/t3d/src/LowPassFilterModule.f9a  $
!PVCS
!PVCS   Rev 1.2   Sat May 01 22:08:50 1999   nnc
!PVCSMade argument MASK of CreateLaplacian intent(in).
!PVCS
!PVCS   Rev 1.1   Mon Nov 30 14:52:40 1998   nnc
!PVCSAdded documentation.
!PVCS
!PVCS   Rev 1.0   Mon Nov 16 14:19:00 1998   dcg
!PVCSInitial revision.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

module LowPassFilterModule
 
  implicit none
  private
 
  public  :: HammingFilterCoef, PolynomialFilter, GaussianFilter, CreateLaplacian, DeleteLaplacian
  private :: laplacian
 
  ! This should give 8-byte reals (double precision)
  integer, parameter, public :: r8 = selected_real_kind (10, 50)
 
  type, public :: ASetType
    integer, dimension(:), pointer :: attr
  end type ASetType
 
  integer, dimension(:), pointer, private, save :: xnbrs, nbrs
 
  contains
 
  subroutine HammingFilterCoef( f, n, k_pb, sigma )
 
    real(kind=r8), dimension(0:), intent(out) :: f
    integer, intent(in) :: n
    real(kind=r8), intent(in) :: k_pb
    real(kind=r8), intent(in), optional :: sigma
 
    integer :: j
    real(kind=r8) :: pi, theta_pb
    real(kind=r8), dimension(0:size(f)-1) :: w
 
    pi = 4.0_r8 * atan(1.0_r8)
 
    theta_pb = acos( 1.0_r8 - 0.5_r8 * k_pb )
 
    if (present(sigma)) then
      theta_pb = theta_pb + sigma
    else
      theta_pb = theta_pb + 1.0_r8 / (0.2331592_r8 + 0.1592799_r8 * n)
    end if
 
    !! Truncated fourier series for ideal LP filter (rectangular window)
    f(0) = theta_pb / pi
    do j = 1, n
      f(j) = 2.0_r8 * sin( j * theta_pb ) / (j * pi)
    end do
 
    !! Hamming window coefficients.
    w(0) = 1.0_r8
    do j = 1, n
      w(j) = 0.54_r8 + 0.46_r8 * cos( j * pi / (n + 1) )
    end do
 
    f = f * w       !! The Hamming filter coefficients.
    f = f / sum(f)  !! Normalize (strictly speaking this shouldn't be done)
 
    end subroutine HammingFilterCoef
 
 
    function PolynomialFilter( x, f ) result( y )
 
      real(kind=r8), dimension(:,:), intent(inout) :: x
      real(kind=r8), dimension(0:),  intent(in)    :: f
      real(kind=r8), dimension(size(x,dim=1),size(x,dim=2)) :: y
 
      integer :: n, j
      real(kind=r8), dimension(:,:), pointer :: y0, y1, y2, yt
      real(kind=r8), dimension(size(x,dim=1),size(x,dim=2),3), target :: work
 
      y0 => work(:,:,1)
      y1 => work(:,:,2)
      y2 => work(:,:,3)
 
      n = size(f) - 1
 
      y2 = 0.0_r8
      y1 = f(n) * x
 
      do j = n-1, 1, -1
        y0 = 2.0_r8 * y1 - laplacian(y1) - y2 + f(j) * x
        yt => y2  !! Shift the sequence values
        y2 => y1
        y1 => y0
        y0 => yt
      end do
 
      y = y1 - 0.5_r8 * laplacian(y1) - y2 + f(0) * x
 
    end function PolynomialFilter
 
 
    function GaussianFilter( x, lambda ) result( y )
 
      real(kind=r8), dimension(:,:), intent(inout) :: x
      real(kind=r8),                 intent(in)    :: lambda
      real(kind=r8), dimension(size(x,dim=1),size(x,dim=2)) :: y
 
      y = x - lambda * laplacian(x)
 
    end function GaussianFilter
 
 
    subroutine CreateLaplacian( mask, xset, aset, cvtx, cnbr, cmat )
 
      use GraphModule
 
      integer, dimension(:), intent(in) :: mask              ! Node mask
      integer, dimension(:), intent(in) :: xset              ! Node attribute set index
      type(ASetType), dimension(:), intent(in) :: aset       ! Attribute sets
      integer, dimension(:,:), intent(in) :: cvtx            ! Cell vertices
      integer, dimension(:,:), intent(in), optional :: cnbr  ! Cell neighbors (encoded)
      integer, dimension(:), intent(in), optional :: cmat    ! Cell materials
 
      integer, dimension(size(mask)) :: tag
      integer, dimension(:), pointer :: nbr, index
      integer :: nnod, ncell, nvtx, i, j, k, jnbr, dimen
      logical :: multimaterial
      type(NGraphType) :: g
 
      integer, dimension(3,4), parameter :: tet_face_vtx = &
          reshape( source=(/ 2,3,4, 1,4,3, 1,2,4, 1,3,2 /), shape=(/3,4/) )
      integer, dimension(2,3), parameter :: tri_face_vtx = &
          reshape( source=(/ 2,3, 3,1, 1,2 /), shape=(/2,3/) )
 
      nnod  = size(mask)
      nvtx  = size(cvtx, dim=1)
      ncell = size(cvtx, dim=2)

      tag = mask
 
      if (present(cnbr) .and. present(cmat)) then
        multimaterial = .true.
      else
        multimaterial = .false.
      end if
 
      g = CreateGraph (nnod, directed=.true., mult_edge=.true.)
 
      !! Add the edges of the surface/line network to the graph.
 
      if (multimaterial) then
 
        !! Make an initial pass through the cells adding the edges of
        !! material interface faces and boundary faces only.
 
        do j = 1, ncell
          do k = 1, nvtx
            if (cnbr(k,j) > 0) cycle  ! This is an interior face
            if (cnbr(k,j) < 0) then   ! This is an interface face
              jnbr = 1 + (abs(cnbr(k,j)) - 1) / nvtx
              if (cmat(jnbr) <= cmat(j)) cycle  ! We'll do this one later.
            end if
            select case (nvtx)
            case (3)
              call AddClique (g, cvtx(tri_face_vtx(:,k),j))
            case (4)
              call AddClique (g, cvtx(tet_face_vtx(:,k),j))
            end select
          end do
        end do
 
        dimen = nvtx - 2
 
      else
 
        do j = 1, ncell
         call AddClique (g, cvtx(:,j))
        end do
 
        dimen = nvtx - 1
 
      end if
 
      !! Examine the surface edges to determine the topological hierarchy of
      !! point types: surface interior, surface boundary, and boundary of
      !! surface boundary.  Nodes of one type are blinded to those of the
      !! preceding types by dropping the relevent edges.
 
      do j = 1, nnod
 
        call GetNeighborList (g, j, nbr, index)
        if (size(index) == 0) cycle
 
        if (tag(j) == 0) then
 
          call DeleteEdge (g, j) !! Ignore this node; blind it to all other nodes.
 
        else
 
          tag(j) = 0  !! Ignore this node in the subsequent pass throught the cells.
 
          select case (dimen)
 
          case (1)  !! Line Network
 
            !! The degree of this node is SIZE(INDEX):
            !!   degree = 1 <==> line endpoint node
            !!   degree = 2 <==> line interior node
            !!   degree > 2 <==> line junction node
 
            if (size(index) /= 2) then  !! Blind this node to all other nodes.
              call DeleteEdge (g, j)
            end if
 
          case (2)  !! Surface Network
 
            !! index = 1 <==> surface boundary edge
            !! index = 2 <==> surface interior edge
            !! index > 2 <==> surface singularity edge (e.g. triple line edge)
 
            if (any(index /= 2)) then
 
              !! This node lies on a surface boundary or singularity.
              !! Blind it to all nodes but those connected by boundary or
              !! singularity edges by dropping all surface-interior edges.
 
              do i = 1, size(index)
                if (index(i) == 2) then
                  call DeleteEdge (g, j, nbr(i))
                end if
              end do
 
              !! The only edges remaining for this node are boundary or singularity
              !! edges which link it to other boundary or singularity nodes.
              !! COUNT(INDEX /= 2) is the resulting degree of this node:
              !!   degree = 1 <==> line endpoint node (very strange!)
              !!   degree = 2 <==> line interior node
              !!   degree > 2 <==> line junction node (e.g. a quadruple point)
 
              if (count(index /= 2) /= 2) then  !! Blind this node to all other nodes.
                call DeleteEdge (g, j)
              end if
 
            end if
 
          end select
 
        end if
 
        deallocate (nbr, index)
 
      end do
 
      if (multimaterial) then
        !! Make a second pass through the cells, adding the edges for the material
        !! nodes.  Note that the interface and boundary nodes have been masked off.
        do j = 1, ncell
          do k = 1, nvtx
            if (tag(cvtx(k,j)) == 0) cycle
            select case (nvtx)
            case (3)
              call AddEdge (g, cvtx(k,j), cvtx(tri_face_vtx(:,k),j))
            case (4)
              call AddEdge (g, cvtx(k,j), cvtx(tet_face_vtx(:,k),j))
            end select
          end do
        end do
      end if
 
      !! Finally we examine all edges, dropping those that don't respect the hierarchy
      !! of attributes: N is blind to M if there is an attribute of N not shared by M.
 
      do j = 1, nnod
       if (xset(j) == 0) cycle
        call GetNeighborList (g, j, nbr)
        if (size(nbr) == 0) cycle
        do k = 1, size(nbr)
          if (xset(j) == xset(nbr(k))) cycle
          if (xset(nbr(k)) == 0) then
            call DeleteEdge (g, j, nbr(k))
            cycle
          end if
          do i = 1, size(aset(xset(j)) % attr)
            if (any( aset(xset(j)) % attr(i) == aset(xset(nbr(k))) % attr)) cycle
            call DeleteEdge (g, j, nbr(k))
            exit
          end do
        end do
        deallocate (nbr)
      end do
 
      !! Generate the neighbor structure which defines the topological Laplacian.
      call GetNeighborStructure (g, xnbrs, nbrs)
 
      call DeleteGraph (g)
 
    end subroutine CreateLaplacian
 
 
    subroutine DeleteLaplacian( )
 
      deallocate (xnbrs, nbrs)
 
    end subroutine DeleteLaplacian
 
 
    function laplacian (x) result (y)
 
      real(kind=r8), dimension(:,:), intent(in) :: x
      real(kind=r8), dimension(size(x,dim=1),size(x,dim=2)) :: y
 
      integer :: j, degree
 
      do j = 1, size(x,dim=2)
        degree = xnbrs(j+1) - xnbrs(j)
        if (degree == 0) then
          y(:,j) = 0.0_r8
        else
          y(:,j) = x(:,j) - sum( x(:,nbrs(xnbrs(j):xnbrs(j+1)-1)), dim=2 ) / degree
        end if
      end do
 
    end function laplacian
 
end module LowPassFilterModule
