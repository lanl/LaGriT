!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!  LPFILTER -- Smooth mesh object using low-pass filtering.
!!
!!      call lpfilter (cmo, mpary, mpno, degree, k_pb, mode, ierror)
!!
!!  This routine smooths 1D, 2D, and 3D meshes and networks using a low-pass
!!  filtering technique described in [1,2].  While primarily intended for
!!  surfaces, the routine can be applied in a variety of situations.
!!
!!  Network smoothing.  Here the mesh object is simply treated as a complex
!!  of cells.  This could be a simple 2D surface or a network of 2D surfaces.
!!  In either case, the boundary and singularities of the mesh form a 1D
!!  network of curves.  The singularities of this 1D network form an isolated
!!  point set.  The smoothing respects this hierarchy of topological features.
!!  The isolated point set is kept fixed and the 1D curve network is smoothed
!!  without regard to the attached surfaces.
!!
!!  Material smoothing.  Here the mesh object is treated as a multi-material
!!  mesh.  The material interface network of 2D surfaces is identified as a
!!  topological feature, and smoothing applied to an extended hierarchy of
!!  topological features as above, with the surfaces smoothed without
!!  regard to the attached material volumes.
!!
!!  WARNING: MATERIAL SMOOTHING IS NOT ROBUST AS IT WILL LIKELY INVERT CELLS.
!!
!!  The smoothing also respects the hierarchy of surface constraints.  These
!!  constraints (if any) serve to further partition the topological hierarchy 
!!  into groups with common constraints.  Provided that the surface constraints
!!  are all planar, the smoothed mesh will satisfy the constraints.
!!
!!  The full matrix of possibilities is given below.
!!
!!                      Geometric Degree
!!
!!                     |  1  |  2  |  3  |
!!                   --+-----+-----+-----+
!!                   1 |  *  |  N  |  N  |
!!      Topological  --+-----+-----+-----+
!!        Degree     2 |  *  |  M  | N/M |
!!                   --+-----+-----+-----+
!!                   3 |  *  |  *  |  M  |
!!                   --+-----+-----+-----+
!!
!!  See the documentation that accompanies LowPassFilterModule for more details.
!!
!!
!!  [1] Gabriel Taubin, "A signal processing approach to fair surface design".
!!      Computer Graphics, p351-358, August 1995 (Proceedings SIGGRAPH'95).
!!  [2] Taubin, Zhang, and Golub, "Optimal surface smoothing as filter design".
!!      IBM research report, RC-20404(#90237), Computer Sciences, 3/12/96.
!!
!!  INPUT ARGUMENTS
!!
!!    cmo    -- Mesh object name.
!!    mpary  -- Array of nodes to be smoothed.
!!    mpno   -- Length of mpary.
!!    degree -- Polynomial degree of the Hamming filter.
!!    k_pb   -- Pass-band value.  Should lie in the interval (0,2).
!!    mode   -- Smoothing mode.  This controls how the mesh object will be used.
!!              If mode == 1 then the mesh is regarded as a multi-material mesh.
!!              Otherwise only the ITET information is used, and the mesh is
!!              regarded simply as a complex of cells.
!!
!!  OUTPUT ARGUMENTS
!!
!!    ierror -- Error flag (== 0 ==> OK, /= 0 ==> Error)
!!
!!
!!  CHANGE HISTORY
!!
!!    $Log: lpfilter.f90,v $
!!    Revision 2.00  2007/11/05 19:46:00  spchu
!!    Import to CVS
!!
!PVCS
!PVCS   Rev 1.3   Sat May 01 22:09:46 1999   nnc
!PVCSFixed error in which the smoothed position of a child node was
!PVCSoften overwritten with its original value.
!PVCS
!PVCS   Rev 1.2   Wed Feb 17 12:36:16 1999   nnc
!PVCSAdded code to properly handle the case when NCONBND
!PVCSis not defined in the mesh object.
!PVCS
!PVCS   Rev 1.1   Mon Nov 30 16:26:32 1998   nnc
!PVCSAdded documentation.
!PVCS
!PVCS   Rev 1.0   Mon Nov 16 14:18:56 1998   dcg
!PVCSInitial revision.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

subroutine lpfilter( cmo, mpary, mpno, degree, k_pb, mode, ierror )
 
  use LowPassFilterModule
  implicit none
 
  character*(*) cmo
  integer mpary(*), mpno, degree, mode, ierror
  real*8 k_pb
 
  pointer (ipxic,xic), (ipyic,yic), (ipzic,zic)
  real*8 xic(*), yic(*), zic(*)
 
  pointer (ipitet,itet), (ipjtet,jtet), (ipitetoff,itetoff), (ipjtetoff,jtetoff), &
          (ipitettyp,itettyp), (ipitetclr,itetclr), (ipitp1,itp1), (ipisn1,isn1), &
          (ipicr1,icr1), (ipicontab,icontab)
  integer itet(*), jtet(*), itetoff(*), jtetoff(*), itettyp(*), itetclr(*), &
          itp1(*), isn1(*), icr1(*), icontab(50,*)
 
  integer :: topo_dim, geom_dim, ncells, nnodes, mbndry, nconbnd, ilen, ityp
 
  integer,        dimension(:),   allocatable :: mask, pmap, xset, cmat
  integer,        dimension(:,:), allocatable :: cvtx, cnbr
  real(kind=r8),  dimension(:,:), allocatable :: x
  real(kind=r8),  dimension(:),   allocatable :: fc, cvol
  type(ASetType), dimension(:),   allocatable :: aset
  real(kind=r8),  dimension(3)                :: q1, q2, q3
  real(kind=r8),  dimension(2)                :: p1, p2
 
  character(len=132) :: logmess
 
  integer :: i, j, k, node, nattr, nneg, nvtx
 
  external cmo_get_info, unpackpc, writloga
 
  write(logmess,fmt='(a,f5.3,a,i3)') 'LPFILTER: pass band = ', k_pb, ', filter degree = ', degree
  call writloga ('default', 0, logmess, 0, ierror)
  if (mode == 1) then
    write(logmess,fmt='(a)') 'LPFILTER: WARNING!  Material smoothing mode may tangle the mesh'
    call writloga ('default', 0, logmess, 0, ierror)
  end if
 
  call cmo_get_info ('ndimensions_topo', cmo,  topo_dim, ilen, ityp, ierror)
  call cmo_get_info ('ndimensions_geom', cmo,  geom_dim, ilen, ityp, ierror)
 
  call cmo_get_info ('nelements', cmo,    ncells, ilen, ityp, ierror)
  call cmo_get_info (   'nnodes', cmo,    nnodes, ilen, ityp, ierror)
  call cmo_get_info (   'mbndry', cmo,    mbndry, ilen, ityp, ierror)
  call cmo_get_info (     'itet', cmo,    ipitet, ilen, ityp, ierror)
  call cmo_get_info (     'jtet', cmo,    ipjtet, ilen, ityp, ierror)
  call cmo_get_info (  'itetoff', cmo, ipitetoff, ilen, ityp, ierror)
  call cmo_get_info (  'jtetoff', cmo, ipjtetoff, ilen, ityp, ierror)
  call cmo_get_info (  'itetclr', cmo, ipitetclr, ilen, ityp, ierror)
  call cmo_get_info (  'itettyp', cmo, ipitettyp, ilen, ityp, ierror)
  call cmo_get_info (     'itp1', cmo,    ipitp1, ilen, ityp, ierror)
  call cmo_get_info (     'isn1', cmo,    ipisn1, ilen, ityp, ierror)
  call cmo_get_info (      'xic', cmo,     ipxic, ilen, ityp, ierror)
  call cmo_get_info (      'yic', cmo,     ipyic, ilen, ityp, ierror)
  call cmo_get_info (      'zic', cmo,     ipzic, ilen, ityp, ierror)
 
 !!!
 !!! Verify that the mesh object is purely simplicial.
 
  select case (topo_dim)
  case (1)
    nvtx = 2
  case (2)
    if (any(itettyp(1:ncells) /= 3)) then
      write(logmess,fmt='(a)') 'LPFILTER: 2D mesh must be purely triangular.  Aborting.'
      call writloga('default', 0, logmess, 0, ierror)
      return
    end if
    nvtx = 3
  case (3)
    if (any(itettyp(1:ncells) /= 5)) then
      write(logmess,fmt='(a)') 'LPFILTER: 3D mesh must be purely tetrahedral.  Aborting.'
      call writloga('default', 0, logmess, 0, ierror)
      return
    end if
    nvtx = 4
  end select
 
 !!!
 !!! Construct a "clean" cell structure.
 
  allocate (pmap(nnodes))
  call unpackpc (nnodes, itp1, isn1, pmap)   !! Child --> parent map.
 
  allocate (cvtx(nvtx,ncells))
  do j = 1, ncells
    do k = 1, nvtx
      cvtx(k,j) = pmap(itet(k+itetoff(j)))
    end do
  end do
 
  if (mode == 1) then
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
  end if
 
 !!!
 !!! Construct the node "attribute" structure (here, surface constraints).
 
  call cmo_get_info (  'nconbnd', cmo,   nconbnd, ilen, ityp, ierror)
  if (ierror /= 0) nconbnd = 0
  call cmo_get_info (  'icontab', cmo, ipicontab, ilen, ityp, ierror)
  if (ierror /= 0) nconbnd = 0
  call cmo_get_info (     'icr1', cmo,    ipicr1, ilen, ityp, ierror)

  allocate (xset(nnodes))
  if (nconbnd == 0) then
    xset = 0
  else
    do j = 1, nnodes
      xset(j) = icr1(j)
    end do
  end if
 
  allocate (aset(nconbnd))
  do j = 1, nconbnd
    nattr = icontab(1,j)
    if (nattr <= 0) then
      nullify(aset(j) % attr)
    else
      allocate (aset(j) % attr(nattr))
      do i = 1, nattr
        aset(j) % attr(i) = icontab(2+i,j)
      end do
    end if
  end do
 
 !!!
 !!! Mask array.
 
  allocate (mask(nnodes))
  mask = 0
  do j = 1, mpno
    mask(pmap(mpary(j))) = 1
  end do
 
 !!!
 !!! Smooth the mesh by low pass filtering.
 
  if (mode == 1) then
    call CreateLaplacian (mask, xset, aset, cvtx, cnbr, cmat)
    deallocate (pmap, xset, aset, cnbr, cmat)
  else
    call CreateLaplacian (mask, xset, aset, cvtx)
    deallocate (pmap, xset, aset)
  end if
 
  allocate (x(geom_dim,nnodes), fc(0:degree))
  call HammingFilterCoef (fc, degree, k_pb)
 
  select case (geom_dim)
 
  case (2) !! Mesh in 2D
 
    do j = 1, nnodes
      x(1,j) = xic(j)
      x(2,j) = yic(j)
    end do
 
    x = PolynomialFilter (x, fc)
    call DeleteLaplacian ()
 
   !!!
   !!! Check for a tangled mesh.
 
    if (topo_dim == geom_dim) then
      allocate (cvol(ncells))
      do j = 1, ncells
        p1 = x(:,cvtx(2,j)) - x(:,cvtx(1,j))
        p2 = x(:,cvtx(3,j)) - x(:,cvtx(1,j))
        cvol(j) = 0.5_r8 * (p1(1) * p2(2) - p1(2) * p2(1))
      end do
      nneg = count(cvol < 0.0_r8)
      if (nneg > 0) then
        write(logmess,fmt='(a,i6,a,es10.3)') 'LPFILTER: WARNING! ', nneg, &
          ' cells have negative volume.  Min volume = ', minval(cvol)
        call writloga ('default', 0, logmess, 0, ierror)
      end if
      deallocate (cvol)
    end if
 
   !!!
   !!! Copy the smoothed node positions into the mesh object.
 
    do j = 1, nnodes
      if (mask(j) == 0) cycle
      xic(j) = x(1,j)
      yic(j) = x(2,j)
      if (itp1(j) /= 41) cycle
      node = isn1(j)
      do while (node /= j)
        xic(node) = xic(j)
        yic(node) = yic(j)
        node = isn1(node)
      end do
    end do
 
  case (3)  !! Mesh in 3D
 
    do j = 1, nnodes
      x(1,j) = xic(j)
      x(2,j) = yic(j)
      x(3,j) = zic(j)
    end do
 
    x = PolynomialFilter (x, fc)
    call DeleteLaplacian ()
 
   !!!
   !!! Check for a tangled mesh.
 
    if (topo_dim == geom_dim) then
      allocate (cvol(ncells))
      do j = 1, ncells
        q1 = x(:,cvtx(2,j)) - x(:,cvtx(1,j))
        q2 = x(:,cvtx(3,j)) - x(:,cvtx(1,j))
        q3 = x(:,cvtx(4,j)) - x(:,cvtx(1,j))
        cvol(j) = ( q1(1) * (q2(2) * q3(3) - q2(3) * q3(2)) + &
                    q2(1) * (q3(2) * q1(3) - q3(3) * q1(2)) + &
                    q3(1) * (q1(2) * q2(3) - q1(3) * q2(2)) ) / 6.0_r8
      end do
      nneg = count(cvol < 0.0_r8)
      if (nneg > 0) then
        write(logmess,fmt='(a,i6,a,es10.3)') 'LPFILTER: WARNING! ', nneg, &
          ' cells have negative volume.  Min volume = ', minval(cvol)
        call writloga ('default', 0, logmess, 0, ierror)
      end if
      deallocate (cvol)
    end if
 
   !!!
   !!! Copy the smoothed node positions into the mesh object.
 
    do j = 1, nnodes
      if (mask(j) == 0) cycle
      xic(j) = x(1,j)
      yic(j) = x(2,j)
      zic(j) = x(3,j)
      if (itp1(j) /= 41) cycle
      node = isn1(j)
      do while (node /= j)
        xic(node) = xic(j)
        yic(node) = yic(j)
        zic(node) = zic(j)
        node = isn1(node)
      end do
    end do
 
  end select
 
  deallocate (cvtx, x, fc, mask)
 
end subroutine lpfilter
 
 
