!-----------------------------------------------------------------------
! del_vor_jtet: cell neighbors (jtet) in LaGriT's convention, and the
! conformity and orientation checks made while building them.
!
! Local faces as LaGriT's ielmface1 (blockcom.h): face j is the face
! opposite vertex j.
!   triangle  face 1 = (2,3), face 2 = (3,1), face 3 = (1,2)
!   tet       face 1 = (2,3,4), 2 = (1,4,3), 3 = (1,2,4), 4 = (1,3,2)
! For a triangle the face order is the cell's own direction around
! it, which the orientation check uses.
!
! jtet(jtetoff(it)+j), nef = nen faces per cell:
!   nef*(jt-1)+jf           face jf of cell jt, same material
!   mbndry + nef*(jt-1)+jf  face jf of cell jt, another material
!   mbndry                  no neighbor (exterior boundary)
!
! Triangles (2D meshes and fracture networks): an edge is paired with
! a triangle of the SAME itetclr if there is one. Several fractures
! may share an edge (a fracture intersection, 2 triangles of each);
! such an edge is counted as an intersection edge. With no triangle
! of the same itetclr and exactly one of another, the edge is a
! material interface; otherwise it is the fracture's boundary.
!
! Fatal problems found here are counted, with the first cells:
!   nonconforming  a tet face in 3 or more tets; a triangle edge in 3
!                  or more triangles of one itetclr
!   orientation    two neighboring triangles of one itetclr that run
!                  along their shared edge in the same direction
!-----------------------------------------------------------------------
module del_vor_jtet
   use del_vor_kinds, only: ik
   use del_vor_topology, only: build_incidence
   implicit none
   private
   public :: build_jtet, jtet_stats_t, iface_tri, iface_tet, &
             default_mbndry

   integer, parameter :: iface_tri(2, 3) = &
                         reshape([2, 3, 3, 1, 1, 2], [2, 3])
   integer, parameter :: iface_tet(3, 4) = reshape( &
                         [2, 3, 4, 1, 4, 3, 1, 2, 4, 1, 3, 2], [3, 4])

   type :: jtet_stats_t
      integer(ik) :: ninterior = 0      ! faces paired, same itetclr
      integer(ik) :: ninterface = 0     ! faces paired, other itetclr
      integer(ik) :: nboundary = 0      ! faces with no neighbor
      integer(ik) :: nintersect = 0     ! 2D: intersection edges
      integer(ik) :: nnonconf = 0       ! faces in too many cells
      integer(ik) :: nonconf_cell = 0   ! first such cell
      integer(ik) :: nonconf_face = 0   ! and its local face
      integer(ik) :: norient = 0        ! badly oriented pairs
      integer(ik) :: orient_cell(2) = 0 ! first such pair
   end type jtet_stats_t

contains

   ! LaGriT's default boundary flag, raised if the mesh has more face
   ! codes than that.
   pure function default_mbndry(nen, nelements) result(mb)
      integer(ik), intent(in) :: nen, nelements
      integer(ik) :: mb
      mb = max(16000000_ik, nen*nelements + 1)
   end function default_mbndry

   subroutine build_jtet(nnodes, nelements, nen, itetoff, itet, &
                         itetclr, mbndry, jtetoff, jtet, st)
      integer(ik), intent(in) :: nnodes, nelements, nen
      integer(ik), intent(in) :: itetoff(nelements), itet(*)
      integer(ik), intent(in) :: itetclr(nelements), mbndry
      integer(ik), intent(out) :: jtetoff(nelements)
      integer(ik), intent(out) :: jtet(nen*nelements)
      type(jtet_stats_t), intent(out) :: st

      integer(ik), allocatable :: inc_off(:), inc(:)
      integer, parameter :: maxm = 64
      integer(ik) :: mcell(maxm), mface(maxm)
      integer(ik) :: it, kt, fv(3), vmin, ic, jf, code, nf
      integer :: j, k, nm, nsame, nother, ksame, kother, nclr
      logical :: same(maxm)

      nf = nen - 1
      do it = 1, nelements
         jtetoff(it) = nen*(it - 1)
      end do
      jtet = 0
      call build_incidence(nnodes, nelements, nen, itetoff, itet, &
                           inc_off, inc)

      do it = 1, nelements
         do j = 1, int(nen)
            if (jtet(jtetoff(it) + j) /= 0) cycle
            call face_vertices(it, j, fv)
            vmin = minval(fv(1:nf))
            ! every other cell holding all the face's vertices
            nm = 0
            do ic = inc_off(vmin), inc_off(vmin + 1) - 1
               kt = inc(ic)
               if (kt == it) cycle
               jf = other_face(kt, fv)
               if (jf == 0) cycle
               if (nm < maxm) then
                  nm = nm + 1
                  mcell(nm) = kt
                  mface(nm) = jf
               end if
            end do
            nsame = 0
            nother = 0
            ksame = 0
            kother = 0
            do k = 1, nm
               same(k) = itetclr(mcell(k)) == itetclr(it)
               if (same(k)) then
                  nsame = nsame + 1
                  ksame = k
               else
                  nother = nother + 1
                  kother = k
               end if
            end do

            if (nen == 4) then
               ! tets: at most one neighbor, of any material
               if (nm == 0) then
                  call set_boundary(it, j)
               else if (nm == 1) then
                  call pair(it, j, mcell(1), mface(1), same(1))
               else
                  call nonconforming(it, j)
               end if
            else
               ! triangles: pair within the fracture (itetclr)
               if (nsame >= 2) then
                  call nonconforming(it, j)
               else if (nsame == 1) then
                  call pair(it, j, mcell(ksame), mface(ksame), .true.)
                  call check_orientation(it, j, mcell(ksame), &
                                         mface(ksame))
               else if (nother == 1) then
                  call pair(it, j, mcell(kother), mface(kother), &
                            .false.)
               else
                  call set_boundary(it, j)
               end if
               ! intersection edge: 3 or more triangles of 2 or more
               ! fractures; counted once, by its lowest cell
               if (nm >= 2 .and. it < minval(mcell(1:nm))) then
                  nclr = 1
                  if (nother > 0) nclr = 2
                  if (nclr == 2) st%nintersect = st%nintersect + 1
               end if
            end if
         end do
      end do
      deallocate (inc_off, inc)

   contains

      subroutine face_vertices(ic1, jj, f)
         integer(ik), intent(in) :: ic1
         integer, intent(in) :: jj
         integer(ik), intent(out) :: f(3)
         integer :: q
         f = 0
         if (nen == 3) then
            do q = 1, 2
               f(q) = itet(itetoff(ic1) + iface_tri(q, jj))
            end do
         else
            do q = 1, 3
               f(q) = itet(itetoff(ic1) + iface_tet(q, jj))
            end do
         end if
      end subroutine face_vertices

      ! The local face of cell kc made of the vertices f, or 0: kc must
      ! hold all of them; the face is opposite its remaining vertex.
      integer(ik) function other_face(kc, f)
         integer(ik), intent(in) :: kc, f(3)
         integer :: q, r, nfound
         integer(ik) :: v
         logical :: hit
         other_face = 0
         nfound = 0
         do q = 1, int(nen)
            v = itet(itetoff(kc) + q)
            hit = .false.
            do r = 1, int(nf)
               if (f(r) == v) hit = .true.
            end do
            if (hit) then
               nfound = nfound + 1
            else
               other_face = q
            end if
         end do
         if (nfound /= nf) other_face = 0
      end function other_face

      subroutine pair(ic1, j1, ic2, j2, samemat)
         integer(ik), intent(in) :: ic1, ic2, j2
         integer, intent(in) :: j1
         logical, intent(in) :: samemat
         code = nen*(ic2 - 1) + j2
         if (.not. samemat) code = code + mbndry
         jtet(jtetoff(ic1) + j1) = code
         code = nen*(ic1 - 1) + j1
         if (.not. samemat) code = code + mbndry
         jtet(jtetoff(ic2) + j2) = code
         if (samemat) then
            st%ninterior = st%ninterior + 1
         else
            st%ninterface = st%ninterface + 1
         end if
      end subroutine pair

      subroutine set_boundary(ic1, j1)
         integer(ik), intent(in) :: ic1
         integer, intent(in) :: j1
         jtet(jtetoff(ic1) + j1) = mbndry
         st%nboundary = st%nboundary + 1
      end subroutine set_boundary

      subroutine nonconforming(ic1, j1)
         integer(ik), intent(in) :: ic1
         integer, intent(in) :: j1
         jtet(jtetoff(ic1) + j1) = mbndry
         st%nnonconf = st%nnonconf + 1
         if (st%nonconf_cell == 0) then
            st%nonconf_cell = ic1
            st%nonconf_face = j1
         end if
      end subroutine nonconforming

      ! Face j1 of triangle ic1 runs a -> b; the same edge of ic2 must
      ! run b -> a.
      subroutine check_orientation(ic1, j1, ic2, j2)
         integer(ik), intent(in) :: ic1, ic2, j2
         integer, intent(in) :: j1
         integer(ik) :: a, b, c, d
         a = itet(itetoff(ic1) + iface_tri(1, j1))
         b = itet(itetoff(ic1) + iface_tri(2, j1))
         c = itet(itetoff(ic2) + iface_tri(1, j2))
         d = itet(itetoff(ic2) + iface_tri(2, j2))
         if (.not. (c == b .and. d == a)) then
            st%norient = st%norient + 1
            if (st%orient_cell(1) == 0) st%orient_cell = [ic1, ic2]
         end if
      end subroutine check_orientation

   end subroutine build_jtet

end module del_vor_jtet
