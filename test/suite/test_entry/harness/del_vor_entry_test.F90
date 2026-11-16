!-----------------------------------------------------------------------
! del_vor_entry_test: test_entry's harness. It reads an AVS mesh with
! del_vor's reader, then calls the LaGriT entry point through
! call_entry.f (fixed form, LaGriT's flags, no interface), once per
! OPTIONS argument, in one process:
!
!   del_vor_entry_test MESH.inp VOLFILE [-sink] [-tamper WHAT]
!                      OPTIONS [OPTIONS ...]
!
! -sink         every log line goes through log_set_sink to a routine
!               that prints it as 'lagrit| line' (LaGriT's writloga
!               will take its place)
! -tamper WHAT  spoil the mesh first: type (cell 1 a hex), offset
!               (itetoff(1) = 1), vertex (itet(1) = nnodes + 1), nen
!               (nen 5)
!
! Prints 'harness: call K: ierr = N' after each call; writes the
! volumes of the last call that returned 0 to VOLFILE (one per line,
! 17 digits; not written if no call did). Exit 0 if the last call
! returned 0, 3 otherwise (del_vor itself never stops).
!-----------------------------------------------------------------------
module entry_test_sink
   implicit none
contains
   ! The harness's log hook: a module procedure (an internal one would
   ! need an executable stack for its trampoline).
   subroutine to_lagrit(msg)
      character(len=*), intent(in) :: msg
      write (*, '(a)') 'lagrit| '//msg
   end subroutine to_lagrit
end module entry_test_sink

program del_vor_entry_test
   use entry_test_sink, only: to_lagrit
   use del_vor_kinds, only: ik, rk
   use del_vor_log, only: log_set_sink
   use del_vor_mesh, only: mesh_t
   use del_vor_read_avs, only: read_avs
   implicit none
   external :: call_entry
   type(mesh_t) :: m
   real(rk), allocatable :: vol(:)
   character(len=4096) :: arg, mesh, volfile, tamper
   integer :: na, ia, ierr4, lu, ncall
   integer(ik) :: ierr, nen

   na = command_argument_count()
   if (na < 3) then
      write (*, '(a)') 'usage: del_vor_entry_test MESH VOLFILE '// &
         '[-sink] [-tamper WHAT] OPTIONS ...'
      stop 2
   end if
   call get_command_argument(1, mesh)
   call get_command_argument(2, volfile)
   call read_avs(trim(mesh), m, ierr4)
   if (ierr4 /= 0) then
      write (*, '(a)') 'harness: cannot read '//trim(mesh)
      stop 2
   end if
   nen = m%nen
   tamper = ' '
   ia = 3
   do while (ia <= na)
      call get_command_argument(ia, arg)
      if (trim(arg) == '-sink') then
         call log_set_sink(to_lagrit)
      else if (trim(arg) == '-tamper') then
         ia = ia + 1
         call get_command_argument(ia, tamper)
      else
         exit
      end if
      ia = ia + 1
   end do
   select case (trim(tamper))
   case ('type')
      m%itettyp(1) = 8
   case ('offset')
      m%itetoff(1) = 1
   case ('vertex')
      m%itet(1) = m%nnodes + 1
   case ('nen')
      nen = 5
   end select

   allocate (vol(m%nnodes))
   ierr = 0
   ncall = 0
   do while (ia <= na)
      call get_command_argument(ia, arg)
      ncall = ncall + 1
      vol = -1.0_rk
      if (nen == 5) then
         ! a wrong nen: the arrays stay as they are, only nen lies
         call call_entry(m%nnodes, m%nelements, nen, m%xic, m%yic, &
                         m%zic, m%itettyp, m%itetoff, m%itet, &
                         m%itetclr, m%imt1, m%itp1, m%isn1, trim(arg), &
                         vol, ierr)
      else
         call call_entry(m%nnodes, m%nelements, m%nen, m%xic, m%yic, &
                         m%zic, m%itettyp, m%itetoff, m%itet, &
                         m%itetclr, m%imt1, m%itp1, m%isn1, trim(arg), &
                         vol, ierr)
      end if
      write (*, '(a,i0,a,i0)') 'harness: call ', ncall, ': ierr = ', &
         ierr
      if (ierr == 0 .and. all(vol /= -1.0_rk)) then
         open (newunit=lu, file=trim(volfile), status='replace')
         write (lu, '(1pe24.16)') vol
         close (lu)
      end if
      ia = ia + 1
   end do
   if (ierr /= 0) stop 3

end program del_vor_entry_test
