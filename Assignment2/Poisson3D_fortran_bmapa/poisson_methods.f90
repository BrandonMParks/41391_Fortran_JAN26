module poisson_methods

  ! Please do not put any computational variable in this module.
  ! Pass all variables as arguments to the subroutine.
  use precision, only: dp   ! use dp as the data-type (and for defining constants!)
  use pde_init, only: init  ! for initializing the BCs and initial solution state

  implicit none

  private
  public :: jacobi
  public :: gauss_seidel

contains

! ------------------------------------------------------------------
! Jacobi solver subroutine
! ------------------------------------------------------------------
  subroutine jacobi(u, f, t0, itermax, tolerance, iters_done, diffmax_final, t_total, t_update, nthreads_used)
    use omp_lib, only: omp_get_num_threads, omp_get_wtime

    real(dp), intent(inout) :: u(:,:,:)
    real(dp), intent(inout) :: f(:,:,:)
    real(dp), intent(in) :: t0
    integer, intent(in) :: itermax
    real(dp), intent(in) :: tolerance

    integer, intent(out), optional :: iters_done
    real(dp), intent(out), optional :: diffmax_final
    real(dp), intent(out), optional :: t_total
    real(dp), intent(out), optional :: t_update
    integer, intent(out), optional :: nthreads_used

    integer :: n, i, j, k, iter
    real(dp) :: delta, delta2
    real(dp) :: diffmax
    real(dp) :: u_new_val
    real(dp) :: diffmax_line
    real(dp) :: t_par_total, t0_par
    real(dp) :: t_total_start
    logical :: done

    integer :: threads_used_local

    real(dp), allocatable, target :: buf_a(:,:,:), buf_b(:,:,:)
    real(dp), pointer :: u_old(:,:,:), u_new(:,:,:), u_tmp(:,:,:)


    ! Common initializations - BCs and initial state
    call init(u, f, t0)

    n = size(u, 1)
    if (size(u, 2) /= n .or. size(u, 3) /= n) error stop 'poisson_methods/jacobi: u must be cubic'
    if (any(shape(f) /= shape(u))) error stop 'poisson_methods/jacobi: f must have same shape as u'
    if (n < 3) error stop 'poisson_methods/jacobi: need at least one interior point (n >= 3)'

    delta = 2._dp / real(n - 1, dp)
    delta2 = delta * delta

    allocate(buf_a(n, n, n))
    allocate(buf_b(n, n, n))

    u_old => buf_a
    u_new => buf_b

    ! NUMA first-touch for the Jacobi buffers: initialize/copy in parallel.
    print '(a)', 'Entering jacobi; first parallel region beginning.'
    call flush(6)
    !$omp parallel do collapse(2) default(shared) private(i, j, k) schedule(static,3)
    do k = 1, n
      do j = 1, n
        do i = 1, n
          u_old(i, j, k) = u(i, j, k)
          u_new(i, j, k) = u(i, j, k)
        end do
      end do
    end do
    !$omp end parallel do

    t_par_total = 0._dp
    t_total_start = omp_get_wtime()

    threads_used_local = -1

    done = .false.

    !$omp parallel default(shared) private(i, j, k, u_new_val, diffmax_line, iter)
    !$omp single
    threads_used_local = omp_get_num_threads()
    print '(a,i0)', 'OpenMP: jacobi threads used = ', threads_used_local
    !$omp end single

    do iter = 1, itermax

      !$omp single
      diffmax = 0._dp

      ! Time only the parallelized update portion.
      t0_par = omp_get_wtime()
      !$omp end single

      ! Update interior points only using the 7-point stencil
      !$omp do collapse(2) schedule(static) reduction(max:diffmax)
      do k = 2, n - 1
        do j = 2, n - 1
          diffmax_line = 0._dp
          !$omp simd reduction(max:diffmax_line)
          do i = 2, n - 1
            u_new_val = ( &
              u_old(i - 1, j, k) + u_old(i + 1, j, k) + &
              u_old(i, j - 1, k) + u_old(i, j + 1, k) + &
              u_old(i, j, k - 1) + u_old(i, j, k + 1) + &
              delta2 * f(i, j, k) ) / 6._dp

            diffmax_line = max(diffmax_line, abs(u_new_val - u_old(i, j, k)))
            u_new(i, j, k) = u_new_val
          end do
          diffmax = max(diffmax, diffmax_line)
        end do
      end do
      !$omp end do

      !$omp single
      t_par_total = t_par_total + (omp_get_wtime() - t0_par)

      if (diffmax < tolerance) then
        done = .true.
      else
        done = .false.
      end if

      ! Swap buffers (no data copy).
      u_tmp => u_old
      u_old => u_new
      u_new => u_tmp

      if (mod(iter, 100) == 0) then
        print '(a,i0,a,es12.5)', 'jacobi: iter=', iter, ' diffmax=', diffmax
      end if
      !$omp end single
      if (done) exit
    end do
    !$omp end parallel

    ! Copy final solution back to u (parallel).
    !$omp parallel do collapse(2) default(shared) private(i, j, k) schedule(static)
    do k = 1, n
      do j = 1, n
        do i = 1, n
          u(i, j, k) = u_old(i, j, k)
        end do
      end do
    end do
    !$omp end parallel do

    print '(a)', 'Exiting jacobi; last OpenMP region completed'
    call flush(6)

    write(*,'(a,es23.16)') 'jacobi: total parallel update wall time (s) = ', t_par_total

    if (present(iters_done)) iters_done = iter
    if (present(diffmax_final)) diffmax_final = diffmax
    if (present(t_total)) t_total = omp_get_wtime() - t_total_start
    if (present(t_update)) t_update = t_par_total
    if (present(nthreads_used)) nthreads_used = threads_used_local

    deallocate(buf_a)
    deallocate(buf_b)

  end subroutine jacobi

! ------------------------------------------------------------------
! Gauss-Seidel solver subroutine
! ------------------------------------------------------------------
subroutine gauss_seidel(u, f, t0, itermax, tolerance, iters_done, diffmax_final, t_total, t_update, nthreads_used)
  use precision, only: dp
  use pde_init,  only: init
  use omp_lib,   only: omp_get_wtime, omp_get_num_threads
  implicit none

  ! Arguments (kept exactly as your updated routine)
  real(dp), intent(inout) :: u(:,:,:)
  real(dp), intent(inout) :: f(:,:,:)
  real(dp), intent(in)    :: t0
  integer,  intent(in)    :: itermax
  real(dp), intent(in)    :: tolerance
  integer,  intent(out), optional :: iters_done
  real(dp), intent(out), optional :: diffmax_final
  real(dp), intent(out), optional :: t_total
  real(dp), intent(out), optional :: t_update
  integer,  intent(out), optional :: nthreads_used

  ! Locals
  integer :: n, i, j, k, iter
  real(dp) :: delta, delta2, inv6
  real(dp) :: u_new_val, u_old_val
  real(dp) :: diffmax
  logical  :: done
  real(dp) :: t_total_start, t0_par, t_par_total
  integer  :: threads_used_local

  ! Tiling and tile-indexing (unit-stride tile counters)
  integer :: jb, kb               ! tile sizes in j and k (physical)
  integer :: ntj, ntk             ! number of tiles in j and k
  integer :: tj, tk               ! tile indices (unit stride)
  integer :: j0, j1, k0, k1       ! physical bounds per tile
  integer :: istat, elen
  character(len=32) :: eval

  ! ------------------- Initialization -------------------
  call init(u, f, t0)

  n = size(u, 1)
  if (size(u, 2) /= n .or. size(u, 3) /= n) error stop 'gauss_seidel: u must be cubic (n x n x n)'
  if (any(shape(f) /= shape(u)))            error stop 'gauss_seidel: f must have same shape as u'
  if (n < 3)                                error stop 'gauss_seidel: need at least one interior point (n >= 3)'

  delta  = 2._dp / real(n - 1, dp)
  delta2 = delta * delta
  inv6   = 1._dp / 6._dp

  ! --- Choose tile sizes (heuristics), overridable by env ---
  jb = max(16, min(64, n/32*32))  ! prefer cache-friendly sizes; clamp
  kb = max(16, min(64, n/32*32))
  if (jb > n-2) jb = max(1, n-2)
  if (kb > n-2) kb = max(1, n-2)

  eval  = ''; elen = 0
  call get_environment_variable('GS_TILE_J', eval, length=elen, status=istat)
  if (istat == 0 .and. elen > 0) then
    read(eval(1:elen), *, iostat=istat) jb
    if (istat /= 0) jb = max(16, min(64, jb))
    jb = max(1, min(jb, n-2))
  end if
  eval  = ''; elen = 0
  call get_environment_variable('GS_TILE_K', eval, length=elen, status=istat)
  if (istat == 0 .and. elen > 0) then
    read(eval(1:elen), *, iostat=istat) kb
    if (istat /= 0) kb = max(16, min(64, kb))
    kb = max(1, min(kb, n-2))
  end if

  ! Number of tiles (unit-stride indices tj/tk go from 1..ntj/ntk)
  ntj = ( (n-2) + jb - 1 ) / jb
  ntk = ( (n-2) + kb - 1 ) / kb

  t_total_start      = omp_get_wtime()
  t_par_total        = 0._dp
  threads_used_local = -1

  print '(a)', 'Entering GS; first OpenMP region beginning.'
  call flush(6)

  ! -------------------------- Gauss-Seidel (tiled wavefront) -------------------------
  !
  ! Wavefront over unit-stride tile indices (tk, tj):
  !   - ordered(2) with collapse(2)
  !   - depend(sink: tk-1, tj) and depend(sink: tk, tj-1)
  ! Within each tile: iterate over physical (k0:k1, j0:j1) and inner i=2..n-1.
  !
  !$omp parallel default(shared) &
  !$omp private(i,j,k,tj,tk,j0,j1,k0,k1,iter,u_new_val,u_old_val) &
  !$omp shared(n,u,f,delta2,inv6,itermax,tolerance,jb,kb,ntj,ntk,threads_used_local)
    !$omp single
      threads_used_local = omp_get_num_threads()
      print '(a,i0)', 'OpenMP: GS threads used = ', threads_used_local
      print '(a,2(i0,a))', 'GS tiles: jb=', jb, ', kb=', kb
      call flush(6)
    !$omp end single

    done = .false.

    do iter = 1, itermax

      !$omp single
        t0_par  = omp_get_wtime()
      !$omp end single

      !$omp do ordered(2) collapse(2) schedule(static,1)
      do tk = 1, ntk
        do tj = 1, ntj
          if (tk > 1) then
            !$omp ordered depend(sink: tk-1, tj)
          end if
          if (tj > 1) then
            !$omp ordered depend(sink: tk,   tj-1)
          end if

          ! Compute physical bounds for this tile
          k0 = 2 + (tk-1)*kb
          k1 = min(n - 1, k0 + kb - 1)
          j0 = 2 + (tj-1)*jb
          j1 = min(n - 1, j0 + jb - 1)

          ! Process the tile
          do k = k0, k1
            do j = j0, j1
              do i = 2, n - 1
                u_old_val = u(i, j, k)
                u_new_val = ( &
                  u(i-1, j,   k) + u(i+1, j,   k) + &
                  u(i,   j-1, k) + u(i,   j+1, k) + &
                  u(i,   j,   k-1) + u(i,   j,   k+1) + &
                  delta2 * f(i, j, k) ) * inv6
                u(i, j, k) = u_new_val
              end do
            end do
          end do

          !$omp ordered depend(source)
        end do
      end do
      !$omp end do

      !$omp single
        t_par_total = t_par_total + (omp_get_wtime() - t0_par)
      !$omp end single
    end do
  !$omp end parallel

  print '(a)', 'Exiting GS; last OpenMP region completed'
  write(*,'(a,es23.16)') 'GS: total parallel update wall time (s) = ', t_par_total
  call flush(6)

  ! ------------------------------ Outputs --------------------------------
  if (present(iters_done))    iters_done    = iter
  if (present(diffmax_final)) diffmax_final = diffmax
  if (present(t_total))       t_total       = omp_get_wtime() - t_total_start
  if (present(t_update))      t_update      = t_par_total
  if (present(nthreads_used)) nthreads_used = threads_used_local
end subroutine gauss_seidel

  
end module poisson_methods
