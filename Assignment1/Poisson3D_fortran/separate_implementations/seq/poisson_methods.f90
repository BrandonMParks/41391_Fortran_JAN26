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
  subroutine jacobi(u, f, t0, itermax, tolerance)
  !
    use iso_fortran_env, only: int64

    real(dp), intent(inout) :: u(:,:,:)
    real(dp), intent(inout) :: f(:,:,:)
    real(dp), intent(in) :: t0
    integer, intent(in) :: itermax
    real(dp), intent(in) :: tolerance

    integer :: n, i, j, k, iter
    real(dp) :: delta, delta2
    real(dp) :: diffmax, diff
    real(dp) :: u_new_val

    integer(int64) :: clk_rate, clk_start, clk_end
    real(dp) :: t_update_total

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

    buf_a = u
    buf_b = u

    u_old => buf_a
    u_new => buf_b

    call system_clock(count_rate=clk_rate)
    t_update_total = 0._dp

    print '(a)', 'Entering jacobi (sequential poisson_methods; no OpenMP regions)'
    call flush(6)

    do iter = 1, itermax
      diffmax = 0._dp

      ! Keep boundary values fixed (Dirichlet BCs)
      u_new(1, :, :) = u_old(1, :, :)
      u_new(n, :, :) = u_old(n, :, :)
      u_new(:, 1, :) = u_old(:, 1, :)
      u_new(:, n, :) = u_old(:, n, :)
      u_new(:, :, 1) = u_old(:, :, 1)
      u_new(:, :, n) = u_old(:, :, n)

      ! Update interior points
      call system_clock(count=clk_start)
      do k = 2, n - 1
        do j = 2, n - 1
          do i = 2, n - 1
            u_new_val = ( &
              u_old(i - 1, j, k) + u_old(i + 1, j, k) + &
              u_old(i, j - 1, k) + u_old(i, j + 1, k) + &
              u_old(i, j, k - 1) + u_old(i, j, k + 1) + &
              delta2 * f(i, j, k) ) / 6._dp
            diff = abs(u_new_val - u_old(i, j, k))
            if (diff > diffmax) diffmax = diff
            u_new(i, j, k) = u_new_val
          end do
        end do
      end do
      call system_clock(count=clk_end)
      if (clk_rate > 0_int64) then
        t_update_total = t_update_total + real(clk_end - clk_start, dp) / real(clk_rate, dp)
      end if

      if (diffmax < tolerance) exit

      ! Swap buffers (no data copy)
      u_tmp => u_old
      u_old => u_new
      u_new => u_tmp
      if (mod(iter, 100) == 0) then
        print '(a,i0,a,es12.5)', 'jacobi: iter=', iter, ' diffmax=', diffmax
      end if
    end do

    print '(a)', 'Exiting jacobi (sequential poisson_methods)'
    call flush(6)

    write(*,'(a,es23.16)') 'jacobi: total update wall time (s) = ', t_update_total

    ! Copy final solution back to u
    u = u_old

    deallocate(buf_a)
    deallocate(buf_b)

  end subroutine jacobi

! ------------------------------------------------------------------
! Gauss-Seidel solver subroutine
! ------------------------------------------------------------------
  subroutine gauss_seidel(u, f, t0, itermax, tolerance)
  !
    use iso_fortran_env, only: int64

    real(dp), intent(inout) :: u(:,:,:)
    real(dp), intent(inout) :: f(:,:,:)
    real(dp), intent(in) :: t0
    integer, intent(in) :: itermax
    real(dp), intent(in) :: tolerance

    integer :: n, i, j, k, iter
    real(dp) :: delta, delta2
    real(dp) :: diffmax, diff
    real(dp) :: u_old_val, u_new_val

    integer(int64) :: clk_rate, clk_start, clk_end
    real(dp) :: t_wall


    ! Common initializations - BCs and initial state
    call init(u, f, t0)

    n = size(u, 1)
    if (size(u, 2) /= n .or. size(u, 3) /= n) error stop 'poisson_methods/gauss_seidel: u must be cubic'
    if (any(shape(f) /= shape(u))) error stop 'poisson_methods/gauss_seidel: f must have same shape as u'
    if (n < 3) error stop 'poisson_methods/gauss_seidel: need at least one interior point (n >= 3)'

    delta = 2._dp / real(n - 1, dp)
    delta2 = delta * delta

    call system_clock(count_rate=clk_rate)
    call system_clock(count=clk_start)

    do iter = 1, itermax
      diffmax = 0._dp

      ! Update interior points only
      ! Gauss-Seidel is in-place: uses newest available neighbors
      do k = 2, n - 1
        do j = 2, n - 1
          do i = 2, n - 1
            u_old_val = u(i, j, k)

            u_new_val = ( &
              u(i - 1, j, k) + u(i + 1, j, k) + &
              u(i, j - 1, k) + u(i, j + 1, k) + &
              u(i, j, k - 1) + u(i, j, k + 1) + &
              delta2 * f(i, j, k) ) / 6._dp

            u(i, j, k) = u_new_val

            diff = abs(u_new_val - u_old_val)
            if (diff > diffmax) diffmax = diff
          end do
        end do
      end do

      ! if (diffmax < tolerance) exit

      if (mod(iter, 100) == 0) then
        print '(a,i0,a,es12.5)', 'gauss_seidel: iter=', iter, ' diffmax=', diffmax
      end if
    end do

    call system_clock(count=clk_end)
    if (clk_rate > 0_int64) then
      t_wall = real(clk_end - clk_start, dp) / real(clk_rate, dp)
    else
      t_wall = -1._dp
    end if
    write(*,'(a,es23.16)') 'gauss_seidel: wall time (s) = ', t_wall


  end subroutine gauss_seidel
  
end module poisson_methods
