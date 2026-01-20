module pde_init
  ! PDE boundary and initial state initialization
  use precision, only: dp ! use dp as the data-type (and for defining constants!)

  implicit none

  private
  public :: init

contains

! ------------------------------------------------------------------
! PDE boundary and initial state initialization subroutine
! ------------------------------------------------------------------
  subroutine init(u, f, t0)
    ! This function performs initializations common to both the
    ! Jacobi and Gauss-Seidel solvers.
  
    real(dp), intent(inout) :: u(:,:,:)
    real(dp), intent(inout) :: f(:,:,:)
    real(dp), intent(in) :: t0

    integer :: n, i, j, k
    real(dp) :: delta
    real(dp) :: x, y, z

    real(dp), parameter :: t_hot = 20._dp
    real(dp), parameter :: t_cold = 0._dp
    real(dp), parameter :: f_radiator = 200._dp

    ! Radiator region in physical coordinates
    real(dp), parameter :: x_min = -1._dp
    real(dp), parameter :: x_max = -3._dp / 8._dp
    real(dp), parameter :: y_min = -1._dp
    real(dp), parameter :: y_max = -1._dp / 2._dp
    real(dp), parameter :: z_min = -2._dp / 3._dp
    real(dp), parameter :: z_max = 0._dp

    n = size(u, 1)
    if (size(u, 2) /= n .or. size(u, 3) /= n) error stop 'pde_init/init: u must be cubic (N x N x N)'
    if (any(shape(f) /= shape(u))) error stop 'pde_init/init: f must have same shape as u'
    if (n < 2) error stop 'pde_init/init: N must be >= 2'

    ! Grid includes boundary points.
    ! If the interior size is N, then the stored array size is n = N+2 and
    ! we map index 1 -> (x,y,z) = -1 and index n -> (x,y,z) = +1.
    delta = 2._dp / real(n - 1, dp)
    
    !$omp parallel default(shared) private(i, j, k, x, y, z)
    !$omp do schedule(static)
    do k = 1, n
      z = -1._dp + real(k - 1, dp) * delta
      do j = 1, n
        y = -1._dp + real(j - 1, dp) * delta
        do i = 1, n
          x = -1._dp + real(i - 1, dp) * delta

          ! Start values (interior defaults)
          u(i, j, k) = t0
          f(i, j, k) = 0._dp

          ! Radiator source term f(x,y,z)
          if (x >= x_min .and. x <= x_max .and. &
              y >= y_min .and. y <= y_max .and. &
              z >= z_min .and. z <= z_max) then
            f(i, j, k) = f_radiator
          end if

          ! Dirichlet boundary conditions:
          ! five walls at 20C, one wall at y=-1 (j=1) at 0C.
          if (i == 1 .or. i == n .or. j == 1 .or. j == n .or. k == 1 .or. k == n) then
            u(i, j, k) = t_hot
          end if
          if (j == 1) then
            u(i, j, k) = t_cold
          end if
        end do
      end do
    end do
    !$omp end do
    !$omp end parallel

  end subroutine init

end module pde_init
