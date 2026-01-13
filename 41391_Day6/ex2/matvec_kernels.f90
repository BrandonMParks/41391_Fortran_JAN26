MODULE matvec_kernels
  USE precision, ONLY: wp
  IMPLICIT NONE

  PUBLIC :: matvec_omp
  PUBLIC :: matvec_omp_full
  PRIVATE

CONTAINS

  SUBROUTINE matvec_omp(A, x, y)
    !! y = A*x
    !!
    !! Typical fast version:
    !! - Parallelize over rows (i)
    !! - Vectorize the inner loop (j) with omp simd
    REAL(wp), INTENT(IN)  :: A(:,:)
    REAL(wp), INTENT(IN)  :: x(:)
    REAL(wp), INTENT(OUT) :: y(:)

    INTEGER :: i, j
    INTEGER :: nrows, ncols
    REAL(wp) :: sum

    IF (SIZE(A,2) /= SIZE(x)) ERROR STOP 'matvec_omp: A columns must match x size'
    IF (SIZE(A,1) /= SIZE(y)) ERROR STOP 'matvec_omp: A rows must match y size'

    nrows = SIZE(A,1)
    ncols = SIZE(A,2)

    !$omp parallel do default(none) shared(A, x, y, nrows, ncols) private(i, j, sum) schedule(static)
    DO i = 1, nrows
      sum = 0.0_wp
      !$omp simd reduction(+:sum)
      DO j = 1, ncols
        sum = sum + A(i,j) * x(j)
      END DO
      y(i) = sum
    END DO
    !$omp end parallel do

  END SUBROUTINE matvec_omp

  SUBROUTINE matvec_omp_full(A, x, y)
    !! y = A*x
    !!
    !! "Fully" parallelized over the 2D iteration space (i,j) using collapse(2).
    !! This is mainly a teaching example; it often performs worse due to the final combine.
    REAL(wp), INTENT(IN)  :: A(:,:)
    REAL(wp), INTENT(IN)  :: x(:)
    REAL(wp), INTENT(OUT) :: y(:)

    INTEGER :: i, j
    INTEGER :: nrows, ncols
    REAL(wp), ALLOCATABLE :: y_priv(:)

    IF (SIZE(A,2) /= SIZE(x)) ERROR STOP 'matvec_omp_full: A columns must match x size'
    IF (SIZE(A,1) /= SIZE(y)) ERROR STOP 'matvec_omp_full: A rows must match y size'

    nrows = SIZE(A,1)
    ncols = SIZE(A,2)
    y = 0.0_wp

    !$omp parallel default(none) shared(A, x, y, nrows, ncols) private(i, j, y_priv)
      ALLOCATE(y_priv(nrows))
      y_priv = 0.0_wp

      !$omp do collapse(2) schedule(static)
      DO i = 1, nrows
        DO j = 1, ncols
          y_priv(i) = y_priv(i) + A(i,j) * x(j)
        END DO
      END DO
      !$omp end do

      !$omp critical
        y(:) = y(:) + y_priv(:)
      !$omp end critical

      DEALLOCATE(y_priv)
    !$omp end parallel

  END SUBROUTINE matvec_omp_full

END MODULE matvec_kernels
