PROGRAM main
  USE precision, ONLY: wp
  USE utilities, ONLY: stopwatch
  USE matvec_kernels, ONLY: matvec_omp, matvec_omp_full
  USE omp_lib, ONLY: omp_set_num_threads, omp_get_max_threads, omp_get_num_threads, omp_get_thread_num

  IMPLICIT NONE

  INTEGER :: n
  INTEGER :: nthreads
  INTEGER :: ios
  INTEGER :: i
  INTEGER :: j
  CHARACTER(LEN=32) :: arg1
  CHARACTER(LEN=32) :: arg2

  REAL(wp), ALLOCATABLE :: A(:,:)
  REAL(wp), ALLOCATABLE :: x(:)
  REAL(wp), ALLOCATABLE :: y(:)

  REAL(wp) :: checksum

  ! Matrix dimensions (square matrix)
  n = 2000
  ! Number of threads to parallelize with
  ! 0 = use OMP_NUM_THREADS env. var. default
  nthreads = 0

  ! Optional CLI args:
  !   1) n        - matrix dimension (square)
  !   2) nthreads - OpenMP threads
  arg1 = ''
  arg2 = ''
  ! Get command line arguments
  CALL get_command_argument(1, arg1)
  CALL get_command_argument(2, arg2)

  ! Process command line argument 1
  IF (LEN_TRIM(arg1) > 0) THEN
    READ(arg1, *, IOSTAT=ios) n
    IF (ios /= 0 .OR. n < 1) THEN
      WRITE(*,'(A,1X,A)') 'ERROR: invalid n argument:', TRIM(arg1)
      ERROR STOP
    END IF
  END IF
  ! Process command line argument 2
  IF (LEN_TRIM(arg2) > 0) THEN
    READ(arg2, *, IOSTAT=ios) nthreads
    IF (ios /= 0 .OR. nthreads < 1) THEN
      WRITE(*,'(A,1X,A)') 'WARNING: invalid nthreads argument:', TRIM(arg2)
      nthreads = 0
    END IF
  END IF

  ! Overwrite default OpenMP threads conditionally
  IF (nthreads > 0) CALL omp_set_num_threads(nthreads)
  WRITE(*,'(A,I0)') 'OpenMP max threads = ', omp_get_max_threads()

  ALLOCATE(A(n,n), x(n), y(n))

  CALL stopwatch('star')

  ! Print number of threads used
  !$omp parallel default(none)
    IF (omp_get_thread_num() == 0) THEN
      WRITE(*,'(A,I0)') 'OpenMP threads used = ', omp_get_num_threads()
    END IF
  !$omp end parallel

  ! Initialize A and x for parallel multiplication
  ! non-zero init for debugging purposes
  !$omp parallel do default(none) shared(x, n) private(i) schedule(static)
  DO i = 1, n
    x(i) = 1.0_wp / REAL(i, KIND=wp)
  END DO
  !$omp end parallel do

  !$omp parallel do default(none) shared(A, n) private(i, j) collapse(2) schedule(static)
  DO i = 1, n
    DO j = 1, n
      A(i,j) = 1.0_wp / REAL(i + j, KIND=wp)
    END DO
  END DO
  !$omp end parallel do

  ! Compute y = A*x
  CALL matvec_omp_full(A, x, y)

  checksum = SUM(y)
  CALL stopwatch('stop')

  WRITE(*,'(A,I0)') 'n = ', n
  WRITE(*,'(A,1X,ES24.16)') 'checksum(sum(y)) =', checksum
  WRITE(*,'(A,1X,ES24.16)') 'y(1)            =', y(1)
  WRITE(*,'(A,1X,ES24.16)') 'y(n)            =', y(n)

END PROGRAM main
