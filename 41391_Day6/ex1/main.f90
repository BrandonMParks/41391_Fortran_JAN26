! Computes pi using the midpoint-rule summation approximation
!   pi = ∫_0^1 4/(1 + x^2) dx
!   pi ≈ (1/N) * Σ_{s=1..N} 4/(1 + ((s - 0.5)/N)^2)
PROGRAM main
  USE precision, ONLY: wp
  USE utilities, ONLY: stopwatch
  USE integrands, ONLY: ex1_frac
  USE omp_lib, ONLY: omp_set_num_threads, omp_get_max_threads, omp_get_num_threads, omp_get_thread_num

  IMPLICIT NONE

  INTEGER :: N
  INTEGER :: s
  INTEGER :: nthreads
  INTEGER :: ios
  REAL(wp) :: term
  REAL(wp) :: pi_est
  REAL(wp) :: pi_true
  CHARACTER(LEN=8) :: datestr
  CHARACTER(LEN=10) :: timestr
  CHARACTER(LEN=32) :: arg1

  ! Set number of summation terms
  N = 1000000000

  ! Error checks and status outputs
  IF (N < 1) THEN
    WRITE(*,'(A,1X,I0)') 'ERROR: N must be >= 1. You provided:', N
    ERROR STOP
  END IF
  CALL date_and_time(date=datestr, time=timestr)
  WRITE (*,'(1a,1a,"-",1a,"-",1a,1x,1a,":",1a,":",1a)') &
     'Start time: ', datestr(1:4), datestr(5:6), datestr(7:8), &
     timestr(1:2), timestr(3:4), timestr(5:10)
  CALL stopwatch('star')
  WRITE(*,'(A,I0)') 'Using N = ', N

  ! OpenMP threads control
  ! - Default (nthreads=0): use OMP_NUM_THREADS / runtime defaults
  ! - Optionally: pass number of threads as first CLI arg, e.g. ./main.exe 8
  nthreads = 0
  arg1 = ''
  CALL get_command_argument(1, arg1)
  IF (LEN_TRIM(arg1) > 0) THEN
    READ(arg1, *, IOSTAT=ios) nthreads
    IF (ios /= 0 .OR. nthreads < 1) THEN
      WRITE(*,'(A,1X,A)') 'WARNING: invalid thread count argument:', TRIM(arg1)
      nthreads = 0
    END IF
  END IF
  IF (nthreads > 0) CALL omp_set_num_threads(nthreads)
  WRITE(*,'(A,I0)') 'OpenMP max threads = ', omp_get_max_threads()

  ! Main summation loop (OpenMP parallel)
  pi_est = 0.0_wp
  !$omp parallel default(none) shared(N) private(s, term) reduction(+:pi_est)
    IF (omp_get_thread_num() == 0) THEN
      WRITE(*,'(A,I0)') 'OpenMP threads used = ', omp_get_num_threads()
    END IF
    !$omp do schedule(static)
      DO s = 1, N
        CALL ex1_frac(s, N, term)
        pi_est = pi_est + term
      END DO
    !$omp end do
  !$omp end parallel

  ! Compare true vs. estimated pi value
  pi_true = ACOS(-1.0_wp)
  WRITE(*,'(A,1X,ES24.16)') 'pi_est  =', pi_est
  WRITE(*,'(A,1X,ES24.16)') 'pi_true =', pi_true
  WRITE(*,'(A,1X,ES24.16)') 'abs err =', ABS(pi_est - pi_true)

  CALL date_and_time(date=datestr, time=timestr)
  WRITE (*,'(1a,1a,"-",1a,"-",1a,1x,1a,":",1a,":",1a)') &
     'End time:   ', datestr(1:4), datestr(5:6), datestr(7:8), &
     timestr(1:2), timestr(3:4), timestr(5:10)
  CALL stopwatch('stop')

  print*, '**************************'
  print*, 'Selfnote: Make sure that you are using the "hpcintro" job queue!'
  print*, '**************************'
END PROGRAM main
