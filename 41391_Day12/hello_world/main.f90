! Parallel "hello world" with MPI
PROGRAM main
  USE, INTRINSIC :: iso_fortran_env, ONLY: output_unit
  USE mpi
  USE precision, ONLY: wp

  IMPLICIT NONE

  INTEGER :: ierr, rank, nranks, namelen, r
  CHARACTER(LEN=MPI_MAX_PROCESSOR_NAME) :: processor_name
  CHARACTER(LEN=8) :: datestr
  CHARACTER(LEN=10) :: timestr

  ! Start timer
  CALL date_and_time(date=datestr, time=timestr)
  WRITE (*,'(1a,1a,"-",1a,"-",1a,1x,1a,":",1a,":",1a)') &
     'Start time: ', datestr(1:4), datestr(5:6), datestr(7:8), &
     timestr(1:2), timestr(3:4), timestr(5:10)
  CALL stopwatch('star')

  CALL MPI_Init(ierr)
  CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
  CALL MPI_Comm_size(MPI_COMM_WORLD, nranks, ierr)
  CALL MPI_Get_processor_name(processor_name, namelen, ierr)

  IF (rank == 0) THEN
     WRITE (*,'(A)') 'hello world!'
  END IF

  ! Ordered output by rank
  DO r = 0, nranks - 1
     CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
     IF (rank == r) THEN
        WRITE (*,'(A,I0,A,I0,A,A)') 'Rank ', rank, ' of ', nranks, ' on ', TRIM(processor_name(1:namelen))
        CALL flush(output_unit)
     END IF
  END DO
  CALL MPI_Barrier(MPI_COMM_WORLD, ierr)

  CALL MPI_Finalize(ierr)

  ! Stop timer
  CALL date_and_time(date=datestr, time=timestr)
  WRITE (*,'(1a,1a,"-",1a,"-",1a,1x,1a,":",1a,":",1a)') &
     'End time:   ', datestr(1:4), datestr(5:6), datestr(7:8), &
     timestr(1:2), timestr(3:4), timestr(5:10)
  CALL stopwatch('stop')

CONTAINS

  SUBROUTINE stopwatch(oper)
    CHARACTER(LEN=*), INTENT(IN) :: oper
    INTEGER :: time_array(8)
    REAL(wp), SAVE :: start_time = 0.0_wp
    REAL(wp) :: now

    CALL date_and_time(values=time_array)
    now = REAL(time_array(5), KIND=wp) * 3600.0_wp + REAL(time_array(6), KIND=wp) * 60.0_wp + &
          REAL(time_array(7), KIND=wp) + 0.001_wp * REAL(time_array(8), KIND=wp)

    SELECT CASE (oper)
    CASE ('STAR', 'star')
       start_time = now
    CASE ('STOP', 'stop')
       WRITE (output_unit, '(8x, 1a, 1f16.6)') 'elapsed wall clock time:', now - start_time
    CASE DEFAULT
       WRITE (output_unit, '(A)') 'ERROR: stopwatch operation must be STAR or STOP'
       ERROR STOP
    END SELECT
  END SUBROUTINE stopwatch

END PROGRAM main
