MODULE utilities

USE precision, ONLY: wp
USE matlab_io, ONLY: get_run_output_dir

IMPLICIT NONE

PUBLIC :: stopwatch
PUBLIC :: input
PUBLIC :: input_t
PRIVATE

    TYPE :: input_t
        INTEGER :: nx = 21
        INTEGER :: ny = 21
        REAL(wp) :: lx = 1.0_wp
        REAL(wp) :: ly = 1.0_wp
        INTEGER :: nstep = 200
        REAL(wp) :: diff = 1.0_wp
        REAL(wp) :: safety = 1.0_wp
        INTEGER :: output_every = 10
        REAL(wp) :: dt = -1.0_wp   ! optional; if < 0 then main computes dt from safety*dt_limit
    END TYPE input_t

CONTAINS

    SUBROUTINE input(filename, inp)
        !! Read initialization values from a Fortran NAMELIST file.
        !!
        !! The file must contain a namelist group:
        !!   &inp
        !!     inp%nx = 21,
        !!     inp%ny = 21,
        !!     ...
        !!   /

        CHARACTER(LEN=*), INTENT(IN) :: filename
        TYPE(input_t), INTENT(INOUT) :: inp

        INTEGER :: unit, ios

        NAMELIST /inputs/ inp

        OPEN(NEWUNIT=unit, FILE=TRIM(filename), STATUS='OLD', ACTION='READ', IOSTAT=ios)
        IF (ios /= 0) THEN
            WRITE(*,'(A,1X,A)') 'ERROR: could not open input file:', TRIM(filename)
            ERROR STOP
        END IF

        READ(unit, NML=inputs, IOSTAT=ios)
        CLOSE(unit)
        IF (ios /= 0) THEN
            WRITE(*,'(A,1X,A)') 'ERROR: could not read NAMELIST group "inputs" from:', TRIM(filename)
            ERROR STOP
        END IF

        IF (inp%nx < 3 .OR. inp%ny < 3) THEN
            WRITE(*,'(A,1X,I0,1X,I0)') 'ERROR: nx and ny must be >= 3. Got:', inp%nx, inp%ny
            ERROR STOP
        END IF
        IF (inp%nstep < 1) THEN
            WRITE(*,'(A,1X,I0)') 'ERROR: nstep must be >= 1. Got:', inp%nstep
            ERROR STOP
        END IF
        IF (inp%diff <= 0.0_wp) THEN
            WRITE(*,'(A,1X,ES12.4)') 'ERROR: diff must be > 0. Got:', inp%diff
            ERROR STOP
        END IF
        IF (inp%output_every < 1) inp%output_every = 1
    END SUBROUTINE input

    SUBROUTINE stopwatch(oper)
        !! This subroutine computes elapsed wallclock time
        !!
        !! Timing information is written in terminal window

        character(len=4), intent(in) :: oper
            !! Select which "button" to press on your Stopwatch:
            !!
            !! * 'STAR' or 'star' = reset and start the stopwatch
            !! * 'STOP' or 'stop' = print time spent since last 'star' operation (the stop watch is not reset)
        integer time_array_0(8), time_array_1(8)
        real(wp), save :: start_time, finish_time

        select case (oper)
        case ('STAR', 'star')
            call date_and_time(values=time_array_0)
            start_time = REAL(time_array_0(5), KIND=wp) * 3600.0_wp + REAL(time_array_0(6), KIND=wp) * 60.0_wp + &
                REAL(time_array_0(7), KIND=wp) + 0.001_wp * REAL(time_array_0(8), KIND=wp)
        case ('STOP', 'stop')
            call date_and_time(values=time_array_1)
            finish_time = REAL(time_array_1(5), KIND=wp) * 3600.0_wp + REAL(time_array_1(6), KIND=wp) * 60.0_wp + &
                REAL(time_array_1(7), KIND=wp) + 0.001_wp * REAL(time_array_1(8), KIND=wp)
            write (6, '(8x, 1a, 1f16.6)') 'elapsed wall clock time:', finish_time - start_time
        case default
            write (*, '("ERROR: in Processor/stopwatch")')
            stop
        end select
    END SUBROUTINE stopwatch

END MODULE utilities