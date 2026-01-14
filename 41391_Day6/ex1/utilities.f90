MODULE utilities

USE precision, ONLY: wp

IMPLICIT NONE

PUBLIC :: stopwatch
PRIVATE

CONTAINS

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
            write (6, '(8x, 1a, 1f16.10)') 'elapsed wall clock time:', finish_time - start_time
        case default
            write (*, '("ERROR: in Processor/stopwatch")')
            stop
        end select
    END SUBROUTINE stopwatch

END MODULE utilities