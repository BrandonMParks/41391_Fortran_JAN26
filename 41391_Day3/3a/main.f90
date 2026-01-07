PROGRAM main_3b
  USE swap_arrays, ONLY: swap_auto, swap_ptr
  IMPLICIT NONE

  INTEGER :: n
  REAL, ALLOCATABLE :: a(:), b(:)
  CHARACTER(LEN=32) :: mode

  ! Choose an array size that exceeds typical stack limits.
  ! Example: n=10_000_000 -> 10 million REALs ~ 40 MB per array.
  ! The main arrays are ALLOCATABLE (heap) so they can be large.
  n = 1000000

  mode = "ptr"  ! default: safe swap using pointer+allocate
  CALL get_mode(mode)

  ALLOCATE(a(n), b(n))

  CALL init_arrays(a, b)

  SELECT CASE (ADJUSTL(mode))
  CASE ("auto")
     ! This may fail (stack overflow) for large n, depending on OS/ulimit.
     CALL swap_auto(a, b)
  CASE ("ptr")
     CALL swap_ptr(a, b)
  END SELECT

  CALL quick_check(a, b)

  DEALLOCATE(a, b)

CONTAINS

!-------------------------------------------------------------
  SUBROUTINE get_mode(mode_arg)
    CHARACTER(LEN=*), INTENT(INOUT) :: mode_arg
    CHARACTER(LEN=256) :: arg
    INTEGER :: stat

    CALL get_command_argument(1, arg, STATUS=stat)
    IF (stat == 0) THEN
       IF (LEN_TRIM(arg) > 0) mode_arg = TRIM(arg)
    END IF
  END SUBROUTINE get_mode
!-------------------------------------------------------------


!-------------------------------------------------------------
  SUBROUTINE init_arrays(a_out, b_out)
    REAL, INTENT(OUT) :: a_out(:)
    REAL, INTENT(OUT) :: b_out(:)
    INTEGER :: i

    DO i = 1, SIZE(a_out)
       a_out(i) = REAL(i)
       b_out(i) = -REAL(i)
    END DO
  END SUBROUTINE init_arrays
!-------------------------------------------------------------


!-------------------------------------------------------------
  SUBROUTINE quick_check(a_in, b_in)
    REAL, INTENT(IN) :: a_in(:)
    REAL, INTENT(IN) :: b_in(:)

    PRINT *, "mode swap done; sample values:"
    PRINT *, "a(1), a(end) = ", a_in(1), a_in(SIZE(a_in))
    PRINT *, "b(1), b(end) = ", b_in(1), b_in(SIZE(b_in))

    ! After swap, a should be negative and b positive.
    IF (a_in(1) >= 0.0 .OR. b_in(1) <= 0.0) THEN
       ERROR STOP "quick_check: !! array swap FAIL !!"
    ELSE
       PRINT*, "main/quick_check: array swap performed successfully!"
    END IF
  END SUBROUTINE quick_check
!-------------------------------------------------------------

END PROGRAM main_3b
