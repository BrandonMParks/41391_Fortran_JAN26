MODULE swap_arrays
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: swap_auto, swap_ptr

CONTAINS

  ! Swap using an AUTOMATIC local array (stack).
  ! For very large arrays this can overflow the stack (which is the point of the exercise).
!-------------------------------------------------------------
  SUBROUTINE swap_auto(a, b)
    REAL, INTENT(INOUT) :: a(:)
    REAL, INTENT(INOUT) :: b(:)
    REAL :: tmp(SIZE(a))

    IF (SIZE(a) /= SIZE(b)) ERROR STOP "swap_auto: size mismatch"

    tmp = a
    a = b
    b = tmp
  END SUBROUTINE swap_auto
!-------------------------------------------------------------


  ! Swap using a POINTER temp array allocated on the heap.
!-------------------------------------------------------------
  SUBROUTINE swap_ptr(a, b)
    REAL, INTENT(INOUT) :: a(:)
    REAL, INTENT(INOUT) :: b(:)
    REAL, POINTER :: tmp(:)

    IF (SIZE(a) /= SIZE(b)) ERROR STOP "swap_ptr: size mismatch"

    ALLOCATE(tmp(SIZE(a)))
    tmp = a
    a = b
    b = tmp
    DEALLOCATE(tmp)
  END SUBROUTINE swap_ptr
!-------------------------------------------------------------

END MODULE swap_arrays
