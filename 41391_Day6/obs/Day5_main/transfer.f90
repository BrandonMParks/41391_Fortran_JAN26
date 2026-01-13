MODULE transfer
  USE precision, ONLY: wp
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: advance_solution

CONTAINS

!-------------------------------------------------------------
  SUBROUTINE advance_solution(field_old, field_new)
    REAL(wp), POINTER, INTENT(INOUT) :: field_old(:,:), field_new(:,:)
    REAL(wp), POINTER :: tmp(:,:)

    tmp => field_old
    field_old => field_new
    field_new => tmp
  END SUBROUTINE advance_solution
!-------------------------------------------------------------

!!!! ELEMENTAL SUBROUTINE code - simply uncomment the below and comment the above SUBROUTINE to swap !!!!
  ! PURE ELEMENTAL SUBROUTINE advance_solution(field_old, field_new)
  !   REAL(wp), INTENT(INOUT) :: field_old
  !   REAL(wp), INTENT(INOUT) :: field_new
  !   REAL(wp) :: tmp

  !   tmp = field_old
  !   field_old = field_new
  !   field_new = tmp
  ! END SUBROUTINE advance_solution
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

END MODULE transfer
