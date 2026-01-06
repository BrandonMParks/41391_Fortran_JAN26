MODULE time_stepping
  IMPLICIT NONE
CONTAINS

  SUBROUTINE advance_solution(field_old, field_new)
    REAL, POINTER, INTENT(INOUT) :: field_old(:,:), field_new(:,:)
    REAL, POINTER :: tmp(:,:)

    tmp => field_old
    field_old => field_new
    field_new => tmp
  END SUBROUTINE advance_solution

END MODULE time_stepping
