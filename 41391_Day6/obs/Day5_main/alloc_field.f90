MODULE alloc_field
  USE precision, ONLY: wp
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: setup_fields

CONTAINS

!-------------------------------------------------------------
  SUBROUTINE setup_fields(field1_store, field2_store, field1, field2, nx, ny)
    INTEGER, INTENT(IN) :: nx, ny
    REAL(wp), ALLOCATABLE, TARGET, INTENT(INOUT) :: field1_store(:,:), field2_store(:,:)
    REAL(wp), POINTER, INTENT(OUT) :: field1(:,:), field2(:,:)

    CALL ensure_allocated(field1_store, nx, ny, 'field1_store', field1)
    CALL ensure_allocated(field2_store, nx, ny, 'field2_store', field2)
  END SUBROUTINE setup_fields
!-------------------------------------------------------------

!-------------------------------------------------------------
  SUBROUTINE ensure_allocated(arr, nx, ny, name, p)
    REAL(wp), ALLOCATABLE, TARGET, INTENT(INOUT) :: arr(:,:)
    INTEGER, INTENT(IN) :: nx, ny
    CHARACTER(LEN=*), INTENT(IN) :: name
    REAL(wp), POINTER, INTENT(OUT), OPTIONAL :: p(:,:)

    IF (.NOT. ALLOCATED(arr)) THEN
      ALLOCATE(arr(nx, ny))
    ELSE
      IF (SIZE(arr, 1) /= nx .OR. SIZE(arr, 2) /= ny) THEN
        ERROR STOP 'alloc_field/setup_fields: already-allocated array has wrong shape: '//name
      END IF
    END IF
    IF (PRESENT(p)) p => arr
  END SUBROUTINE ensure_allocated
!-------------------------------------------------------------

END MODULE alloc_field
