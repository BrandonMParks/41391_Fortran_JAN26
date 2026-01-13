MODULE matlab_io
  USE precision, ONLY: wp
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: get_run_output_dir
  PUBLIC :: write_matlab_field

CONTAINS

  SUBROUTINE ensure_output_dir_exists()
    LOGICAL :: exists

    INQUIRE(FILE='output', EXIST=exists)
    IF (.NOT. exists) THEN
      ! This is intentionally simple and Windows/Unix-friendly.
      CALL execute_command_line('mkdir output', WAIT=.TRUE.)
    END IF
  END SUBROUTINE ensure_output_dir_exists

  SUBROUTINE get_run_output_dir(dir)
    CHARACTER(LEN=:), ALLOCATABLE, INTENT(OUT) :: dir
    CALL ensure_output_dir_exists()
    dir = 'output'
  END SUBROUTINE get_run_output_dir

  SUBROUTINE write_matlab_field(filename, field, timestamp)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    REAL(wp), INTENT(IN) :: field(:,:)
    REAL(wp), INTENT(IN) :: timestamp

    ! Minimal stub kept for backwards compatibility with earlier exercises.
    ! This Day6 pi driver does not use MATLAB output.
    WRITE(*,'(A)') 'write_matlab_field: not used in this program.'
    WRITE(*,'(A,1X,A)') 'Requested file:', TRIM(filename)
    WRITE(*,'(A,1X,ES12.4)') 'Timestamp:', timestamp
    WRITE(*,'(A,1X,I0,A,I0)') 'Field shape:', SIZE(field,1), ' x ', SIZE(field,2)
  END SUBROUTINE write_matlab_field

END MODULE matlab_io
