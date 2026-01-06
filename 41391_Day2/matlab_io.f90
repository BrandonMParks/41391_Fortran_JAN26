MODULE matlab_io
  IMPLICIT NONE
CONTAINS

  SUBROUTINE write_matlab_field(filename, field, timestamp)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    REAL, INTENT(IN) :: field(:,:)
    REAL, INTENT(IN) :: timestamp

    INTEGER :: nx, ny
    INTEGER :: i, j
    INTEGER :: unit, ios
    CHARACTER(LEN=:), ALLOCATABLE :: base
    CHARACTER(LEN=512) :: out_filename
    CHARACTER(LEN=32) :: tstamp
    INTEGER :: dotpos
    INTEGER :: tint, tfrac

    nx = SIZE(field, 1)
    ny = SIZE(field, 2)

    base = TRIM(filename)
    dotpos = INDEX(base, '.', BACK=.TRUE.)
    IF (dotpos > 0) base = base(:dotpos-1)

    tint = INT(timestamp)
    tfrac = NINT((timestamp - REAL(tint)) * 10000.0)
    IF (tfrac >= 10000) THEN
      tint = tint + 1
      tfrac = tfrac - 10000
    END IF
    IF (tfrac < 0) tfrac = 0

    WRITE(tstamp,'(I2.2,"_",I4.4)') tint, tfrac
    WRITE(out_filename,'(A)') 'output/' // TRIM(base) // TRIM(tstamp) // '.m'

    OPEN(NEWUNIT=unit, FILE=TRIM(out_filename), STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)
    IF (ios /= 0) THEN
      PRINT*, 'Error: could not open MATLAB output file: ', TRIM(out_filename)
      RETURN
    END IF

    WRITE(unit,'(A)') '% Auto-generated MATLAB script'
    WRITE(unit,'(A)') 'field = [ ...'

    DO j = 1, ny
      WRITE(unit,'(A)', ADVANCE='NO') '  '
      DO i = 1, nx
        WRITE(unit,'(ES16.8)', ADVANCE='NO') field(i,j)
        IF (i < nx) WRITE(unit,'(A)', ADVANCE='NO') ' '
      END DO

      IF (j < ny) THEN
        WRITE(unit,'(A)') '; ...'
      ELSE
        WRITE(unit,'(A)') ' ];'
      END IF
    END DO

    WRITE(unit,'(A)') ''
    WRITE(unit,'(A)') '% Surface plot (auto-generated)'
    WRITE(unit,'(A)') '[ny, nx] = size(field);'
    WRITE(unit,'(A)') '[X, Y] = meshgrid(1:nx, 1:ny);'
    WRITE(unit,'(A)') 'fig = figure(''Color'',''w'');'
    WRITE(unit,'(A)') 'ax = axes(''Parent'', fig);'
    WRITE(unit,'(A)') 'h = surf(ax, X, Y, field);'
    WRITE(unit,'(A)') 'shading(ax, ''interp'');'
    WRITE(unit,'(A)') 'colormap(ax, parula);'
    WRITE(unit,'(A)') 'cb = colorbar(ax);'
    WRITE(unit,'(A)') 'cb.Label.String = ''field value'';'
    WRITE(unit,'(A)') 'grid(ax, ''on'');'
    WRITE(unit,'(A)') 'box(ax, ''on'');'
    WRITE(unit,'(A)') 'axis(ax, ''tight'');'
    WRITE(unit,'(A)') 'view(ax, 45, 30);'
    WRITE(unit,'(A)') 'xlabel(ax, ''i (x index)'');'
    WRITE(unit,'(A)') 'ylabel(ax, ''j (y index)'');'
    WRITE(unit,'(A)') 'zlabel(ax, ''field'');'
    WRITE(unit,'(A)') 'title(ax, sprintf(''field (%d x %d)'', ny, nx));'
    WRITE(unit,'(A)') 'set(ax, ''FontSize'', 12, ''LineWidth'', 1);'
    WRITE(unit,'(A)') 'set(h, ''EdgeColor'', ''k'');'
    WRITE(unit,'(A)') 'drawnow;'

    CLOSE(unit)
  END SUBROUTINE write_matlab_field

END MODULE matlab_io
