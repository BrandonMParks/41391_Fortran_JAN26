MODULE matlab_io
  USE iso_c_binding
  USE precision, ONLY: wp
  IMPLICIT NONE
  PRIVATE

  PUBLIC :: write_matlab_field
  INTERFACE
    INTEGER(C_INT) FUNCTION WinCreateDir(lpPathName, lpSecurityAttributes) BIND(C, NAME='CreateDirectoryA')
      USE iso_c_binding
      CHARACTER(KIND=C_CHAR), DIMENSION(*), INTENT(IN) :: lpPathName
      TYPE(C_PTR), VALUE :: lpSecurityAttributes
    END FUNCTION WinCreateDir
  END INTERFACE
  CHARACTER(LEN=:), ALLOCATABLE, SAVE :: run_output_dir
  LOGICAL, SAVE :: run_dir_initialized = .FALSE.
  INTEGER, SAVE :: field_write_count = 0
CONTAINS

!-------------------------------------------------------------
  SUBROUTINE write_matlab_field(filename, field, timestamp)
    CHARACTER(LEN=*), INTENT(IN) :: filename
    REAL(wp), INTENT(IN) :: field(:,:)
    REAL(wp), INTENT(IN) :: timestamp

    INTEGER :: nx, ny
    INTEGER :: i, j
    INTEGER :: unit, ios
    CHARACTER(LEN=:), ALLOCATABLE :: base
    CHARACTER(LEN=32) :: tstamp
    INTEGER :: dotpos
    INTEGER :: tint, tfrac
    CHARACTER(LEN=8) :: datestr
    CHARACTER(LEN=10) :: timestr
    CHARACTER(LEN=15) :: folder_stamp
    LOGICAL :: exists
    ! INTEGER :: exitstat
    CHARACTER(LEN=256) :: cmd_path
    CHARACTER(KIND=C_CHAR, LEN=256) :: cpath
    INTEGER(C_INT) :: wres
    INTEGER :: k, n
    LOGICAL :: fields_exists, plot_exists
    CHARACTER(LEN=512) :: fields_filename
    CHARACTER(LEN=512) :: plot_filename
    INTEGER :: step_idx

    nx = SIZE(field, 1)
    ny = SIZE(field, 2)

    base = TRIM(filename)
    dotpos = INDEX(base, '.', BACK=.TRUE.)
    IF (dotpos > 0) base = base(:dotpos-1)

    tint = INT(timestamp)
    tfrac = NINT((timestamp - REAL(tint, KIND=wp)) * 10000.0_wp)
    IF (tfrac >= 10000) THEN
      tint = tint + 1
      tfrac = tfrac - 10000
    END IF
    IF (tfrac < 0) tfrac = 0

    WRITE(tstamp,'(I2.2,"_",I4.4)') tint, tfrac

    ! Initialize per-run output subfolder once, named DD_MM_YY_HHMMSS
    IF (.NOT. run_dir_initialized) THEN
      ! Ensure base ./output exists
      cmd_path = 'output'
      INQUIRE(FILE=TRIM(cmd_path), EXIST=exists)
      IF (.NOT. exists) THEN
        n = LEN_TRIM(cmd_path)
        cpath = C_NULL_CHAR
        DO k = 1, n
          cpath(k:k) = TRANSFER(cmd_path(k:k), cpath(k:k))
        END DO
        cpath(n+1:n+1) = C_NULL_CHAR
        wres = WinCreateDir(cpath, C_NULL_PTR)
      END IF

      CALL DATE_AND_TIME(date=datestr, time=timestr)
      folder_stamp = datestr(7:8)//'_'//datestr(5:6)//'_'//datestr(3:4)//'_'// &
                     timestr(1:2)//timestr(3:4)//timestr(5:6)
      run_output_dir = 'output/' // folder_stamp

      cmd_path = 'output\' // folder_stamp
      INQUIRE(FILE=TRIM(cmd_path), EXIST=exists)
      IF (.NOT. exists) THEN
        ! Try Windows API CreateDirectoryA for robust creation
        n = LEN_TRIM(cmd_path)
        cpath = C_NULL_CHAR
        DO k = 1, n
          cpath(k:k) = TRANSFER(cmd_path(k:k), cpath(k:k))
        END DO
        cpath(n+1:n+1) = C_NULL_CHAR
        wres = WinCreateDir(cpath, C_NULL_PTR)
        INQUIRE(FILE=TRIM(cmd_path), EXIST=exists)
        IF (.NOT. exists) THEN
          PRINT*, 'Warning: could not create output subfolder: ', TRIM(run_output_dir)
        END IF
      END IF

      run_dir_initialized = .TRUE.
      field_write_count = 0
    END IF

    fields_filename = TRIM(run_output_dir) // '/fields_data.m'
    plot_filename   = TRIM(run_output_dir) // '/plot_fields.m'

    ! Create plot script once (do not recreate every timestep)
    INQUIRE(FILE=TRIM(plot_filename), EXIST=plot_exists)
    IF (.NOT. plot_exists) THEN
      OPEN(NEWUNIT=unit, FILE=TRIM(plot_filename), STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)
      IF (ios == 0) THEN
        WRITE(unit,'(A)') '% Auto-generated MATLAB plotting script'
        WRITE(unit,'(A)') '% Loads fields from fields_data.m in the same folder and animates a patch-style plot'
        WRITE(unit,'(A)') "thisDir = fileparts(mfilename('fullpath'));"
        WRITE(unit,'(A)') "run(fullfile(thisDir,'fields_data.m'));"
        WRITE(unit,'(A)') "nSteps = numel(fields);"
        WRITE(unit,'(A)') "if nSteps == 0"
        WRITE(unit,'(A)') "  error('No fields found in fields_data.m');"
        WRITE(unit,'(A)') "end"
        WRITE(unit,'(A)') "vmin = inf; vmax = -inf;"
        WRITE(unit,'(A)') "for kk = 1:nSteps"
        WRITE(unit,'(A)') "  f = fields{kk};"
        WRITE(unit,'(A)') "  vmin = min(vmin, min(f(:)));"
        WRITE(unit,'(A)') "  vmax = max(vmax, max(f(:)));"
        WRITE(unit,'(A)') "end"
        WRITE(unit,'(A)') "if ~exist('times','var')"
        WRITE(unit,'(A)') "  times = 1:nSteps;"
        WRITE(unit,'(A)') "end"
        WRITE(unit,'(A)') "gifFile = fullfile(thisDir,'time_temp.gif');"
        WRITE(unit,'(A)') "if exist(gifFile,'file')"
        WRITE(unit,'(A)') "  delete(gifFile);"
        WRITE(unit,'(A)') "end"
        WRITE(unit,'(A)') "delayTime = 0.10;"
        WRITE(unit,'(A)') "fontSize = 18;"
        WRITE(unit,'(A)') "figure(1);"
        WRITE(unit,'(A)') "for k = 1:nSteps"
        WRITE(unit,'(A)') "  field = fields{k};"
        WRITE(unit,'(A)') "  clf;"
        WRITE(unit,'(A)') "  h = pcolor(field);"
        WRITE(unit,'(A)') "  set(h, 'EdgeColor', 'none');"
        WRITE(unit,'(A)') "  axis equal tight;"
        WRITE(unit,'(A)') "  cb = colorbar;"
        WRITE(unit,'(A)') "  cb.Label.String = 'Temperature, T';"
        WRITE(unit,'(A)') "  cb.FontSize = fontSize;"
        WRITE(unit,'(A)') "  cb.Label.FontSize = fontSize;"
        WRITE(unit,'(A)') "  caxis([vmin vmax]);"
        WRITE(unit,'(A)') "  title(sprintf('t = %g [s]', times(k)));"
        WRITE(unit,'(A)') "  set(gca,'FontSize',fontSize);"
        WRITE(unit,'(A)') "  set(get(gca,'Title'),'FontSize',fontSize);"
        WRITE(unit,'(A)') "  drawnow;"
        WRITE(unit,'(A)') "  fr = getframe(gcf);"
        WRITE(unit,'(A)') "  im = frame2im(fr);"
        WRITE(unit,'(A)') "  [A,map] = rgb2ind(im,256);"
        WRITE(unit,'(A)') "  if k == 1"
        WRITE(unit,'(A)') "    imwrite(A,map,gifFile,'gif','LoopCount',inf,'DelayTime',delayTime);"
        WRITE(unit,'(A)') "  else"
        WRITE(unit,'(A)') "    imwrite(A,map,gifFile,'gif','WriteMode','append','DelayTime',delayTime);"
        WRITE(unit,'(A)') "  end"
        WRITE(unit,'(A)') "end"
        WRITE(unit,'(A)') ""
        WRITE(unit,'(A)') "% Final state as a surface plot"
        WRITE(unit,'(A)') "finalField = fields{end};"
        WRITE(unit,'(A)') "figure(2); clf;"
        WRITE(unit,'(A)') "hs = surf(finalField);"
        WRITE(unit,'(A)') "zlabel('Temperature, T','FontSize',fontSize)"
        WRITE(unit,'(A)') "shading interp;"
        WRITE(unit,'(A)') "cb2 = colorbar;"
        WRITE(unit,'(A)') "cb2.Label.String = 'Temperature, T';"
        WRITE(unit,'(A)') "cb2.FontSize = fontSize;"
        WRITE(unit,'(A)') "cb2.Label.FontSize = fontSize;"
        WRITE(unit,'(A)') "caxis([vmin vmax]);"
        WRITE(unit,'(A)') "axis tight;"
        WRITE(unit,'(A)') "view(45,30);"
        WRITE(unit,'(A)') "title(sprintf('Final Temperature (t = %g [s])', times(end)));"
        WRITE(unit,'(A)') "set(gca,'FontSize',fontSize);"
        WRITE(unit,'(A)') "set(get(gca,'Title'),'FontSize',fontSize);"
        WRITE(unit,'(A)') "set(hs, 'EdgeColor', 'k');"
        WRITE(unit,'(A)') "pngFile = fullfile(thisDir,'final_temp.png');"
        WRITE(unit,'(A)') "print(gcf, pngFile, '-dpng', '-r150');"
        CLOSE(unit)
      ELSE
        PRINT*, 'Warning: could not create MATLAB plot script: ', TRIM(plot_filename)
      END IF
    END IF

    ! Ensure fields_data.m exists and has initial containers
    INQUIRE(FILE=TRIM(fields_filename), EXIST=fields_exists)
    IF (.NOT. fields_exists) THEN
      OPEN(NEWUNIT=unit, FILE=TRIM(fields_filename), STATUS='REPLACE', ACTION='WRITE', IOSTAT=ios)
      IF (ios /= 0) THEN
        PRINT*, 'Error: could not create MATLAB fields file: ', TRIM(fields_filename)
        RETURN
      END IF
      WRITE(unit,'(A)') '% Auto-generated MATLAB data file'
      WRITE(unit,'(A)') '% Each timestep appended as fields{idx} and times(idx)'
      WRITE(unit,'(A)') 'fields = {};'
      WRITE(unit,'(A)') 'times = [];'
      CLOSE(unit)
      field_write_count = 0
    END IF

    ! Append this timestep to fields_data.m as the next cell element
    step_idx = field_write_count + 1
    OPEN(NEWUNIT=unit, FILE=TRIM(fields_filename), STATUS='OLD', ACTION='WRITE', POSITION='APPEND', IOSTAT=ios)
    IF (ios /= 0) THEN
      PRINT*, 'Error: could not open MATLAB fields file for append: ', TRIM(fields_filename)
      RETURN
    END IF

    WRITE(unit,'(A)') ''
    WRITE(unit,'(A)') '% ------------------------------------------------------------'
    WRITE(unit,'(A,I0,A,A)') '% step ', step_idx, '  (tag ', TRIM(tstamp)//')'
    WRITE(unit,'(A,I0,A,ES24.16)') 'times(', step_idx, ') = ', timestamp
    WRITE(unit,'(A,I0,A)') 'fields{', step_idx, '} = [ ...'

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

    CLOSE(unit)
    field_write_count = step_idx
  END SUBROUTINE write_matlab_field
!-------------------------------------------------------------

END MODULE matlab_io
