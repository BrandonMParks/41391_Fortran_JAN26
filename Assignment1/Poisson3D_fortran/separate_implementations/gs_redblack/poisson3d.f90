program poisson3d

  ! Import different methods
  use precision
  use poisson_methods
  use write_output
  use utilities, only: stopwatch
  use omp_lib, only: omp_get_max_threads, omp_get_dynamic, omp_get_thread_limit, &
      omp_set_num_threads, omp_set_dynamic, omp_get_wtime

  ! Define options for user
  ! Please note that while we assign on definition, save is implicit.
  ! But please write save to show intent.
  integer, save :: N = 100                  ! interior grid dimension
  real(dp), save :: T0 = 0._dp              ! initial temperature value
  integer, save :: itermax = 1000           ! max solver iterations
  real(dp), save :: tolerance = 1.e-4_dp    ! solver convergence criteria
  integer, save :: output = 4               ! 3 for binary, 4 for VTK
  integer, save :: algorithm = 1            ! 1 for Jacobi, 2 for Gauss-Seidel
  ! don't ask why, this is for historical reasons and to be compatible with the C-version
  ! of the assignment

  real(dp), allocatable, dimension(:,:,:) :: u, u_old, f
  integer :: iostat
  integer :: nthreads_cli

  ! Performance logging
  integer :: iters_done, threads_used
  real(dp) :: diffmax_final
  real(dp) :: t_solver_total, t_solver_update
  real(dp) :: t_wall0, t_wall1

  character(len=256) :: omp_display_env
  integer :: env_len, env_status

  logical :: write_perf
  character(len=256) :: ostype_env, perf_csv_env
  integer :: ostype_len, ostype_status
  integer :: perf_len, perf_status, perf_csv_i, perf_ios

  ! Read in namelist.
  ! Call program namelist
  ! Create a file called: input.dat
  ! The content may be:
  ! &INPUT
  !   N = 100
  !   itermax = 3000
  !   T0 = 10.
  !   tolerance = 1e-5
  !   output = 0
  !   algorithm = 1
  ! /
  ! If one of the keywords are not in the
  ! input.dat namelist, then the default value
  ! will be used.
  call read_namelist()

  ! Optional: override OpenMP thread count via command-line argument:
  !   poisson3d.exe --threads=4
  !   poisson3d.exe --threads 4
  !   poisson3d.exe -t 4
  call parse_cmdline_threads(nthreads_cli)
  if (nthreads_cli > 0) then
    call omp_set_dynamic(.false.)
    call omp_set_num_threads(nthreads_cli)
  end if

  ! Performance CSV-write toggle:
  write_perf = .true.
  perf_csv_i = -1
  call get_environment_variable('PERF_CSV', perf_csv_env, length=perf_len, status=perf_status)
  if (perf_status == 0 .and. perf_len > 0) then
    read(perf_csv_env(1:perf_len), *, iostat=perf_ios) perf_csv_i
    if (perf_ios == 0) write_perf = (perf_csv_i /= 0)
  else
    call get_environment_variable('OSTYPE', ostype_env, length=ostype_len, status=ostype_status)
    if (ostype_status == 0 .and. ostype_len > 0) then
      if (index(trim(ostype_env(1:ostype_len)), 'linux') > 0 .or. &
          index(trim(ostype_env(1:ostype_len)), 'Linux') > 0 .or. &
          index(trim(ostype_env(1:ostype_len)), 'LINUX') > 0) then
        write_perf = .false.
      end if
    end if
  end if

  ! Enforce OMP_DISPLAY_ENV=verbose for reproducible OpenMP diagnostics.
  call get_environment_variable('OMP_DISPLAY_ENV', omp_display_env, length=env_len, status=env_status)
  if (env_status /= 0 .or. env_len <= 0) then
    write(*,'(a)') 'WARNING: OMP_DISPLAY_ENV is not set.'
    write(*,'(a)') 'For reproducible OpenMP diagnostics, set it to verbose.'
    write(*,'(a)') 'Examples:'
    write(*,'(a)') '  Linux/macOS: export OMP_DISPLAY_ENV=verbose'
    write(*,'(a)') '  PowerShell:  $env:OMP_DISPLAY_ENV="verbose"'
    write(*,'(a)') '  Makefile:    make run RUN_ARGS="-t 2"'
  end if
  if (env_status == 0 .and. env_len > 0) then
    omp_display_env = adjustl(omp_display_env(1:env_len))
    if (trim(omp_display_env) /= 'verbose' .and. trim(omp_display_env) /= 'VERBOSE') then
      write(*,'(a)') 'WARNING: OMP_DISPLAY_ENV is set but not "verbose".'
      write(*,'(a,a)') 'Current value: ', trim(omp_display_env)
    end if
  end if

  !!!!!!!!!!!!!!!!!!!!! Beginning of analysis section !!!!!!!!!!!!!!!!!!!!!

  ! Allocate solution (old and new) and boundary condition arrays
  allocate(u(N+2,N+2,N+2), stat=iostat)
  call check_iostat(iostat, "Could not allocate 'u' matrix!")
  allocate(f(N+2, N+2, N+2), stat=iostat)
  call check_iostat(iostat, "Could not allocate 'f' matrix!")
  
  ! Iterative solution - Jacobi and Gauss-Seidel
  print*, 'Starting Poisson 3D solver with the following parameters:'
  print*, '  N (interior grid size)      = ', N
  print*, '  itermax (max iterations)    = ', itermax
  print*, '  convergence tolerance       = ', tolerance
  print*, '  Iterative algorithm         = ', algorithm
  print*, '  OpenMP max threads          = ', omp_get_max_threads()
  print*, '  OpenMP thread limit         = ', omp_get_thread_limit()
  print*, '  OpenMP dynamic threads      = ', omp_get_dynamic()
  print*, 'Solving...'

  iters_done = -1
  diffmax_final = -1._dp
  threads_used = -1
  t_solver_total = -1._dp
  t_solver_update = -1._dp
  t_wall0 = omp_get_wtime()

  select case ( algorithm )
  case ( 1 )  ! Jacobi
    call jacobi(u, f, T0, itermax, tolerance, &
      iters_done=iters_done, diffmax_final=diffmax_final, &
      t_total=t_solver_total, t_update=t_solver_update, nthreads_used=threads_used)

  case ( 2 )  ! Gauss-Seidel
    call gauss_seidel(u, f, T0, itermax, tolerance, &
      iters_done=iters_done, diffmax_final=diffmax_final, t_total=t_solver_total)

  case default
    print*, 'ERROR: Algorithm selected does not have a corresponding implemented method.'
    print*, 'Ensure "input.dat" prescribes an implemented method. Exiting...'
    stop
  end select

  t_wall1 = omp_get_wtime()

  if (write_perf) then
    call write_perf_csv('output/perf.csv', N, itermax, tolerance, algorithm, &
      omp_get_max_threads(), nthreads_cli, omp_get_dynamic(), &
      t_wall1 - t_wall0, t_solver_total, t_solver_update, iters_done, diffmax_final, threads_used)
  end if

  ! deallocate loading & solution copy arrays
  deallocate(f)
  if ( allocated(u_old) ) deallocate(u_old)

  !!!!!!!!!!!!!!!!!!!!!!!! End of analysis section !!!!!!!!!!!!!!!!!!!!!!!!

  ! checksum for cross-run comparisons to verify parallel implementation
  call checksum(u)

  ! Output solution to selected filetype
  ! Keep u until we have written in out
  select case ( output )
  case ( 0 ) ! pass, valid but not used value
    ! do nothing
  case ( 3 ) ! write to binary file
    call write_binary(u)
  case ( 4 ) ! write to VTK file
    call write_vtk(u)
  case ( 5 ) ! write MATLAB binary data file + plot script
    call write_matlab(u)
  case default
    write(*,'(a,i0)') 'Unknown output type requested: ', output
    stop
  end select

  ! deallocate solution array
  deallocate(u)

  ! Done
  print*, 'poisson3d successfully executed. Exiting...'

contains

  subroutine write_perf_csv(path, N, itermax, tolerance, algorithm, omp_max_threads, nthreads_cli, omp_dynamic, &
      t_wall_total, t_solver_total, t_solver_update, iters_done, diffmax_final, threads_used)
    character(len=*), intent(in) :: path
    integer, intent(in) :: N, itermax, algorithm, omp_max_threads, nthreads_cli, iters_done, threads_used
    logical, intent(in) :: omp_dynamic
    real(dp), intent(in) :: tolerance, t_wall_total, t_solver_total, t_solver_update, diffmax_final

    integer :: unit
    logical :: exists
    integer :: dt(8)
    character(len=32) :: algo_name
    integer :: omp_dyn_i
    character(len=*), parameter :: csv_header = &
      'date,time,N,itermax,tolerance,algorithm,' // &
      'omp_max_threads,cli_threads,omp_dynamic,threads_used,' // &
      'wall_total_s,solver_total_s,solver_update_s,iters_done,diffmax_final'

    inquire(file=path, exist=exists)
    call date_and_time(values=dt)

    select case (algorithm)
    case (1)
      algo_name = 'jacobi'
    case (2)
      algo_name = 'gauss_seidel'
    case default
      algo_name = 'unknown'
    end select

    omp_dyn_i = 0
    if (omp_dynamic) omp_dyn_i = 1

    open(newunit=unit, file=path, status='unknown', position='append', action='write')
    if (.not. exists) then
      write(unit,'(a)') csv_header
    end if

    write(unit,'(i4.4,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,i2.2,a,'// &
               'i0,a,i0,a,es12.5,a,a,a,i0,a,i0,a,i0,a,i0,a,'// &
               'es23.16,a,es23.16,a,es23.16,a,i0,a,es23.16)') &
      dt(1), '-', dt(2), '-', dt(3), ',', dt(5), ':', dt(6), ':', dt(7), ',', &
      N, ',', itermax, ',', tolerance, ',', trim(algo_name), ',', &
      omp_max_threads, ',', nthreads_cli, ',', omp_dyn_i, ',', threads_used, ',', &
      t_wall_total, ',', t_solver_total, ',', t_solver_update, ',', iters_done, ',', diffmax_final

    close(unit)
  end subroutine write_perf_csv

  subroutine parse_cmdline_threads(nthreads)
    integer, intent(out) :: nthreads

    integer :: argc, i, ios
    character(len=256) :: arg, val

    nthreads = -1
    argc = command_argument_count()
    if (argc == 0) return

    do i = 1, argc
      call get_command_argument(i, arg)
      arg = adjustl(arg)

      if (arg == '--help' .or. arg == '-h') then
        print*, 'Usage:'
        print*, '  poisson3d.exe [--threads=NUM]'
        print*, '  poisson3d.exe --threads NUM'
        print*, '  poisson3d.exe -t NUM'
        print*, ''
        print*, 'Notes:'
        print*, '  If no thread argument is given, OpenMP uses OMP_NUM_THREADS / defaults.'
        stop
      end if

      if (index(arg, '--threads=') == 1) then
        val = arg(len('--threads=') + 1:)
        read(val, *, iostat=ios) nthreads
        if (ios /= 0 .or. nthreads <= 0) then
          print*, 'ERROR: Invalid thread count in ', trim(arg)
          stop
        end if
      else if (arg == '--threads' .or. arg == '-t') then
        if (i == argc) then
          print*, 'ERROR: Missing value after ', trim(arg)
          stop
        end if
        call get_command_argument(i + 1, val)
        read(val, *, iostat=ios) nthreads
        if (ios /= 0 .or. nthreads <= 0) then
          print*, 'ERROR: Invalid thread count: ', trim(val)
          stop
        end if
      end if
    end do
  end subroutine parse_cmdline_threads

  subroutine read_namelist()
    integer :: unit, io_err
    
    namelist /INPUT/ N, itermax, T0, tolerance, output, algorithm
    
    ! open and read file
    unit = 128273598
    open(unit, FILE="input.dat", action='read', iostat=io_err)
    call check_iostat(io_err, &
        "Could not open file 'input.dat', perhaps it does not exist?")
    read(unit, nml=INPUT, iostat=io_err)
    call check_iostat(io_err, &
        "Error on reading name-list, please correct file content")
    close(unit)

  end subroutine read_namelist

  subroutine check_iostat(iostat, msg)
    integer, intent(in) :: iostat
    character(len=*), intent(in) :: msg

    if ( iostat == 0 ) return

    write(*,'(a,i0,/,tr3,a)') 'ERROR = ', iostat, msg
    stop

  end subroutine check_iostat

  subroutine checksum(u)
    ! checksum for cross-run comparisons to verify parallel implementation
    ! takes a slice at the mid-plane in y-direction
    real(dp), intent(in) :: u(:,:,:)
    integer :: n_tot, jmid
    real(dp) :: s1, umin, umax

    n_tot = size(u, 1)
    jmid = (n_tot + 1) / 2

    s1 = sum(u(:, jmid, :))
    umin = minval(u(:, jmid, :))
    umax = maxval(u(:, jmid, :))

    write(*,'(a,i0)')   'Checksum slice: y-mid plane index j = ', jmid
    write(*,'(a,es23.16)') '  sum(u)      = ', s1
    write(*,'(a,es23.16)') '  min(u)      = ', umin
    write(*,'(a,es23.16)') '  max(u)      = ', umax

  end subroutine checksum

end program poisson3d
