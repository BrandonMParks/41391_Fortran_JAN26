program poisson3d

  ! Import different methods
  use precision
  use poisson_methods
  use write_output
  use omp_lib, only: omp_get_max_threads, omp_get_dynamic, omp_get_thread_limit, &
      omp_set_num_threads, omp_set_dynamic

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

  ! Optional: override OpenMP thread count via command-line argument.
  ! Examples:
  !   poisson3d.exe --threads=4
  !   poisson3d.exe --threads 4
  !   poisson3d.exe -t 4
  call parse_cmdline_threads(nthreads_cli)
  if (nthreads_cli > 0) then
    call omp_set_dynamic(.false.)
    call omp_set_num_threads(nthreads_cli)
  end if

  ! Allocate solution (old and new) and boundary condition arrays
  allocate(u(N+2,N+2,N+2), stat=iostat)
  call check_iostat(iostat, "Could not allocate 'u' matrix!")
  allocate(f(N+2, N+2, N+2), stat=iostat)
  call check_iostat(iostat, "Could not allocate 'f' matrix!")

  !!!!!!!!!!!!!!!!!!!!! Beginning of analysis section !!!!!!!!!!!!!!!!!!!!!

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

  select case ( algorithm )
  case ( 1 )  ! Jacobi
    call jacobi(u, f, T0, itermax, tolerance)

  case ( 2 )  ! Gauss-Seidel
    call gauss_seidel(u, f, T0, itermax, tolerance)

  case default
    print*, 'ERROR: Algorithm selected does not have a corresponding implemented method.'
    print*, 'Ensure "input.dat" prescribes an implemented method. Exiting...'
    stop
  end select

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
