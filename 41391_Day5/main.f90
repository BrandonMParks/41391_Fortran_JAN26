! Solves the steady-state 2D Diffusion equation using
! 2nd-Order Finite Difference and a standard Euler time step scheme
! Brandon Parks (bmapa@dtu.dk) - 09-01-2026
PROGRAM main
USE matlab_io
USE transfer, ONLY: advance_solution
USE alloc_field, ONLY: setup_fields
USE precision, ONLY: wp
USE utilities

IMPLICIT NONE
TYPE(input_t) :: inp
REAL(wp), ALLOCATABLE, TARGET :: field1_store(:,:), field2_store(:,:)
REAL(wp), POINTER :: field1(:,:), field2(:,:)
REAL(wp) :: dx,dy,rdx2,rdy2,dt_limit
INTEGER :: i,j,istep
CHARACTER(LEN=8) :: datestr
CHARACTER(LEN=10) :: timestr

! Start timer
CALL date_and_time(date=datestr, time=timestr)
WRITE (*,'(1a,1a,"-",1a,"-",1a,1x,1a,":",1a,":",1a)') &
   'Start time: ', datestr(1:4), datestr(5:6), datestr(7:8), &
   timestr(1:2), timestr(3:4), timestr(5:10)
call stopwatch('star')

! Read runtime parameters from text input file
CALL input('input', inp)

dx = inp%lx/REAL(inp%nx-1, KIND=wp); dy = inp%ly/REAL(inp%ny-1, KIND=wp)
rdx2 = 1.0_wp/dx**2; rdy2 = 1.0_wp/dy**2
dt_limit = (MIN(dx,dy)**2)/(4.0_wp*inp%diff)

IF (inp%dt <= 0.0_wp) THEN
   inp%dt = inp%safety*dt_limit
   PRINT*,'Input dt greater than Fourier limit: dt set to Fourier limit.'
END IF

!print*,'dt_limit = ',dt_limit ,' safety = ',inp%safety
print*,'dt = ',inp%dt,' tmax = ',inp%nstep*inp%dt

CALL setup_fields(field1_store, field2_store, field1, field2, inp%nx, inp%ny)

! Initial condition + Dirichlet boundary conditions (u=1 on boundary)
field2(:,:) = 0.0_wp
field2(1,:) = 1.0_wp
field2(inp%nx,:) = 1.0_wp
field2(:,1) = 1.0_wp
field2(:,inp%ny) = 1.0_wp

! Perform time stepping
DO istep=1,inp%nstep
   ! compute
   ! Start from the old field
   field1(:,:) = field2(:,:)

   DO j=2,inp%ny-1
      DO i=2,inp%nx-1
         field1(i,j) = field2(i,j) + inp%dt*inp%diff*( &
            (field2(i+1,j) - 2.0_wp*field2(i,j) + field2(i-1,j))*rdx2 + &
            (field2(i,j+1) - 2.0_wp*field2(i,j) + field2(i,j-1))*rdy2 )
      ENDDO ! Nx
   ENDDO ! Ny

   ! Enforce fixed boundary values because it feels unsafe not to
   field1(1,:) = 1.0_wp
   field1(inp%nx,:) = 1.0_wp
   field1(:,1) = 1.0_wp
   field1(:,inp%ny) = 1.0_wp

   ! Advance solution
   call advance_solution(field2, field1)

   ! Output every few time steps
   IF (MOD(istep, inp%output_every) == 0) THEN
      call write_matlab_field('field.m', field2, inp%dt*REAL(istep, KIND=wp))
   ENDIF

ENDDO ! time step

IF (0.0 .GE. 1.0) THEN  ! manual toggle to output the final field values for validation
   print*, 'Final field2 (rows j=1..Ny):'
   DO j=1,inp%ny
      print*, field2(:,j)
   ENDDO
END IF

print*, 't_final = ', (istep-1)*inp%dt, 'Nx = ', inp%nx, 'Ny = ', inp%ny, 'dt*Nx*Ny = ', istep*inp%nx*inp%ny

! Output
call write_matlab_field('final_field.m', field2, inp%dt*REAL(istep-1, KIND=wp))

! Stop timer
CALL date_and_time(date=datestr, time=timestr)
WRITE (*,'(1a,1a,"-",1a,"-",1a,1x,1a,":",1a,":",1a)') &
   'End time:   ', datestr(1:4), datestr(5:6), datestr(7:8), &
   timestr(1:2), timestr(3:4), timestr(5:10)
call stopwatch('stop')
END PROGRAM main
