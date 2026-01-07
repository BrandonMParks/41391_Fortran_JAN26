! Small program
PROGRAM main
USE matlab_io
USE transfer
USE alloc_field
USE precision, ONLY: wp
INTEGER, PARAMETER :: Nx = 21, Ny = 21
REAL(wp), ALLOCATABLE, TARGET :: field1_store(:,:), field2_store(:,:)
REAL(wp), POINTER :: field1(:,:), field2(:,:)
REAL(wp) :: dx,dy,Lx,Ly,rdx2,rdy2,dt,diff,dt_limit,safety
INTEGER :: i,j,istep,nstep

! Initialize
Lx = 1.0_wp; Ly = 1.0_wp
dx = Lx/REAL(Nx-1, KIND=wp); dy = Ly/REAL(Ny-1, KIND=wp)
rdx2 = 1.0_wp/dx**2; rdy2 = 1.0_wp/dy**2
nstep = 200
diff = 1.0_wp
dt_limit = (MIN(dx,dy)**2)/(4.0_wp*diff)
safety = 1.0_wp
dt = safety*dt_limit

print*,'dt_limit = ',dt_limit,' safety = ',safety
print*,'dt = ',dt,' tmax = ',nstep*dt

CALL setup_fields(field1_store, field2_store, field1, field2, Nx, Ny)

! Initial condition + Dirichlet boundary conditions (u=1 on boundary)
field2(:,:) = 0.0_wp
field2(1,:) = 1.0_wp
field2(Nx,:) = 1.0_wp
field2(:,1) = 1.0_wp
field2(:,Ny) = 1.0_wp

! Perform time stepping
DO istep=1,nstep
   ! compute
   ! Start from the old field (keeps boundaries as-is)
   field1(:,:) = field2(:,:)

   DO j=2,Ny-1
      DO i=2,Nx-1
         field1(i,j) = field2(i,j) + dt*diff*( &
            (field2(i+1,j) - 2.0_wp*field2(i,j) + field2(i-1,j))*rdx2 + &
            (field2(i,j+1) - 2.0_wp*field2(i,j) + field2(i,j-1))*rdy2 )
      ENDDO ! Nx
   ENDDO ! Ny

   ! Enforce fixed boundary values on the new field
   field1(1,:) = 1.0_wp
   field1(Nx,:) = 1.0_wp
   field1(:,1) = 1.0_wp
   field1(:,Ny) = 1.0_wp

   ! Advance solution
   call advance_solution(field2, field1)

   ! Output every few time steps
   IF (MOD(istep, 10) == 0) THEN
      call write_matlab_field('field.m', field2, dt*REAL(istep, KIND=wp))
   ENDIF

ENDDO ! time step

print*, 'Final field2 (rows j=1..Ny):'
DO j=1,Ny
   print*, field2(:,j)
ENDDO

print*, 't_final = ', (istep-1)*dt, 'Nx = ', Nx, 'Ny = ', Ny, 'dt*Nx*Ny = ', istep*Nx*Ny

!       Output
call write_matlab_field('final_field.m', field2, dt*REAL(istep-1, KIND=wp))

END PROGRAM main

