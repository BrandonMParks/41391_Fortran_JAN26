! Small program
PROGRAM example_mod
USE matlab_io
USE time_stepping
INTEGER, PARAMETER :: Nx = 21, Ny = 21
REAL, TARGET, DIMENSION(Nx,Ny) :: field1_store, field2_store
REAL, POINTER, DIMENSION(:,:) :: field1, field2
REAL :: dx,dy,Lx,Ly,rdx2,rdy2,dt,diff,dt_limit,safety
INTEGER :: i,j,istep,nstep

! Initialize
Lx = 1.0; Ly = 1.0
dx = Lx/REAL(Nx-1); dy = Ly/REAL(Ny-1)
rdx2 = 1.0/dx**2; rdy2 = 1.0/dy**2
nstep = 200
diff = 1.0
dt_limit = (MIN(dx,dy)**2)/(4.0*diff)
safety = 1.0
dt = safety*dt_limit

print*,'dt_limit = ',dt_limit,' safety = ',safety
print*,'dt = ',dt,' tmax = ',nstep*dt

field1 => field1_store
field2 => field2_store

! Initial condition + Dirichlet boundary conditions (u=1 on boundary)
field2(:,:) = 0.0
field2(1,:) = 1.0
field2(Nx,:) = 1.0
field2(:,1) = 1.0
field2(:,Ny) = 1.0
!field1(:,:) = field2(:,:)

! Perform time stepping
DO istep=1,nstep
   ! compute
   ! Start from the old field (keeps boundaries as-is)
   field1(:,:) = field2(:,:)

   DO j=2,Ny-1
      DO i=2,Nx-1
         field1(i,j) = field2(i,j) + dt*diff*( &
            (field2(i+1,j) - 2.0*field2(i,j) + field2(i-1,j))*rdx2 + &
            (field2(i,j+1) - 2.0*field2(i,j) + field2(i,j-1))*rdy2 )
      ENDDO ! Nx
   ENDDO ! Ny

   ! Enforce fixed boundary values on the new field
   field1(1,:) = 1.0
   field1(Nx,:) = 1.0
   field1(:,1) = 1.0
   field1(:,Ny) = 1.0

   ! Advance solution
   call advance_solution(field2, field1)

   ! Output every few time steps
   print*, istep/10, istep/10.0
   IF (istep/10 .EQ. istep/10.0) THEN
      call write_matlab_field('field.m', field2, dt*REAL(istep))
   ENDIF

ENDDO ! time step

print*, 'Final field2 (rows j=1..Ny):'
DO j=1,Ny
   print*, field2(:,j)
ENDDO

print*, 't_final = ', (istep-1)*dt, 'Nx = ', Nx, 'Ny = ', Ny, 'dt*Nx*Ny = ', istep*Nx*Ny

!       Output
call write_matlab_field('final_field.m', field2, dt*REAL(istep-1))

END PROGRAM example_mod

