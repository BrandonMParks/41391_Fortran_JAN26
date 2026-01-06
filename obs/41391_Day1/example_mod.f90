      ! Small program
      PROGRAM main
      INTEGER, PARAMETER :: Nx = 20, Ny = 20
      REAL, DIMENSION(Nx,Ny) :: field1,field2
      REAL :: dx,dy,Lx,Ly,rdx2,rdy2,stime,dt,diff
      INTEGER :: istep,nstep

      ! Initialize
      Lx = 1.0; Ly = 1.0
      dx = Lx/REAL(Nx-1); dy = Ly/REAL(Ny-1)
      rdx2 = 1.0/dx**2; rdy2 = 1.0/dy**2
      nstep = 200
      diff = 1.0
      dt = 0.25*MIN(dx,dy)**2/diff

      print*,'dt = ',dt,' tmax = ',nstep*dt

      ! Perform time stepping
      DO istep=1,nstep
         ! compute
         DO j=2,Ny-1
            DO i=1,Nx-1
               IF (i==1) print*,'Implementation incomplete.'
               field1(I,j) = 0.0
            ENDDO ! Nx
         ENDDO ! Ny

      ENDDO ! time step
      print*, 'num_dt = ', istep, 'Nx = ', Nx, 'Ny = ', Ny
!       Output

      END PROGRAM main

