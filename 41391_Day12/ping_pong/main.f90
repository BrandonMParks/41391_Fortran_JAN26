PROGRAM main
   USE, INTRINSIC :: iso_fortran_env, ONLY: real64, output_unit
   USE mpi

   IMPLICIT NONE

   INTEGER, PARAMETER :: root = 0
   INTEGER, PARAMETER :: peer = 1
   INTEGER, PARAMETER :: nrepeat = 10
   INTEGER, PARAMETER :: max_pow2 = 18

   INTEGER :: ierr, rank, nranks
   INTEGER :: k, n, i
   INTEGER :: status(MPI_STATUS_SIZE)
   INTEGER :: tag
   REAL(real64) :: t0, t1, dt_round, dt_oneway
   REAL(real64) :: bw_bytes_per_s
   REAL(real64) :: bw_min, bw_max, bw_sum
   REAL(real64) :: t_avg_oneway

   REAL(real64), ALLOCATABLE :: sendbuf(:), recvbuf(:)
   REAL(real64), ALLOCATABLE :: x_bytes(:), y_time(:)
   REAL(real64) :: latency_s, bw_fit_bytes_per_s

   CALL MPI_Init(ierr)
   CALL MPI_Comm_rank(MPI_COMM_WORLD, rank, ierr)
   CALL MPI_Comm_size(MPI_COMM_WORLD, nranks, ierr)

   IF (nranks < 2) THEN
      IF (rank == root) THEN
         WRITE (output_unit,'(A)') 'ERROR: run with at least 2 MPI processes.'
         WRITE (output_unit,'(A)') 'Example: mpiexec -n 2 ./bin/main'
      END IF
      CALL MPI_Abort(MPI_COMM_WORLD, 1, ierr)
   END IF

   IF (rank == root) THEN
      WRITE (output_unit,'(A)') 'MPI ping-pong benchmark (ranks 0 and 1)'
      WRITE (output_unit,'(A,I0)') 'Repeats per message size: ', nrepeat
      WRITE (output_unit,'(A)') 'Size doubles | Size bytes | avg one-way (us) | BW min/avg/max (GB/s)'
   END IF

   ALLOCATE(x_bytes(0:max_pow2), y_time(0:max_pow2))

   DO k = 0, max_pow2
      n = 2**k
      ALLOCATE(sendbuf(n), recvbuf(n))
      sendbuf = REAL(rank, real64)
      recvbuf = -1.0_real64

      bw_min = HUGE(1.0_real64)
      bw_max = 0.0_real64
      bw_sum = 0.0_real64
      t_avg_oneway = 0.0_real64
      tag = 1000 + k

      CALL MPI_Barrier(MPI_COMM_WORLD, ierr)
      DO i = 1, nrepeat
         IF (rank == root) THEN
            t0 = MPI_Wtime()
            CALL MPI_Send(sendbuf, n, MPI_DOUBLE_PRECISION, peer, tag, MPI_COMM_WORLD, ierr)
            CALL MPI_Recv(recvbuf, n, MPI_DOUBLE_PRECISION, peer, tag, MPI_COMM_WORLD, status, ierr)
            t1 = MPI_Wtime()

            dt_round = t1 - t0
            dt_oneway = 0.5_real64 * dt_round

            bw_bytes_per_s = (2.0_real64 * REAL(n, real64) * 8.0_real64) / dt_round
            bw_min = MIN(bw_min, bw_bytes_per_s)
            bw_max = MAX(bw_max, bw_bytes_per_s)
            bw_sum = bw_sum + bw_bytes_per_s
            t_avg_oneway = t_avg_oneway + dt_oneway

         ELSE IF (rank == peer) THEN
            CALL MPI_Recv(recvbuf, n, MPI_DOUBLE_PRECISION, root, tag, MPI_COMM_WORLD, status, ierr)
            CALL MPI_Send(recvbuf, n, MPI_DOUBLE_PRECISION, root, tag, MPI_COMM_WORLD, ierr)
         END IF
      END DO

      IF (rank == root) THEN
         t_avg_oneway = t_avg_oneway / REAL(nrepeat, real64)
         x_bytes(k) = REAL(n, real64) * 8.0_real64
         y_time(k) = t_avg_oneway

         WRITE (output_unit,'(I10,1X,I11,1X,F15.3,1X,F9.3,1X,F9.3,1X,F9.3)') &
            n, INT(x_bytes(k)), 1.0e6_real64 * t_avg_oneway, &
            (bw_min / 1.0e9_real64), ( (bw_sum/REAL(nrepeat, real64)) / 1.0e9_real64 ), (bw_max / 1.0e9_real64)
      END IF

      DEALLOCATE(sendbuf, recvbuf)
   END DO

   IF (rank == root) THEN
      CALL fit_latency_bandwidth(x_bytes, y_time, latency_s, bw_fit_bytes_per_s)
      WRITE (output_unit,'(A)') ''
      WRITE (output_unit,'(A,F10.3)') 'Estimated latency (us): ', 1.0e6_real64 * latency_s
      WRITE (output_unit,'(A,F10.3)') 'Estimated bandwidth (GB/s): ', bw_fit_bytes_per_s / 1.0e9_real64
   END IF

   DEALLOCATE(x_bytes, y_time)
   CALL MPI_Finalize(ierr)

CONTAINS

   SUBROUTINE fit_latency_bandwidth(x, y, latency, bandwidth)
      REAL(real64), INTENT(IN) :: x(:)
      REAL(real64), INTENT(IN) :: y(:)
      REAL(real64), INTENT(OUT) :: latency
      REAL(real64), INTENT(OUT) :: bandwidth

      INTEGER :: j, npts
      REAL(real64) :: sx, sy, sxx, sxy
      REAL(real64) :: denom, slope, intercept

      npts = SIZE(x)
      sx = 0.0_real64
      sy = 0.0_real64
      sxx = 0.0_real64
      sxy = 0.0_real64

      DO j = 1, npts
         sx = sx + x(j)
         sy = sy + y(j)
         sxx = sxx + x(j) * x(j)
         sxy = sxy + x(j) * y(j)
      END DO

      denom = REAL(npts, real64) * sxx - sx * sx
      IF (denom <= 0.0_real64) THEN
         latency = 0.0_real64
         bandwidth = 0.0_real64
         RETURN
      END IF

      slope = (REAL(npts, real64) * sxy - sx * sy) / denom
      intercept = (sy - slope * sx) / REAL(npts, real64)

      latency = MAX(0.0_real64, intercept)
      IF (slope > 0.0_real64) THEN
         bandwidth = 1.0_real64 / slope
      ELSE
         bandwidth = 0.0_real64
      END IF
   END SUBROUTINE fit_latency_bandwidth

END PROGRAM main
