#!/usr/bin/env bash

### General options 

# -- specify queue -- 
#BSUB -q hpcintro

# -- Job name (MPI ping-pong) --
#BSUB -J bmp_mpi_pingpong

# -- Number of MPI processes (and cores to request) --
#    Ping-pong uses ranks 0 and 1.
#BSUB -n 2

# -- Ensure cores are on the same host -- 
#BSUB -R "span[hosts=1]"

# -- Memory requirements (2–4 GB) -- 
#BSUB -R "rusage[mem=2GB]"
#BSUB -M 2GB

# -- Walltime limit (hh:mm) -- 
#BSUB -W 00:30

# -- Output and error files --
#BSUB -oo output/Output_%J.out
#BSUB -eo output/Error_%J.err

job_id=${LSB_JOBID:-unknown}

echo "=== Job ${job_id} started ==="
echo "Working directory: $(pwd)"
echo "Hostname: $(hostname)"
echo "Start time: $(date)"

MPI_NP=${LSB_DJOB_NUMPROC:-1}
echo "Cores allocated by LSF (-n) = ${MPI_NP}"
echo "MPI processes to start (x)   = ${MPI_NP}"

exe=bin/main
if [[ ! -x "${exe}" ]]; then
  echo "ERROR: executable not found or not executable: ${exe}" >&2
  echo "Build it first (e.g. run 'make' in this directory)." >&2
  exit 1
fi

echo ""
echo "===== Running: mpiexec -n ${MPI_NP} ${exe} ====="
echo "Time: $(date)"

mpiexec -n "${MPI_NP}" "${exe}"

echo "End time: $(date)"
echo "=== Job ${job_id} finished ==="

