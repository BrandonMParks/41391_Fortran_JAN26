#!/usr/bin/env bash

set -euo pipefail
### General options 

# -- specify queue -- 
#BSUB -q hpcintro

# -- Job name (single job that runs a sequential sweep) --
#BSUB -J bmp_hpc_sweep

# -- Reserve a full node (24 cores on this machine) --
#BSUB -n 24

# -- Ensure cores are on the same host -- 
#BSUB -R "span[hosts=1]"

# -- Memory requirements (2–4 GB) -- 
#BSUB -R "rusage[mem=8GB]"
#BSUB -M 8GB

# -- Walltime limit (hh:mm) -- 
#BSUB -W 00:60 

# -- Email notifications (optional) --
###BSUB -u bmapa@dtu.dk
###BSUB -B  # at job start
###BSUB -N  # at job end

# -- Output and error files --
#BSUB -oo output/Output_%J.out
#BSUB -eo output/Error_%J.err

job_id=${LSB_JOBID:-unknown}
job_index=${LSB_JOBINDEX:-0}

echo "=== Job ${job_id} started (index=${job_index}) ==="
echo "Working directory: $(pwd)"
echo "Hostname: $(hostname)"
echo "Start time: $(date)"

nodeCores=${LSB_DJOB_NUMPROC:-24}
echo "Cores allocated by LSF (-n) = $nodeCores"

# ----------------------------------------
# OpenMP settings for reproducible performance
# ----------------------------------------

# Always print OpenMP runtime configuration
export OMP_DISPLAY_ENV=verbose

# Default OpenMP thread count (overridden inside the sweep loop below)
export OMP_NUM_THREADS="${OMP_NUM_THREADS:-1}"

# Disable dynamic teams/threads for stability
export OMP_DYNAMIC=false

# Pin threads to physical cores (important on 2-socket NUMA nodes)
export OMP_PLACES=cores
export OMP_PROC_BIND=close

# ----------------------------------------
# Thread sweep: run the same executable multiple times
# ----------------------------------------

# Thread counts to benchmark.
# Default: sweep from 1 up to the number of allocated cores.
# Override on submission if you want something else, e.g.:
#   bsub ... -env "THREADS_LIST=1 2 4 8 12 16 24" < omp_job.sh
THREADS_LIST="${THREADS_LIST:-$(seq 1 "$nodeCores")}"

# ----------------------------------------
# Run compiled program
# ----------------------------------------
for t in $THREADS_LIST; do
  if (( t > nodeCores )); then
    echo "Skipping t=$t (exceeds allocated cores=$nodeCores)"
    continue
  fi

  export OMP_NUM_THREADS="$t"

  echo ""
  echo "===== Running sweep case: threads=$t ====="
  echo "Time: $(date)"

  ./bin/poisson3d -t "$t"
done

mapfile -t HOSTS < <(sort -u "$LSB_DJOB_HOSTFILE")

for h in "${HOSTS[@]}"; do
  echo "----- HW report for $h -----"
  lsrun -m "$h" bash -lc '
    echo "Host: $(hostname -s)"
    echo "Time: $(date)"
    echo "-- CPU --"; lscpu
    echo "-- Memory --"; free -h
    echo "-- NUMA --"; numactl -H || true
    echo "-- Block devices --"; lsblk -o NAME,SIZE,MODEL,TYPE,MOUNTPOINT
    echo "-- GPU --"; command -v nvidia-smi >/dev/null && nvidia-smi -L || echo "No nvidia-smi"
    echo "-- Kernel --"; uname -a
  '
  echo
done

echo "End time: $(date)"
echo "=== Job ${job_id} finished (index=${job_index}) ==="

