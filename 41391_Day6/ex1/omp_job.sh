#!/bin/sh 
### General options 

# -- specify queue -- 
#BSUB -q hpcintro

# -- Set number of parameters to sweep (1–9 here) and job name -- 
#BSUB -J bmp_hpc[1-225] 

# -- Number of cores (1 per job) -- 
#BSUB -n 1 

# -- Ensure cores are on the same host -- 
#BSUB -R "span[hosts=1]"

# -- Memory requirements (2–4 GB) -- 
#BSUB -R "rusage[mem=8GB]"
#BSUB -M 8GB

# -- Walltime limit (hh:mm) -- 
#BSUB -W 48:00 

# -- Email notifications (optional) --
###BSUB -u bmapa@dtu.dk
###BSUB -B  # at job start
###BSUB -N  # at job end

# -- Output and error files -- 
#BSUB -oo results/Output_%J_%I.out 
#BSUB -eo results/Error_%J_%I.err 

echo "=== Job $LSB_JOBINDEX started ==="
echo "Working directory: $(pwd)"
echo "Hostname: $(hostname)"
echo "Start time: $(date)"

# ----------------------------------------
# Define parameter values (same length!)
# ----------------------------------------

caseNum_list=""

zi="0.25 0.75 1"
meshes="200 300 400"

caseIdx=0
for dvVal1 in $zi; do
	for pn in $pns; do
		for ns in $nsuts; do
			for cd in $meshes; do
				caseIdx=$((caseIdx+1))
				caseNum_list="$caseNum_list $caseIdx"
			done
		done
	done
done

# Split strings into arrays
set -- $caseNum_list
caseNum_array=("$@")

# Convert LSF job index to 0-based
INDEX=$(($LSB_JOBINDEX - 1))

# Export parameters as environment variables for MATLAB
export caseNum=${caseNum_array[$INDEX]}

echo "Selected index: $INDEX"
echo "Case Number	= $caseNum"

# ----------------------------------------
# Run compiled program
# ----------------------------------------
./main_omp/

echo "End time: $(date)"
echo "=== Job $LSB_JOBINDEX finished ==="

