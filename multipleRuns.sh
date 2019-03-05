#!/bin/bash

#$ -cwd
#$ -P onr
#$ -l h_rt=120:00:00   # Specify the hard time limit for the job
#$ -N chartr-multipleTasks           # Give job a name
#$ -o logs/chartr-RS2002-$TASK_ID.stdout
#$ -e logs/chartr-RS2002-$TASK_ID.stderr
#$ -pe omp 28 
	
module load R/3.5.2
export R_LIBS=/usr2/faculty/cchandr1/R/x86_64-pc-linux-gnu-library/3.5

echo "=========================================================="
echo "Chand: CHARTr jobs"
echo "Starting on:$(date)"
echo "Running on node:$(hostname)"
echo "Current Directory: $(pwd)"
echo "Current Job Id: $JOB_ID"
echo "Current Job name: $JOB_NAME"
echo "Current Task Id: $SGE_TASK_ID"
echo "==========================================================="

R CMD BATCH chartr-simultaneousTasks.r logs/R-${JOB_ID}-${SGE_TASK_ID}.Rout


echo "=========================================================="
echo "Finished on: $(date)"
echo "=========================================================="


