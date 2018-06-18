#!/usr/bin/env zsh

#BSUB -J CARNIVAL_job
#BSUB -W 100:00
#BSUB -M 100000
#BSUB -B -N -u panuwat.trairatphisan@gmail.com

### Change to the work directory
cd /home/pt178902/InvCARNIVAL

for i in {1..3}
do
bsub -P jrc_combine -J CARNIVAL_job -W 100:00 -M 100000 -o $output_dir/output_${i}.txt -e $output_dir/error_${i}.txt command Rscript InverseCR_Driver_Parallel.R $i
sleep 10s
done
