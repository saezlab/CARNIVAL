#!/usr/bin/env zsh

#BSUB -J CARNIVAL
#BSUB -W 100:00
#BSUB -M 100000
#BSUB -B -N -u your-email-name@domain.com

### Change to the work directory
cd /home/your-personalised-account/CARNIVAL_Rpackage

for i in {1..3}
do
for cond in {'0.001','0.01','0.1','1','2'}
do
bsub -P jrc_combine -J CARNIVAL -W 100:00 -M 100000 -o $output_dir/output_${i}.txt -e $output_dir/error_${i}.txt command Rscript CARNIVAL_Driver_Parallel_Example.R $i $cond
sleep 10s
done
done
