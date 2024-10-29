#!/bin/bash

echo "running batch scripts to creat job files from template.job"
cd /lfs/home/ychen/scripts/R/Rscripts/SPOT_CLASS/fishnet/
# LOAD R MODULE 
#module load r/3.1.1

# submit the job on queue 
# chmod u+x $CONFIG_FILE
# qsub -jeo -q short $CONFIG_FILE
CONT=0

# argunment 1 to 6 are parameters for the function SPOT_step1.R
 
# arg1 : xmin min of XID of GRID BOX within the AOI(Area Of Interest)
# arg2 : xmax max of XID of GRID BOX within the AOI
# arg3 : ymin min of YID of GRID BOX within the AOI 
# arg4 : ymax max of YID of GRID BOX within the AOI
# arg5 : wrk_yr selected working year
# arg6 : aoi_region selected region of AOI (which was define in the function)
# arg7 : tunning paramter for the decision tree 
  

ref_buf=250
img_path="/work/vivianlin0921/PCA/classificationresults/taiwan/2021/all/2021"


for irun in {1..260}
do
#
#set image index as irun 
img_index="${irun}"  

#counter for the iteration or loop 
CONT=$(($CONT+1))
echo "The No. of for loop: $CONT ."

#copy the template from template.job (script for submitting R job with some argunments)
TEMPLATE_FILE="template.submit"
CONFIG_FILE="R_upscale_img_index_${img_index}.job"
cp $TEMPLATE_FILE $CONFIG_FILE

#dynamic text for the argunments, input and output files and directory
log_file="R_upscale_img_index_${img_index}.log"
#
TARGET_KEY="work_dir"
REPLACEMENT_VALUE="\/lfs\/home\/ychen\/scripts\/R\/Rscripts\/SPOT_CLASS\/fishnet\/"
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="R_filename"
REPLACEMENT_VALUE="upscale_mesh.R"
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

# RE-SET THE PARAMETER VALUES FOR EACH JOB
TARGET_KEY="img_index"
REPLACEMENT_VALUE=${img_index}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

## RE-SET THE PARAMETER VALUES FOR EACH JOB
#TARGET_KEY="img_path"
#REPLACEMENT_VALUE=${img_path}
#sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="ref_buf"
REPLACEMENT_VALUE=${ref_buf}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

TARGET_KEY="log_file"
REPLACEMENT_VALUE=${log_file}
sed -i -c "s/$TARGET_KEY/$REPLACEMENT_VALUE/" $CONFIG_FILE

echo " arg1:$img_index, arg:$ref_buf "  
# submit the job on queuing system  
chmod u+x $CONFIG_FILE

#run on curie by tesing Queue
#ccc_msub -q standard -T 1800 -A gen6328 -Q TEST $CONFIG_FILE 

#normal run on CURIE
#ccc_msub -q standard -T 86400 -A gen6328 $CONFIG_FILE 
#run on airain
#ccc_msub -q ivybridge -T 86400 -A dsm $CONFIG_FILE 
# excute the job 
./$CONFIG_FILE &
echo $CONFIG_FILE

done


