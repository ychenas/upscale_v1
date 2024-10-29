#!/bin/sh

#init git 
git init

#add files 
git add *

#commit files 
git commit -m " first commit!"

# push existing repository
# set git hub path on cloud
git remote add origin git@github.com:ychenas/upscale_v1.git

#add branch name, here is main 
git branch -M main


#push commit files to the server/origin as master or branch 
git push -u origin main


