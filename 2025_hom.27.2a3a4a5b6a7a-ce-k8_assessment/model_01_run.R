## Run analysis, write model results

## Before: empty
## After: folder with the assessment run and the retros



# Authors: Rosana Ourens (adapted from Laura Wise for pil.27.8c9a) 

# Date: 2025

# Load libraries ----------------------------------------------------------
library(r4ss)
library(icesTAF)

# Working directory and folders -------------------------------------------

# check working directory

getwd()


# Run script for stock assessment -----------------------------------------

#*************************************************
#*      RUN THE MODEL in SS3
#*************************************************

#directories 
wd<-getwd()
run_dir <- paste0(wd,"/model/run")
SS3_exe_path <- paste0(wd,"/boot/software/ss3.exe")


# Get input files --------------------------------------------------------

cp("boot/data/data.ss", "model/run")
cp("boot/data/control.ss", "model/run")
cp("boot/data/forecast.ss", "model/run")
cp("boot/data/starter.ss", "model/run")
cp("boot/data/wtatage.ss", "model/run")


# Run SS from r 

r4ss::run(dir = run_dir, exe = SS3_exe_path,show_in_console = T,skipfinished=F)


#*************************************************
#*      RUN THE MODEL FOR THE RETROS
#*************************************************

retro(run_dir, years = +0:-5, exe=SS3_exe_path )#Run 


# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

rm(list=ls())


