## Extract results of interest, write TAF output tables

## Before: folders with the assessment run and the retros
## After:folders with the corresponding outputs 

# Working directory and folders -------------------------------------------

# check working directory

getwd()

# create output folder and sub-folders using the function mkdir from icesTAF

mkdir("output")
mkdir("output/plots")
mkdir("output/plots/SS3_run")
mkdir("output/plots/diagnosis/retro")
mkdir("output/tables")


source("output_01_run.R")
source("output_02_diagnosis.R")
source("output_03_stf.R")
