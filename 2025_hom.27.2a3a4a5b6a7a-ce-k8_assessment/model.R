## Extract results of interest, write TAF output tables

## Before: folders with the assessment run and the retros
## After:folders with the corresponding outputs 

# Working directory and folders -------------------------------------------

# check working directory
library(icesTAF)

getwd()

# create output folder and sub-folders using the function mkdir from icesTAF
mkdir("model/run")
mkdir("model/stf")

source("model_01_run.R")
source("model_02_stf.R")
