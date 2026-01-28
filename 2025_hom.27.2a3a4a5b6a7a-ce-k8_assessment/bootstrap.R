# Script information ------------------------------------------------------

# Title: Stock assessment and short term forecast for western horse mackerel (WGWIDE)
#        1) TAF bootstrap procedure to set up required data and software

# See also: https://github.com/ices-taf/doc/wiki/Creating-a-TAF-analysis 

# Authors: Rosana Ourens (rosana.ourens@cefas.gov.uk) 

# Date: 2025

# Load libraries ----------------------------------------------------------

library(icesTAF)

# Check the working directory ---------------------------------------------

getwd()

# Create repository and R project -----------------------------------------

# Clone the github repository created by ICES secretariat
# The cloned repository is in D:/GitHUB/ices-taf/2023_pil.27.8c9a_assessment
# Create an R project in that folder

# Make the skeleton -------------------------------------------------------

# create initial directories and R scripts for a new TAF analysis
# 2025_hom.27.2a4a5b6a7a-ce-k8_assessment    
# ¦--bootstrap   
# ¦   --initial 
# ¦       --data
# ¦--data.R      
# ¦--model.R     
# ¦--output.R    
# °--report.R    

taf.skeleton()

# Upload initial data -----------------------------------------------------

# Include data in the folder: 2025_hom.27.2a4a5b6a7a-ce-k8_assessment\bootstrap\initial\data 
# Include all five input files needed to run Stock Synthesis
# It can be done by-hand or copying it using the following command:

# # Starter file
# file.copy(from="C:/Users/RO03/Documents/WHM/WGWIDE/WGWIDE_2025/assessment_2025/Run1/starter.ss",
#           to="./boot/initial/data/starter.ss", overwrite=T)
# 
# # Control file
# file.copy(from="C:/Users/RO03/Documents/WHM/WGWIDE/WGWIDE_2025/assessment_2025/Run1/control.ss",
#           to="./boot/initial/data/control.ss", overwrite=T)
# 
# # Data file
# file.copy(from="C:/Users/RO03/Documents/WHM/WGWIDE/WGWIDE_2025/assessment_2025/Run1/data.ss",
#           to="./boot/initial/data/data.ss", overwrite=T)
# 
# # Weight at age file
# file.copy(from="C:/Users/RO03/Documents/WHM/WGWIDE/WGWIDE_2025/assessment_2025/Run1/wtatage.ss",
#           to="./boot/initial/data/wtatage.ss", overwrite=T)
# 
# # Forecast file
# file.copy(from="C:/Users/RO03/Documents/WHM/WGWIDE/WGWIDE_2025/assessment_2025/Run1/forecast.ss",to="./boot/initial/data/forecast.ss", overwrite=T)

# Document data and create the data.bib file ------------------------------

# use function draft.data() to create the data.bib file
# ?draft.data

# NOTE that the columns of the csv files should be separated by comma (",")

# data for stock assessment (5 different file)

draft.data(originator="WGWIDE", 
           year=2025, 
           title="Forecast input file", 
           period="1982-2024",
           source="file",
           data.files=c("forecast.ss"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)


draft.data(originator="WGWIDE", 
           year=2025, 
           title="Starter input file", 
           period="1982-2024",
           source="file",
           data.files=c("starter.ss"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

draft.data(originator="WGWIDE", 
           year=2025, 
           title="Control input file", 
           period="1982-2024",
           source="file",
           data.files=c("control.ss"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

draft.data(originator="WGWIDE", 
           year=2025, 
           title="Data input file", 
           period="1982-2024",
           source="file",
           data.files=c("data.ss"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)

draft.data(originator="WGWIDE", 
           year=2025, 
           title="Weight at age input file", 
           period="1982-2024",
           source="file",
           data.files=c("wtatage.ss"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)


draft.data(originator="WGWIDE", 
           year=2025, 
           title="input data excel", 
           period="1982-2024",
           source="file",
           data.files=c("Input_SS_model2025.xlsx"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)


draft.data(originator="WGWIDE", 
           year=2025, 
           title="Reference points", 
           period="2025",
           source="file",
           data.files=c("RefPoints.xlsx"), 
           data.scripts=NULL, # we need to set this to NULL or it will automatically take all the files with extension .R
           file="boot/data.bib",
           append=T)
# Upload software ---------------------------------------------------------

# create the folder "2025_hom.27.2a4a5b6a7a-ce-k8_assessment\boot\initial\software"

if (!dir.exists("boot/initial/software")){
  dir.create("boot/initial/software")
}

# # copy the SS3 executable file to the folder "2025_hom.27.2a4a5b6a7a-ce-k8_assessment\boot\initial\software"
# 
# file.copy(from="C:/Users/RO03/Documents/WHM/WGWIDE/WGWIDE_2025/assessment_2025/ss3.exe",
#           to="./boot/initial/software/ss3.exe",
#           overwrite=T) # SS3 executable for windows


# Document software and create the software.bib file ----------------------

# use function draft.software to create the software.bib file

# ?draft.software

# document the SS3 model

draft.software(package=c("boot/initial/software/ss3.exe"), 
               author = "Richard D. Methot Jr., Chantel R. Wetzel, Ian G. Taylor, Kathryn L. Doering,
Elizabeth F. Gugliotti, and Kelli F. Johnson", 
               year = 2025, 
               title = "SS3 executable (for Windows)",
               version = "3.30.23.2",
               source = NULL, 
               file = "boot/software.bib", 
               append = FALSE)


# Process the data.bib and software.bib metafiles -------------------------

# the function taf.bootstrap() processes:
#   the data.bib file and creates/copies all the data files into the folder "bootstrap/data"
#   and 
#   the software.bib file and creates/copies the software into the folder bootstrap/software
# ?taf.bootstrap

# apply taf.bootstrap

taf.bootstrap(taf=TRUE)

# Session info ------------------------------------------------------------

sessionInfo()

# End of script -----------------------------------------------------------

