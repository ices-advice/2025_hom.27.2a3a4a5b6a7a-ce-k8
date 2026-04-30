# Script information ------------------------------------------------------


# Date: 2025

# Load libraries --------------------------------------------------------------
rm(list=ls())

library(TAF)
library(r4ss)
library(foreach)
library(tidyverse)

### Directories
run.dir <- paste0(getwd(),"/model/run")
mkdir("model/stf/Scenarios")
stf.dir <- paste0(getwd(),"/model/stf")
scenarios.dir <- paste0(getwd(),"/model/stf/Scenarios")
SS3_exe_path <- paste0(getwd(),"/boot/software/ss3.exe")



#*****************************
#Reference Points
#************************
MSYBtrigger = 787443
Fmsy = 0.080
Blim=566678
Bpa= MSYBtrigger
Flim=0.22
Fp05=0.080
Fpa=Fp05 

#****************************
#*Forecast
#****************************

#number of years (-1) over which to average weights, selectivity
Naver = 2  # most recent 3-years

#Create scenarios
file.copy(file.path(run.dir, "forecast.ss"),file.path(stf.dir, "forecast.ss"),overwrite = TRUE)

#read in the forecast file
fore <- r4ss::SS_readforecast(file = file.path(stf.dir, "forecast.ss"),verbose = FALSE)

#read in assessment ouput
replist <- r4ss::SS_output(dir = run.dir, verbose=TRUE, printstats=TRUE) 

#exploitation
dat <- replist$exploitation
#### mean years
Nfor <- fore$Nforecastyrs
startyear <- max(dat$Yr) - Nfor - Naver
endyear <- max(dat$Yr) - Nfor
year_inter <- endyear+1


### intermediate year assumption
fore_dat_int <- fore$ForeCatch[1,]

##### ----- SET GEOMEAN Recruitment (use or not depends on uncertainty)
# Get assessment outputs for geomean recruitment
ass.sum <- SSsummarize(SSgetoutput(dirvec=run.dir))
hist.rec <- as.data.frame(ass.sum$recruits) %>% filter(Yr %in% 2003:(year_inter-1)) %>% .[,1]  # 
virg.rec <- as.data.frame(ass.sum$recruits) %>% filter(Label == "Recr_Virgin") %>% .[,1]
gmrec <- exp(mean(log(hist.rec)))#Geometric mean since 2003
fore_rec_int<-as.data.frame(ass.sum$recruits) %>% filter(Yr==year_inter)#forecast value using S-R 



#***********************************
#*Zero catch scenario
#************************************
#### Run the zero catch scenario to obtain values for the intermediate year
##### -----

aux=fore_dat_int # F last year

### CREATE forecast.ss with interim year F
for (i in 1){
  fore_dat=fore_dat_int;aux_fore=fore_dat_int
  for(j in 2:(Nfor-1)){
    aux_fore$year=endyear+j
    aux_fore$catch_or_F=0
    aux_fore$basis <- 99
    fore_dat=rbind(fore_dat,aux_fore)
  }
  j=Nfor
  aux_fore$year=endyear+j
  aux_fore$catch_or_F=0
  aux_fore$basis <- 99
  fore_dat=rbind(fore_dat,aux_fore)
  
  # input ------------------------------------------------------------------------
  fore$InputBasis<- -1 # 99 for F, 2 for Catch, -1 for mix of catch and F
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  fore$fcast_rec_option <- 2 #= value*(virgin recruitment) # Select this option to use the geomean, 0 to use the BH model
  fore$fcast_rec_val <- gmrec/virg.rec # geomean / virgin rec # 
  
  ## write all forecast files/scenarios
  
  r4ss::SS_writeforecast(fore, dir = scenarios.dir, file = paste0("forecast",0, ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}


# create forecast folder and subfolders
for (i in 1){
  
  dir.FMult <- file.path(scenarios.dir,paste0("FMult",0))
  dir.create(path = dir.FMult, showWarnings = F, recursive = T)
  
  
  file.copy(file.path(run.dir, "starter.ss"), file.path(dir.FMult, "starter.ss"))
  file.copy(paste(run.dir, "control.ss", sep="/"), paste(dir.FMult, "control.ss", sep="/"))
  file.copy(paste(run.dir, "data.ss", sep="/"), paste(dir.FMult, "data.ss", sep="/"))	
  file.copy(paste(run.dir, "wtatage.ss", sep="/"), paste(dir.FMult, "wtatage.ss", sep="/"))
  
  #copy the forecast file
  file.copy(paste(scenarios.dir,paste0("forecast",0,".ss") , sep="/"),
            paste(dir.FMult, "forecast.ss", sep="/"),overwrite = TRUE)
  
  
}

# -- run the forecast models
all.models <- c(paste0("FMult", 0))

for (m in all.models) {r4ss::run(dir = file.path(stf.dir,"Scenarios",m), exe = SS3_exe_path, show_in_console = TRUE, skipfinished = FALSE)}


#Scenarios to be run:
#F0,fmsy,fmsyreduced,fpa,flim,fsq,ssb=blimn, ssb=btrigger


#********************************
#*Run the scenarios based on F
#*******************************


### Find the ratio between fbar and Fapic needed to estimate the F catch scenarios
dat$Fapic <- apply(as.matrix(dat[,-c(1:6)]),1,sum,na.rm=TRUE)

Fapic <- aggregate(Fapic~Yr,dat,mean)
Fbar <- aggregate(annual_F~Yr,dat,mean)

endyr = endyear
Fratio = Fapic$Fapic[Fapic$Yr%in%endyear]/Fbar$annual_F[Fbar$Yr%in%endyear]
Fratio

#read in F0 forecast ouput
replistF0 <- r4ss::SS_output(dir = file.path(stf.dir,"Scenarios",m), verbose=TRUE, printstats=TRUE) 


FMult <- c(0, Fmsy*Fratio, Fmsy*Fratio*ifelse(replistF0$derived_quants$Value[replistF0$derived_quants$Label %in% "SSB_2026"]/MSYBtrigger >1, 1, replistF0$derived_quants$Value[replistF0$derived_quants$Label %in% "SSB_2026"]/MSYBtrigger),Fpa*Fratio,Flim*Fratio,replistF0$derived_quants$Value[replistF0$derived_quants$Label %in% "F_2025"]*Fratio)



FMult_names <- c("F0","Fmsy","Fmsyreduced","Fpa","Flim","Fsq")
l_FMult <- length(FMult)



### CREATE forecast.ss with interim year F
for (i in 1:l_FMult){
  fore_dat=fore_dat_int;aux_fore=fore_dat_int
  for(j in 2:(Nfor-1)){
    aux_fore$year=endyear+j
    aux_fore$catch_or_F=FMult[i]
    aux_fore$basis <- 99
    fore_dat=rbind(fore_dat,aux_fore)
  }
  j=Nfor
  aux_fore$year=endyear+j
  aux_fore$catch_or_F=FMult[i]
  aux_fore$basis <- 99
  fore_dat=rbind(fore_dat,aux_fore)
  
  # input ------------------------------------------------------------------------
  fore$InputBasis<- -1 # 99 for F, 2 for Catch, -1 for mix of catch and F
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  fore$fcast_rec_option <- 2 #= value*(virgin recruitment) 
  fore$fcast_rec_val <- gmrec/virg.rec # geomean / virgin rec 
  
  ## write all forecast files/scenarios
  
  r4ss::SS_writeforecast(fore, dir = scenarios.dir, file = paste0("forecast",FMult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}


# create forecast folder and subfolders
for (i in 1:l_FMult){
  
  dir.FMult <- file.path(scenarios.dir,paste0(FMult_names[i]))
  dir.create(path = dir.FMult, showWarnings = F, recursive = T)
  
  
  file.copy(file.path(run.dir, "starter.ss"), file.path(dir.FMult, "starter.ss"))
  file.copy(paste(run.dir, "control.ss", sep="/"), paste(dir.FMult, "control.ss", sep="/"))
  file.copy(paste(run.dir, "data.ss", sep="/"), paste(dir.FMult, "data.ss", sep="/"))	
  file.copy(paste(run.dir, "wtatage.ss", sep="/"), paste(dir.FMult, "wtatage.ss", sep="/"))
  
  #copy the forecast file
  file.copy(paste(scenarios.dir,paste0("forecast",FMult_names[i],".ss") , sep="/"),
            paste(dir.FMult, "forecast.ss", sep="/"),overwrite = TRUE)
  
  
}

# -- run the forecast models
all.models <- c(paste0(FMult_names))

for (m in all.models) {r4ss::run(dir = file.path(stf.dir,"Scenarios",m), exe = SS3_exe_path, show_in_console = TRUE, skipfinished = FALSE)}




#**************************************************
##################### IMprovement of F scenarios
#********************************************************
## the F values are not completely achieved. I found better multipliers:
FMult <- c(seq(1.04,1.05,by=0.001)*Fmsy*Fratio)
#Fmsy6 hits FMSY


FMult_names <- c(paste0("Fmsy_",1:10))
l_FMult <- length(FMult)



### CREATE forecast.ss with interim year F
for (i in 1:l_FMult){
  fore_dat=fore_dat_int;aux_fore=fore_dat_int
  for(j in 2:(Nfor-1)){
    aux_fore$year=endyear+j
    aux_fore$catch_or_F=FMult[i]
    aux_fore$basis <- 99
    fore_dat=rbind(fore_dat,aux_fore)
  }
  j=Nfor
  aux_fore$year=endyear+j
  aux_fore$catch_or_F=FMult[i]
  aux_fore$basis <- 99
  fore_dat=rbind(fore_dat,aux_fore)
  
  # input ------------------------------------------------------------------------
  fore$InputBasis<- -1 # 99 for F, 2 for Catch, -1 for mix of catch and F
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  fore$fcast_rec_option <- 2 #
  fore$fcast_rec_val <- gmrec/virg.rec 
  
  ## write all forecast files/scenarios
  
  r4ss::SS_writeforecast(fore, dir = scenarios.dir, file = paste0("forecast",FMult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}


# create forecast folder and subfolders
for (i in 1:l_FMult){
  
  dir.FMult <- file.path(scenarios.dir,paste0(FMult_names[i]))
  dir.create(path = dir.FMult, showWarnings = F, recursive = T)
  
  
  file.copy(file.path(run.dir, "starter.ss"), file.path(dir.FMult, "starter.ss"))
  file.copy(paste(run.dir, "control.ss", sep="/"), paste(dir.FMult, "control.ss", sep="/"))
  file.copy(paste(run.dir, "data.ss", sep="/"), paste(dir.FMult, "data.ss", sep="/"))	
  file.copy(paste(run.dir, "wtatage.ss", sep="/"), paste(dir.FMult, "wtatage.ss", sep="/"))
  
  #copy the forecast file
  file.copy(paste(scenarios.dir,paste0("forecast",FMult_names[i],".ss") , sep="/"),
            paste(dir.FMult, "forecast.ss", sep="/"),overwrite = TRUE)
  
  
}

# -- run the forecast models
all.models <- c(paste0(FMult_names))

for (m in all.models) {r4ss::run(dir = file.path(stf.dir,"Scenarios",m), exe = SS3_exe_path, show_in_console = TRUE, skipfinished = FALSE)}


### Fsq scenario

FMult <- c(seq(1.0,1.9,by=0.01)*replistF0$derived_quants$Value[replistF0$derived_quants$Label %in% "F_2025"]*Fratio)
#Fsq5 hits Fsq


FMult_names <-  c(paste0("Fsq_",1:10))
l_FMult <- length(FMult)



### CREATE forecast.ss with interim year F
for (i in 1:l_FMult){
  fore_dat=fore_dat_int;aux_fore=fore_dat_int
  for(j in 2:(Nfor-1)){
    aux_fore$year=endyear+j
    aux_fore$catch_or_F=FMult[i]
    aux_fore$basis <- 99
    fore_dat=rbind(fore_dat,aux_fore)
  }
  j=Nfor
  aux_fore$year=endyear+j
  aux_fore$catch_or_F=FMult[i]
  aux_fore$basis <- 99
  fore_dat=rbind(fore_dat,aux_fore)
  
  # input ------------------------------------------------------------------------
  fore$InputBasis<- -1 # 99 for F, 2 for Catch, -1 for mix of catch and F
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  fore$fcast_rec_option <- 2 #
  fore$fcast_rec_val <- gmrec/virg.rec 
  
  ## write all forecast files/scenarios
  
  r4ss::SS_writeforecast(fore, dir = scenarios.dir, file = paste0("forecast",FMult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}


# create forecast folder and subfolders
for (i in 1:l_FMult){
  
  dir.FMult <- file.path(scenarios.dir,paste0(FMult_names[i]))
  dir.create(path = dir.FMult, showWarnings = F, recursive = T)
  
  
  file.copy(file.path(run.dir, "starter.ss"), file.path(dir.FMult, "starter.ss"))
  file.copy(paste(run.dir, "control.ss", sep="/"), paste(dir.FMult, "control.ss", sep="/"))
  file.copy(paste(run.dir, "data.ss", sep="/"), paste(dir.FMult, "data.ss", sep="/"))	
  file.copy(paste(run.dir, "wtatage.ss", sep="/"), paste(dir.FMult, "wtatage.ss", sep="/"))
  
  #copy the forecast file
  file.copy(paste(scenarios.dir,paste0("forecast",FMult_names[i],".ss") , sep="/"),
            paste(dir.FMult, "forecast.ss", sep="/"),overwrite = TRUE)
  
  
}

# -- run the forecast models
all.models <- c(paste0(FMult_names))

for (m in all.models) {r4ss::run(dir = file.path(stf.dir,"Scenarios",m), exe = SS3_exe_path, show_in_console = TRUE, skipfinished = FALSE)}

#**************************
#*SSB SCenarios
#**************************
##### SSB = Btrigger
#

## since fishing at Flim is just below MSY Btrigger we need to fish a bit less
FMult <- c(seq(0.728302,0.728311,by=0.000001)*Flim*Fratio)
#Brigger_5 hits Btrigger

FMult_names <- c(paste0("Btrigger",1:10))
l_FMult <- length(FMult)



### CREATE forecast.ss with interim year F
for (i in 1:l_FMult){
  fore_dat=fore_dat_int;aux_fore=fore_dat_int
  for(j in 2:(Nfor-1)){
    aux_fore$year=endyear+j
    aux_fore$catch_or_F=FMult[i]
    aux_fore$basis <- 99
    fore_dat=rbind(fore_dat,aux_fore)
  }
  j=Nfor
  aux_fore$year=endyear+j
  aux_fore$catch_or_F=FMult[i]
  aux_fore$basis <- 99
  fore_dat=rbind(fore_dat,aux_fore)
  
  # input ------------------------------------------------------------------------
  fore$InputBasis<- -1 # 99 for F, 2 for Catch, -1 for mix of catch and F
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  fore$fcast_rec_option <- 2 #
  fore$fcast_rec_val <- gmrec/virg.rec 
  
  ## write all forecast files/scenarios
  
  r4ss::SS_writeforecast(fore, dir = scenarios.dir, file = paste0("forecast",FMult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}


# create forecast folder and subfolders
for (i in 1:l_FMult){
  
  dir.FMult <- file.path(scenarios.dir,paste0(FMult_names[i]))
  dir.create(path = dir.FMult, showWarnings = F, recursive = T)
  
  
  file.copy(file.path(run.dir, "starter.ss"), file.path(dir.FMult, "starter.ss"))
  file.copy(paste(run.dir, "control.ss", sep="/"), paste(dir.FMult, "control.ss", sep="/"))
  file.copy(paste(run.dir, "data.ss", sep="/"), paste(dir.FMult, "data.ss", sep="/"))	
  file.copy(paste(run.dir, "wtatage.ss", sep="/"), paste(dir.FMult, "wtatage.ss", sep="/"))
  
  #copy the forecast file
  file.copy(paste(scenarios.dir,paste0("forecast",FMult_names[i],".ss") , sep="/"),
            paste(dir.FMult, "forecast.ss", sep="/"),overwrite = TRUE)
  
  
}

# -- run the forecast models
all.models <- c(paste0(FMult_names))

for (m in all.models) {r4ss::run(dir = file.path(stf.dir,"Scenarios",m), exe = SS3_exe_path, show_in_console = TRUE, skipfinished = FALSE)}



#####SSB
## since fishing at Flim is just below MSY Btrigger we need to fish much higher

FMult <- c(seq(2.6142,2.6143,by=0.00001)*Flim*Fratio)
#Blim 10 achieves Blim


FMult_names <- c(paste0("Blim",1:10))
l_FMult <- length(FMult)



### CREATE forecast.ss with interim year F
for (i in 1:l_FMult){
  fore_dat=fore_dat_int;aux_fore=fore_dat_int
  for(j in 2:(Nfor-1)){
    aux_fore$year=endyear+j
    aux_fore$catch_or_F=FMult[i]
    aux_fore$basis <- 99
    fore_dat=rbind(fore_dat,aux_fore)
  }
  j=Nfor
  aux_fore$year=endyear+j
  aux_fore$catch_or_F=FMult[i]
  aux_fore$basis <- 99
  fore_dat=rbind(fore_dat,aux_fore)
  
  # input ------------------------------------------------------------------------
  fore$InputBasis<- -1 # 99 for F, 2 for Catch, -1 for mix of catch and F
  fore$ForeCatch<-fore_dat # input ForeCatch(orF) data
  fore$fcast_rec_option <- 2 
  fore$fcast_rec_val <- gmrec/virg.rec # geomean / virgin rec 
  
  ## write all forecast files/scenarios
  
  r4ss::SS_writeforecast(fore, dir = scenarios.dir, file = paste0("forecast",FMult_names[i], ".ss"), 
                         overwrite = TRUE, verbose = FALSE)
}


# create forecast folder and subfolders
for (i in 1:l_FMult){
  
  dir.FMult <- file.path(scenarios.dir,paste0(FMult_names[i]))
  dir.create(path = dir.FMult, showWarnings = F, recursive = T)
  
  
  file.copy(file.path(run.dir, "starter.ss"), file.path(dir.FMult, "starter.ss"))
  file.copy(paste(run.dir, "control.ss", sep="/"), paste(dir.FMult, "control.ss", sep="/"))
  file.copy(paste(run.dir, "data.ss", sep="/"), paste(dir.FMult, "data.ss", sep="/"))	
  file.copy(paste(run.dir, "wtatage.ss", sep="/"), paste(dir.FMult, "wtatage.ss", sep="/"))
  
  #copy the forecast file
  file.copy(paste(scenarios.dir,paste0("forecast",FMult_names[i],".ss") , sep="/"),
            paste(dir.FMult, "forecast.ss", sep="/"),overwrite = TRUE)
  
  
}

# -- run the forecast models
all.models <- c(paste0(FMult_names))

for (m in all.models) {r4ss::run(dir = file.path(stf.dir,"Scenarios",m), exe = SS3_exe_path, show_in_console = TRUE, skipfinished = FALSE)}



