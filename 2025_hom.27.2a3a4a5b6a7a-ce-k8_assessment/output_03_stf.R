
library(icesTAF)
library(r4ss)
library(dplyr)
library(tidyverse)
#*****************************
#Reference Points
#************************
MSYBtrigger = 787443
Fmsy = 0.08
Blim=566678
Bpa= MSYBtrigger
Flim=0.22
Fp05=0.08
Fpa=Fp05 

################## Directories

stf.dir <- paste0(getwd(),"/model/stf")
scenarios.dir <- paste0(getwd(),"/model/stf/Scenarios")
SS3_exe_path <- paste0(getwd(),"/boot/software/ss3.exe")
mkdir("output/stf")
stf_out<-paste0(getwd(),"/output/stf")


################# Get the outputs from the stf
#retrieve and summarise outputs

################# Get the outputs
#retrieve and summarise outputs

FMult_names <- c("Fmsy_6","Fmsy_6","F0","Fsq_5","Btrigger5","Blim10")


forecastModels <- r4ss::SSgetoutput(dirvec = file.path(stf.dir,"Scenarios", c(FMult_names)), getcovar = FALSE)
save(forecastModels,file=file.path(scenarios.dir, "forecast.RData"))
forecastSummary <- SSsummarize(forecastModels)

#SSB, F, Recr
SSB <- as.data.frame(forecastSummary$SpawnBio)
SSB.SD <- as.data.frame(forecastSummary$SpawnBioSD)   #SD
SSB.Lower <- as.data.frame(forecastSummary$SpawnBioLower)   #SSB + 1.96SD
SSB.Upper <- as.data.frame(forecastSummary$SpawnBioUpper)   #SSB - 1.96SD
Fvalue <- as.data.frame(forecastSummary$Fvalue)
Recr <- as.data.frame(forecastSummary$recruits)

#summarise results
all.scen <-FMult_names
num.scen <- length(all.scen)
dfSummary <- 
  data.frame(Scenario.Type = c("Fmsy","Fpa","F0","Fsq","Btrigger","Blim"),
             Val = all.scen,
             Catch_2026 = NA, Catch_2027 = NA, 
             F_2026 = as.numeric(Fvalue[Fvalue$Yr==2026,paste0('replist',seq(1,num.scen))]),
             F_2027 = as.numeric(Fvalue[Fvalue$Yr==2027,paste0('replist',seq(1,num.scen))]),
             SSB_2026 = as.numeric(SSB[SSB$Yr==2026,paste0('replist',seq(1,num.scen))]),
             SSB_2026_SD = as.numeric(SSB.SD[SSB.SD$Yr==2026,paste0('replist',seq(1,num.scen))]),
             SSB_2027 = as.numeric(SSB[SSB$Yr==2027,paste0('replist',seq(1,num.scen))]),
             SSB_2027_SD = as.numeric(SSB.SD[SSB.SD$Yr==2027,paste0('replist',seq(1,num.scen))]),
             Rec_2026 = as.numeric(Recr[Recr$Yr==2026,paste0('replist',seq(1,num.scen))]),
             Rec_2027 = as.numeric(Recr[Recr$Yr==2027,paste0('replist',seq(1,num.scen))]),
             Fmsy=Fmsy, Blim=Blim, Change_advice=NA,Change_ssb=NA)

#probability of being below Blim
dfSummary$pBlim_2026 <- pnorm(Blim, dfSummary$SSB_2027, dfSummary$SSB_2027_SD)

#catches
for (i in 1:num.scen){
  output = forecastModels[[i]]
  catch <- output$timeseries %>% filter(Era == "FORE" ) %>% 
    dplyr::select("Yr", starts_with("dead(B)")) %>% 
    mutate(FMult = FMult_names[i])
  names(catch) <- c("Year","Catch","FMult")
  catch <- catch %>% pivot_wider(names_from = Year, values_from = Catch, names_prefix = "Catch_")
  dfSummary$Catch_2026[i] <- catch$Catch_2026
  dfSummary$Catch_2027[i] <- catch$Catch_2027
}

## Change in advice and ssb
last_adv=75545

for (i in 1:num.scen){
  
  dfSummary$Change_ssb[i]<-(dfSummary$SSB_2027[i]-dfSummary$SSB_2026[i])/dfSummary$SSB_2026[i]*100
  dfSummary$Change_advice[i]<-(dfSummary$Catch_2026[i]-last_adv)/last_adv*100
}

write.csv(dfSummary, file = paste0(stf_out,"/catch_option_table.csv"))








