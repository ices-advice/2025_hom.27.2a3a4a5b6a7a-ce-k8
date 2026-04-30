## Output for the model including forecasted years under the FMSY scenario
library(ss3om)
library(tidyverse)
ass.yr <- lubridate::year(Sys.Date()) 
wd<-getwd()

fmsy_dir <- paste0(wd,"/model/stf/Scenarios/Fmsy_6")## Model with the FMSY scenario (basis for the advice)



whm <- ss3om::readFLSss3(fmsy_dir, name = "whm", repfile = "Report.sso", compfile = "CompReport.sso",forecast=T,wtatage=T,desc = paste0('Advice for ', ass.yr+1))
plot(whm)


harvest(whm) 
ssb(whm)
rec(whm)
fbar(whm)
save(whm,file=paste0(wd,"/output/FLStock_WHM_ss3om.RData"))

