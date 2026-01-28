# Script information ------------------------------------------------------

# generate the outputs from stock assessment for WHM


# Date: 2025

# Load packages -----------------------------------------------------------

library(icesTAF)
library(r4ss)


############## Directories
wd<-getwd()
run_dir <- paste0(wd,"/model/run")
mkdir(paste0(wd,"/output/plots/SS3_run"))
res.plots <- paste0(wd,"/output/plots/SS3_run")
mkdir(paste0(wd,"/output/tables"))
tables_dir<-paste0(wd,"/output/tables")

# ref points
MSYBtrig = 787443
Fmsy = 0.08
Blim=566678
Bpa= MSYBtrig
#Flim=0.22
Fp05=0.08
Fpa=Fp05 


#***************************************************
#* Standard outputs SS3
#*********************************************

model_run <- SS_output(run_dir)
SS_plots(model_run,btarg = -1, minbthresh = -1,dir = res.plots)




#***************************************
#* Derived quantities
#* ************************************
#* 
iniYr<-1982
FinYr<-2024


# F values

fbar_raw<-model_run$derived_quants[substr(model_run$derived_quants$Label,1,2) == "F_",]
fbar_lo <- fbar_raw$Value - qnorm(1-(1-0.95)/2)*fbar_raw$StdDev 
fbar_hi <- fbar_raw$Value + qnorm(1-(1-0.95)/2)*fbar_raw$StdDev 
fbar <- cbind.data.frame(year=substr(fbar_raw$Label,3,6),Value=fbar_raw$Value, fbar_lo, fbar_hi)
fbar<-head(fbar,-3)



plot(fbar$Value~fbar$year, type="l",ylab='Fbar (1-10)',xlab="",ylim=c(0,max(fbar$fbar_hi)))
lines(fbar$fbar_hi~fbar$year, lty=2)
lines(fbar$fbar_lo~fbar$year, lty=2)
#abline(h=Flim,lty=3,col='red')
abline(h=Fmsy,lty=3,col='blue')
#text(1985,0.23, "Flim", col = "red", adj = c(0, -.1))
text(1985,0.09, "Fmsy", col = "blue", adj = c(0, -.1))

dev.print(jpeg,paste0(wd,"/output/plots/fbar.jpg"), width = 9, height = 5, res = 300, units = "in")

write.csv(fbar,file=paste0(tables_dir,"/fbar.csv"))


# SSB values
ssb_raw <- rbind(model_run$derived_quants[substr(model_run$derived_quants$Label,1,5)	=="SSB_1",],
                 model_run$derived_quants[substr(model_run$derived_quants$Label,1,5)	=="SSB_2",])
ssb_raw<-head(ssb_raw,-2)

ssb_lo <- ssb_raw$Value - qnorm(1-(1-0.95)/2)*ssb_raw$StdDev   
ssb_hi <- ssb_raw$Value + qnorm(1-(1-0.95)/2)*ssb_raw$StdDev

year.range<-iniYr:(FinYr+1)        
plot(ssb_raw$Value~year.range, type="l",ylab='SSB in t',xlab="",ylim=c(0,max(ssb_hi)))
lines(ssb_hi~year.range, lty=2)
lines(ssb_lo~year.range, lty=2)
abline(h=Blim,lty=3,col='red')
abline(h=MSYBtrig,lty=3,col='blue')
text(1985,Blim-300000, "Blim", col = "red", adj = c(0, -.1))
text(1985,MSYBtrig+200000, "MSYBtrig", col = "blue", adj = c(0, -.1))
ssb <- cbind.data.frame("label"=ssb_raw$Label,"ssb"=ssb_raw$Value, ssb_lo, ssb_hi)


dev.print(jpeg,paste0(wd,"/output/plots/ssb.jpg"), width = 9, height = 5, res = 300, units = "in")

write.csv(ssb,file=paste0(tables_dir,"/ssb.csv"))

# recruitment values
rec_raw <- rbind(model_run$derived_quants[substr(model_run$derived_quants$Label,1,6)	=="Recr_1",],
                 model_run$derived_quants[substr(model_run$derived_quants$Label,1,6)	=="Recr_2",])

rec_raw<-head(rec_raw,-3)
## recruitment is assumed to have a lognormal distribution:
rec_hi <- exp(log(rec_raw$Value) + qnorm(1-(1-0.95)/2) * sqrt(log(1 + (rec_raw$StdDev/rec_raw$Value) * (rec_raw$StdDev/rec_raw$Value))))  
rec_lo <- exp(log(rec_raw$Value) - qnorm(1-(1-0.95)/2) * sqrt(log(1 + (rec_raw$StdDev/rec_raw$Value) * (rec_raw$StdDev/rec_raw$Value))))  

year.range<-iniYr:(FinYr)   
plot(rec_raw$Value~year.range, type="l",ylab='Recruitment',xlab="",ylim=c(0,1.2e09))
lines(rec_hi~year.range, lty=2)
lines(rec_lo~year.range, lty=2)



dev.print(jpeg,paste0(wd,"/output/plots/rec.jpg"), width = 9, height = 5, res = 300, units = "in")

rec <- cbind.data.frame("label"=rec_raw$Label,"rec"=rec_raw$Value, rec_lo, rec_hi)


write.csv(rec,file=paste0(tables_dir,"/rec.csv"))


### Extract numbers at age
natage <- model_run$natage[model_run$natage$'Beg/Mid'=="B" & model_run$natage$Yr>=iniYr,c(8,13:28)]
natage<-natage[-nrow(natage),]
View(natage)

natage<-head(natage,-2)

write.csv(natage,file=paste0(tables_dir,"/natage.csv"))


### Extract F at age
Fatage <-model_run$fatage[,c(5,8:23)]
Fatage<-subset(Fatage,Yr>=iniYr&Yr<=FinYr)
View(Fatage)
write.csv(Fatage,file=paste0(tables_dir,"/fatage.csv"))

### Extract total biomass
TBio <- data.frame("Year"=model_run$timeseries$Yr,"biomass"=model_run$timeseries$Bio_all)
TBio<-TBio[TBio$Year>=iniYr&TBio$Year<=FinYr,]
write.csv(TBio,file=paste0(tables_dir,"/TotalBio.csv"))

# End of script -----------------------------------------------------------

rm(list=ls())




