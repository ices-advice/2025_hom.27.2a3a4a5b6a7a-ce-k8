# Script information ------------------------------------------------------

# generate outputs from retrospective analysis for WHM (WGWIDE)

# Authors: Rosana Ourens
# Date: Aug 2025

# load libraries ----------------------------------------------------------

library(icesTAF)
library(r4ss)
library(ss3diags)
library(ggplot2)
library(gridExtra)
library(icesAdvice)
############## Directories
wd<-getwd()
run_dir <- paste0(wd,"/model/run")
mkdir(paste0(wd,"/output/plots/diagnosis/retro"))
plotretro_dir <- paste0(wd,"/output/plots/diagnosis/retro")


#******************************************
##### Retros 
#*********************************************

retroModels <- SSgetoutput(dirvec=file.path(run_dir, "retrospectives",paste("retro",+0:-5, sep="")))


# Summarize output
retroSummary <- SSsummarize(retroModels)
# Set the ending year of each model in the set
endyrvec <- retroSummary$endyrs + 0:-5
SSplotComparisons(retroSummary,print=T,
                  endyrvec = endyrvec,
                  legendlabels = paste("Data", 0:-5, "years"),plotdir=plotretro_dir)


sspar(mfrow=c(2,2),plot.cex = 0.9)
SSplotRetro(retroSummary,forecastrho = T,add=T,subplots="SSB", endyrvec=2025:2020)
SSplotRetro(retroSummary,forecastrho = T,add=T,legend = F,xmin=2015, endyrvec=2025:2020)
SSplotRetro(retroSummary, subplots = "F",add=T,legendloc = "left",legendcex = 0.8,endyrvec=2024:2019)
SSplotRetro(retroSummary,subplots = "F", xmin=2015,forecastrho = T,add=T,legend = F,endyrvec=2024:2019)

dev.print(jpeg,paste0(plotretro_dir,"/Retros.jpg"), width = 9, height = 5, res = 300, units = "in")

# Do Hindcast with Cross-Validation of CPUE observations
sspar(mfrow=c(2,2),plot.cex = 0.9)
SSplotHCxval(retroSummary,xmin=2006,add=T,legendcex = 0.6)
dev.print(jpeg,paste0(wd,"/output/plots/diagnosis/MASE.jpg"), width = 8, height = 5, res = 300, units = "in")


###########################################################
################## Estimate Mohn's rho for recruitment as well
##################################################################
source(paste0(wd,"/boot/initial/software/utilities retro.R"))



path2 <- "retrospectives"

Base<- SS_output(file.path(run_dir,path2, "retro0"),covar=T)
Retro1<- SS_output(file.path(run_dir,path2, "retro-1"),covar=T)
Retro2<- SS_output(file.path(run_dir,path2, "retro-2"),covar=T)
Retro3<- SS_output(file.path(run_dir,path2, "retro-3"),covar=T)
Retro4<- SS_output(file.path(run_dir,path2, "retro-4"),covar=T)
Retro5<- SS_output(file.path(run_dir,path2, "retro-5"),covar=T)

# select run outputs
Abrecf <- derived_quants(Base)
Bbrecf <- derived_quants(Retro1)
Cbrecf <- derived_quants(Retro2)
Dbrecf <- derived_quants(Retro3)
Ebrecf <- derived_quants(Retro4)
Fbrecf <- derived_quants(Retro5)

Base$endyr<-2025 #intermediate year here
# year settings
years <- (Base$startyr:Base$endyr)

# run nicknames
nick<-c("Base","Retro1","Retro2","Retro3","Retro4","Retro5")
# run assessement output names
runs<-c("Abrecf", "Bbrecf", "Cbrecf", "Dbrecf" , "Ebrecf", "Fbrecf")

# set up containers
c(fbarlengthretro, ssbretro, recretro) := reset(runs, years)

## Spawning biomass
ssbretro<-ssb_function(nick,years,runs)

## Recruitment
recretro<-recr_function(nick, years, runs)

# remove natural mortality from total mortality to get F, columns 5 to 14 represent ages 1 to 10

fbarBase<-apply((Base$Z_at_age[,(5:14)] - Base$M_at_age[,(5:14)]),1,mean)
fbarRetro1<-apply((Retro1$Z_at_age[,(5:14)] - Retro1$M_at_age[,(5:14)]),1,mean)
fbarRetro2<-apply((Retro2$Z_at_age[,(5:14)] - Retro2$M_at_age[,(5:14)]),1,mean)
fbarRetro3<-apply((Retro3$Z_at_age[,(5:14)] - Retro3$M_at_age[,(5:14)]),1,mean)
fbarRetro4<-apply((Retro4$Z_at_age[,(5:14)] - Retro4$M_at_age[,(5:14)]),1,mean)
fbarRetro5<-apply((Retro5$Z_at_age[,(5:14)] - Retro5$M_at_age[,(5:14)]),1,mean)

for (i in 1:length(nick)) {
  fbarlengthretro[1:(length(years)),i] <- as.vector(eval(as.name(paste0("fbar",nick[i])))[(1:(length(years)))])
}

## now run just this for the data for copying in to excel
ssbretro
recretro
fbarlengthretro



lastYr <- 2025 # to update every year

yr=c(1982:lastYr)
TH=c("base","-1","-2","-3","-4","-5")

colnames(ssbretro)<-TH
rownames(ssbretro)<-1982:(lastYr)
dim(ssbretro)
ssbretro[nrow(ssbretro),c(2:6)]<- NA
ssbretro[nrow(ssbretro)-1,c(3:6)]<- NA
ssbretro[nrow(ssbretro)-2,c(4:6)]<- NA
ssbretro[nrow(ssbretro)-3,c(5:6)]<- NA
ssbretro[nrow(ssbretro)-4,6]<- NA
mohn(ssbretro,peels=5,plot=T)

ssbretro_df <- as.data.frame(ssbretro)
ssbretro_df$yr <- as.numeric(rownames(ssbretro_df))
names(ssbretro_df) <- c("base",paste("retro",c(1:5),sep=""),"year")

# need to add the confidence intervals to the figure of retrospectives
ssbretro_df2 <- ssbretro_df
ssb <- rbind(Abrecf[substr(Abrecf$Label,1,5)	=="SSB_1",],
             Abrecf[substr(Abrecf$Label,1,5)	=="SSB_2",])
ssb<-head(ssb,-2)
ssb <- cbind.data.frame(ssb$Value, lo=ssb$Value-1.96*ssb$StdDev, hi=ssb$Value+1.96*ssb$StdDev)
ssbretro_df2$lo <- ssb$lo
ssbretro_df2$hi <- ssb$hi

colors = c("retro1"="#000043FF","retro2"="#0033FFFF","retro3"="#01CCA4FF","retro4"="#BAFF12FF","retro5"="#FFCC00FF", "base"="#FF3300FF")
#linetypes = c("retro1"=2,"retro2"=3,"retro3"=4,"retro4"=5,"retro5"=6, "base"=1)
linetypes = c("retro1"=1,"retro2"=1,"retro3"=1,"retro4"=1,"retro5"=1, "base"=1)
g1 <- ggplot(ssbretro_df2, aes(x=yr)) +  geom_line(aes(y=base, color="base", linetype="base"), size=1) +
  geom_line(aes(y=retro1, color="retro1", linetype="retro1"), linewidth=0.7) + 
  geom_line(aes(y=retro2, color="retro2", linetype="retro2"), linewidth=0.7) +
  geom_line(aes(y=retro3, color="retro3", linetype="retro3"), linewidth=0.7) + 
  geom_line(aes(y=retro4, color="retro4", linetype="retro4"), linewidth=0.7)+ 
  geom_line(aes(y=retro5, color="retro5", linetype="retro5"), linewidth=0.7) +
  geom_line(aes(y=lo), linetype=2)+
  geom_line(aes(y=hi), linetype=2)+
  labs(y="SSB") + # coord_cartesian(ylim=c(0,40000)) +
  scale_color_manual(values= colors) + scale_linetype_manual(values= linetypes)+ theme(legend.position = "none") +
  geom_text(x=2010, y=5000000, label=paste('mohns rho =',round(mohn(ssbretro,peels=5,plot=F),3)))

g1
## 

fbarlengthretro <- fbarlengthretro#[-38,]
colnames(fbarlengthretro)<-TH
rownames(fbarlengthretro)<-yr
dim(fbarlengthretro)
# remove last observation as we don't know f in the intermediate year
fbarlengthretro<-fbarlengthretro[-nrow(fbarlengthretro),]

fbarlengthretro[nrow(fbarlengthretro),c(2:6)]<- NA
fbarlengthretro[nrow(fbarlengthretro)-1,c(3:6)]<- NA
fbarlengthretro[nrow(fbarlengthretro)-2,c(4:6)]<- NA
fbarlengthretro[nrow(fbarlengthretro)-3,c(5:6)]<- NA
fbarlengthretro[nrow(fbarlengthretro)-4,6]<- NA
mohn(fbarlengthretro,peels=5,plot=T)

fbarlengthretro_df <- as.data.frame(fbarlengthretro)
fbarlengthretro_df$yr <- as.numeric(rownames(fbarlengthretro_df ))
names(fbarlengthretro_df) <- c("base",paste("retro",c(1:5),sep=""),"year")

# need to add the confidence intervals to the figure of retrospectives
#fbar <- cbind.data.frame(fbar=Fbar.Final[,2], lo=Fbar.Final[,2]-Fbar.Final[,3], hi=Fbar.Final[,2]+Fbar.Final[,3])

fbar<-read.csv(paste0(wd,"/output/tables/fbar.csv"))
fbar<-fbar[,-1]


fbarlengthretro_df2 <- fbarlengthretro_df
fbarlengthretro_df2$lo <- fbar$fbar_lo #[-length(fbar$lo)]
fbarlengthretro_df2$hi <- fbar$fbar_hi #i[-length(fbar$hi)]

colors = c("retro1"="#000043FF","retro2"="#0033FFFF","retro3"="#01CCA4FF","retro4"="#BAFF12FF","retro5"="#FFCC00FF", "base"="#FF3300FF")
linetypes = c("retro1"=1,"retro2"=1,"retro3"=1,"retro4"=1,"retro5"=1, "base"=1)
g2 <- ggplot(fbarlengthretro_df2, aes(x=year)) +  geom_line(aes(y=base, color="base", linetype="base"), size=1) +
  geom_line(aes(y=retro1, color="retro1", linetype="retro1"), linewidth=0.7) + 
  geom_line(aes(y=retro2, color="retro2", linetype="retro2"), linewidth=0.7) +
  geom_line(aes(y=retro3, color="retro3", linetype="retro3"), linewidth=0.7) + 
  geom_line(aes(y=retro4, color="retro4", linetype="retro4"), linewidth=0.7)+ 
  geom_line(aes(y=retro5, color="retro5", linetype="retro5"), linewidth=0.7) +
  geom_line(aes(y=lo), linetype=2)+
  geom_line(aes(y=hi), linetype=2)+
  labs(y="Fbar(1-10)") +coord_cartesian(ylim=c(0,0.4)) +
  scale_color_manual(values= colors) + scale_linetype_manual(values= linetypes)+ theme(legend.position = "none")+
  geom_text(x=2010, y=0.37, label=paste('mohns rho =',round(mohn(fbarlengthretro,peels=5,plot=F),3)))
g2

recretro <- recretro
colnames(recretro)<-TH
rownames(recretro)<-yr
dim(recretro)
# remove last observation as it is a forecast value
recretro<-recretro[-nrow(recretro),]



recretro[nrow(recretro),c(2:6)]<- NA
recretro[nrow(recretro)-1,c(3:6)]<- NA
recretro[nrow(recretro)-2,c(4:6)]<- NA
recretro[nrow(recretro)-3,c(5:6)]<- NA
recretro[nrow(recretro)-4,6]<- NA
mohn(recretro,peels=5,plot=T)


recretro_df <- as.data.frame(recretro)
recretro_df$yr <- as.numeric(rownames(recretro_df))
names(recretro_df) <- c("base",paste("retro",c(1:5),sep=""),"year")

# need to add the confidence intervals to the figure of retrospectives
recretro_df2 <- recretro_df
#rec <- cbind.data.frame(rec$Value, lo=rec$Value-1.96*rec$StdDev, hi=rec$Value+1.96*rec$StdDev)

rec<-read.csv(paste0(wd,"/output/tables/rec.csv"))
rec<-rec[,-1]

rec<-dplyr::rename(rec,Value=rec)
recretro_df2$lo <- rec$rec_lo
recretro_df2$hi <- rec$rec_hi
colors=rich.colors.short(n=6)
colors = c("retro1"="#000043FF","retro2"="#0033FFFF","retro3"="#01CCA4FF","retro4"="#BAFF12FF","retro5"="#FFCC00FF", "base"="#FF3300FF")
linetypes = c("retro1"=1,"retro2"=1,"retro3"=1,"retro4"=1,"retro5"=1, "base"=1)
g3 <- ggplot(recretro_df2, aes(x=year)) +  geom_line(aes(y=base, color="base", linetype="base"), size=1) +
  geom_line(aes(y=retro1, color="retro1", linetype="retro1"), size=0.7) + 
  geom_line(aes(y=retro2, color="retro2", linetype="retro2"), size=0.7) +
  geom_line(aes(y=retro3, color="retro3", linetype="retro3"), size=0.7) + 
  geom_line(aes(y=retro4, color="retro4", linetype="retro4"), size=0.7)+ 
  geom_line(aes(y=retro5, color="retro5", linetype="retro5"), size=0.7) +
  geom_line(aes(y=lo), linetype=2)+
  geom_line(aes(y=hi), linetype=2)+
  scale_color_manual(values= colors) + labs(y="recruitment") + #coord_cartesian(ylim=c(0,0.3)) +
  scale_linetype_manual(values= linetypes)+ theme(legend.position = "none")+
  geom_text(x=2010, y=300000000, label=paste('mohns rho (5yrs) =',round(mohn(recretro,peels=5,plot=F),3)))

g3

## PLOT ALL

retros_plot<-grid.arrange(g1,g2,g3, ncol=2,nrow=2)
dev.print(jpeg,paste0(wd,"/output/plots/diagnosis/retro/retro_fig_report.jpg"), width = 8, height = 5, res = 300, units = "in")


#****************************************
# Basic Residual Diags
#****************************************
# Reference run
ss3rep = retroModels[[1]]

# Check Runs Test and Joint residuals option for mean composition data
sspar(mfrow=c(2,2),plot.cex = 0.8)

# For surveys
SSplotRunstest(ss3rep,subplots="cpue",add=T,legendcex = 0.6)
dev.print(jpeg,paste0(wd,"/output/plots/diagnosis/Residuals-Surveys.jpg"), width = 8, height = 7, res = 300, units = "in")

# For age
sspar(mfrow=c(1,1),plot.cex = 0.8)

#SSplotRunstest(ss3rep,subplots="len",add=T,legendcex = 0.6)
SSplotRunstest(ss3rep,subplots="age",add=T,legendcex = 0.6)
dev.print(jpeg,paste0(wd,"/output/plots/diagnosis/Residuals-age.jpg"), width = 8, height = 7, res = 300, units = "in")

# Check conflict between indices and mean age
###RMSE < 30% indicates no apparent conflict between data sources

sspar(mfrow=c(1,2),plot.cex = 0.8)
SSplotJABBAres(ss3rep,subplots="cpue",add=T,col=sscol(3)[c(1,3,2)])
#SSplotJABBAres(ss3rep,subplots="len",add=T,col=sscol(3)[c(1,3,2)])
SSplotJABBAres(ss3rep,subplots="age",add=T,col=sscol(3)[c(1,3,2)])

dev.print(jpeg,paste0(wd,"/output/plots/diagnosis/JointResiduals.jpg"), width = 8, height = 3.5, res = 300, units = "in")


#**************************************
#   Compare final model with model from last year
#**************************************
mkdir(paste0(wd,"/output/plots/diagnosis/lastmodelComp"))
Comparison_dir <- paste0(wd,"/output/plots/diagnosis/lastmodelComp")

model_run <- SS_output(run_dir)
oldwd   <- paste0(wd,"/boot/initial/data/assessment2024")
out_old <- SS_output(oldwd)
tocomp  <- SSsummarize(list(out_old, model_run))
SSplotComparisons(tocomp,  png=T,endyrvec=c(2024,2025),type="b", lty=c(1,2),subplot=1:20,tickEndYr=T,xlim=c(1980,2025),
                  legendlabels=c("Assessment 2024","Assessment 2025") ,
                  plotdir=Comparison_dir)





sessionInfo()

# End of script -----------------------------------------------------------

rm(list = ls())

