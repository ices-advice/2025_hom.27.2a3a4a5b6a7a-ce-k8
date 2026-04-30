### ------------------------------------------------------------------------ ###
### Prepare plots and tables for report ####
### ------------------------------------------------------------------------ ###


library(icesTAF)
library(r4ss)
library(dplyr)
library(tidyr)
############## Directories
wd<-getwd()
run_dir <- paste0(wd,"/model/run")
mkdir("report")
rep.dir<-paste0(wd,"/report")

# ref points
MSYBtrig = 787443
Fmsy = 0.08
Blim=566678
Bpa= MSYBtrig
#Flim=0.22
Fp05=0.08
Fpa=Fp05 


#**************************
### input data
#************************
file.copy(from="output/plots/SS3_run/plots/data_plot2.png",
          to="report/InputData.png", 
          overwrite=T)
file.copy(from="data/WeightAtAge.png",
          to="report/WeightAtAge.png", 
          overwrite=T)

file.copy(from="data/CatchAtAge.png",
          to="report/CatchAtAge.png", 
          overwrite=T)
file.copy(from="data/Catch.png",
          to="report/Catch.png", 
          overwrite=T)
file.copy(from="data/Surveys.png",
          to="report/Surveys.png", 
          overwrite=T)
file.copy(from="data/catage.csv",
          to="report/catage.csv", 
          overwrite=T)

########## Fixed parameters in the model
## call the ss3 model
model_run <- SS_output(run_dir)

NatMort<-model_run$Natural_Mortality[1,-c(1:12)]
NatMortL<-pivot_longer(NatMort,cols=c(1:16),names_to="age", values_to="NatMort")

Mat<-data.frame(age=as.numeric(seq(0,15,1)),Maturity=c(0.0131372, 0.0750336, 0.356599, 0.62325, 0.79538, 0.885876, 0.932668, 0.955833, 0.969667, 0.978212, 0.985787, 0.989482, 0.992577, 0.994437, 0.996234, 0.997641))

params<-merge(Mat,NatMortL)

write.csv(params,paste0(rep.dir,"/params.csv"),row.names = F)


#**************************
####### model outputs
#***************************

file.copy(from="output/plots/SS3_run/plots/ts_summaryF.png",
          to="report/FishingMortality.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/ts7_Spawning_output_with_95_asymptotic_intervals_intervals.png",
          to="report/SSB.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/ts11_Age-0_recruits_(1000s)_with_95_asymptotic_intervals.png",
          to="report/Recruitment.png", 
          overwrite=T)

file.copy(from="output/plots/SS3_run/plots/sel02_multiple_fleets_age2.png",
          to="report/FleetSelectivity.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/sel15_Commercial_age_seldevs.png",
          to="report/CommercialSelectivityDeviations.png", 
          overwrite=T)


ssb<-read.csv(paste0(wd,"/output/tables/ssb.csv"))%>%
  mutate(year=substr(label,5,8))
ssb<-ssb[,c(3,6)]

rec<-read.csv(paste0(wd,"/output/tables/rec.csv"))%>%
  mutate(year=substr(label,6,9))
rec<-rec[,c(3,6)]

fbar<-read.csv(paste0(wd,"/output/tables/fbar.csv"))
fbar<-fbar[,c(2,3)]%>%
  rename("Fbar"="Value")

TotalBio<-read.csv(paste0(wd,"/output/tables/TotalBio.csv"))
TotalBio<-TotalBio[,c(2,3)]%>%
  rename("TotBio"="biomass","year"="Year")

catch<-data.frame(catch=model_run$catch$ret_bio,year=model_run$catch$Yr)
summaryOut<-join(TotalBio,ssb,type="full")%>%
  join(.,rec)%>%
  join(.,catch)%>%
  join(.,fbar)
write.csv(summaryOut,paste0(rep.dir,"/SummaryTable.csv"))
  

#****************************************
################### model diagnosis
#********************************************
#Fit to age data
file.copy(from="output/plots/SS3_run/plots/comp_agefit__aggregated_across_time.png",
          to="report/AgeFit_overall.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/comp_agefit_flt1mkt0_page1.png",
          to="report/AgeFit_1.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/comp_agefit_flt1mkt0_page2.png",
          to="report/AgeFit_2.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/comp_agefit_residsflt1mkt0_page2.png",
          to="report/AgeFit_residuals.png", 
          overwrite=T)
file.copy(from="output/plots/diagnosis/Residuals-age.jpg",
          to="report/Resid_age.jpg", 
          overwrite=T)

### Fit to survey data
file.copy(from="output/plots/SS3_run/plots/index2_cpuefit_Acoustics.png",
          to="report/AcousticsFit.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/index2_cpuefit_CPUE.png",
          to="report/CPUEFit.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/index2_cpuefit_IBTS.png",
          to="report/IBTSFit.png", 
          overwrite=T)
file.copy(from="output/plots/SS3_run/plots/index2_cpuefit_MEGS.png",
          to="report/MEGSfit.png", 
          overwrite=T)

file.copy(from="output/plots/diagnosis/Residuals-Surveys.jpg",
          to="report/Resid_surveys.jpg", 
          overwrite=T)
file.copy(from="output/plots/diagnosis/JointResiduals.jpg",
          to="report/RMSE.jpg", 
          overwrite=T)
file.copy(from="output/plots/diagnosis/MASE.jpg",
          to="report/MASE.jpg", 
          overwrite=T)

### Retros
file.copy(from="output/plots/diagnosis/retro/Retros.jpg",
          to="report/Retros.jpg", 
          overwrite=T)
file.copy(from="output/plots/diagnosis/retro/compare2_spawnbio_uncertainty.png",
          to="report/SSBrETRO.png", 
          overwrite=T)
file.copy(from="output/plots/diagnosis/retro/compare8_Fvalue_uncertainty.png",
          to="report/Fretro.png", 
          overwrite=T)
file.copy(from="output/plots/diagnosis/retro/compare10_recruits_uncertainty.png",
          to="report/RecRetro.png", 
          overwrite=T)
file.copy(from="output/plots/diagnosis/retro/retro_fig_report.jpg",
          to="report/CombinedRetro.jpg", 
          overwrite=T)
#****************************
########## Short-term forecast
#*****************************

file.copy(from="output/stf/catch_option_table.csv",
          to="report/catch_option_table.csv", 
          overwrite=T)

#*********************
#*#Reference points
#*#**************
file.copy(from="boot/data/RefPoints.xlsx",
          to="report/RefPoints.xlsx", overwrite = T)
