# Script information ------------------------------------------------------

# Read assessment data (after the bootstrap procedure) and write TAF data tables

# Before running the script in folder ./bootstrap/initial/data we have: 
#         forecast.ss 
#         control.ss
#         data.ss
#         starter.ss
#         wtatage.ss
#         Input_SS_model2025.xlsx

# After running the script in folder ./data we have:
#   Figures/tables of input data: catch, catch at age, surveys, weight at age      
# Authors: Rosana Ourens 

# Date: Aug 2025

# Load libraries ----------------------------------------------------------

library(icesTAF)
library(ggplot2)
library(dplyr)
library(ggmisc)
library(tidyr)
library(ggrepel)
# Working directory and folders -------------------------------------------

# check working directory

getwd()

# create data folder using the function mkdir in icesTAF

mkdir("data")

# Read data ---------------------------------------------------------------

# read input files
#inputs <- r4ss::SS_read( "boot/data", verbose = TRUE)
fn  <- "boot/data/Input_SS_model2025.xlsx"





#### Catch data

#catch<-readxl::read_excel(path=fn, range="TotCatch!A3:D46")
#write.csv(catch,paste0(getwd(),"/data/catch.csv"),row.names = F)

#catch_l<-catch[,-2]%>%
#  pivot_longer(!year,names_to="catch_cat",values_to="tonnes")
#x11()
#ggplot(catch_l,aes(year,tonnes,fill=catch_cat))+
#  geom_col()+
#  theme(legend.title=element_blank())

#  
#savePlot(paste0(getwd(),"/data/Catch.png"), type="png")

################## Catch at age
catage<-readxl::read_excel(path=fn,range="CAA!A2:Y45") 
catage<-catage[,-c(2:9)]
write.csv(catage,paste0(getwd(),"/data/catage.csv"),row.names = F)


ibya <- 
  readxl::read_excel(path=fn,range="CAA!A2:Y45") %>% 
  tidyr::pivot_longer(names_to = "age", values_to = "data", cols=c(10:25)) %>% 
  mutate(age = as.numeric(age)) %>% 
  mutate(yc = Yr-age) %>% 
  mutate(stock="Western horse mackerel")

x11()
ibya %>% 
  #filter(age > 0) %>% 
  #filter(year >= 2000) %>% 
  
  
  ggplot() +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.y = element_blank()) +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  
  geom_col(aes(Yr, data, fill = factor(yc))) + 
  scale_fill_crayola() +
  labs(x = NULL, y = NULL, title="Horse mackerel relative catch at age") +
  facet_grid(age ~ stock, scale = "free_y", switch = "y")

savePlot(paste0(getwd(),"/data/CatchAtAge.png"), type="png")



#### weight at age

waa <- 
  readxl::read_excel(path=fn,range="CWAA!A4:W349") %>% 
  filter(name=="#wt_flt_1")%>%  
  tidyr::pivot_longer(names_to = "age", values_to = "weight", cols=c(7:22)) 

waa_fig<-ggplot(waa,aes(Year,weight)) +
  
  theme_bw() +
  theme(legend.position = "none") +
  theme(panel.border     = element_rect(colour="black" , size=0.1)) +
  theme(axis.ticks.y     = element_blank() ) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size=10)) +
  theme(panel.spacing = unit(0.2, "lines")) +
  
  geom_line(aes(col = factor(age))) + 
  #scale_fill_crayola() +
  geom_label_repel(data=filter(waa,Year>2023),aes(label=age, col=age),label.padding = unit(0.2, "lines"))+
  labs(x = NULL, y = "Weight (kg)") 


waa_fig

png(paste0(getwd(),"/data/WeightAtAge.png"), width=2100, height=2000, res=200)
waa_fig
dev.off()



############################# Surveys

megs <- 
  readxl::read_excel(path=fn,range="Surveys SS input!A27:F38") %>% 
  mutate(Survey = "EggSurvey") 

ibts <- 
  readxl::read_excel(path=fn,range="Surveys SS input!A2:F24") %>% 
  mutate(Survey = "IBTS")


cpue <- 
  readxl::read_excel(path=fn,range="Surveys SS input!A41:F47") %>% 
  mutate(Survey = "CPUE") 

acoustics <- 
  readxl::read_excel(path=fn,range="Surveys SS input!A55:F68") %>% 
  mutate(Survey = "Acoustics") 

surveys<-rbind(megs,ibts,cpue,acoustics)

P3<-ggplot(surveys, aes(yr,obs,col=Survey))+geom_point()+geom_line(data=surveys[surveys$Survey!="EggSurvey",])+
  facet_grid(Survey~.,scales="free_y")+ylab("Indices")+
  geom_pointrange(aes(ymin=exp(log(obs) - qnorm(1-(1-0.95)/2) * cv) , ymax=exp(log(obs) + qnorm(1-(1-0.95)/2) * cv) ))

P3

png(paste0(getwd(),"/data/Surveys.png"), width=2100, height=2000, res=200)
P3
dev.off()

