# Start -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
setwd("~/GitHub/parksinthepandemic/code")

# Load packages ----------------------------------------------------------
#install.packages('plotrix')
library(plotrix)
#install.packages('tibble')
library(tibble)
#install.packages('ggplot2')
library(ggplot2)
#install.packages('gridExtra')
library(gridExtra)
#install.packages('grid)
library(grid)
#install.packages('ggplot2)
library(ggplot2)
#install.packages('lattice')
library(lattice)
#install.packages("tidyr")
library(tidyr)

# Load dataset ------------------------------------------------------------

#getwd()
#Global_Mobility_Report<-read.csv("inputdata/Global_Mobility_Report_030620.csv")
#Global_Mobility_Report$date<-as.Date(Global_Mobility_Report$date,format = "%d/%m/%y")

Global_Mobility_Report<-read.csv("inputdata/Global_Mobility_Report.csv")
Global_Mobility_Report$date<-as.Date(Global_Mobility_Report$date)
Global_Mobility_Report$parks_percent_change_from_baseline<-as.numeric(Global_Mobility_Report$parks_percent_change_from_baseline)

#add sub_country column
Global_Mobility_Report<-Global_Mobility_Report %>% add_column(sub_country = NA, .after = which(colnames(Global_Mobility_Report)=="country_region"))

# Subset the data to only present the UK data, separated by GB cod --------
UK<-subset(Global_Mobility_Report,country_region_code == "GB")
#UK<-UK[!(UK$sub_country%in%""), ]

# Assigning country to UK districts ---------------------------------------

#saveRDS(Districts_by_country,"Districts_by_country.RDS")

Districts_by_country<-readRDS("inputdata/Districts_by_country.RDS")

# Code to assign districts to the new data set ----------------------------
#England
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[1]])]<-"England"
#Wales
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[2]])]<-"Wales"
#Scotland
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[3]])]<-"Scotland"
#Northern Ireland
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[4]])]<-"N_Ireland"

# Calculating UK mean and SE for each country division of the UK ----------
Park_UK_mean<-as.data.frame(t(tapply(UK$parks_percent_change_from_baseline, list(UK$sub_country,UK$date),mean, na.rm=T)))
Park_UK_mean<-rownames_to_column(Park_UK_mean,var="Date")
Park_UK_mean$Date<-as.Date(Park_UK_mean$Date)

Park_UK_SE<-as.data.frame(t(tapply(UK$parks_percent_change_from_baseline, list(UK$sub_country,UK$date),std.error, na.rm=T)))
Park_UK_SE<-rownames_to_column(Park_UK_SE,var="Date")
Park_UK_SE$Date<-as.Date(Park_UK_SE$Date)

# Plots -------------------------------------------------------------------
#England
ENG_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=England))+
  geom_line()+
  geom_ribbon(aes(ymin=England-Park_UK_SE$England,
                  ymax=England+Park_UK_SE$England),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"))+
  ggtitle("England")+xlab("Date")+ylab("Parks percentage change from baseline")


#Wales
WAL_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=Wales))+
  geom_line()+
  geom_ribbon(aes(ymin=Wales-Park_UK_SE$Wales,
                  ymax=Wales+Park_UK_SE$Wales),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"))+
  ggtitle("Wales")+xlab("Date")+ylab("Parks percentage change from baseline")

#Scotland
SCOT_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=Scotland))+
  geom_line()+
  geom_ribbon(aes(ymin=Scotland-Park_UK_SE$Scotland,
                  ymax=Scotland+Park_UK_SE$Scotland),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"))+
  ggtitle("Scotland")+xlab("Date")+ylab("Parks percentage change from baseline")

#Northern Ireland
NI_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=N_Ireland))+
  geom_line()+
  geom_ribbon(aes(ymin=N_Ireland-Park_UK_SE$N_Ireland,
                  ymax=N_Ireland+Park_UK_SE$N_Ireland),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey"))+
  ggtitle("N Ireland")+xlab("Date")+ylab("Parks percentage change from baseline")

grid.arrange(ENG_graph,WAL_graph,SCOT_graph,NI_graph, nrow = 2)

