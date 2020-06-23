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
install.packages('reshape2')
library(reshape2)

# Load dataset ------------------------------------------------------------
#Loads Global Mobility report from the designated work directory
Global_Mobility_Report<-read.csv("inputdata/Global_Mobility_Report.csv")
#Makes the date variables as a date class in R.
Global_Mobility_Report$date<-as.Date(Global_Mobility_Report$date)
#Makes the parks-percentage change from baseline variables numeric. 
Global_Mobility_Report$parks_percent_change_from_baseline<-as.numeric(Global_Mobility_Report$parks_percent_change_from_baseline)

#add sub_country column
Global_Mobility_Report<-Global_Mobility_Report %>% add_column(sub_country = NA, .after = which(colnames(Global_Mobility_Report)=="country_region"))

# Subset the data to only present the UK data, separated by GB cod --------
UK<-subset(Global_Mobility_Report,country_region_code == "GB")

# Assigning country to UK districts ---------------------------------------
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
#Calculates the average mean across the districts for each time point.
Park_UK_mean<-as.data.frame(t(tapply(UK$parks_percent_change_from_baseline, list(UK$sub_country,UK$date),mean, na.rm=T)))
#Puts dates into a column for analysis under a new column known as Date.
Park_UK_mean<-rownames_to_column(Park_UK_mean,var="Date")
#Ensure the Date variables is the date class present in R. 
Park_UK_mean$Date<-as.Date(Park_UK_mean$Date)

#Calculates the SE across the districts for each mean calculated. 
Park_UK_SE<-as.data.frame(t(tapply(UK$parks_percent_change_from_baseline, list(UK$sub_country,UK$date),std.error, na.rm=T)))
#Puts dates into a column for analysis under a new column known as Date.
Park_UK_SE<-rownames_to_column(Park_UK_SE,var="Date")
#Ensure the Date variables is the date class present in R.
Park_UK_SE$Date<-as.Date(Park_UK_SE$Date)

# Plots for the whole of time series avalible to us -------------------------------------------------------------------
#Creates the graph referring to England districts. 

ENG_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=England, group=Date)) +

  geom_col(position = position_dodge(width=0.2)) +
  
  geom_errorbar(aes(ymin=England-Park_UK_SE$England,
                    ymax=England+Park_UK_SE$England), 
                    width=0, position = position_dodge(width=0.9)) +
                  
  coord_cartesian(ylim=c(-60,160)) +
    
  geom_hline(yintercept=0) + 
    
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
    
  ggtitle("England")+xlab("Date")+ylab("Parks percentage change from baseline")


#Creates the graph for the Wales districts
WAL_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=Wales, group=Date)) +
  
  geom_col(position = position_dodge(width=0.2)) +
  
  geom_errorbar(aes(ymin=Wales-Park_UK_SE$Wales,
                    ymax=Wales+Park_UK_SE$Wales), 
                width=0, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  
  ggtitle("Wales")+xlab("Date")+ylab("Parks percentage change from baseline")

#Creates a graph for Scotland' district
SCOT_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=Scotland, group=Date)) +
  
  geom_col(position = position_dodge(width=0.2)) +
  
  geom_errorbar(aes(ymin=Scotland-Park_UK_SE$Scotland,
                    ymax=Scotland+Park_UK_SE$Scotland), 
                width=0, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  
  ggtitle("Scotland")+xlab("Date")+ylab("Parks percentage change from baseline")


#Creates a graph for Northern Irelands' district
NI_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=Scotland, group=Date)) +
  
  geom_col(position = position_dodge(width=0.2)) +
  
  geom_errorbar(aes(ymin=Scotland-Park_UK_SE$Scotland,
                    ymax=Scotland+Park_UK_SE$Scotland), 
                width=0, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  
  ggtitle("NI")+xlab("Date")+ylab("Parks percentage change from baseline")


#This plots all the graphs onto one image.
grid.arrange(ENG_graph,WAL_graph,SCOT_graph,NI_graph, nrow = 2)

#better panel plot

library(reshape2)
Park_UK_mean_all<-melt(Park_UK_mean,'Date')
Park_UK_mean_all<-cbind(Park_UK_mean_all,se=melt(Park_UK_SE,'Date')[,3])
Park_UK_mean_all<-cbind(Park_UK_mean_all,graphcolour=c(rep("#D55E00",nrow(Park_UK_mean_all)/4),
                                                     rep("#009E73",nrow(Park_UK_mean_all)/4),
                                                     rep("#0072B2",nrow(Park_UK_mean_all)/4),
                                                     rep("#F0E442",nrow(Park_UK_mean_all)/4)))
                        
levels(Park_UK_mean_all$variable)[2]<-'Northern Ireland'

UK_graphs<-ggplot(data=Park_UK_mean_all,aes(x=Date,y=value)) +
         
         geom_col(position = position_dodge(width=0.2), fill=Park_UK_mean_all$graphcolour, colour='black') +
         
         geom_errorbar(aes(ymin=value-se,
                           ymax=value+se), 
                           width=0, position = position_dodge(width=0.9)) +
         
         coord_cartesian(ylim=c(-60,160)) +
         
         geom_hline(yintercept=0) + 
         
         theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
               panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
               panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
               strip.background = element_blank(),
               strip.text.y = element_text(size=15, angle=0)) +
  
          facet_wrap(~variable, nrow=4, strip.position='right') +
  
          xlab("Date") +
          ylab("Parks percentage change from baseline")

UK_graphs

# Extract the mean and SE values for only May ------------------------------------------------------------
Park_UK_mean_may<-Park_UK_mean[Park_UK_mean$Date >= "2020-05-01" & Park_UK_mean$Date <= "2020-05-31",]
Park_UK_SE_may<-Park_UK_SE[Park_UK_SE$Date >= "2020-05-01" & Park_UK_SE$Date <= "2020-05-31",]

# May Plots -------------------------------------------------------------------
#Creates a graph for Englands' districts.

ENG_graph_may<-
  ggplot(data=Park_UK_mean_may,aes(x=Date,y=England, group=Date)) +
  
  geom_col(position = position_dodge(width=0.2)) +
  
  geom_errorbar(aes(ymin=England-Park_UK_SE_may$England,
                    ymax=England+Park_UK_SE_may$England), 
                width=0, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  
  ggtitle("England")+xlab("Date")+ylab("Parks percentage change from baseline")


#Creates the graph for the Wales districts
WAL_graph_may<-
  ggplot(data=Park_UK_mean_may,aes(x=Date,y=Wales, group=Date)) +
  
  geom_col(position = position_dodge(width=0.2)) +
  
  geom_errorbar(aes(ymin=Wales-Park_UK_SE_may$Wales,
                    ymax=Wales+Park_UK_SE_may$Wales), 
                width=0, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  
  ggtitle("Wales")+xlab("Date")+ylab("Parks percentage change from baseline")

#Creates a graph for Scotland' district
SCOT_graph_may<-
  ggplot(data=Park_UK_mean_may,aes(x=Date,y=Scotland, group=Date)) +
  
  geom_col(position = position_dodge(width=0.2)) +
  
  geom_errorbar(aes(ymin=Scotland-Park_UK_SE_may$Scotland,
                    ymax=Scotland+Park_UK_SE_may$Scotland), 
                width=0, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  
  ggtitle("Scotland")+xlab("Date")+ylab("Parks percentage change from baseline")


#Creates a graph for Northern Irelands' district
NI_graph_may<-
  ggplot(data=Park_UK_mean_may,aes(x=Date,y=N_Ireland, group=Date)) +
  
  geom_col(position = position_dodge(width=0.2)) +
  
  geom_errorbar(aes(ymin=N_Ireland-Park_UK_SE_may$N_Ireland,
                    ymax=N_Ireland+Park_UK_SE_may$N_Ireland), 
                width=0, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                        colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                        colour = "grey")) +
  
  ggtitle("NI")+xlab("Date")+ylab("Parks percentage change from baseline")


#This plots all the graphs onto one image.
grid.arrange(ENG_graph_may,WAL_graph_may,SCOT_graph_may,NI_graph_may, nrow = 4)


#better panel plot

Park_UK_mean_all_may<-melt(Park_UK_mean_may,'Date')
Park_UK_mean_all_may<-cbind(Park_UK_mean_all_may,se=melt(Park_UK_SE_may,'Date')[,3])
Park_UK_mean_all_may<-cbind(Park_UK_mean_all_may,graphcolour=c(rep("#D55E00",nrow(Park_UK_mean_all_may)/4),
                                                       rep("#009E73",nrow(Park_UK_mean_all_may)/4),
                                                       rep("#0072B2",nrow(Park_UK_mean_all_may)/4),
                                                       rep("#F0E442",nrow(Park_UK_mean_all_may)/4)))

levels(Park_UK_mean_all_may$variable)[2]<-'Northern Ireland'

UK_graphs<-ggplot(data=Park_UK_mean_all_may,aes(x=Date,y=value)) +
  
  geom_col(position = position_dodge(width=0.2), fill=Park_UK_mean_all_may$graphcolour, colour='black') +
  
  geom_errorbar(aes(ymin=value-se,
                    ymax=value+se), 
                width=0, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        strip.background = element_blank(),
        strip.text.y = element_text(size=15, angle=0)) +
  
  facet_wrap(~variable, nrow=4, strip.position='right') +
  
  xlab("Date") +
  ylab("Parks percentage change from baseline")

UK_graphs

       
