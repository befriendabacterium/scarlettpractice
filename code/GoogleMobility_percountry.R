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
#install.packages('reshape2')
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
UK$sub_country[(UK$sub_region_1%in%Districts_by_country[[4]])]<-"Northern Ireland"

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

# wHOLE SERIES: PLOT PREP -------------------------------------------------------------------

#transform the dataframe so all countries are in one column
Park_UK_mean_all<-melt(Park_UK_mean,'Date')
#append the standard error column to it
Park_UK_mean_all<-cbind(Park_UK_mean_all,se=melt(Park_UK_SE,'Date')[,3])
#make colourblind pallette for country
cbPalette_country <- c("#D55E00","#009E73","#0072B2","#F0E442")

# WHOLE SERIES COLOURED BY COUNTRY ---------------------------------------

UK_graphs_colbycountry<-ggplot(data=Park_UK_mean_all,aes(x=Date,y=value, fill=Park_UK_mean_all$variable)) +
  
  geom_col(position = position_dodge(width=0.2), colour='black', size=0.25) +
  
  geom_errorbar(aes(ymin=value-se,
                    ymax=value+se), 
                    width=0, size=0.25, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  
  scale_fill_manual(name = "Country", values=cbPalette_country , 
                    breaks=c('England','Northern Ireland','Scotland','Wales'),
                    labels=c('England','Northern Ireland','Scotland','Wales'))+
  
  
  facet_wrap(~variable, nrow=4, strip.position='top') +
  
  xlab("Date") +
  ylab("Parks percentage change from baseline")

UK_graphs_colbycountry

ggsave(file="outputdata/wholeseries_colbycountry.pdf", width = 210, height = 297, units = "mm")


# WHOLE SERIES COLOURED BY WEEKDAYS ---------------------------------------

#make colourblind pallette for weekdays
cbPalette_weekday <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

                     
UK_graphs_colbyweekday<-ggplot(data=Park_UK_mean_all,aes(x=Date,y=value, fill=weekdays(Park_UK_mean_all$Date))) +
         
         geom_col(position = position_dodge(width=0.2), colour='black', size=0.25) +
         
         geom_errorbar(aes(ymin=value-se,
                           ymax=value+se), 
                           width=0, size=0.25, position = position_dodge(width=0.9)) +
         
         coord_cartesian(ylim=c(-60,160)) +
         
         geom_hline(yintercept=0) + 
         
         theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
               panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
               panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
               strip.background = element_blank(),
               strip.text.y = element_text(size=15, angle=0)) +
  
          scale_fill_manual(name = "Weekday", values=cbPalette_weekday[c(2,6,7,5,1,3,4)], 
                    breaks=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                    labels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))+
  
  
  
          facet_wrap(~variable, nrow=4, strip.position='top') +
  
          xlab("Date") +
          ylab("Parks percentage change from baseline")

UK_graphs_colbyweekday

ggsave(file="outputdata/wholeseries_colbyweekday.pdf", width = 210, height = 297, units = "mm")

# MAY SERIES: PLOT PREP -------------------------------------------------------------------

Park_UK_mean_may<-Park_UK_mean[Park_UK_mean$Date >= "2020-05-01" & Park_UK_mean$Date <= "2020-05-31",]
Park_UK_SE_may<-Park_UK_SE[Park_UK_SE$Date >= "2020-05-01" & Park_UK_SE$Date <= "2020-05-31",]

#transform the dataframe so all countries are in one column
Park_UK_mean_may<-melt(Park_UK_mean_may,'Date')
#append the standard error column to it
Park_UK_mean_may<-cbind(Park_UK_mean_may,se=melt(Park_UK_SE_may,'Date')[,3])
#make colourblind pallette for country
cbPalette_country <- c("#D55E00","#009E73","#0072B2","#F0E442")


# May Plots -------------------------------------------------------------------
#Creates graphs for May data only

# MAY SERIES COLOURED BY COUNTRY ---------------------------------------

UK_graphs_colbycountry_may<-ggplot(data=Park_UK_mean_may,aes(x=Date,y=value, fill=Park_UK_mean_may$variable)) +
  
  geom_col(position = position_dodge(width=0.2), colour='black', size=0.25) +
  
  geom_errorbar(aes(ymin=value-se,
                    ymax=value+se), 
                width=0, size=0.25, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        strip.background = element_blank(),
        strip.text = element_blank()) +
  
  scale_fill_manual(name = "Country", values=cbPalette_country , 
                    breaks=c('England','Northern Ireland','Scotland','Wales'),
                    labels=c('England','Northern Ireland','Scotland','Wales'))+
  
  
  facet_wrap(~variable, nrow=4, strip.position='top') +
  
  xlab("Date") +
  ylab("Parks percentage change from baseline")

UK_graphs_colbycountry_may

ggsave(file="outputdata/mayseries_colbycountry.pdf", width = 210, height = 297, units = "mm")

# MAY SERIES COLOURED BY WEEKDAYS ---------------------------------------

#make colourblind pallette for weekdays
cbPalette_weekday <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


UK_graphs_colbyweekday_may<-ggplot(data=Park_UK_mean_may,aes(x=Date,y=value, fill=weekdays(Park_UK_mean_may$Date))) +
  
  geom_col(position = position_dodge(width=0.2), colour='black', size=0.25) +
  
  geom_errorbar(aes(ymin=value-se,
                    ymax=value+se), 
                width=0, size=0.25, position = position_dodge(width=0.9)) +
  
  coord_cartesian(ylim=c(-60,160)) +
  
  geom_hline(yintercept=0) + 
  
  theme(panel.background = element_rect(fill = "white", colour = "black", size = 1, linetype = "solid"),
        panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = "grey"), 
        panel.grid.minor = element_line(size = 0.25, linetype = 'solid',colour = "grey"),
        strip.background = element_blank(),
        strip.text.y = element_text(size=15, angle=0)) +
  
  scale_fill_manual(name = "Weekday", values=cbPalette_weekday[c(2,6,7,5,1,3,4)], 
                    breaks=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'),
                    labels=c('Monday','Tuesday','Wednesday','Thursday','Friday','Saturday','Sunday'))+
  
  
  
  facet_wrap(~variable, nrow=4, strip.position='top') +
  
  xlab("Date") +
  ylab("Parks percentage change from baseline")

UK_graphs_colbyweekday_may

ggsave(file="outputdata/mayseries_colbyweekday.pdf", width = 210, height = 297, units = "mm")
