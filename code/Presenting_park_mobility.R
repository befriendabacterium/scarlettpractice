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
#install.packages("plyr")
library(plyr)
#install.packages("RColorBrewer")
library(RColorBrewer)

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

# Make one combined data set ----------------------------------------------
#Combine all the country columns and generate a seperate column for countries.
Park_UK<-gather(Park_UK_mean,Date)
Park_UK<-Park_UK %>% add_column(date = (rep(c(Park_UK_mean$Date),times = 4)), .before = which(colnames(Park_UK)=="Date"))
#Add the SE column by country and date
Park_UK<-Park_UK %>% add_column(SE = (gather(Park_UK_SE,Date)[,2]), .after = which(colnames(Park_UK)=="value"))
#Renaming colnames
colnames(Park_UK) = c("Date", "Country","Mean_v","SE_v")
#Add lower SE range
Park_UK<-Park_UK %>% mutate (Min = Mean_v - SE_v)
#Add higher SE range
Park_UK<-Park_UK %>% mutate (Max = Mean_v + SE_v)

# Plots for the whole of time series -------------------------------------------------------------------
#Graph with increased thickness
ggplot(data = Park_UK, aes(x = Date, y = Mean_v, colour = Country))+ 
  geom_line(size = 0.5)+
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("")+xlab("Date")+ylab("Parks percentage change from baseline")

ggsave('C:/Users/scarl/OneDrive/Documents/Jobs/Summer internships/Exeter summer internship 2020/R/Graphs/Park_mobility_whole_thicker_lines.pdf', width = 6, height = 4, dpi = 400)

#Colour friendly palette one
ggplot(data = Park_UK, aes(x = Date, y = Mean_v, colour = Country))+ 
  geom_line(size = 0.3)+
  scale_color_brewer(palette = "BrBG") +
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("")+xlab("Date")+ylab("Parks percentage change from baseline")

ggsave('C:/Users/scarl/OneDrive/Documents/Jobs/Summer internships/Exeter summer internship 2020/R/Graphs/Park_mobility_whole_colourblind_friendly_1.pdf', width = 6, height = 4, dpi = 400)

#Colour blind friendly 2
ggplot(data = Park_UK, aes(x = Date, y = Mean_v, colour = Country))+ 
  geom_line(size = 0.3)+
  scale_color_brewer(palette = "Dark2") +
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("")+xlab("Date")+ylab("Parks percentage change from baseline")

ggsave('C:/Users/scarl/OneDrive/Documents/Jobs/Summer internships/Exeter summer internship 2020/R/Graphs/Park_mobility_colourblind_friendly_2.pdf', width = 6, height = 4, dpi = 400)

# Extract the mean and SE values for only May ------------------------------------------------------------
Park_UK_may<-Park_UK[Park_UK$Date >= "2020-05-01" & Park_UK$Date <= "2020-05-31",]

# May Plot -------------------------------------------------------------------
ggplot(data = Park_UK_may, aes(x = Date, y = Mean_v, colour = Country))+ 
  geom_line(size = 0.3)+
  scale_color_brewer(palette = "BrBG")+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid")) +
  ggtitle("")+xlab("Date")+ylab("Parks percentage change from baseline")
ggsave('C:/Users/scarl/OneDrive/Documents/Jobs/Summer internships/Exeter summer internship 2020/R/Graphs/Park_mobility_may_colourblind_friendly.pdf', width = 6, height = 4, dpi = 400)

# Plots for the whole of time series avalible to us -------------------------------------------------------------------
#Creates the graph referring to England districts. 
ENG_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=England))+
  geom_line(size = 0.2)+
  geom_ribbon(aes(ymin=England-Park_UK_SE$England,
                  ymax=England+Park_UK_SE$England),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("England")+xlab("Date")+ylab("Parks percentage change from baseline")


#Creates the graph for the Wales districts
WAL_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=Wales))+
  geom_line(size = 0.2)+
  geom_ribbon(aes(ymin=Wales-Park_UK_SE$Wales,
                  ymax=Wales+Park_UK_SE$Wales),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("Wales")+xlab("Date")+ylab("Parks percentage change from baseline")

#Creates a graph for Scotlands' districts.
SCOT_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=Scotland))+
  geom_line(size = 0.2)+
  geom_ribbon(aes(ymin=Scotland-Park_UK_SE$Scotland,
                  ymax=Scotland+Park_UK_SE$Scotland),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("Scotland")+xlab("Date")+ylab("Parks percentage change from baseline")

#Creates a graph for Northern Irelands' district
NI_graph<-
  ggplot(data=Park_UK_mean,aes(x=Date,y=N_Ireland))+
  geom_line(size = 0.2)+
  geom_ribbon(aes(ymin=N_Ireland-Park_UK_SE$N_Ireland,
                  ymax=N_Ireland+Park_UK_SE$N_Ireland),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("N Ireland")+xlab("Date")+ylab("Parks percentage change from baseline")

#This plots all the graphs onto one image.
grid.arrange(ENG_graph,WAL_graph,SCOT_graph,NI_graph, nrow = 2)
Whole_park<-arrangeGrob(ENG_graph,WAL_graph,SCOT_graph,NI_graph, nrow = 2)
ggsave('C:/Users/scarl/OneDrive/Documents/Jobs/Summer internships/Exeter summer internship 2020/R/Graphs/Park_mobility_whole_without_grids.pdf',Whole_park, width = 8 , height = 6, dpi = 400)

# Extract the mean and SE values for only May ------------------------------------------------------------
Park_UK_mean_may<-Park_UK_mean[Park_UK_mean$Date >= "2020-05-01" & Park_UK_mean$Date <= "2020-05-31",]
Park_UK_SE_may<-Park_UK_SE[Park_UK_SE$Date >= "2020-05-01" & Park_UK_SE$Date <= "2020-05-31",]
#Calculates the average mean for percentage change in England across the whole of May.
Park_England_average_may<-mean(Park_UK_mean_may$England)

# May Plots -------------------------------------------------------------------
#Creates a graph for Englands' districts.
ENG_graph_may<-
  ggplot(data=Park_UK_mean_may,aes(x=Date,y=England))+
  geom_line(size = 0.2)+
  geom_ribbon(aes(ymin=England-Park_UK_SE_may$England,
                  ymax=England+Park_UK_SE_may$England),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("England")+xlab("Date")+ylab("Parks percentage change from baseline")


#Creates a graph for Wales' districts
WAL_graph_may<-
  ggplot(data=Park_UK_mean_may,aes(x=Date,y=Wales))+
  geom_line(size = 0.2)+
  geom_ribbon(aes(ymin=Wales-Park_UK_SE_may$Wales,
                  ymax=Wales+Park_UK_SE_may$Wales),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("Wales")+xlab("Date")+ylab("Parks percentage change from baseline")

#Creates a graph for Scotlands' districts
SCOT_graph_may<-
  ggplot(data=Park_UK_mean_may,aes(x=Date,y=Scotland))+
  geom_line(size = 0.2)+
  geom_ribbon(aes(ymin=Scotland-Park_UK_SE_may$Scotland,
                  ymax=Scotland+Park_UK_SE_may$Scotland),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("Scotland")+xlab("Date")+ylab("Parks percentage change from baseline")

#Creates a graph for Northern Irelands' districts
NI_graph_may<-
  ggplot(data=Park_UK_mean_may,aes(x=Date,y=N_Ireland))+
  geom_line(size = 0.2)+
  geom_ribbon(aes(ymin=N_Ireland-Park_UK_SE_may$N_Ireland,
                  ymax=N_Ireland+Park_UK_SE_may$N_Ireland),alpha=0.4)+
  coord_cartesian(ylim=c(-60,160))+
  geom_hline(yintercept=0)+ 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "black",
                                        size = 1, linetype = "solid"))+
  ggtitle("N Ireland")+xlab("Date")+ylab("Parks percentage change from baseline")

#Creates one plot that contains all the four countrys graphs. 
grid.arrange(ENG_graph_may,WAL_graph_may,SCOT_graph_may,NI_graph_may, nrow = 2)
Whole_park<-arrangeGrob(ENG_graph_may,WAL_graph_may,SCOT_graph_may,NI_graph_may, nrow = 2)
ggsave('C:/Users/scarl/OneDrive/Documents/Jobs/Summer internships/Exeter summer internship 2020/R/Graphs/Park_mobility_may.pdf',Whole_park, width = 8 , height = 6, dpi = 400)
