# Start -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)

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
getwd()
Global_Mobility_Report<-read.csv("inputdata/Global_Mobility_Report_030620.csv")
Global_Mobility_Report$date<-as.Date(Global_Mobility_Report$date,format = "%d/%m/%y")
Global_Mobility_Report$parks_percent_change_from_baseline<-as.numeric(Global_Mobility_Report$parks_percent_change_from_baseline)

# Subset the data to only present the UK data, separated by GB cod --------
UK<-subset(Global_Mobility_Report,country_region_code == "GB")
#UK<-UK[!(UK$sub_country%in%""), ]

# Assigning country to UK districts ---------------------------------------
#saveRDS(Districts_by_country,"Districts_by_country.RDS")
Districts_by_country<-readRDS("inputdata/Districts_by_country.RDS")

# Code to assign districts to the new data set ----------------------------
#England
England_col<-UK[(UK$sub_region_1%in%Districts_by_country[[1]]),]
#plyr::count(UK$sub_region_1%in%Districts_by_country[[1]])
England_col$sub_country<-England_col$sub_country%>%replace_na("England")
#Wales
Wales_col<-UK[(UK$sub_region_1%in%Districts_by_country[[2]]),]
Wales_col$sub_country<-Wales_col$sub_country%>%replace_na("Wales")
#Scotland
Scotland_col<-UK[(UK$sub_region_1%in%Districts_by_country[[3]]),]
Scotland_col$sub_country<-Scotland_col$sub_country%>%replace_na("Scotland")
#Northern Ireland
N_Ireland_col<-UK[(UK$sub_region_1%in%Districts_by_country[[4]]),]
N_Ireland_col$sub_country<-N_Ireland_col$sub_country%>%replace_na("N Ireland")
#Connect them all together
UK<-rbind(England_col,Wales_col,Scotland_col,N_Ireland_col)

# Subset the data to only present park data -------------------------------
Park_UK <-subset(UK,select=c(sub_region_1,sub_country,date,parks_percent_change_from_baseline))
#Park_UK<-na.omit(Park_UK)

# Calculating UK mean and SE for each country division of the UK ----------
Park_UK_mean<-tapply(Park_UK$parks_percent_change_from_baseline, list(Park_UK$sub_country,Park_UK$date),mean, na.rm=T)
Park_UK_SE<-tapply(Park_UK$parks_percent_change_from_baseline, list(Park_UK$sub_country,Park_UK$date),std.error, na.rm=T)

# Converting it into a dataframe -------------------------------------------
#Mean Values
UK_mean_df<-as.data.frame(t(Park_UK_mean))
UK_mean_df<- rownames_to_column(UK_mean_df,var="Date")
names(UK_mean_df)<-c("Date","England","N_Ireland","Scotland","Wales")
UK_mean_df$Date<-as.Date(UK_mean_df$Date)
#SE values
UK_SE_df<-as.data.frame(t(Park_UK_SE))
UK_SE_df<-rownames_to_column(UK_SE_df,var="Date")
names(UK_SE_df)<-c("Date","England","N_Ireland","Scotland","Wales")
UK_SE_df$Date<-as.Date(UK_SE_df$Date)

# Plots -------------------------------------------------------------------
#England
ENG_graph<-
  ggplot(data=UK_mean_df,aes(x=Date,y=England))+
  geom_line()+
  geom_ribbon(aes(ymin=England-UK_SE_df$England,
                  ymax=England+UK_SE_df$England),alpha=0.4)+
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
  ggplot(data=UK_mean_df,aes(x=Date,y=Wales))+
  geom_line()+
  geom_ribbon(aes(ymin=Wales-UK_SE_df$Wales,
                  ymax=Wales+UK_SE_df$Wales),alpha=0.4)+
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
  ggplot(data=UK_mean_df,aes(x=Date,y=Scotland))+
  geom_line()+
  geom_ribbon(aes(ymin=Scotland-UK_SE_df$Scotland,
                  ymax=Scotland+UK_SE_df$Scotland),alpha=0.4)+
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
  ggplot(data=UK_mean_df,aes(x=Date,y=N_Ireland))+
  geom_line()+
  geom_ribbon(aes(ymin=N_Ireland-UK_SE_df$N_Ireland,
                  ymax=N_Ireland+UK_SE_df$N_Ireland),alpha=0.4)+
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
