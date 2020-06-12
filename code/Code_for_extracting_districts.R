# Start -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)

# Load packages ----------------------------------------------------------
#install.packages('plotrix')
library(plotrix)
#install.packages('tibble')
library(tibble)
#install.packages("tidyr")
library(tidyr)

# Extracting the districts ------------------------------------------------
Global_Mobility_Report<-read.csv("inputdata/Global_Mobility_UK_Country.csv")
Global_Mobility_Report$date<-as.Date(Global_Mobility_Report$date,format = "%d/%m/%y")
#Identify UK only countries
UK<-subset(Global_Mobility_Report,country_region_code == "GB", select = c(sub_country,sub_region_1))
#Remove unrelated NA values
UK<-UK[!(UK$sub_country%in%""), ]
#Removing duplicates to obtain a list
UK<-UK[!duplicated(UK),]
#Determine the districts for each country into a vector and add additional new ones they have added.
#England
England_Districts<-subset(UK,sub_country == "England",select=c(sub_region_1))[,1]
#Wales
Wales_Districts<-subset(UK,sub_country == "Wales",select=c(sub_region_1))[,1]
#Scotland
Scotland_Districts<-subset(UK,sub_country == "Scotland", select=c(sub_region_1))[,1]
#N Ireland
N_Ireland_Districts<-subset(UK,sub_country == "N Ireland", select=c(sub_region_1))[,1]
#List
Districts_by_country<-list(England_Districts,Wales_Districts,Scotland_Districts,N_Ireland_Districts)
saveRDS(Districts_by_country,"inputdata/Districts_by_country.RDS")
