# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)
setwd('Github/parksinthepandemic/code/')

# LOAD PACKAGES -----------------------------------------------------------

#install.packages('XML')
library(XML) # HTML processing
#install.packages('RCurl')
library(RCurl)
#install.packages('rvest')
library(rvest)
#install.packages("httr")
library(httr)
#install.packages("readr")
library(readr)
#install.packages("stringr")
library(stringr)
#install.packages("data.table")
library(data.table)
#install.packages
library(dplyr)
#install.packages('tibble')
library(tibble)
#install.packages("tidyr")
library(tidyr)
#install.packages("ggplot2")
library(ggplot2)


# start -------------------------------------------------------------------
#load Met Office data for May
metdata_may<-read.csv('inputdata/UKcovidreportingregions_metoffice_global_daily_bbox_20200501-20200531.csv')
#load Met Office data for Jan st-April 19th
metdata_early<-read.csv('inputdata/UKcovidreportingregions_metoffice_global_daily_bbox_20200101-20200419.csv')
#make a vector of dates within Google's Basline period
baselinerange<-seq(as.Date('2020-01-02'),as.Date('2020-02-06'),1)
#G
metdata_baseline<-metdata_early[as.Date(metdata_early$date)%in%baselinerange,]

googledata<-read.csv('inputdata/Global_Mobility_Report.csv')
googledata<-googledata[googledata$country_region=='United Kingdom',]

googledata_may<-googledata[month(googledata$date)==5,]
googledata_may_york<-googledata_may[googledata_may$sub_region_1=='York',]
metdata_may_york<-metdata_may[metdata_may$name=='York',]
maydates<-metdata_may_york$date
maydays<-weekdays(as.Date(maydates))

metdata_may_york<-metdata_may_york[,5:ncol(metdata_may_york)]
metdata_baseline_york<-metdata_baseline[metdata_may$name=='York',]
metdata_baseline_york$weekday<-weekdays(as.Date(metdata_baseline_york$date))
metdata_baseline_york<-metdata_baseline_york[,5:ncol(metdata_baseline_york)]

baselineweather<-aggregate(metdata_baseline_york,list(metdata_baseline_york$weekday), median)

wday<-c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

baselineweather<-baselineweather[match(wday,baselineweather$Group.1),]
daysofweek<-metdata_baseline_york$weekday
baselineweather<-baselineweather[,-c(1,22)]
metdata_baseline_york<-metdata_baseline_york[,-21]

metdata_may_york_rel2baseline<-metdata_may_york

for (w in 1:7){
  for (c in 1:20){
  temp<-metdata_may_york[maydays%in%wday[w],c]-baselineweather[w,c]
  metdata_may_york_rel2baseline[maydays%in%wday[w],c]<-temp/baselineweather[w,c]*100
    }
}

matched<-match(googledata_may_york$date,maydates)
metdata_may_york_rel2baseline<-metdata_may_york[matched,]

plot(googledata_may_york$parks_percent_change_from_baseline~as.Date(googledata_may_york$date),ylim=c(-300,300), col='darkgreen')
lines(googledata_may_york$parks_percent_change_from_baseline[!is.na(googledata_may_york$parks_percent_change_from_baseline)]
      ~as.Date(googledata_may_york$date)[!is.na(googledata_may_york$parks_percent_change_from_baseline)], col='darkgreen')
points(metdata_may_york_rel2baseline$short_wave_radiation_mean_mean..W.m.2.~as.Date(googledata_may_york$date),col='red')
lines(metdata_may_york_rel2baseline$short_wave_radiation_mean_mean..W.m.2.~as.Date(googledata_may_york$date),col='red')
points(metdata_may_york_rel2baseline$precipitation_flux_mean_mean..kg.m.2.s.1.~as.Date(googledata_may_york$date),col='blue')
lines(metdata_may_york_rel2baseline$precipitation_flux_mean_mean..kg.m.2.s.1.~as.Date(googledata_may_york$date),col='blue')

library(rpart)
model<-rpart(googledata_may_york$parks_percent_change_from_baseline[!is.na(googledata_may_york$parks_percent_change_from_baseline)]
                ~as.Date(googledata_may_york$date)[!is.na(googledata_may_york$parks_percent_change_from_baseline)]
                +metdata_may_york_rel2baseline$short_wave_radiation_mean_mean..W.m.2.[!is.na(googledata_may_york$parks_percent_change_from_baseline)])

lines(predict(model)~as.Date(googledata_may_york$date)[!is.na(googledata_may_york$parks_percent_change_from_baseline)])
length(predict(model))
lines(metdata_may_york_rel2baseline[,3]~as.Date(googledata_may_york$date),col='orange')



