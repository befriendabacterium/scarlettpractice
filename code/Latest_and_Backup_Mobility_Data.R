# START -------------------------------------------------------------------
rm(list=ls())
set.seed(1234)

# LOAD PACKAGES -----------------------------------------------------------

#install.packages('XML')
library(XML) # HTML processing
#install.packages('RCurl')
library(RCurl)
#install.packages('rvest')
library(rvest)
#install.packages('stringr')
library(stringr)

#DOWNLOAD New data set -----------------------------------------------------------
#Extracting the recent csv file. 
base.url<-"https://www.google.com/covid19/mobility/"
base.url2<-getURL(base.url)
parsed<-htmlParse(base.url2)
doc.links<-xpathSApply(parsed,path = "//a",xmlGetAttr,"href")
csv.url<- as.character(doc.links[grep('csv', doc.links)])
Google_Mobility_data<-read.csv(csv.url, header = TRUE)

# Making a code for back up -----------------------------------------------
#Extracting text from the google mobility page.
read_text<-read_html(base.url2)
text<-html_text(read_text)
#Extracting the data via inbetween strings
read_date<-str_match(text, "Reports created\\s*(.*?)\\s*.By")
#Assigning a value to the date
read_date_1<-read_date[2]
#Creating the name of the file using the string function
Report=paste("Global_Mobility_Report_",read_date_1,sep="")
#Saving the Back up data set
setwd("~/GitHub/parksinthepandemic/code/inputdata/Back up data")
save(Google_Mobility_data,file = paste(Report,".csv",sep=""))
#Saving the Current data set
setwd("~/GitHub/parksinthepandemic/code/inputdata")
save(Google_Mobility_data,file = "Current_Global_Mobility_Report.csv")
#Resetting the work directory to work for future code
