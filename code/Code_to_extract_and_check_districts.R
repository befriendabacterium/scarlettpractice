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
#install.packages("httr")
library(httr)
#install.packages("readr")
library(readr)
#install.packages("stringr")
library(stringr)
#install.packages("magrittr")
library(magrittr)
#install.packages("data.table")
library(data.table)
#install.packages
library(dplyr)

# Code to download list file of districts ---------------------------------
Districts_by_country<-readRDS("Districts_by_country.RDS")

# Checking the districts match up in N_Ireland -----------------------------------------
NI.url<-"https://en.wikipedia.org/wiki/Local_government_in_Northern_Ireland#Local_Government_Districts"
NI.url2<-GET(NI.url)
NI.doc<-readHTMLTable(doc = content(NI.url2,"text"))
NI.df<-data.frame(NI.doc[2])
colnames(NI.df)=NI.df[1,]
NI_districts<-NI.df[-1,1]
NI_Districts_data<-Districts_by_country[[4]]
match(NI_districts,NI_Districts_data)

# Checking the districts match up in Wales --------------------------------
W.url<-"https://en.wikipedia.org/wiki/Local_government_in_Wales"
W.url2<-GET(W.url)
#Trying to read in the attached files to use title instead.
W.doc<-readHTMLTable(doc = content(W.url2,"text"))
W.df<-data.frame(W.doc[2])
W.district<-W.df[1,2]
W.paste<-strsplit(as.character(W.district),'\n')
W.district_2<-unlist(W.paste,recursive = T)
W.district_3<-gsub("\\(.*?)","",W.district_2)
W.district_4<-gsub("[^[:alnum:]]","",W.district_3)
W_Districts_data<-gsub("[^[:alnum:]]","",Districts_by_country[[2]])
match(W.district_4,W_Districts_data)

#Does match but doesn't word for word - e.g. removed principle area.- if you click on direct link then title has it.

# Checking the districts match up in Scotland -----------------------------
S.url<-"https://en.wikipedia.org/wiki/Subdivisions_of_Scotland"
S.url2<-GET(S.url)
S.doc<-readHTMLTable(doc = content(S.url2,"text"))
S.df<-data.frame(S.doc[3])
colnames(S.df)=S.df[1,]
S.df<-S.df[-1,]
S_districts<-str_sort(S.df[,2])
S_districts<-S.df[,2]
S_Districts_data<-Districts_by_country[[3]]
match(S_Districts_data, S_districts)
#Does match but words are not exact again, conflict for city of Aberdeen = Aberdeen city

#Checking the districts match up in England
E1.url<-"https://en.wikipedia.org/wiki/Unitary_authorities_of_England"
E1.url2<-GET(E1.url)
E1.doc<-readHTMLTable(doc = content(E1.url2,"text"))
E1.df<-data.frame(E1.doc[3])
colnames(E1.df)=E1.df[1,]
E1.df<-E1.df[-1,]
E1_districts<-gsub("\\[.*?]","",E1.df[,1])
#Next
E2.url<-"https://en.wikipedia.org/wiki/Non-metropolitan_district"
E2.url2<-GET(E2.url)
E2.doc<-readHTMLTable(doc = content(E2.url2,"text"))
E2.df<-data.frame(E2.doc[3])
colnames(E2.df)=E2.df[1,]
E2_districts<-E2.df[-1,1]
#Next
E3.url<-"https://en.wikipedia.org/wiki/Metropolitan_borough"
E3.url2<-GET(E3.url)
E3.doc<-readHTMLTable(doc = content(E3.url2,"text"))
E3.df<-data.frame(E3.doc[2])
colnames(E3.df)=E3.df[1,]
E3_districts<-E3.df[-1,1]
#Combining each wiki into one.
E_districts<-c(E1_districts,E2_districts,E3_districts)
E_Districts_data<-Districts_by_country[[1]]
#Matching them
match(E_districts,E_Districts_data)
