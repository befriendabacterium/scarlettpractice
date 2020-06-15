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
#install.packages("data.table")
library(data.table)
#install.packages
library(dplyr)
#install.packages('tibble')
library(tibble)
#install.packages("tidyr")
library(tidyr)

# Code to download list file of districts ---------------------------------
Districts_by_country<-readRDS("Districts_by_country.RDS")
#Creating a list of all the wikipedia links required
Wiki_links<-list("https://en.wikipedia.org/wiki/Local_government_in_Northern_Ireland#Local_Government_Districts",
"https://en.wikipedia.org/wiki/Local_government_in_Wales",
"https://en.wikipedia.org/wiki/Subdivisions_of_Scotland",
"https://en.wikipedia.org/wiki/Unitary_authorities_of_England",
"https://en.wikipedia.org/wiki/Non-metropolitan_district",
"https://en.wikipedia.org/wiki/Metropolitan_borough"
)



# Checking the districts match up in N_Ireland -----------------------------------------
#Creating a data frame based of the second table on the wikipedia page
NI.df<-as.data.frame(readHTMLTable(doc = content((GET(Wiki_links[[1]])),"text"))[2])
#Change the column names to the ones placed in the data frame
colnames(NI.df)=NI.df[1,]
#Remove the first row(was originally the column names)and select the first column
NI_districts<-NI.df[-1,1]
#Check to see if the districts in both the Google mobility data and the wikipedia page are the same.
match(NI_districts,Districts_by_country[[4]])

# Checking the districts match up in Wales --------------------------------
#Extract and create the data frame from data on the wikipedia page corresponding to the second table.
W.df<-data.frame(readHTMLTable(doc=content((GET(Wiki_links[[2]])),"text"))[2])
#Extract the countries associated to the Wales districts then extract one box and obtain a string seperated out by n/ and then un list.
W_districts<-strsplit(as.character(W.df[1,2]),'\n') %>% 
  unlist(recursive = T) 
#Remove special charactersand bracketed information
W_districts<-str_trim(gsub("\\(.*?)|\\???","",W_districts))
#Checks for match
match(W_district,Districts_by_country[[2]])

# Checking the districts match up in Scotland -----------------------------
#Extract the table from the wikipedia page corresponding to the Scottish districts.(Table 3)
S.df<-data.frame(readHTMLTable(doc = content ((GET(Wiki_links[[3]])),"text"))[[3]])
#Make the first row names into the column names 
colnames(S.df)=S.df[1,]
#Remove the first row (once contained the now column names)and select the second column
S_districts<-S.df[-1,2]
match(S_districts, Districts_by_country[[3]])

#Checking the districts match up in England

#Extracting the unitary authorities data frame from the wikipedia page.
E1.df<-data.frame(readHTMLTable(doc = content((GET(Wiki_links[[4]])),"text"))[3])
#Changing the column names to first row of names
colnames(E1.df)=E1.df[1,]
#Just the E1 characters
E1_districts<-gsub("\\[.*?]","",E1.df[-1,1])

#Creating a data frame for the non-metropolitan_districts from the wikipedia page.
E2.df<-data.frame(readHTMLTable(doc = content((GET(Wiki_links[[5]])),"text"))[3])
#Changing the first row into the column names
colnames(E2.df)=E2.df[1,]
#Just E2 districts
E2_districts<-E2.df[-1,1]

#Creating a metropolitan boroughs data frame from the wikipedia page.
E3.df<-data.frame(readHTMLTable(doc = content((GET(Wiki_links[[6]])),"text"))[2])
#Changing the first row into the column names. 
colnames(E3.df)=E3.df[1,]
#Just E3 districts
E3_districts<-E3.df[-1,1]

#Combining All of the districts into one vector while removing full stops from parts of E1 vector. 
E_districts<-c(E1_districts,E2_districts,E3_districts,"Greater London")
#Matching them
match(E_districts,Districts_by_country[[1]])


# Making a data frame for all of the above --------------------------------
Wiki.df<-as.data.frame(c(E_districts,NI_districts,S_districts,W_districts))
#Changing the initial column name and adding new columns
colnames(Wiki.df)<-c("wiki_district")
Wiki.df[,"wiki_type"]<- NA 
Wiki.df[,"wiki_link"]<-NA

#Adding each type and link to each list
#Northern ireland link
Wiki.df$wiki_link[(Wiki.df$wiki_district%in%NI_districts)]<-Wiki_links[[1]]
Wiki.df$wiki_type[(Wiki.df$wiki_district%in%NI_districts)]<-"districts"
#Wales link
Wiki.df$wiki_link[(Wiki.df$wiki_district%in%W_districts)]<-Wiki_links[[2]]
Wiki.df$wiki_type[(Wiki.df$wiki_district%in%W_districts)]<-"principle_area"
#Scotland link
Wiki.df$wiki_link[(Wiki.df$wiki_district%in%S_districts)]<-Wiki_links[[3]]
Wiki.df$wiki_type[(Wiki.df$wiki_district%in%S_districts)]<-"council_areas"
#E1 link
Wiki.df$wiki_link[(Wiki.df$wiki_district%in%E1_districts)]<-Wiki_links[[4]]
Wiki.df$wiki_type[(Wiki.df$wiki_district%in%E1_districts)]<-"unitary_authority"
#E2 link
Wiki.df$wiki_link[(Wiki.df$wiki_district%in%E2_districts)]<-Wiki_links[[5]]
Wiki.df$wiki_type[(Wiki.df$wiki_district%in%E2_districts)]<-"non_metropoliton_counties"
#E3 link
Wiki.df$wiki_link[(Wiki.df$wiki_district%in%E3_districts)]<-Wiki_links[[6]]
Wiki.df$wiki_type[(Wiki.df$wiki_district%in%E3_districts)]<-"metropoliton_counties"
