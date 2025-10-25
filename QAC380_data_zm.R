#QAC380 
#Merging Data 

library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data")

#Load all the csv's 
ClientData = read.csv("ClientData.csv")
ClientAddy = read.csv("ClientAddressData.csv")
EpisodeData = read.csv("EpisodeData.csv")
OffenseData = read.csv("OffenseData.csv")
EmploymentData = read.csv("EmploymentData.csv") 
DrugofChoice = read.csv("DrugOfChoiceData.csv")
ORAS_CST = read.csv("ScoresCSTData.csv")
ASUSDimensions = read.csv("ASUS_dimension_scores.csv")

#rename variables as needed

names(OffenseData)[names(OffenseData)== "StudyEpisodeID"] <- "StudyEpisodeId"
names(DrugofChoice)[names(DrugofChoice)== "EpisodeStudyID"] <- "StudyEpisodeId"
names(ASUSDimensions)[names(ASUSDimensions)== "StudyEpisodeID"] <- "StudyEpisodeId"
names(ASUSDimensions)[names(ASUSDimensions)== "StudyClientID"] <- "StudyClientId"

#Merge the data into one 

#MergedData3 = merge(MergedData2, OffenseData, by = "StudyEpisodeId")
#MergedData5 = merge(MergedData4, DrugofChoice, by = "StudyEpisodeId")


#MergedData1 = merge(ClientData, ClientAddy, by = "StudyClientId")
#MergedData2 = merge(MergedData1, EpisodeData, by = "StudyClientId")
#MergedData7 = merge(MergedData5, ASUSDimensions, by = "StudyEpisodeId")
#MergedData4 = merge(MergedData3, EmploymentData, by = "StudyEpisodeId")
#MergedData6 = merge(MergedData5, ORAS_CST, by = "StudyEpisodeId")

#attempt 2 
MergedData1 = merge(ClientData, ClientAddy, by = "StudyClientId")
MergedData2 = merge(MergedData1, EpisodeData, by = "StudyClientId")
MergedData3 = merge(MergedData2, ASUSDimensions, by = "StudyClientId")
MergedData4 = merge(MergedData3, EmploymentData, by = "StudyClientId")

MergedData5 = merge(MergedData4, ORAS_CST, by = "StudyClientId")

MergedData5$StudyEpisodeId <- coalesce(MergedData5$StudyEpisodeId.x, MergedData5$StudyEpisodeId.y)
MergedData5$StudyEpisodeId.x <- NULL
MergedData5$StudyEpisodeId.y <- NULL

MergedData6 = merge(MergedData5, DrugofChoice, by = "StudyEpisodeId")
MergedData7 = merge(MergedData6, OffenseData, by = "StudyEpisodeId")



#observations: addy has 3775 obs, OffenseData#observations: addy has 3775 obs, client data has 3280. merging them gives 3775
#episode data has 4275 but merging all has 6566 obs. this means that 1484 observations 
#were lost -- likely, multiple episodes from a same individual. 



#Outside Data (Disproportionately Impacted Areas) 

setwd("~/Downloads")
DIA = read.csv("2021_Disproportionately_Impacted_Areas_20251024.csv")
freq(DIA$Disproportionately.Impacted.Area..DIA.)
