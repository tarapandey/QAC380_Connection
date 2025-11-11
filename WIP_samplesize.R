#QAC380 
#Merging Data 

library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Users/shitaloli/Desktop/TheConnection Client")

#Load all the csv's 
ClientData = read.csv("ClientData.csv")
ClientAddress = read.csv("ClientAddressData.csv")
EpisodeData = read.csv("EpisodeData.csv")
OffenseData = read.csv("OffenseData.csv")
EmploymentData = read.csv("EmploymentData.csv") 
DrugofChoice = read.csv("DrugOfChoiceData.csv")
ORAS_CST = read.csv("ScoresCSTData.csv")
ASUSDimensions = read.csv("ASUS_dimension_scores.csv")
OutsideData = read.csv("2022_Disproportionately_Impacted_Areas.csv")

#rename variables as needed

names(OffenseData)[names(OffenseData)== "EpisodeStudyID"] <- "StudyEpisodeId"
names(DrugofChoice)[names(DrugofChoice)== "EpisodeStudyID"] <- "StudyEpisodeId"
names(ASUSDimensions)[names(ASUSDimensions)== "StudyEpisodeID"] <- "StudyEpisodeId"
names(ASUSDimensions)[names(ASUSDimensions)== "StudyClientID"] <- "StudyClientId"

#Merge the data into one 

MergedData1 = merge(ClientData, ClientAddress, by = "StudyClientId")
MergedData2 = merge(MergedData1, EpisodeData, by = "StudyClientId")
MergedData3 = merge(MergedData2, ASUSDimensions, by = "StudyClientId")
MergedData4 = merge(MergedData3, EmploymentData, by = "StudyClientId")


FinalMerge = left_join(MergedData4, OutsideData, by = c("City" = "Town.s."))

glimpse(FinalMerge)


