#QAC380 
#Merging Data 

library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Users/tarapandey/Clone_QAC_380")

#Load all the csv's 
ClientData = read.csv("ClientData.csv")
ClientAddress = read.csv("ClientAddressData.csv")
EpisodeData = read.csv("EpisodeData.csv")
OffenseData = read.csv("OffenseData.csv")
EmploymentData = read.csv("EmploymentData.csv") 
DrugofChoice = read.csv("DrugOfChoiceData.csv")
ORAS_CST = read.csv("ScoresCSTData.csv")
ASUSDimensions = read.csv("ASUS_dimension_scores.csv")
OutsideData = read.csv("2022_Disproportionately_Impacted_Areas copy.csv")

#average data by town
avg_town <- OutsideData %>%
  group_by(Town.s.) %>%
  summarize(
    median_income = mean(Median.Household.Income, na.rm = TRUE),
    conviction_rate = mean(Conviction.Rate, na.rm = TRUE),
    conviction_count = mean(Conviction.Count, na.rm = TRUE))

#avg_town only includes town and median income

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


FinalMerge = left_join(MergedData4, avg_town, by = c("City" = "Town.s."))

glimpse(FinalMerge)


