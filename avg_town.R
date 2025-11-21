#QAC380 
#Merging Data 

library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Users/tarapandey/QAC_380_Connection_Data")

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

#remove duplicates

ClientAddress <- ClientAddress[!duplicated(ClientAddress), ]
EmploymentData <- EmploymentData[!duplicated(EmploymentData), ]

#double checking for duplicates in Episode Id (we want to include all episodes)
length(unique(EpisodeData$StudyEpisodeId)) #4275 - len of array
length(unique(ASUSDimensions$StudyEpisodeId)) #2466 - len of array
length(unique(EmploymentData$StudyEpisodeId)) #1387 - much smaller than len of array

#create a subset of the data to only include the episode and client ids, and employment status at admission and discharge
#the rest of the data includes many repeats per episode id 

Employment_of_Interest = EmploymentData[, c("StudyClientId", "StudyEpisodeId", "EmploymentStatusAtAdmission", "EmploymentStatusCurrentToDischarge")]
Employment_of_Interest <- Employment_of_Interest[!duplicated(Employment_of_Interest), ]

#zuhayr ethnicity/race data management
ClientData$Ethnicity[ClientData$Ethnicity == "Unknown" ] <- NA

ClientData$EthGrouped <- recode(ClientData$Ethnicity,
                                "No, Not of Hispanic, Latino, or Spanish Origin." = "Not Hispanic",
                                "Yes, another Hispanic, Latino, or Spanish origin" = "Hispanic",
                                "Yes, Puerto Rican" = "Hispanic",
                                "Yes, Cuban" = "Hispanic",
                                "Yes, Mexican, Mexican American, Chicano." = "Hispanic",
                                "Yes, of Hispanic/Latino Origin" = "Hispanic",
                                "Yes, South or Central American" = "Hispanic")

# Freq distribution for: ClientData: client race

ClientData$Race[ClientData$Race == "Not on file" ] <- NA
ClientData$Race[ClientData$Race == "Undisclosed" ] <- NA


ClientData$RaceGrouped <- recode(ClientData$Race,
                                 "Caucasian or White" = "White",
                                 "African American or Black" = "Black",
                                 "American Indian or Alaskan Native" = "Other",
                                 "Asian" = "Other",
                                 "Multi-Racial" = "Multi",
                                 "Native Hawaiian/Other Pacific Islander" = "Other",
                                 "Other Pacific Islander" = "Other",
                                 "Some other race" = "Other"
)

# Freq distribution for: ClientData: client race

table(FinalMerge$PrimaryLanguage)

#Merge the data into one 
MergedData1 = merge(ClientData, ClientAddress, by = "StudyClientId", all = TRUE)
MergedData2 = merge(MergedData1, EpisodeData, by = "StudyClientId", all = TRUE)
MergedData3 = merge(MergedData2, ASUSDimensions, by = "StudyClientId", all = TRUE)
MergedData4 = merge(MergedData3, Employment_of_Interest, by = "StudyClientId", all = TRUE)

FinalMerge = left_join(MergedData4, avg_town, by = c("City" = "Town.s."))

library(dplyr)

FinalMerge <- FinalMerge %>% 
  select(-starts_with("q"))


glimpse(FinalMerge)

FinalMerge$RaceGrouped <- as.factor(FinalMerge$RaceGrouped)
FinalMerge$BiologicalGender <- as.factor(FinalMerge$BiologicalGender)
FinalMerge$EthGrouped <- as.factor(FinalMerge$EthGrouped)

#inclusion of AOD variables

#AOD_DISRUPTION1
set.seed(1)
km_AOD1 <- kmeans(FinalMerge$AOD_DISRUPTION1[is.finite(FinalMerge$AOD_DISRUPTION1)], centers = 3)

FinalMerge$Disruption1Group <- NA
FinalMerge$Disruption1Group[is.finite(FinalMerge$AOD_DISRUPTION1)] <- km_AOD1$cluster

FinalMerge$Disruption1Group <- as.factor(FinalMerge$Disruption1Group)

#AOD_DISRUPTION2
set.seed(1)
km_AOD2 <- kmeans(FinalMerge$AOD_DISRUPTION2[is.finite(FinalMerge$AOD_DISRUPTION2)], centers = 3)

FinalMerge$Disruption2Group <- NA
FinalMerge$Disruption2Group[is.finite(FinalMerge$AOD_DISRUPTION2)] <- km_AOD2$cluster

FinalMerge$Disruption2Group <- as.factor(FinalMerge$Disruption2Group)

library(poLCA)

LCA_vector <- cbind(RaceGrouped, BiologicalGender, EthGrouped, Disruption1Group, Disruption2Group) ~ 1
poLCA(LCA_vector, FinalMerge, nclass = 2)

summary(FinalMerge$AOD_DISRUPTION1)
