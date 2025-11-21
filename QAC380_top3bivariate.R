#QAC 380
#first three bivariate graphs


#copied Tara's code: 

library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data")

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

#DISCHARGE STATUS 
EpisodeData$DischargeStatus[EpisodeData$DischargeStatus == "Deceased" ] <- NA
EpisodeData$DischargeStatus[EpisodeData$DischargeStatus == "" ] <- NA
EpisodeData <- EpisodeData[!is.na(EpisodeData$DischargeStatus), ]


EpisodeData$DischargeStatus <- recode(EpisodeData$DischargeStatus,
                                       "Absconded/AWOL" = "Unsuccessful",
                                       "Arrested New" = "Unsuccessful",                                "Arrested New" = "Unsuccessful",
                                       "Completed Program/End of Sentence" = "Successful",
                                       "Completed Program/Parole" = "Successful",
                                       "Completed Program/Treatment" = "Successful",
                                       "Completed Program/Treatment & Referred" = "Successful",
                                       "Escaped" = "Unsuccessful",
                                       "Incarcerated" = "Unsuccessful",
                                       "New Arrest - Sexual Offense" = "Unsuccessful",
                                       "Remanded" = "Unsuccessful",
                                       "Discharged to Higher Level of Care" = "Other",
                                       "Medical" = "Other",
                                       "Moved out of area" = "Other",
                                       "Transferred" = "Other",
                                       
                                       
)

#EPLOYMENT @ ADMISSION
EmploymentData$EmploymentStatusAtAdmission <- recode(EmploymentData$EmploymentStatusAtAdmission,
                                                     "Employed - F/T Competitive  (35 hrs or more)" = "Employed",
                                                     "Employed - P/T Competitive (35 hrs or less)" = "Employed",
                                                     "NILF Temporarily unable to work due to Medical/Mental Health Issues" = "NILF",
                                                     "NILF Temporarily unable to work due to lack of identification" = "NILF",
                                                     "NILF Pending Disability" = "NILF",
                                                     "NILF SSI/SSDI/SAGA" = "NILF",
                                                     "Enrolled in School w/Work Study" = "Employed",
                                                     "(N/A) Temporarily unable to work due to lack of identification" = "NILF"
)

EmploymentData <- EmploymentData[!is.na(EmploymentData$EmploymentStatusAtAdmission), ]

# RACE

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

#Merge the data into one 
MergedData1 = merge(ClientData, ClientAddress, by = "StudyClientId", all = TRUE)
MergedData2 = merge(MergedData1, EpisodeData, by = "StudyClientId", all = TRUE)
MergedData3 = merge(MergedData2, ASUSDimensions, by = "StudyClientId", all = TRUE)
MergedData4 = merge(MergedData3, Employment_of_Interest, by = "StudyClientId", all = TRUE)

FinalMerge = left_join(MergedData4, avg_town, by = c("City" = "Town.s."))



glimpse(FinalMerge)

#stats for Discharge Status x Employment @ Admission
table(MergedData4$DischargeStatus, MergedData4$EmploymentStatusAtAdmission)

ggplot(MergedData4, aes(x = DischargeStatus, fill = EmploymentStatusAtAdmission)) +
  geom_bar(position = "stack")

ggplot(MergedData4, aes(x = DischargeStatus, fill = EmploymentStatusAtAdmission)) +
  geom_bar(position = "dodge")

#stats for Discharge Status x Race 
table(MergedData4$DischargeStatus, MergedData4$RaceGrouped)

ggplot(MergedData4, aes(x = DischargeStatus, fill = RaceGrouped)) +
  geom_bar(position = "stack") 

ggplot(MergedData4, aes(x = DischargeStatus, fill = RaceGrouped)) +
  geom_bar(position = "dodge")

#stats for Discharge Status x Length of Stay

ggplot(MergedData4, aes(x=DischargeStatus, y=LengthOfStay)) +
  geom_boxplot()

