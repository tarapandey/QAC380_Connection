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

#use lifetime drug use or lifetime drug use rather than 6 months 

#observations: addy has 3775 obs, OffenseData#observations: addy has 3775 obs, client data has 3280. merging them gives 3775
#episode data has 4275 but merging all has 6566 obs. this means that 1484 observations 
#were lost -- likely, multiple episodes from a same individual. 

#Merging for bivariate graphs:

#AgeOfFirstUse × Successful Discharge

#MergedData6 = merge(MergedData5, DrugofChoice, by = "StudyEpisodeId")



#Outside Data (Disproportionately Impacted Areas) 

setwd("~/Downloads")
DIA = read.csv("2021_Disproportionately_Impacted_Areas_20251024.csv")
freq(DIA$Disproportionately.Impacted.Area..DIA.)



#Shital Code 
library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data")

ClientData = read.csv("ClientData.csv")

ClientAddress = read.csv("ClientAddressData.csv")

EpisodeData = read.csv("EpisodeData.csv")

OffenseData = read.csv("OffenseData.csv")

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

freq_table <- as.data.frame(table(ClientData$RaceGrouped))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Client Race", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: ClientData: client ethnicity

ClientData$Ethnicity[ClientData$Ethnicity == "Unknown" ] <- NA

ClientData$EthGrouped <- recode(ClientData$Ethnicity,
                                "No, Not of Hispanic, Latino, or Spanish Origin." = "Not Hispanic",
                                "Yes, another Hispanic, Latino, or Spanish origin" = "Hispanic",
                                "Yes, Puerto Rican" = "Hispanic",
                                "Yes, Cuban" = "Hispanic",
                                "Yes, Mexican, Mexican American, Chicano." = "Hispanic",
                                "Yes, of Hispanic/Latino Origin" = "Hispanic",
                                "Yes, South or Central American" = "Hispanic")

freq_table <- as.data.frame(table(ClientData$EthGrouped))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Client Ethnicity", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

# Freq distribution for: EpisodeData: Discharge Status of the client for the program 

EpisodeData$DischargeStatus[EpisodeData$DischargeStatus == "Deceased" ] <- NA
#EpisodeData$DischargeStatus[EpisodeData$DischargeStatus == "Other" ] <- NA
EpisodeData$DischargeStatus[EpisodeData$DischargeStatus == "" ] <- NA


EpisodeData$DischargeGrouped <- recode(EpisodeData$DischargeStatus,
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

freq_table <- as.data.frame(table(EpisodeData$DischargeGrouped))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "red", color = "black") + 
  labs(title = "Frequency Distribution of Disharge Status - With 'Other'", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Age of first use


DrugOfChoice <- read.csv("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data/DrugOfChoiceData.csv")

DrugOfChoice$AgeOfFirstUse[DrugOfChoice$AgeOfFirstUse == 0 ] <- NA

freq_table_AFU <- as.data.frame(table(DrugOfChoice$AgeOfFirstUse))
colnames(freq_table_AFU) <- c("Category", "Count")

ggplot(freq_table_AFU, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + # Customize bar appearance
  labs(title = "Frequency Distribution of Age of First Use of Substances", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
