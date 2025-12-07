#QAC380 
#Merging Data 

library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data")

ScoresCSTData = read.csv("ScoresCSTData.csv")

EmploymentData = read.csv("EmploymentData.csv")

EpisodeData = read.csv("EpisodeData.csv")

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
# Count for employment
table(EmploymentData$EmploymentStatusAtAdmission)

# Percent for employment
prop.table(table(EmploymentData$EmploymentStatusAtAdmission)) * 100

# Count for risk level
table(ScoresCSTData$RiskLevel)

# Percent for risk level
prop.table(table(ScoresCSTData$RiskLevel)) * 100

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
                                      "Discharged to Higher Level of Care" = "Unsuccessful",
                                      "Medical" = "Unsuccessful",
                                      "Moved out of area" = "Unsuccessful",
                                      "Transferred" = "Unsuccessful",
)
# Count for risk level
table(EpisodeData$DischargeStatus)

# Percent for risk level
prop.table(table(EpisodeData$DischargeStatus)) * 100
                                      