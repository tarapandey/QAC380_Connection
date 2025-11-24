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

#ORAS
ORAS <- read.csv("ScoresCSTData.csv")
ORAS_Risk <- subset(ORAS, select = c(StudyClientId, RiskLevel))
FinalMerge <- merge(FinalMerge, ORAS_Risk, 
                   by = c("StudyClientId"),
                   all = TRUE)

#Employment

#Employment Status Current to Discharge

FinalMerge$EmploymentStatusCurrentToDischarge <- recode(FinalMerge$EmploymentStatusCurrentToDischarge,
                                                            "Employed - F/T Competitive  (35 hrs or more)" = "Employed",
                                                            "Employed - P/T Competitive (35 hrs or less)" = "Employed",
                                                            "NILF Temporarily unable to work due to Medical/Mental Health Issues" = "NILF",
                                                            "NILF Temporarily unable to work due to lack of identification" = "NILF",
                                                            "NILF Pending Disability" = "NILF",
                                                            "NILF SSI/SSDI/SAGA" = "NILF",
                                                            "Enrolled in School w/Work Study" = "Employed",
                                                            "(N/A) Temporarily unable to work due to lack of identification" = "NILF",
                                                            "Employed - F/T Semi-Skilled Position" = "Employed",
                                                            "Employed - P/T Semi-Skilled Position" = "Employed",
)

#Employment Status At Admission

FinalMerge$EmploymentStatusAtAdmission <- recode(FinalMerge$EmploymentStatusAtAdmission,
                                                     "Employed - F/T Competitive  (35 hrs or more)" = "Employed",
                                                     "Employed - P/T Competitive (35 hrs or less)" = "Employed",
                                                     "NILF Temporarily unable to work due to Medical/Mental Health Issues" = "NILF",
                                                     "NILF Temporarily unable to work due to lack of identification" = "NILF",
                                                     "NILF Pending Disability" = "NILF",
                                                     "NILF SSI/SSDI/SAGA" = "NILF",
                                                     "Enrolled in School w/Work Study" = "Employed",
                                                     "(N/A) Temporarily unable to work due to lack of identification" = "NILF"
)


#LCA
#AOD_INVOLVEMENT1
FinalMerge$AODInv1Group <- cut(
  FinalMerge$AOD_INVOLVEMENT1,
  breaks = c(0, 3, 6, 11, 40),
  labels = c(1, 2, 3, 4)
)
FinalMerge$AODInv1Group <- as.factor(FinalMerge$AODInv1Group)

#AOD_INVOLVEMENT2
FinalMerge$AODInv2Group <- cut(
  FinalMerge$AOD_INVOLVEMENT2,
  breaks = c(0, 7, 16, 25, 40),
  labels = c(1, 2, 3, 4)
)
FinalMerge$AODInv2Group <- as.factor(FinalMerge$AODInv2Group)

#AOD_DISRUPTION1
FinalMerge$AOD1Group <- cut(
  FinalMerge$AOD_DISRUPTION1,
  breaks = c(0, 1, 6, 21, 80),
  labels = c(1, 2, 3, 4)
)
FinalMerge$AOD1Group <- as.factor(FinalMerge$AOD1Group)

#AOD_DISRUPTION2
FinalMerge$AOD2Group <- cut(
  FinalMerge$AOD_DISRUPTION2,
  breaks = c(0, 21, 41, 57, 80),
  labels = c(1, 2, 3, 4)
)
FinalMerge$AOD2Group <- as.factor(FinalMerge$AOD2Group)

#AOD_USE_BENEFITS
FinalMerge$AOD_BENEFITS <- cut(
  FinalMerge$AOD_USE_BENEFITS,
  breaks = c(0, 1, 4, 10, 30),
  labels = c(1, 2, 3, 4)
)
FinalMerge$AOD_BENEFITS <- as.factor(FinalMerge$AOD_BENEFITS)

#SOCIAL_NON_CONFORMING
FinalMerge$SOC_NONCON <- cut(
  FinalMerge$SOCIAL_NON_CONFORMING,
  breaks = c(0, 5, 7, 12, 36),
  labels = c(1, 2, 3, 4)
)
FinalMerge$SOC_NONCON <- as.factor(FinalMerge$SOC_NONCON)

#LEGAL_NON_CONFORMING
FinalMerge$LEG_NONCON <- cut(
  FinalMerge$LEGAL_NON_CONFORMING,
  breaks = c(0, 5, 7, 12, 36),
  labels = c(1, 2, 3, 4)
)
FinalMerge$LEG_NONCON <- as.factor(FinalMerge$LEG_NONCON)

#LEGAL_NON_CONFORMING_6MOS
FinalMerge$LEG_6MOS <- cut(
  FinalMerge$LEGAL_NON_CONFORMING_6MOS,
  breaks = c(-1, 0, 2, 5, 33),
  labels = c(1, 2, 3, 4)
)
FinalMerge$LEG_6MOS <- as.factor(FinalMerge$LEG_6MOS)

#MOOD_ADJUSTMENT
FinalMerge$MOOD <- cut(
  FinalMerge$MOOD_ADJUSTMENT,
  breaks = c(0, 2, 6, 9, 30),
  labels = c(1, 2, 3, 4)
)
FinalMerge$MOOD <- as.factor(FinalMerge$MOOD)

#DEFENSIVE
FinalMerge$DEF <- cut(
  FinalMerge$DEFENSIVE,
  breaks = c(0, 5, 10, 13, 21),
  labels = c(1, 2, 3, 4)
)
FinalMerge$DEF <- as.factor(FinalMerge$DEF)

#MOTIVATION_TO_CHANGE
FinalMerge$MOTIVATION <- cut(
  FinalMerge$MOTIVATION_TO_CHANGE,
  breaks = c(0, 5, 10, 15, 21),
  labels = c(1, 2, 3, 4)
)
FinalMerge$MOTIVATION <- as.factor(FinalMerge$MOTIVATION)

#STRENGTHS
FinalMerge$STREN <- cut(
  FinalMerge$STRENGTHS,
  breaks = c(0, 7, 17, 22, 27),
  labels = c(1, 2, 3, 4)
)
FinalMerge$STREN <- as.factor(FinalMerge$STREN)

names(FinalMerge)[names(FinalMerge) %in% c("EmploymentStatusAtAdmission","EmploymentStatusCurrentToDischarge")] <- c("Employ_Admin","Employ_Discharge")

lca_subset<-subset(FinalMerge, select=c(RaceGrouped, EthGrouped, AOD2Group,AOD1Group,AODInv1Group,
                                    AODInv2Group,AOD_BENEFITS,SOC_NONCON,LEG_NONCON,LEG_6MOS,MOOD,DEF,MOTIVATION,
                                    STREN,Employ_Admin,Employ_Discharge, RiskLevel,
                                    ProgramName))

lca_vars_only<-subset(FinalMerge, select=c(AOD2Group,AOD1Group,AODInv1Group,
                                           AODInv2Group,AOD_BENEFITS,SOC_NONCON,LEG_NONCON,LEG_6MOS,MOOD,DEF,MOTIVATION,
                                           STREN))


lca_subset <- na.omit(lca_subset[, c(
  "AOD2Group","AOD1Group","AODInv1Group","AODInv2Group",
  "AOD_BENEFITS","SOC_NONCON","LEG_NONCON","LEG_6MOS",
  "MOOD","DEF","MOTIVATION","STREN",
  "RaceGrouped","EthGrouped", "Employ_Admin", 
  "Employ_Discharge", "RiskLevel", "ProgramName"
)])


lca_vars_only <- na.omit(lca_subset[, c(
  "AOD2Group","AOD1Group","AODInv1Group","AODInv2Group",
  "AOD_BENEFITS","SOC_NONCON","LEG_NONCON","LEG_6MOS",
  "MOOD","DEF","MOTIVATION","STREN"
)])

library(plyr)

lca_subset$RaceGrouped<-revalue(lca_subset$RaceGrouped, c("Black"="1", "White"="2", "Other"="3"))
FinalMerge$RaceGrouped <- as.factor(FinalMerge$RaceGrouped)

lca_subset$EthGrouped<-revalue(lca_subset$EthGrouped, c("Hispanic"="1", "Not Hispanic"="2"))
FinalMerge$EthGrouped <- as.factor(FinalMerge$EthGrouped)

lca_subset$Employ_Admin<-revalue(lca_subset$Employ_Admin, c("Employed"="1", "Unemployed"="2", "NILF"="3"))
FinalMerge$Employ_Admin <- as.factor(FinalMerge$Employ_Admin)

lca_subset$Employ_Discharge<-revalue(lca_subset$Employ_Discharge, c("Employed"="1", "Unemployed"="2", "NILF"="3"))
FinalMerge$Employ_Discharge <- as.factor(FinalMerge$Employ_Discharge)

lca_subset$RiskLevel<-revalue(lca_subset$RiskLevel, c("Low"="1", "Moderate"="2", "High"="3", "Very High" = "4"))
FinalMerge$RiskLevel <- as.factor(FinalMerge$RiskLevel)

lca_subset$ProgramName<-revalue(lca_subset$ProgramName, c("Eddy Center"="1", "The January Center"="2", "REACH (ReEntry Assisted Community Housing)"="3", "SIERRA Center - Work Release" = "4", "Roger Sherman House" = "5"))
FinalMerge$ProgramName <- as.factor(FinalMerge$ProgramName)

#inclusion of AOD variables

library(poLCA)

f <- cbind(AOD2Group,AOD1Group,AODInv1Group,
           AODInv2Group,AOD_BENEFITS,SOC_NONCON,LEG_NONCON,LEG_6MOS,MOOD,DEF,MOTIVATION,
           STREN)~RaceGrouped + EthGrouped + Employ_Admin + Employ_Discharge + RiskLevel

f1 <- cbind(AOD2Group,AOD1Group,AODInv1Group,
           AODInv2Group,AOD_BENEFITS,SOC_NONCON,LEG_NONCON,LEG_6MOS,MOOD,DEF,MOTIVATION,
           STREN) ~1

lca_subset <- na.omit(lca_subset)
lca_subset[] <- lapply(lca_subset, factor)
lca_subset[] <- lapply(lca_subset, function(x) factor(x, exclude = NULL))

lca_vars_only <- na.omit(lca_vars_only)
lca_vars_only[] <- lapply(lca_vars_only, factor)
lca_vars_only[] <- lapply(lca_vars_only, function(x) factor(x, exclude = NULL))


lCA1 <- poLCA(f1,lca_subset, nclass=1,nrep=15) 
lCA2 <- poLCA(f,lca_subset, nclass=2,nrep=15, graphs = T)
lCA3 <- poLCA(f,lca_subset, nclass=3,nrep=15, graphs = T)

lCAv1 <- poLCA(f1,lca_vars_only, nclass=1,nrep=15) 
lCAv2 <- poLCA(f1,lca_vars_only, nclass=2,nrep=15, graphs = T)
lCAv3 <- poLCA(f1,lca_vars_only, nclass=3,nrep=15, graphs = T)

#names(lca_subset)


# Calculate entropy (3-class mode)l- values closer to 1.0 indicate greater separation of the classes.
entropy<-function (p) sum(-p*log(p))
error_prior <- entropy(LCA3$P) # Class proportions
error_post <- mean(apply(LCA3$posterior, 1, entropy))
LCA3_entropy <- (error_prior - error_post) / error_prior
LCA3_entropy

#predicted class membership is in:
LCA3$predclass[1:30]

#add variable to data set with all variables so it can be used as predictor variable:
lca_subset$class <- LCA3$predclass

plot(lCA1)
plot(lCA2)
plot(lCA3)

plot(lCAv1)
plot(lCAv2)
plot(lCAv3)

#AIC across latent models
aic_values <- numeric()

for (k in 1:6) {
  model <- poLCA(f1, lca_subset, nclass = k, nrep = 10, verbose = FALSE)
  aic_values[k] <- model$aic
}

plot(1:6, aic_values, type = "b",
     xlab = "Number of Classes",
     ylab = "AIC",
     main = "AIC Across Latent Class Models")

