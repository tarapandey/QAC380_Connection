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
EpisodeData$ DischargeStatus[EpisodeData$DischargeStatus == "Deceased" ] <- NA
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

#EMPLOYMENT @ DISCHARGE 
EmploymentData$EmploymentStatusCurrentToDischarge <- recode(EmploymentData$EmploymentStatusCurrentToDischarge,
                                                            "Employed - F/T Competitive  (35 hrs or more)" = "Employed",
                                                            "Employed - P/T Competitive (35 hrs or less)" = "Employed",
                                                            "Employed - P/T Skilled Position" = "Employed",
                                                            "NILF Temporarily unable to work due to Medical/Mental Health Issues" = "NILF",
                                                            "NILF Temporarily unable to work due to lack of identification" = "NILF",
                                                            "NILF Pending Disability" = "NILF",
                                                            "NILF SSI/SSDI/SAGA" = "NILF",
                                                            "Enrolled in School w/Work Study" = "Employed",
                                                            "(N/A) Temporarily unable to work due to lack of identification" = "NILF",
                                                            "Employed - F/T Semi-Skilled Position" = "Employed",
                                                            "Employed - P/T Semi-Skilled Position" = "Employed",
)
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
MergedData1 = merge(ClientData, ClientAddress, by = "StudyClientId", all = FALSE)
MergedData2 = merge(MergedData1, EpisodeData, by = "StudyClientId", all = FALSE)
MergedData3 = merge(MergedData2, ASUSDimensions, by = "StudyClientId", all = FALSE)
MergedData4 = merge(MergedData3, Employment_of_Interest, by = "StudyClientId", all = FALSE)
MergedData5 = merge(MergedData4, ORAS_CST, by = "StudyClientId", all = TRUE)

MergedData5 <- MergedData5 %>% 
  distinct()

FinalMerge = left_join(MergedData4, avg_town, by = c("City" = "Town.s."))

glimpse(FinalMerge)

#stats for Discharge Status x Employment @ Admission
#subset the data

EmploymentDischarge <- MergedData4[, c("DischargeStatus", "EmploymentStatusAtAdmission")]

EmploymentDischarge$EmploymentStatusAtAdmission[EmploymentDischarge$EmploymentStatusAtAdmission == ""] <- NA
EmploymentDischarge$DischargeStatus[EmploymentDischarge$DischargeStatus == ""] <- NA

EmploymentDischarge <- EmploymentDischarge[!is.na(EmploymentDischarge$DischargeStatus), ]
EmploymentDischarge <- EmploymentDischarge[!is.na(EmploymentDischarge$EmploymentStatusAtAdmission), ]

table(EmploymentDischarge$DischargeStatus, EmploymentDischarge$EmploymentStatusAtAdmission)

ggplot(EmploymentDischarge, aes(x = EmploymentStatusAtAdmission, fill = DischargeStatus )) +
  geom_bar(position = "dodge")
#CHANGED EMPLOY ADMIN x DISCHARGE STATUS
plot_data <- EmploymentDischarge %>%
  filter(!is.na(DischargeStatus), DischargeStatus != "",
         !is.na(EmploymentStatusAtAdmission), EmploymentStatusAtAdmission != "") %>%
  group_by(EmploymentStatusAtAdmission) %>%
  count(DischargeStatus) %>%
  mutate(percentage = n / sum(n)) %>%
  filter(DischargeStatus == "Successful") 

ggplot(plot_data, aes(x = EmploymentStatusAtAdmission, y = percentage)) +
  geom_col(fill = "skyblue") + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(x = "EmploymentStatusAtAdmission",
       y = "%  Successful Discharge ", 
       title = "Does Employment Status At Admission Affect Discharge Success?")

employment_discharge_table <- table(EmploymentDischarge$EmploymentStatusAtAdmission, 
                                    EmploymentDischarge$DischargeStatus)

print(employment_discharge_table)

chisq.test(employment_discharge_table)



# 1. Create the table
emp_discharge_table <- table(Employment2Discharge$EmploymentStatusCurrentToDischarge, 
                             Employment2Discharge$DischargeStatus)

# 2. Print the table to see raw counts
print("Contingency Table:")
print(emp_discharge_table)

# 3. Run the Chi-Square Test
test_result <- chisq.test(emp_discharge_table)
print(test_result)

# 4. Check Residuals to see the direction of the effect
# (Positive values > 2 mean that group appears MORE often than expected)
print("Standardized Residuals:")
print(test_result$residuals)




#stats for Discharge Status x Race
RaceDischarge <- MergedData4[, c("DischargeStatus", "RaceGrouped")]

RaceDischarge$Race[RaceDischarge$Race == ""] <- NA
RaceDischarge$DischargeStatus[RaceDischarge$DischargeStatus == ""] <- NA

RaceDischarge <- RaceDischarge[!is.na(RaceDischarge$DischargeStatus), ]
RaceDischarge <- RaceDischarge[!is.na(RaceDischarge$Race), ]

table(RaceDischarge$DischargeStatus, RaceDischarge$RaceGrouped)

ggplot(RaceDischarge, aes(x = RaceGrouped, fill = DischargeStatus )) +
  geom_bar(position = "dodge") 
#CHANGED RACE x DISCHARGE STATUS
plot_data <- RaceDischarge %>%
  filter(!is.na(DischargeStatus), DischargeStatus != "",
         !is.na(Race), Race != "") %>%
  group_by(Race) %>%
  count(DischargeStatus) %>%
  mutate(percentage = n / sum(n)) %>%
  filter(DischargeStatus == "Successful") 

ggplot(plot_data, aes(x = Race, y = percentage)) +
  geom_col(fill = "red") + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(x = "Race",
       y = "%  Successful Discharge ", 
       title = "Does Race Affect Discharge Success?")


# 1. Create a Binary "IsSuccessful" variable
# (This groups "Unsuccessful" and "Other" together as 0, and "Successful" as 1)
RaceDischarge$IsSuccessful <- ifelse(RaceDischarge$DischargeStatus == "Successful", "Yes", "No")

# 2. Create the contingency table (Race vs. Success)
# Note: Ensure you use the correct column name. Your code used 'RaceGrouped' initially.
race_table_binary <- table(RaceDischarge$RaceGrouped, RaceDischarge$IsSuccessful)

# 3. Run the Chi-Square Test
test_result <- chisq.test(race_table_binary)

# 4. Print the result
print(test_result)

# 5. Check the Residuals (CRITICAL STEP)
# This tells you WHICH race is driving the difference (if any)
print(test_result$residuals)





#stats for Discharge Status x Length of Stay
StayDischarge <- MergedData4[, c("DischargeStatus", "LengthOfStay")]

StayDischarge <- StayDischarge[!is.na(StayDischarge$DischargeStatus), ]
StayDischarge <- StayDischarge[!is.na(StayDischarge$LengthOfStay), ]

ggplot(StayDischarge, aes(x=DischargeStatus, y=LengthOfStay)) +
  geom_boxplot()


#stats for Discharge Status x Employment at Discharge 

Employment2Discharge = MergedData4[, c("DischargeStatus", "EmploymentStatusCurrentToDischarge")]
Employment2Discharge <- Employment2Discharge[!is.na(Employment2Discharge$DischargeStatus), ]
Employment2Discharge <- Employment2Discharge[!is.na(Employment2Discharge$EmploymentStatusCurrentToDischarge), ]

ggplot(Employment2Discharge, aes(x = DischargeStatus, fill = EmploymentStatusCurrentToDischarge)) +
  geom_bar(position = "dodge") 

table(EmploymentData$EmploymentStatusCurrentToDischarge)

#CHANGED DISCHARGE STATUS x EMPLOYMENT DISCHARGE 



#ORAS LEVEL x DISCHARGE STATUS 

MergedData5$RiskLevel[MergedData5$RiskLevel == "Overridden" ] <- NA
RiskDischarge = MergedData5[, c("DischargeStatus", "RiskLevel")]
RiskDischarge <- RiskDischarge[!is.na(RiskDischarge$DischargeStatus), ]
RiskDischarge <- RiskDischarge[!is.na(RiskDischarge$RiskLevel), ]

ggplot(RiskDischarge, aes(x = RiskLevel , fill = DischargeStatus)) +
  geom_bar(position = "dodge")
#CHANGED ORAS DISCHARGE

RiskDischarge$RiskLevel <- factor(RiskDischarge$RiskLevel, levels = c( "Low", "Moderate", "High", "Very High"))

plot_data <- RiskDischarge %>%
  filter(!is.na(DischargeStatus), DischargeStatus != "",
         !is.na(RiskLevel), RiskLevel != "") %>%
  group_by(RiskLevel) %>%
  count(DischargeStatus) %>%
  mutate(percentage = n / sum(n)) %>%
  filter(DischargeStatus == "Successful") 

ggplot(plot_data, aes(x = RiskLevel, y = percentage)) +
  geom_col(fill = "cornflowerblue") + 
  scale_y_continuous(labels = scales::percent, limits = c(0,1)) +
  labs(x = "Risk Level",
       y = "%  Successful Discharge ", 
       title = "Does Risk Level Affect Discharge Success?")

risk_discharge_table <- table(RiskDischarge$RiskLevel, RiskDischarge$DischargeStatus)

print("Contingency Table:")
print(risk_discharge_table)

test_result <- chisq.test(risk_discharge_table)

print(test_result)

chisq_result <- chisq.test(risk_discharge_table)

# 2. Print the residuals
round(chisq_result$residuals, 2)

RiskDischarge %>%
  filter(!is.na(RiskLevel)) %>%  # Keep only rows where RiskLevel is NOT NA
  ggplot(aes(x = RiskLevel)) +
  geom_bar(fill = "blue")

freq(MergedData4$Race)


freq(MergedData4$STRENGTHS)
freq(MergedData4$DischargeStatus)

#Bivariate ____ (LCA VARIABLES) and Successful Discharge Status

MergedData4 %>% 
  filter(STRENGTHS == 1) %>%       
  count(DischargeStatus) %>%              
  mutate(Percentage = n / sum(n) * 100) %>% 
  filter(DischargeStatus == "Successful")

#Bivariate ____ (LCA VARIABLES) and UNSuccessful Discharge Status

MergedData4 %>% 
  filter(STRENGTHS == 1) %>%       
  count(DischargeStatus) %>%              
  mutate(Percentage = n / sum(n) * 100) %>% 
  filter(DischargeStatus == "Unsuccessful")

#X2 TEST

test_data <- MergedData4 %>%
  filter(DischargeStatus %in% c("Successful", "Unsuccessful"))
my_table <- table(test_data$STRENGTHS, test_data$DischargeStatus)
test_result <- chisq.test(my_table)
print(test_result)



counts <- table(MergedData4$RaceGrouped, MergedData4$DischargeStatus)

# 2. Convert counts to row percentages (1 means row-wise)
percentages <- prop.table(counts, 1) * 100

# 3. Combine them visually (Optional, for a cleaner view)
# This shows the raw count and the percentage side-by-side
print("Counts:")
print(counts)

print("Percentages:")
print(round(percentages, 2))


contingency_table <- table(MergedData4$RaceGrouped, MergedData4$DischargeStatus)
chi_test_result <- chisq.test(contingency_table)
print(chi_test_result)

summary(MergedData4$(AOD_INVOLVEMENT1)

freq(MergedData4$AOD_INVOLVEMENT1)
