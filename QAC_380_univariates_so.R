library (dplyr) 

ASUS = read.csv("/Users/shitaloli/Desktop/TheConnection Client/ASUS_dimension_scores.csv")
Employment = read.csv("/Users/shitaloli/Desktop/TheConnection Client/EmploymentData.csv")
ScoresCSTData = read.csv("/Users/shitaloli/Desktop/TheConnection Client/ScoresCSTData.csv")
ClientData = read.csv("/Users/shitaloli/Desktop/TheConnection Client/ClientData.csv")
EpisodeData = read.csv("/Users/shitaloli/Desktop/TheConnection Client/EpisodeData.csv")
OffenseData = read.csv("/Users/shitaloli/Desktop/TheConnection Client/OffenseData.csv")

summary(OffenseData$YearsServed)
summary(EpisodeData$LengthOfStay)
summary(ASUS$AOD_DISRUPTION1)
summary(ASUS$AOD_DISRUPTION2)

ClientData$Ethnicity[ClientData$Ethnicity == "Unknown" ] <- NA

ClientData$EthnicityGrouped <- recode(ClientData$Ethnicity,
                                "No, Not of Hispanic, Latino, or Spanish Origin." = "Not Hispanic",
                                "Yes, another Hispanic, Latino, or Spanish origin" = "Hispanic",
                                "Yes, Puerto Rican" = "Hispanic",
                                "Yes, Cuban" = "Hispanic",
                                "Yes, Mexican, Mexican American, Chicano." = "Hispanic",
                                "Yes, of Hispanic/Latino Origin" = "Hispanic",
                                "Yes, South or Central American" = "Hispanic")
ClientData %>%
  filter(!is.na(EthnicityGrouped)) %>%
  count(EthnicityGrouped) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2)
  )

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
ClientData %>%
  filter(!is.na(RaceGrouped)) %>%
  count(RaceGrouped) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2)
  )
EpisodeData$DischargeStatus[EpisodeData$DischargeStatus == "Deceased" ] <- NA
EpisodeData$DischargeStatus[EpisodeData$DischargeStatus == "Other" ] <- NA
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
                                       "Discharged to Higher Level of Care" = "Unsuccessful",
                                       "Medical" = "Unsuccessful",
                                       "Moved out of area" = "Unsuccessful",
                                       "Transferred" = "Unsuccessful",
                                       
                                       
)

EpisodeData %>%
  filter(!is.na(DischargeGrouped)) %>%
  count(DischargeGrouped) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2)
  )
EpisodeData %>%
  filter(ProgramName %in% c("Eddy Center", "SIERRA Center - Work Release", "REACH (ReEntry Assisted Community Housing)", "	
Roger Sherman House", "The January Center")) %>%
  count(ProgramName) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2)
  )
ScoresCSTData %>%
  filter(RiskLevel %in% c("Low", "Moderate", "High","Very High")) %>%
  count(RiskLevel) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2)
  )
Employment$EmploymentStatusAtAdmission <- recode(Employment$EmploymentStatusAtAdmission,
                                                     "Employed - F/T Competitive  (35 hrs or more)" = "Employed",
                                                     "Employed - P/T Competitive (35 hrs or less)" = "Employed",
                                                     "NILF Temporarily unable to work due to Medical/Mental Health Issues" = "NILF",
                                                     "NILF Temporarily unable to work due to lack of identification" = "NILF",
                                                     "NILF Pending Disability" = "NILF",
                                                     "NILF SSI/SSDI/SAGA" = "NILF",
                                                     "Enrolled in School w/Work Study" = "Employed",
                                                     "(N/A) Temporarily unable to work due to lack of identification" = "NILF"
)
Employment %>%
  filter(EmploymentStatusAtAdmission %in% c("Employed", "Unemployed","NILF")) %>%
  count(EmploymentStatusAtAdmission) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2)
  )
Employment$EmploymentStatusCurrentToDischarge <- recode(Employment$EmploymentStatusCurrentToDischarge,
                                                 "Employed - F/T Competitive  (35 hrs or more)" = "Employed",
                                                 "Employed - P/T Competitive (35 hrs or less)" = "Employed",
                                                 "NILF Temporarily unable to work due to Medical/Mental Health Issues" = "NILF",
                                                 "NILF Temporarily unable to work due to lack of identification" = "NILF",
                                                 "NILF Pending Disability" = "NILF",
                                                 "NILF SSI/SSDI/SAGA" = "NILF",
                                                 "Enrolled in School w/Work Study" = "Employed",
                                                 "(N/A) Temporarily unable to work due to lack of identification" = "NILF"
)
Employment %>%
  filter(EmploymentStatusCurrentToDischarge %in% c("Employed", "Unemployed","NILF")) %>%
  count(EmploymentStatusCurrentToDischarge) %>%
  mutate(
    Percentage = round(n / sum(n) * 100, 2)
  )