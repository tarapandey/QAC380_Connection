
library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Users/shitaloli/Desktop/TheConnection Client")

#DISRUPTION SCORE X RISK LEVEL
ScoresRisk = read.csv("RiskData.csv")

ASUS = read.csv("ASUS_dimension_scores.csv")

EpisodeData = read.csv("EpisodeData.csv")

df <- merge(ScoresRisk, ASUS, 
            by.x = "StudyClientId", 
            by.y = "StudyClientID",
            all = TRUE)

#organizing data
df_long <- df %>%
  pivot_longer(cols = c(AOD_DISRUPTION1, AOD_DISRUPTION2),
               names_to = "AOD_Type",
               values_to = "Disruption")
#boxplot
ggplot(df_long, aes(x = RiskLevel, y = Disruption, fill = RiskLevel)) +
  geom_boxplot() +
  facet_wrap(~ AOD_Type) +
  labs(title = "AOD Disruptions by Risk Level",
       x = "Risk Level",
       y = "Disruption Score") +
  theme_minimal() +
  theme(legend.position = "none")

#To make it violin instead of boxplot, replace geom_boxplot() with:
#geom_violin(trim = FALSE) +
#geom_boxplot(width = 0.1, fill = "white")

#MOTIVATION TO CHANGE X DISCHARGE STATUS

EpisodeData$DischargeGrouped <- recode(
  EpisodeData$DischargeStatus,
  
  # Successful
  "Completed Program/End of Sentence" = "Successful",
  "Completed Program/Parole" = "Successful",
  "Completed Program/Treatment" = "Successful",
  "Completed Program/Treatment & Referred" = "Successful",
  
  # Unsuccessful
  "Absconded/AWOL" = "Unsuccessful",
  "Arrested New" = "Unsuccessful",
  "Escaped" = "Unsuccessful",
  "Incarcerated" = "Unsuccessful",
  "New Arrest - Sexual Offense" = "Unsuccessful",
  "Remanded" = "Unsuccessful",
  
  # Other
  "Discharged to Higher Level of Care" = "Other",
  "Medical" = "Other",
  "Moved out of area" = "Other",
  "Transferred" = "Other",
  
  .default = "Other"
)

# MERGE EPISODE DATA (WITH GROUPED DISCHARGE)
df <- merge(df,
            EpisodeData[, c("StudyClientId", "DischargeGrouped")],
            by = "StudyClientId",
            all.x = TRUE)

# FINAL PLOT 
ggplot(df, aes(x = DischargeGrouped, y = MOTIVATION_TO_CHANGE, fill = DischargeGrouped)) +
  geom_boxplot() +
  labs(title = "Motivation to Change by Discharge Status",
       x = "Discharge Status",
       y = "Motivation to Change Score") +
  theme_minimal() +
  theme(legend.position = "none")

#BAR CHART
ggplot(df, aes(x = DischargeGrouped, y = MOTIVATION_TO_CHANGE, fill = DischargeGrouped)) +
  stat_summary(fun = "mean", geom = "bar") +
  labs(title = "Mean Motivation to Change by Discharge Status",
       x = "Discharge Status",
       y = "Mean Motivation Score") +
  theme_minimal() +
  theme(legend.position = "none")
