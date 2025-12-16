library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Users/shitaloli/Desktop/TheConnection Client")

# -----------------------------
# LOAD DATA
# -----------------------------
ScoresRisk  <- read.csv("RiskData.csv")
ASUS        <- read.csv("ASUS_dimension_scores.csv")
EpisodeData <- read.csv("EpisodeData.csv")

# -----------------------------
# MERGE SCORES + ASUS
# -----------------------------
df <- merge(
  ScoresRisk, ASUS,
  by.x = "StudyClientId",
  by.y = "StudyClientID",
  all = TRUE
)

# -----------------------------
# DISRUPTION → LONG FORMAT + REMOVE NAs
# -----------------------------
df_long <- df %>%
  pivot_longer(
    cols = c(AOD_DISRUPTION1, AOD_DISRUPTION2),
    names_to = "AOD_Type",
    values_to = "Disruption"
  ) %>%
  filter(!is.na(RiskLevel), !is.na(Disruption))

# -----------------------------
# DISCHARGE GROUPING
# -----------------------------
EpisodeData$DischargeGrouped <- recode(
  EpisodeData$DischargeStatus,
  
  # Successful
  "Completed Program/End of Sentence"        = "Successful",
  "Completed Program/Parole"                 = "Successful",
  "Completed Program/Treatment"              = "Successful",
  "Completed Program/Treatment & Referred"   = "Successful",
  
  # Unsuccessful
  "Absconded/AWOL"           = "Unsuccessful",
  "Arrested New"             = "Unsuccessful",
  "Escaped"                  = "Unsuccessful",
  "Incarcerated"             = "Unsuccessful",
  "New Arrest - Sexual Offense" = "Unsuccessful",
  "Remanded"                 = "Unsuccessful",
  "Discharged to Higher Level of Care"  = "Unsuccessful",
  "Medical"                             = "Unsuccessful",
  "Moved out of area"                   = "Unsuccessful",
  "Transferred"                         = "Unsuccessful",
  "Deceased" = "Unsuccessful",
  "Other" = "Unsuccessful",
)
EpisodeData <- EpisodeData[EpisodeData$DischargeGrouped != "", ]
# -----------------------------
# MERGE EPISODE DATA + REMOVE NAs
# -----------------------------
df <- merge(
  df,
  EpisodeData[, c("StudyClientId", "DischargeGrouped")],
  by = "StudyClientId",
  all.x = TRUE
) %>%
  filter(!is.na(DischargeGrouped),
         !is.na(MOTIVATION_TO_CHANGE))

# -----------------------------
# SPLIT AOD DATA
# -----------------------------
df_AOD1 <- df_long %>% filter(AOD_Type == "AOD_DISRUPTION1")
df_AOD2 <- df_long %>% filter(AOD_Type == "AOD_DISRUPTION2")

# Choose one constant color for all graphs:
plot_color <- "steelblue"

# -----------------------------
# AOD DISRUPTION – FACETED LINE GRAPH
# -----------------------------
ggplot(df_long, aes(x = RiskLevel, y = Disruption, group = RiskLevel)) +
  stat_summary(fun = "mean", geom = "line", color = plot_color) +
  stat_summary(fun = "mean", geom = "point", size = 3, color = plot_color) +
  facet_wrap(~ AOD_Type) +
  labs(title = "Mean AOD Disruption by Risk Level",
       x = "Risk Level",
       y = "Mean Disruption Score") +
  theme_minimal()

# -----------------------------
# AOD DISRUPTION 1 — BAR CHART
# -----------------------------
ggplot(df_AOD1, aes(x = RiskLevel, y = Disruption)) +
  stat_summary(fun = "mean", geom = "bar", fill = plot_color) +
  labs(title = "Mean AOD Disruption 1 by Risk Level",
       x = "Risk Level",
       y = "Mean Disruption Score") +
  theme_minimal()

# -----------------------------
# AOD DISRUPTION 2 — BAR CHART
# -----------------------------
ggplot(df_AOD2, aes(x = RiskLevel, y = Disruption)) +
  stat_summary(fun = "mean", geom = "bar", fill = plot_color) +
  labs(title = "Mean AOD Disruption 2 by Risk Level",
       x = "Risk Level",
       y = "Mean Disruption Score") +
  theme_minimal()

# -----------------------------
# MOTIVATION TO CHANGE — BOXPLOT
# -----------------------------
ggplot(df, aes(x = DischargeGrouped, y = MOTIVATION_TO_CHANGE)) +
  geom_boxplot(fill = plot_color) +
  labs(title = "Motivation to Change by Discharge Status",
       x = "Discharge Status",
       y = "Motivation to Change Score") +
  theme_minimal()

# -----------------------------
# MOTIVATION TO CHANGE — MEAN BAR CHART
# -----------------------------
df$DischargeGrouped_Recode <- ifelse(df$DischargeGrouped == "Successful",
                                     "Successful",
                                     "Unsuccessful")

ggplot(df, aes(x = DischargeGrouped, y = MOTIVATION_TO_CHANGE)) +
  stat_summary(fun = "mean", geom = "bar", fill = plot_color) +
  labs(title = "Mean Motivation to Change by Discharge Status",
       x = "Discharge Status",
       y = "Mean Motivation Score") +
  theme_minimal()

df$MotivationCat <- cut(
  df$MOTIVATION_TO_CHANGE,
  breaks = quantile(df$MOTIVATION_TO_CHANGE, probs = c(0, .33, .66, 1), na.rm = TRUE),
  include.lowest = TRUE,
  labels = c("Low", "Medium", "High")
)
df_clean <- df[!is.na(df$DischargeGrouped) & !is.na(df$MotivationCat), ]
tab <- table(df_clean$DischargeGrouped, df_clean$MotivationCat)
chisq.test(tab)