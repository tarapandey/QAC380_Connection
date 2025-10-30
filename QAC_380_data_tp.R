ClientAddressData <- read.csv("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data/ClientAddressData.csv")
ClientData <- read.csv("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data/ClientData.csv")
EpisodeData <- read.csv("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data/EpisodeData.csv")

running_df <- merge(ClientData, EpisodeData, by="StudyClientId")
running_df <- merge(running_df, ClientAddressData, by="StudyClientId")

ASUS <- read.csv("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data/ASUS_dimension_scores.csv")
EmploymentData <- read.csv("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data/EmploymentData.csv")
DrugOfChoice <- read.csv("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data/DrugOfChoiceData.csv")
ORAS <- read.csv("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data/ScoresCSTData.csv")

library(descr)

freq(ASUS$AOD_DISRUPTION1)
freq(ASUS$AOD_DISRUPTION2)
freq(Employment$EmploymentStatusAtAdmission)
freq(Employment$EmploymentStatusCurrentToDischarge)
freq(DrugOfChoice$AgeOfFirstUse)
freq(DrugOfChoice$DrugOfChoice)
freq(ORAS$RiskLevel)

summary(ASUS$AOD_DISRUPTION1)
summary(ASUS$AOD_DISRUPTION2)
summary(DrugOfChoice$AgeOfFirstUse)

library(ggplot2)

#AOD Disruption 1

freq_table_DOC <- as.data.frame(table(ASUS$AOD_DISRUPTION1))
colnames(freq_table_DOC) <- c("Category", "Count")

ggplot(freq_table_DOC, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + # Customize bar appearance
  labs(title = "Frequency Distribution of AOD Disruption", x = "Category", y = "Frequency") +
  theme_minimal()

#AOD Disruption 2

freq_table_DOC <- as.data.frame(table(ASUS$AOD_DISRUPTION2))
colnames(freq_table_DOC) <- c("Category", "Count")

ggplot(freq_table_DOC, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + # Customize bar appearance
  labs(title = "Frequency Distribution of AOD Disruption over 6 months", x = "Category", y = "Frequency") +
  theme_minimal()

#Employment Status at Admission - merged:

library(dplyr)

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


#Employment Status At Admission

freq_table_DOC <- as.data.frame(table(EmploymentData$EmploymentStatusAtAdmission))
colnames(freq_table_DOC) <- c("Category", "Count")

ggplot(freq_table_DOC, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + # Customize bar appearance
  labs(title = "Frequency Distribution of Employment Status At Admission", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


#Employment Status Current to Discharge

EmploymentData$EmploymentStatusCurrentToDischarge <- recode(EmploymentData$EmploymentStatusCurrentToDischarge,
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

freq_table_DOC <- as.data.frame(table(EmploymentData$EmploymentStatusCurrentToDischarge))
colnames(freq_table_DOC) <- c("Category", "Count")

ggplot(freq_table_DOC, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + # Customize bar appearance
  labs(title = "Frequency Distribution of Employment Status Current to Discharge", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Age of First Use
freq_table_AFU <- as.data.frame(table(DrugOfChoice$AgeOfFirstUse))
colnames(freq_table_AFU) <- c("Category", "Count")

ggplot(freq_table_AFU, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + # Customize bar appearance
  labs(title = "Frequency Distribution of Age of First Use of Substances", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Drug Of Choice
freq_table_DOC <- as.data.frame(table(DrugOfChoice$DrugOfChoice))
colnames(freq_table_DOC) <- c("Category", "Count")

ggplot(freq_table_DOC, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + # Customize bar appearance
  labs(title = "Frequency Distribution of Drug of Choice", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#Risk Level

freq_table_DOC <- as.data.frame(table(ORAS$RiskLevel))
colnames(freq_table_DOC) <- c("Category", "Count")

freq_table_DOC$Category <- factor(freq_table_DOC$Category, levels = c("Overriden", "Low", "Moderate", "High", "Very High"))

ggplot(freq_table_DOC, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + # Customize bar appearance
  labs(title = "Frequency Distribution of Risk Level from ORAS Score", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#bivariate dist of distribution of discharge status by ORAS Risk Level

library(ggplot2)

ggplot(Merged, aes(x = DischargeStatus, fill = RiskLevel)) +
  geom_bar(position = "dodge") +
  labs(
    title = "Distribution of Discharge Status by ORAS Risk Level",
    x = "Discharge Status",
    y = "Count",
    fill = "ORAS Risk Level"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5)
  )

#Length of Stay
freq_table_DOC$Category <- as.numeric(as.character(freq_table_DOC$Category))

ggplot(freq_table_DOC, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "steelblue") +
  labs(title = "Frequency Distribution of Drug of Choice",
       x = "Length of Stay (Days)",
       y = "Frequency") +
  theme_minimal() +
  scale_x_continuous(
    breaks = seq(min(freq_table_DOC$Category, na.rm = TRUE),
                 max(freq_table_DOC$Category, na.rm = TRUE),
                 by = 50)
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

#density plot to better show the data

ggplot(EpisodeData, aes(x = LengthOfStay)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density of Length of Stay",
       x = "Days", y = "Density") +
  theme_minimal()

