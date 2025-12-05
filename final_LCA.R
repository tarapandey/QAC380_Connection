library(poLCA)
library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

ASUSDimensions = read.csv("ASUS_dimension_scores.csv")
EpisodeData = read.csv("EpisodeData.csv")
EmploymentData = read.csv("EmploymentData.csv") 

#Data management to categorical
#AOD_INVOLVEMENT1
ASUSDimensions$AOD_Inv1 <- cut(
  ASUSDimensions$AOD_INVOLVEMENT1,
  breaks = c(-1, 3, 6, 11, 40),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$AOD_Inv1 <- as.factor(ASUSDimensions$AOD_Inv1)

#AOD_INVOLVEMENT2
ASUSDimensions$AOD_Inv2 <- cut(
  ASUSDimensions$AOD_INVOLVEMENT2,
  breaks = c(-1, 7, 16, 25, 40),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$AOD_Inv2 <- as.factor(ASUSDimensions$AOD_Inv2)

#AOD_DISRUPTION1
ASUSDimensions$AOD_Dis1 <- cut(
  ASUSDimensions$AOD_DISRUPTION1,
  breaks = c(-1, 1, 6, 21, 80),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$AOD_Dis1 <- as.factor(ASUSDimensions$AOD_Dis1)

#AOD_DISRUPTION2
ASUSDimensions$AOD_Dis2 <- cut(
  ASUSDimensions$AOD_DISRUPTION2,
  breaks = c(-1, 21, 41, 57, 80),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$AOD_Dis2 <- as.factor(ASUSDimensions$AOD_Dis2)

#AOD_USE_BENEFITS
ASUSDimensions$AOD_BENEFITS <- cut(
  ASUSDimensions$AOD_USE_BENEFITS,
  breaks = c(-1, 1, 4, 10, 30),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$AOD_BENEFITS <- as.factor(ASUSDimensions$AOD_BENEFITS)

#SOCIAL_NON_CONFORMING
ASUSDimensions$SOC_NONCON <- cut(
  ASUSDimensions$SOCIAL_NON_CONFORMING,
  breaks = c(-1, 5, 7, 12, 36),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$SOC_NONCON <- as.factor(ASUSDimensions$SOC_NONCON)

#LEGAL_NON_CONFORMING
ASUSDimensions$LEG_NONCON <- cut(
  ASUSDimensions$LEGAL_NON_CONFORMING,
  breaks = c(-1, 6, 10, 17, 42),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$LEG_NONCON <- as.factor(ASUSDimensions$LEG_NONCON)

#LEGAL_NON_CONFORMING_6MOS
ASUSDimensions$LEG_6MOS <- cut(
  ASUSDimensions$LEGAL_NON_CONFORMING_6MOS,
  breaks = c(-1, 0, 2, 5, 33),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$LEG_6MOS <- as.factor(ASUSDimensions$LEG_6MOS)

#MOOD_ADJUSTMENT
ASUSDimensions$MOOD <- cut(
  ASUSDimensions$MOOD_ADJUSTMENT,
  breaks = c(-1, 2, 6, 9, 30),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$MOOD <- as.factor(ASUSDimensions$MOOD)

#DEFENSIVE
ASUSDimensions$DEFENSIVE <- cut(
  ASUSDimensions$DEFENSIVE,
  breaks = c(-1, 5, 10, 13, 21),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$DEFENSIVE <- as.factor(ASUSDimensions$DEFENSIVE)

#MOTIVATION_TO_CHANGE
ASUSDimensions$MOTIVATION <- cut(
  ASUSDimensions$MOTIVATION_TO_CHANGE,
  breaks = c(-1, 5, 10, 15, 21),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$MOTIVATION <- as.factor(ASUSDimensions$MOTIVATION)

#STRENGTHS
ASUSDimensions$STRENGTHS <- cut(
  ASUSDimensions$STRENGTHS,
  breaks = c(-1, 7, 17, 22, 27),
  labels = c(1, 2, 3, 4)
)
ASUSDimensions$STRENGTHS <- as.factor(ASUSDimensions$STRENGTHS)

ASUS_vars<-subset(ASUSDimensions, select=c(AOD_Dis1,AOD_Inv1,AOD_Dis2,
                                           AOD_Inv2,AOD_BENEFITS,SOC_NONCON,LEG_NONCON,
                                           LEG_6MOS,MOOD,DEFENSIVE,MOTIVATION,
                                           STRENGTHS, StudyClientID))

ASUS_vars <- na.omit(ASUS_vars[, c("AOD_Dis1","AOD_Inv1","AOD_Dis2","AOD_Inv2",
  "AOD_BENEFITS","SOC_NONCON","LEG_NONCON","LEG_6MOS",
  "MOOD","DEFENSIVE","MOTIVATION","STRENGTHS", "StudyClientID"
)])

ASUS_vars <- na.omit(ASUS_vars)
ASUS_vars  <- ASUS_vars[!duplicated(ASUS_vars), ]
ASUS_vars[] <- lapply(ASUS_vars, factor)
ASUS_vars[] <- lapply(ASUS_vars, function(x) factor(x, exclude = NULL))

ASUS_f <- cbind(AOD_Dis1,AOD_Inv1,AOD_Dis2, AOD_Inv2,AOD_BENEFITS,SOC_NONCON,
                LEG_NONCON,LEG_6MOS,MOOD,DEFENSIVE,MOTIVATION,STRENGTHS) ~1

lCAv1 <- poLCA(ASUS_f,ASUS_vars, nclass=1,nrep=15) 
lCAv2 <- poLCA(ASUS_f,ASUS_vars, nclass=2,nrep=15, graphs = T)
lCAv3 <- poLCA(ASUS_f,ASUS_vars, nclass=3,nrep=15, graphs = T)
lCAv4 <- poLCA(ASUS_f,ASUS_vars, nclass=4,nrep=15, graphs = T)
lCAv5 <- poLCA(ASUS_f,ASUS_vars, nclass=5,nrep=15, graphs = T)
lCAv6 <- poLCA(ASUS_f,ASUS_vars, nclass=6,nrep=15, graphs = T)

#BIC testing for optimal model
lCAv1$bic
lCAv2$bic
lCAv3$bic
lCAv4$bic
lCAv5$bic
lCAv6$bic

#BIC plot
bic_values <- numeric(6)

for (k in 1:6) {
  model <- poLCA(ASUS_f, ASUS_vars, nclass = k, nrep = 10, verbose = FALSE)
  bic_values[k] <- model$bic
}

plot(1:6, bic_values, type = "b", pch = 19,
     xlab = "Number of Classes",
     ylab = "BIC",
     main = "BIC by Number of LCA Classes")

#AIC testing for optimal model
lCAv5$aic
#AIC plot
aic_values <- numeric(6)

for (k in 1:6) {
  model <- poLCA(ASUS_f, ASUS_vars, nclass = k, nrep = 10, verbose = FALSE)
  aic_values[k] <- model$aic
}

plot(1:6, aic_values, type = "b",
     xlab = "Number of Classes",
     ylab = "AIC",
     main = "AIC Across Latent Class Models")

#size of LCA model
lCAv5$N

#entropy

entropy <- function(p) {
  p <- p[p > 0]    # remove zeros
  sum(-p * log(p))
}

error_prior <- entropy(lCAv5$P) # Class proportions
error_post <- mean(apply(lCAv5$posterior, 1, entropy))
LCA5_entropy <- (error_prior - error_post) / error_prior
LCA5_entropy

#mapping class back to Client ID
ASUS_vars$LCA_class <- lCAv5$predclass
names(ASUS_vars)[names(ASUS_vars)== "StudyClientID"] <- "StudyClientId"

#comparing to discharge outcome

#episode data subset keeping most recent discharge
ASUS_vars$StudyClientId <- as.integer(as.character(ASUS_vars$StudyClientId))

episode_subset <- EpisodeData[, c("StudyClientId", "DischargeStatus", "AdmissionYear")]
episode_subset <- episode_subset %>%
  left_join(ASUS_vars, by = "StudyClientId")

#zuhayr's data management code
episode_subset$DischargeStatus <- recode(episode_subset$DischargeStatus,
                                         "Absconded/AWOL" = "Unsuccessful",
                                         "Arrested New" = "Unsuccessful",                          
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
                                         "Deceased" = "Unsuccessful",
                                         "Other" = "Unsuccessful",
                                         
                                         
)

#making subset for chi squared table 
discharge_chi <- episode_subset %>%
  filter(!is.na(LCA_class))

#drop empty
discharge_chi <- discharge_chi[discharge_chi$DischargeStatus != "", ]

#chi-squared test
discharge_tab <- table(discharge_chi$LCA_class, discharge_chi$DischargeStatus)
discharge_tab

prop.table(discharge_tab)
chisq.test(discharge_tab)

#plot
ggplot(discharge_chi, aes(x = as.factor(LCA_class), fill = DischargeStatus)) +
  geom_bar(position = "dodge") +
  scale_fill_manual(values = c("darkolivegreen4", "lightgray")) +
  labs(
    x = "Latent Class",
    y = "Count",
    fill = "Discharge Status",
    title = "Counts of Discharge Status by Latent Class"
  ) +
  theme_minimal()



