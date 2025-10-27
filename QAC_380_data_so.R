#QAC380 
#Merging Data 

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

freq_table <- as.data.frame(table(ClientData$Race))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Client Race", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: ClientData: client ethnicity
freq_table <- as.data.frame(table(ClientData$Ethnicity))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Client Ethnicity", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: ClieentAddress: Client Zip Code
freq_table <- as.data.frame(table(ClientAddress$Zip))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Client's ZipCode", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: ClientAddress: Client City
freq_table <- as.data.frame(table(ClientAddress$City))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Client's City", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: EpisodeData: Length of stay in the program for the client
freq_table <- as.data.frame(table(EpisodeData$LengthOfStay))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Length of Stay", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: EpisodeData: Discharge Status of the client for the program 
freq_table <- as.data.frame(table(EpisodeData$DischargeStatus))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Disharge Status", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: EpisodeData: The highest grade cmompleted by client at admission 
freq_table <- as.data.frame(table(EpisodeData$HighestGradeCompletedAtAdmission))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Highest Grade Completed at Admission", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: Episdoedata: Program that the client was enrolled in
freq_table <- as.data.frame(table(EpisodeData$ProgramName))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Enrolled Program", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))


# Freq distribution for: OffenseData: Years served in prison by the client 
freq_table <- as.data.frame(table(OffenseData$YearsServed))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Years Served", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

summary(OffenseData$YearsServed)


# Freq distribution for: OffenseData: Sentence charge/Crime committed by client
freq_table <- as.data.frame(table(OffenseData$SentencedChargeDescription))
colnames(freq_table) <- c("Category", "Count")


ggplot(freq_table, aes(x = Category, y = Count)) +
  geom_col(fill = "steelblue", color = "black") + 
  labs(title = "Frequency Distribution of Sentence Charge Description", x = "Category", y = "Frequency") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))
