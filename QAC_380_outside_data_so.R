library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Users/shitaloli/Desktop/TheConnection Client")

OutsideData = read.csv("2022_Disproportionately_Impacted_Areas.csv")

ClientAddress = read.csv("ClientAddressData.csv")

EpisodeData = read.csv("EpisodeData.csv")

ClientEpisodeData = left_join(EpisodeData, ClientAddress, by = "StudyClientId")

MergedData = left_join(ClientEpisodeData, OutsideData, by = c("City" = "Town.s."))

glimpse(MergedData)
