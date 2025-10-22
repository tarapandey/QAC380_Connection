#QAC380 
#Merging Data 

library(ggplot2)
library(tidyverse)
library(descr)
library(dplyr)
library(readr)

setwd("/Volumes/courses/QAC/qac380/Data and Codebooks/Connection/Data")

ClientData = read.csv("ClientData.csv")

ClientAddy = read.csv("ClientAddressData.csv")

MergedData1 = merge(ClientData, ClientAddy, by = "StudyClientId")

EpisodeData = read.csv("EpisodeData.csv")

MergedData2 = merge(MergedData1, EpisodeData, by = "StudyClientId")

#observations: addy has 3775 obs, client data has 3280. merging them gives 3775
#episode data has 4275 but merging all has 6566 obs. this means that 1484 observations 
#were lost -- likely, multiple episodes from a same individual. 
