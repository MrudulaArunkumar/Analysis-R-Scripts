#loading necessary packages
library(tidyverse)


#removing the objects in the environment
rm(list = ls())

#######loading the data files and combining it together#######

#setting up working directory
setwd("D:/PhD/Exp1-Overshadowing/Data")

day1data <- read.csv(file = "day1_exp1data.csv")
day1data <- subset(day1data, select = -c(PreExpDisplay.RESP))
day2data <- read.csv(file = "day2_Exp1data.csv")
day2data <- subset(day2data, select = -c(PreExpDisplay.RESP))
day3data <- read.csv(file = "day3_exp1data.csv")
day3data <- subset(day3data, select = -c(PreExpDisplay.RESP))
day4data <- read.csv(file = "day4_Exp1data.csv")
day4data <- subset(day4data, select = -c(PreExpDisplay.RESP))
day5data <- read.csv(file = "day5_Exp1data.csv")
day5data <- subset(day5data, select = -c(PreExpDisplay.RESP))
day6data <- read.csv(file = "day6_Exp1data.csv")
day7data <- read.csv(file = "day7_Exp1data.csv")
day8data <- read.csv(file= "day8_Exp1data.csv")
day9data <- read.csv(file= "day9_exp1data.csv")

#combining all the data to a single dataframe
Exp1data <- rbind(day1data,day2data,day3data,day4data,day5data,day6data,day7data,day8data,day9data)

write.csv(Exp1data, file = "Exp1data.csv")
