#loading packages
library(plyr)
library(reshape2)
library(ggplot2)
library(ez)
library(schoRsch)
library(tidyverse)
library(dplyr)

rm(list = ls())

setwd("D:/PhD/Pilot studies/exp1_simpleOverCL")

pilotoddeven <- pilotoddeven[!grepl("Practice", pilotoddeven$Condition),]
pilotoddeven <- pilotoddeven[!grepl("learn", pilotoddeven$Condition),]

pilotoddeven$RT_Trial <- pilotoddeven$ExpDisplay.RT
pilotoddeven$ACC_Trial <- pilotoddeven$ExpDisplay.ACC

pilotoddeven$RT_Trial <- as.numeric(as.character(pilotoddeven$RT_Trial))
pilotoddeven$ACC_Trial <- as.numeric(as.character(pilotoddeven$ACC_Trial))
summary(pilotoddeven$RT_Trial)

aggregateACC <- aggregate(data=pilotoddeven, ACC_Trial~Saliency+validity+Subject, mean)
anova_gfo <- ezANOVA(data = aggregateRT,
                     dv = RT_Trial,
                     wid = Subject,
                     within = .(Saliency,validity),
                     detailed = TRUE)
anova_out(anova_gfo)

mean_gfo <- ezStats(data =aggregateACC, 
                    dv = ACC_Trial, 
                    wid = Subject, 
                    within = .(Saliency, validity)) 
