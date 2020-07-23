#loading packages
library(plyr)
library(reshape2)
library(ggplot2)
library(ez)
library(schoRsch)
library(tidyverse)
library(dplyr)



rm(list = ls())

#######CL Part of EXPERIMENT 3#######
#### Creating and Arranging the data frame####
#changing working directory
setwd("D:/PhD/Pilot studies/exp1_simpleOverCL")

#reading the data file for 3rd experiment----using import dataset option
#rawdata_P1 <- read.csv2(file = "Pilotdata1.csv")
#rawdata_P2 <- read.csv2(file = "Pilotdata2.csv")
raw_Pilotdata <- rbind(rawdata_P1,rawdata_P2)
raw_Pilotdata <- pilotoddeven
#making subject the first column
raw_Pilotdata <- raw_Pilotdata %>%
  select(Subject,everything())
#removing practice and learning trials
raw_Pilotdata <- raw_Pilotdata[!grepl("Practice", raw_Pilotdata$Condition),]
raw_Pilotdata <- raw_Pilotdata[!grepl("learn", raw_Pilotdata$Condition),]
#creating new columns with the RT values (to perform further calculations)
raw_Pilotdata$RT_Trial <- raw_Pilotdata$ExpDisplay.RT
raw_Pilotdata$ACC_Trial <- raw_Pilotdata$ExpDisplay.ACC

#Observing RT and ACC data
install.packages("Hmisc")
library(Hmisc)
packageVersion("Hmisc")
summary(raw_Pilotdata$RT_Trial)
table(raw_Pilotdata$ACC_Trial)
round(table(raw_Pilotdata$ACC_Trial)/nrow(raw_Pilotdata)*100, digits = 3)
#6.7% of errors were made in the Trials

####Removing RTS and Outliers####
#removing RT values for incorrect Trials
raw_Pilotdata$RT_Trial[raw_Pilotdata$ACC_Trial == 0] <- NA
summary(raw_Pilotdata$RT_Trial)

#OUtliers
#creating function to remove outliers and farouts
computeTukeys <- function(x){
  P25 <- quantile(x$RT, .25, na.rm = TRUE, type = 6) #type = 6 -> nimmt SPSS
  P75 <- quantile(x$RT, .75, na.rm = TRUE, type = 6)
  x$Outlier <- P75 + 1.5*(P75 - P25)
  x$Farouts <- P75 + 3.0*(P75 - P25)
  return(x)
}

#identifying the outliers on the individual level

raw_Pilotdata<-  ddply(raw_Pilotdata, .(Subject), computeTukeys)
summary(raw_Pilotdata$RT_Trial)
#for global outliers
raw_Pilotdata <- computeTukeys(raw_Pilotdata)

#excluding the outliers...change to Farouts based on the type of analysis
raw_Pilotdata$RT_Trial[raw_Pilotdata$RT_Trial > raw_Pilotdata$Farouts|raw_Pilotdata$RT_Trial < 300] <- NA
summary(raw_Pilotdata$RT_Trial)

#Mean of RTs before excluding farouts was 472ms and after exclusion 472.2ms and for outliers 478ms(global)
#For individual the RTs before exluding farours was 495ms and after exclusion 489ms and for outliers 480ms

####creating new variable for Errorrate and salience ####
raw_Pilotdata$ErrorRate <- 1 - raw_Pilotdata$ACC_Trial

#creating new variable for Salience
raw_Pilotdata$salience<-ifelse(raw_Pilotdata$Distractor1=="Bahn"| raw_Pilotdata$Distractor1=="Tisch",1,0)

# #exclude trials with stimulus repetition from trial n-1 to trial n ####
# 
# names(raw_Pilotdata) 
# 
# #test code
# # temp <- subset(x= raw.data,
# #                select = c("distractor1", "RT"))
# # 
# # temp$stimRep<-ifelse(shift(temp$distractor1, n = 1, type = "lag") == temp$distractor1, 1, 0)
# # table(temp$stimRep)
# # #exclude stim rep trials from CL data
# # summary(temp$RT)
# # temp$RT[temp$stimRep==1]<-NA
# # #works
# 
# summary(raw_Pilotdata$RT)
# raw_Pilotdata$stimRep<-ifelse(shift(raw_Pilotdata$distractor1, n = 1, type = "lag") == raw_Pilotdata$distractor1, 1, 0)
# table(raw.data$stimRep)
# 
# #exclude stim rep trials from CL data

# raw_Pilotdata$RT_Trial[raw_Pilotdata$stimRep==1] <- NA
# summary(raw_Pilotdata$RT_Trial)
# #total NA's (ifo): 2711 

####create aggregate data with value means across different condition####
#change name based on individual or global farouts/outliers
rawpilot.agg.gfo <- aggregate(data = raw_Pilotdata, RT_Trial~Saliency+validity+Subject+CorrectAnswer, mean)
rawpilot.aggE.gfo <- aggregate(data = raw_Pilotdata, ErrorRate~salience+validity+Subject, mean)

#changing variables to factors
rawpilot.agg.gfo$Subject <- as.factor(rawpilot.agg.gfo$Subject)
rawpilot.agg.gfo$salience <- as.factor(rawpilot.agg.gfo$salience)
#for Error rate aggreagated df
rawpilot.aggE.gfo$Subject <- as.factor(rawpilot.aggE.gfo$Subject)
rawpilot.aggE.gfo$salience <- as.factor(rawpilot.aggE.gfo$salience)

####Analysis using ANOVA####
#change name of anova based on individual/global farouts/outliers
anova_gfo <- ezANOVA(data = rawpilot.agg.gfo,
                    dv = ACC_Trial,
                    wid = Subject,
                    within = .(salience,validity,CorrectAnswer),
                    detailed = TRUE)
anova_out(anova_gfo)

#Anova for Error Rate
anovaE_ifo <- ezANOVA(data = rawpilot.aggE.fo,
                      dv = ErrorRate,
                      wid = Subject,
                      within = .(salience,validity),
                      detailed = TRUE)
anova_out(anovaE_ifo)
#graphic 4way Interaction
ezPlot(data = rawpilot.agg.gfo, 
       dv = RT_Trial, 
       wid = Subject, 
       within = .(salience,validity), 
       x=validity, levels = list(validity = list(new_order = c('valid', 'invalid'))),
       row =salience,
)


mean_gfo <- ezStats(data =rawpilot.agg.gfo, 
                    dv = ACC_Trial, 
                    wid = Subject, 
                    within = .(salience, validity)) 


# ####calculating the interaction based on regression analysis####
# model_pilot_ifo <- lm(RT_Trial ~ salience+ Condition + validity + 
#                       salience*Condition + Condition*validity + salience*validity +
#                       salience*Condition*validity, data = rawpilot.agg.gfo)
# meansOver3Cl <- emmip(model_pilot_ifo, salience~validity)
# #Other ways of plotting the interaction
# plot_model(model_pilot_ifo,type = "int", group.terms = salience, terms = "validity", "salience", "Condition")
# ggplot(rawpilot.agg.gfo, aes(x=validity, y=RT_Trial, fill=salience))+
#   geom_bar(stat = "identity",position = "dodge")

