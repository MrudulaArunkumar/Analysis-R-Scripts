
library(tidyverse)

#setting up working directory
#home office
dir <- setwd("C:/FSU Jena_PhD/Exp1/Data/Exp1-Prolific")
#create a variable containing all csv files and make a list
allresults <- list.files(path = dir, pattern = "*.csv")
Exp1data_list <- lapply(allresults,read_csv)
#transform and combine it all into one df
Exp1data_OL<- do.call(rbind,Exp1data_list)

#export this df to one csv file and save it
write.csv(Exp1data_OL, file = "Exp1data_online_fullset.csv")
table(Exp1data_OL$Condition,Exp1data_OL$Validity, Exp1data_OL$Block)





