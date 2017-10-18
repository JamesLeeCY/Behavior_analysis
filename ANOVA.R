rm(list=ls())
library(dplyr)

setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data")
ADHDDMTT <- read.csv("ADHDDMTT.csv", header = F, sep = ",", col.names = c("T", "C", "PB", "MG", "OC", "RP", "RT", "S", "EV", "Var", "FT", "FC", "Ses", "Subj"))
ADHDDMAR <- read.csv("ADHDDMAR.csv", header = F, sep = ",", col.names = c("Subj", "HHH", "MHH", "MMH", "MLH", "LLH", "HHL", "MHL", "MML", "MLL", "LLL", "Condition"))
ADHDDMRT <- read.csv("ADHDDMRT.csv", header = F, sep = ",", col.names = c("Subj", "HHH", "MHH", "MMH", "MLH", "LLH", "HHL", "MHL", "MML", "MLL", "LLL", "Condition"))


# write table for ANOVA
ANOVA <- data.frame()
Subj <- 77
CP <- c("HH", "MH", "MM", "ML", "LL", "HH", "MH", "MM", "ML", "LL")
CM <- c("H","H","H","H","H","L","L","L","L","L")
for (k in 1:10){
for (i in 1:Subj){
  ANOVA[i+Subj*(k-1), 1] <- ADHDDMAR[i, 1]
  ANOVA[i+Subj*(k-1), 2] <- ADHDDMAR[i, k+1]
  ANOVA[i+Subj*(k-1), 3] <- ADHDDMRT[i, k+1]
  ANOVA[i+Subj*(k-1), 4] <- ADHDDMRT[i, 12]
  ANOVA[i+Subj*(k-1), 5] <- CP[k]
  ANOVA[i+Subj*(k-1), 6] <- CM[k]
}
}
colnames(ANOVA) <- c("SB", "AR", "RT", "C", "P", "M")
MANOVA <- filter(ANOVA, C == "ADHD" | C == "CNTL")

# running ANOVA
library(car)
aov.AR<- aov(AR~C*P*M ,  MANOVA)
aovAR <- summary(aov.AR)
Anova( aov.AR, type = 3)

aov.RT<- aov(RT~C*P*M ,  MANOVA)
aovRT <- summary(aov.RT)
Anova( aov.RT, type = 3)

# aov.RT <- aov(RT~C*P*M + Error(SB/(P*M)),  MANOVA)
# aovRT <- summary(aov.RT)
# aovRT

