rm(list=ls())

d <- NULL #The big dataframe to take the whole data
#subject
for (k in 1:77){
  if(k < 10){
  x <- paste("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/ADHD_DM_000", as.character(k), "_MRI", sep = "")
  y <- paste("ADHDDM000", as.character(k), "r", sep = "")
  z <- paste("ADHD_DM_000", as.character(k), "_1_r", sep = "")
  }else{
  x <- paste("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/ADHD_DM_00", as.character(k), "_MRI", sep = "")
  y <- paste("ADHDDM00", as.character(k), "r", sep = "")
  z <- paste("ADHD_DM_00", as.character(k), "_1_r", sep = "")
}
  setwd(x) #set path at each behavior data folder
  
  c <- NULL
  #sessions
for (j in 1:4){
  b <- paste(z, as.character(j), ".csv", sep = "") #write name for each csv file in folder
  a <- read.csv(b, header = FALSE, sep = ",", col.names = c("T", "C", "PB", "MG", "OC", "RP", "RT", "S", "EV", "Var", "FT", "FC"))
  
  # rearrange the origin csv file 
  for (i in seq(1,59, by=2)){
    a[i,5] <- a[i+1,5]
    a[i,8] <- a[i+1,8]
    a[i,1] <- as.numeric(a[i,1])/1000
    a[i+1,1] <- as.numeric(a[i+1,1])/1000 # deal with the problem of odd number in i
    a[i,11] <- a[i+1,1]
    a[i,12] <- as.character(a[i+1,2])
    #replace the 10 condition as number
    a$C <- as.character(a$C)
    a$C[a$C == "ProbHH_valueH"] <- 1
    a$C[a$C == "ProbHH_valueL"] <- 2
    a$C[a$C == "ProbMH_valueH"] <- 3
    a$C[a$C == "ProbMH_valueL"] <- 4
    a$C[a$C == "ProbMM_valueH"] <- 5
    a$C[a$C == "ProbMM_valueL"] <- 6
    a$C[a$C == "ProbML_valueH"] <- 7
    a$C[a$C == "ProbML_valueL"] <- 8
    a$C[a$C == "ProbLL_valueH"] <- 9
    a$C[a$C == "ProbLL_valueL"] <- 10
    #replace the 5 feedback as number
    a$FC <- as.character(a$FC)
    a$FC[a$FC == "A_GAIN"] <-1
    a$FC[a$FC == "A_LOSS"] <-2
    a$FC[a$FC == "R_GAIN"] <-3
    a$FC[a$FC == "R_LOSS"] <-4
    a$FC[a$FC == "Dummy"] <-5
    # replace response as 1 and 0
    a$RP <- as.character(a$RP)
    a$RP[a$RP == "4"] <- 1
    a$RP[a$RP == "3"] <- 0
  }
  a <- a[seq(-2,-60, by=-2), ] # delete the empty row
  
  #calculate EV & VAR
  MTAD <- matrix(NA, nrow = 30, ncol = 4)
  MTAD[,1] <- as.numeric(as.character(a$PB))
  MTAD[,2] <- as.numeric(as.character(a$MG))
  MTAD[,3] <- MTAD[,1]*0.01*MTAD[,2]+(1-MTAD[,1]*0.01)*(-MTAD[,2]) #calculate EV
  MTAD[,4] <- MTAD[,1]*0.01*((MTAD[,2]-MTAD[,3])^(2))+(1-MTAD[,1]*0.01)*(((-MTAD[,2])-MTAD[,3])^2) #calculate Var
  a$EV <- MTAD[,3]
  a$Var <- MTAD[,4]
  
  #write for seesion and subject
  Ses <- (matrix(1, 30, 1))*j
  Subj <- (matrix(1, 30, 1))*k
  ss <- cbind(a, Ses, Subj)
  write.table(ss, file = paste(paste(y,as.character(j), sep = ""),".csv", sep = ""), sep = ",", row.names = FALSE)
  c <- rbind(c, ss)
}
  write.table(c, file = paste(y,".csv", sep = ""), sep = ",", col.names = FALSE)
  #combine each subject row 
  d <- rbind(d, c)
}

setwd("~/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data")
write.table(d, file = "ADHDDMTT.csv", sep = ",", col.names = FALSE, row.names = FALSE)

# 
# # examine the number of feedback condition in each session and subject
# library(dplyr)
# 
# setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data")
# SR <- read.csv('DSM_selfreport_IQ_0522.csv')
# 
# #assign group for SR
# SR <- mutate(SR, Group = NA)
# SR[grep("5....", SR$famid), "Group"] <- "CNTL"
# SR[grep("6...1", SR$famid), "Group"] <- "ADHD"
# SR[grep("6...4", SR$famid), "Group"] <- "SIB"
# CNTL <- grep("5....", SR$famid)
# ADHD <- grep("6...1", SR$famid)
# SIB <- grep("6...4", SR$famid)
# 
# #dataframe for number of trials
# x <- data.frame()
# x[1:1232,1] <- rep(c(1:77), each = 16)
# x[1:1232,2] <- rep(c(1:4), each = 4)
# x[1:1232,3] <- rep(c(1:4), each = 1)
# x[1:1232,4] <- NA
# x[1:1232,5] <- rep(SR$Group, each = 16)
# 
# 
# test <- NULL
# for (i in 1:77){
#   for (j in 1:4){
#     for (k in 1:4){
#       test <- append(test, nrow(subset(d, Subj == i & Ses == j & FC == k)))
#     }
#   }
# }
# x[,4] <- test
# colnames(x) <- c('Subj','Sess','FC','NoT','Group')
# 
# N0 <- subset(x, NoT == 0)
# N1 <- subset(x, NoT == 1)
# 
# 
# # Or this method by YZ (V1 to V4 stand fro Subject, session, feedback condition, frequency of FC)
# y <- as.data.frame(table(d$Subj, d$Ses, d$FC))
# 
