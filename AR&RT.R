rm(list=ls())

Subj <- 77
cc <- matrix(NA, nrow = Subj, ncol = 10)
dd <- matrix(NA, nrow = Subj, ncol = 10)
for (k in 1:Subj){
  if(k < 10){
    x <- paste("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/ADHD_DM_000", as.character(k), "_MRI", sep = "")
    y <- paste("ADHDDM000", as.character(k), sep = "")
    z <- paste("ADHDDM000", as.character(k), "r", sep = "")
  }else{
    x <- paste("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/ADHD_DM_00", as.character(k), "_MRI", sep = "")
    y <- paste("ADHDDM00", as.character(k), sep = "")
    z <- paste("ADHDDM00", as.character(k), "r", sep = "")
  }
  setwd(x)
  
  c <- NULL
  for (j in 1:4){
    b <- paste(z, as.character(j), ".csv", sep = "")
    a <- read.csv(b, header = TRUE, sep = ",", 
                  col.names = c("T", "C", "PB", "MG", "OC", "RP", "RT", "S", "EV", "Var", "FT", "FC", "Ses", "Subj"))
    c <- rbind(c, a)
  } 

# set matrix for put AR
aa <- matrix(NA, nrow = 5, ncol = 2)
colnames(aa) <- c("H", "L")
rownames(aa) <- c("HH", "MH", "MM", "ML", "LL")

#????????????
c$C <- as.character(c$C)
c$C[c$C == "1"] <- "ProbHH_valueH"
c$C[c$C == "2"] <- "ProbHH_valueL"
c$C[c$C == "3"] <- "ProbMH_valueH"
c$C[c$C == "4"] <- "ProbMH_valueL"
c$C[c$C == "5"] <- "ProbMM_valueH"
c$C[c$C == "6"] <- "ProbMM_valueL"
c$C[c$C == "7"] <- "ProbML_valueH"
c$C[c$C == "8"] <- "ProbML_valueL"
c$C[c$C == "9"] <- "ProbLL_valueH"
c$C[c$C == "10"] <- "ProbLL_valueL"


# calculate AR
for (i in c("HH", "MH", "MM", "ML", "LL")){
  for (j in c("H", "L")){
A <- subset(c, C == paste("Prob", i, "_value", j, sep = "") & RP == 1, select = c(RP))
R <- subset(c, C == paste("Prob", i, "_value", j, sep = "") & RP == 0, select = c(RP))
AR <- nrow(A)/(nrow(A)+nrow(R))
aa[i, j] <- AR
  }
}
cc[k, 1:5] <- aa[1:5, 1]
cc[k, 6:10] <- aa[1:5, 2]
write.table(aa, file = paste(paste(y, "AR", sep = ""),".csv", sep = ""), sep = ",", row.names = FALSE)

# set matrix for RT and calculate RT
bb <- matrix(NA, nrow = 5, ncol = 2)
colnames(bb) <- c("H", "L")
rownames(bb) <- c("HH", "MH", "MM", "ML", "LL")
for (i in c("HH", "MH", "MM", "ML", "LL")){
  for (j in c("H", "L")){
    B <- subset(c, C == paste("Prob", i, "_value", j, sep = "") , select = c(RT))
    RT <- as.numeric(colSums(B)/12)
    bb[i, j] <- RT
  }
}
dd[k, 1:5] <- bb[1:5, 1]
dd[k, 6:10] <- bb[1:5, 2]
write.table(bb, file = paste(paste(y, "RT", sep = ""),".csv", sep = ""), sep = ",", row.names = FALSE)
}

Group <- data.frame()
ADHDMX <- c(3,5:10,13:16,18:20,26,28,32,38,40,43,49,57,58,60,61,67,68,69,70,75,76,77)
CNTLMX <- c(1,2,4,12,17,22:25,29,30,33,36,37,39,42,44:48,50:56,59,63,65,73)
SIBLMX <- c(11,21,27,31,34,35,41,62,64,66,71,72,74)
Group[ADHDMX, 1] <- "ADHD"
Group[CNTLMX, 1] <- "CNTL"
Group[SIBLMX, 1] <- "SIB"
ccc <- cbind(cc, Group)
ddd <- cbind(dd, Group)

setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data")
write.table(ccc, file = "ADHDDMAR.csv", sep = ",", col.names = F)
write.table(ddd, file = "ADHDDMRT.csv", sep = ",", col.names = F)

# AR <- cc
# RT <- dd
# 
# for (i in 1:9){
#   jpeg(file = paste("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/ADHD_DM_000", 
#                     i,"_MRI/AR(H)_", i, ".jpg", sep = ""))
#   mytitle = paste("High magnitude in AR_", i, sep = "")
#   plot(AR[i,5:1], type = "o", main = mytitle)
#   dev.off()
# }
# 
# for (i in 10:73){
#   jpeg(file = paste("~/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/ADHD_DM_00", 
#                     i,"_MRI/AR(H)_", i, ".jpg", sep = ""))
#   mytitle = paste("High magnitude in AR_", i, sep = "")
#   plot(AR[i,5:1], type = "o", main = mytitle)
#   dev.off()
# }
# 
# for (i in 1:9){
#   jpeg(file = paste("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/ADHD_DM_000", 
#                     i,"_MRI/AR(L)_", i, ".jpg", sep = ""))
#   mytitle = paste("Low magnitude in AR_", i, sep = "")
#   plot(AR[i,10:6], type = "o", main = mytitle)
#   dev.off()
# }
# 
# for (i in 10:73){
#   jpeg(file = paste("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/ADHD_DM_00", 
#                     i,"_MRI/AR(L)_", i, ".jpg", sep = ""))
#   mytitle = paste("Low magnitude in AR_", i, sep = "")
#   plot(AR[i,10:6], type = "o", main = mytitle)
#   dev.off()
# }


