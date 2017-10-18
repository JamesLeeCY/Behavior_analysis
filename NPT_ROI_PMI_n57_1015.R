rm(list = ls())

library(dplyr)
library(corrplot)
library(Hmisc)
library(ggplot2)
library(ppcor)
library(ggpubr)
setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data/")
pt <- function(a,b){
  paste(a,as.character(b),sep = "")
}

#read data
IQ <- read.csv("DSM_selfreport_IQ_0522.csv")
CPT <- read.csv("DSM_CPT_0522.csv")
DT <- read.csv('DT_sqrt_1015.csv')
AR <- read.csv('ADHDDMAR.csv', header = F)
RT <- read.csv('ADHDDMRT.csv', header = F)
Cantab <- read.csv("DSM_CANTAB_0522.csv")
KE <- read.csv("DSM_KE_0522.csv")

#remove NA & bad data
IQ <- IQ[-c(3,13,22,23,27,31,38,52,54,61,65,67,71,74,75),]
CPT <- CPT[-c(3,13,22,23,27,31,38,52,54,61,65,67,71,74,75),]
AR <- AR[, -c(1,12)]
RT <- RT[, -c(1,12)]
AD <- cbind(DT,AR,RT)
AD <- AD[-c(3,13,22,23,27,31,38,52,54,61,65,67,71,74,75),]
AD <- AD[, -c(1,3)]
Cantab <- Cantab[-c(3,13,22,23,27,31,38,52,54,61,65,67,71,74,75), ]
CGT <- Cantab[,67:86]

#assign group for IQ
IQ <- mutate(IQ, Group = NA)
IQ[grep("5....", IQ$famid), "Group"] <- "CNTL"
IQ[grep("6...1", IQ$famid), "Group"] <- "ADHD"
IQ[grep("6...4", IQ$famid), "Group"] <- "SIB"
CNTL <- grep("5....", IQ$famid)
ADHD <- grep("6...1", IQ$famid)
SIB <- grep("6...4", IQ$famid)

#rearrange dataframe
IQADHD <- IQ[ADHD,]
IQCNTL <- IQ[CNTL,]
NIQ <- rbind(IQADHD, IQCNTL)
NIQ <- NIQ[,c("VIQ","PIQ","FIQ","VCI","POI","WMI")]

CPTADHD <- CPT[ADHD,]
CPTCNTL <- CPT[CNTL,]
NCPT <- rbind(CPTADHD, CPTCNTL)
NCPT <- NCPT[,c("rn_omis", "r_rtsd", "r_var","r_detect", "r_rtbc", "r_sebc", "rn_comis", "r_rt", 
                "r_rpsty", "r_per", "r_rtisi", "r_seisi")]
# colnames(NCPT) <- c("Omissions","Hit_RT_standard_error","Variability","Detectability","Hit_RT_changed_by_block",
#                     "Hit_RT_standard_error_changed_by_block","Commissions","Hit_reaction_time",
#                     "Response_style","Perseverations","Hit_reaction_time_inter-stimulus_intervals",
#                     "Hit_standard_error_changed_by_ISI")

ADA <- AD[ADHD,]
ADC <- AD[CNTL,]
NAD <- rbind(ADA, ADC)
colnames(NAD) <- c('DT','DT_sqrt','AR_HHH', "AR_MHH", "AR_MMH", "AR_MLH", "AR_LLH", "AR_HHL", 
                   "AR_MHL", "AR_MML", "AR_MLL", "AR_LLL", 'RT_HHH', "RT_MHH", "RT_MMH", 
                   "RT_MLH", "RT_LLH", "RT_HHL", "RT_MHL", "RT_MML", "RT_MLL", "RT_LLL")

CGTA <- CGT[ADHD,]
CGTC <- CGT[CNTL,]
NCGT <- rbind(CGTA,CGTC)
# colnames(NCGT) <- c("CGT_Delay_aversion","CGT_Delay_aversion_(9:1)","CGT_Delay_aversion_(8:2)","CGT_Delay_aversion_(7:3)",
#                     "CGT_Delay_aversion_(6:4)","CGT_Deliberation_time","CGT_Deliberation_time_(ascending)",
#                     "CGT_Deliberation_time_(descending)","CGT_Overall_proportion_bet","CGT_Overall_proportion_bet_(ascending)",
#                     "CGT_Overall_proportion_bet_(descending)","CGT_Quality_of_decision_making","CGT_Quality_of_decision_making_(ascending)",
#                     "CGT_Quality_of_decision_making_(descending)","CGT_Risk_adjustment","CGT_Risk_adjustment_(ascending)",
#                     "CGT_Risk_adjustment_(descending)","CGT_Risk_taking","CGT_Risk_taking_(ascending)","CGT_Risk_taking_(descending)")

NPT <- cbind(NAD, NIQ, NCPT,NCGT)

#process ROI data
setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/ROI/ROI_0929_PMI/")
choiceNLP <- data.frame(row.names = 1:57)
for (i in 1:6){
  # combine 10 EV choice ROI 
    FP_choice <- paste('ROI_0929_PMI_', as.character(i), '.csv', sep = "")
    x <- read.csv(FP_choice, header = F, sep = ",",
                  col.names = c(pt('NLP1_',i),pt('NLP2_',i),pt('NLP3_',i),
                                pt('NLP4_',i),pt('NLP5_',i),pt('NLP6_',i)))
    choiceNLP <- cbind(choiceNLP,x)
}

#remove NA @NPT (Be aware of the order is grouping in ROI data with ADHD and HC respectively) 
# Also most NA is in sibling, ADHD:3, HC:1
ROI <- choiceNLP[-c(7,17,24,35),]

#bind NPT and ROI
TT <- cbind(NPT, ROI) 
# TT <- TT[-21,] # remove outlier based on response style (r_rpsty)
TT_ADHD <- TT[1:26,]
TT_CNTL <- TT[27:53,]

#subgrouping LCT and CGT with ROI data
LCTT <- TT[,c(1:22,61:96)]
LCTT_ADHD <- LCTT[1:26,]
LCTT_CNTL <- LCTT[27:53,]

CGTT <- TT[,c(41:96)]
CGTT_ADHD <- CGTT[1:26,]
CGTT_CNTL <- CGTT[27:53,]

Group <- as.factor(c(rep('ADHD',26), rep('HC',27)))
LCTT_All <- cbind(LCTT,Group)
CGTT_All <- cbind(CGTT,Group)

###################
setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/ROI/ROI_0929_PMI/")
#ROI correlation computation, and report p value over 0.05 with mutiple correlation in ALL 
kk <- rcorr(as.matrix(TT), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 96, ncol = 96) # adjust according to the dimension of colume size
R <- kk[["r"]]
P_value <- P[1:60,61:ncol(TT)]
r <- R[1:60,61:ncol(TT)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_All_1015.csv', row.names = F)

#ROI correlation computation, and report p value over 0.05 with mutiple correlation in ADHD
kk <- rcorr(as.matrix(TT_ADHD), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 96, ncol = 96)
R <- kk[["r"]]
P_value <- P[1:60,61:ncol(TT_ADHD)]
r <- R[1:60,61:ncol(TT_ADHD)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_ADHD_1015.csv', row.names = F)

#ROI correlation computation, and report p value over 0.05 with mutiple correlation in ADHD
kk <- rcorr(as.matrix(TT_CNTL), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 96, ncol = 96)
R <- kk[["r"]]
P_value <- P[1:60,61:ncol(TT_CNTL)]
r <- R[1:60,61:ncol(TT_CNTL)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_HC_1015.csv', row.names = F)

## LCT task
#plot correlation matrix
M <- cor(LCTT)
pdf("correlation matrix for LCT and ROI_All.pdf", 12, 12)
corrplot(M, method = "circle", tl.cex = 0.5, col= colorRampPalette(c("blue","white", "red"))(100))
dev.off()

#ROI correlation computation, and report p value over 0.05 with mutiple correlation in ALL LCT
kk <- rcorr(as.matrix(LCTT), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 58, ncol = 58)
R <- kk[["r"]]
P_value <- P[1:22,23:ncol(LCTT)]
r <- R[1:22,23:ncol(LCTT)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_LCT_All_1015.csv', row.names = F)

### plotting correlation figure
ggplot(LCTT_All, aes(x=NLP4_1, y=AR_MMH, color=Group, shape=Group)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggtitle("Correlation between AR_MMH and L_DLPFC in two group from HH_H") +
  theme(text = element_text(size = 10),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
ggsave("Correlation between AR_MMH and L_DLPFC in two group from HH_H.png", 
       width = 150, height = 100, units = 'mm')

ggplot(LCTT_All, aes(x=NLP4_9, y=AR_MMH, color=Group, shape=Group)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggtitle("Correlation between AR_MMH and L_DLPFC in two group from LL_H") +
  theme(text = element_text(size = 10),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
ggsave("Correlation between AR_MMH and L_DLPFC in two group from LL_H.png", 
       width = 150, height = 100, units = 'mm')

ggplot(LCTT_All[LCTT_All$DT >-40,], aes(x=NLP2_3, y=DT, color=Group, shape=Group)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggtitle("Correlation between DT and Left MTL in two group from MH_H") +
  theme(text = element_text(size = 10),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
ggsave("Correlation between DT and Left MTL in two group from MH_H.png", 
       width = 150, height = 100, units = 'mm')

ggplot(LCTT_All, aes(x=NLP5_1, y=AR_MML, color=Group, shape=Group)) +
  geom_point(shape=1) +
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE) +
  ggtitle("Correlation between AR_MML and Left DLPFC in two group from HH_H") +
  theme(text = element_text(size = 10),panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),panel.background = element_blank(), 
        axis.line = element_line(colour = "black")) 
ggsave("Correlation between AR_MML and Left DLPFC in two group from HH_H.png", 
       width = 150, height = 100, units = 'mm')

# ggscatter(LCTT_All[LCTT_All$DT >-40,], x="DT", y="NLP2_3", color = "Group",
#           shape = 21, size = 3,
#           add = "reg.line", add.params = list(color = "blue", fill = "lightgray"),
#           conf.int = TRUE, cor.coef = TRUE,
#           cor.coeff.args = list(method = "pearson", label.sep = "\n")
#           ) 
# ggsave('Correlation between DT and Left MTL (ALL).png',
#        width = 150, height = 100, units = 'mm')


#plot correlation matrix
M <- cor(LCTT_ADHD)
pdf("correlation matrix for LCT and ROI_ADHD.pdf", 12, 12)
corrplot(M, method = "circle", tl.cex = 0.5, col= colorRampPalette(c("blue","white", "red"))(100))
dev.off()

#ROI correlation computation, and report p value over 0.05 with mutiple correlation in ADHD LCT
kk <- rcorr(as.matrix(LCTT_ADHD), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 58, ncol = 58)
R <- kk[["r"]]
P_value <- P[1:22,23:ncol(LCTT)]
r <- R[1:22,23:ncol(LCTT)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_LCT_ADHD_1015.csv', row.names = F)

#plot correlation matrix
M <- cor(LCTT_CNTL)
pdf("correlation matrix for LCT and ROI_HC.pdf", 12, 12)
corrplot(M, method = "circle", tl.cex = 0.5, col= colorRampPalette(c("blue","white", "red"))(100))
dev.off()

#ROI correlation computation, and report p value over 0.05 with mutiple correlation in CNTL LCT
kk <- rcorr(as.matrix(LCTT_CNTL), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 58, ncol = 58)
R <- kk[["r"]]
P_value <- P[1:22,23:ncol(LCTT)]
r <- R[1:22,23:ncol(LCTT)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_LCT_CNTL_1015.csv', row.names = F)

### plotting correlation figure
ggplot(LCTT_CNTL, aes(x=RT_MML, y=Var_9 )) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("Correlation between RT_MML and Putamen_R (Var) (HC)")
ggsave("Correlation between RT_MML and Putamen_R (Var) (HC).png")

#plot correlation matrix
M <- cor(CGTT)
pdf("correlation matrix for CGT and ROI_All.pdf", 12, 12)
corrplot(M, method = "circle", tl.cex = 0.5, col= colorRampPalette(c("blue","white", "red"))(100))
dev.off()

#ROI correlation computation, and report p value over 0.05 with mutiple correlation in ALL CGT
kk <- rcorr(as.matrix(CGTT), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 56, ncol = 56)
R <- kk[["r"]]
P_value <- P[1:20,21:ncol(CGTT)]
r <- R[1:20,21:ncol(CGTT)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_CGT_All_1015.csv', row.names = F)

# plot
ggplot(CGTT, aes(x=CGTdA9, y=AL_8 )) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("NPT(VIQ)_correlated_with_ROI(Thalamus_L (choice))_ADHD")
ggsave("NPT(VIQ)_correlated_with_ROI(Thalamus_L (choice))_ADHD.png")

#plot correlation matrix
M <- cor(CGTT_ADHD)
pdf("correlation matrix for CGT and ROI_ADHD.pdf", 12, 12)
corrplot(M, method = "circle", tl.cex = 0.5, col= colorRampPalette(c("blue","white", "red"))(100))
dev.off()

#ROI correlation computation, and report p value over 0.05 with mutiple correlation in ADHD CGT
kk <- rcorr(as.matrix(CGTT_ADHD), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 56, ncol = 56)
R <- kk[["r"]]
P_value <- P[1:20,21:ncol(CGTT)]
r <- R[1:20,21:ncol(CGTT)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_CGT_ADHD_1015.csv', row.names = F)

#plot correlation matrix
M <- cor(CGTT_CNTL)
pdf("correlation matrix for CGT and ROI_HC.pdf", 12, 12)
corrplot(M, method = "circle", tl.cex = 0.5, col= colorRampPalette(c("blue","white", "red"))(100))
dev.off()

#ROI correlation computation, and report p value over 0.05 with mutiple correlation in CNTL CGT
kk <- rcorr(as.matrix(CGTT_CNTL), type = "pearson")
P <- kk[["P"]]
P <- p.adjust(P, method = "fdr")
P <- matrix(P, nrow = 56, ncol = 56)
R <- kk[["r"]]
P_value <- P[1:20,21:ncol(CGTT)]
r <- R[1:20,21:ncol(CGTT)]

p005_c <- data.frame(which(P_value < 0.05, arr.ind = TRUE))

for(i in 1:nrow(p005_c)){
  p005_c[i,3] <- rownames(r)[p005_c[i,1]]
  p005_c[i,4] <- colnames(r)[p005_c[i,2]]
  p005_c[i,5] <- P_value[p005_c[i,1],p005_c[i,2]]
  p005_c[i,6] <- r[p005_c[i,1],p005_c[i,2]]
}
p005_c <- p005_c[,-c(1,2)]
colnames(p005_c) <- c('F1','F2','p-value','r')
write.csv(p005_c,'ROI_NPT_p005fdr_CGT_CNTL_1015.csv', row.names = F)

###
ggplot(TT_ADHD, aes(x=VIQ, y=choice_4 )) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  ggtitle("NPT(VIQ)_correlated_with_ROI(Thalamus_L (choice))_ADHD")
ggsave("NPT(VIQ)_correlated_with_ROI(Thalamus_L (choice))_ADHD.png")
