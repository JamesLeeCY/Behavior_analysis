rm(list = ls())
library(dplyr)
library(ggplot2)
library(car)
library(lme4)
library(lmerTest) # make summary of lmer test correct
se <- function(x){
  sd(x)/sqrt(length(x))
}
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/ROI/ROI_0904/ROI_LLH/")
choiceLLH <- data.frame()
for (i in 1:10){
  #combine choice ROI for 10 EV
  FP_choice <- paste('ROI_LLH_0831_', as.character(i), '.csv', sep = "")
  x <- read.csv(FP_choice, header = F, sep = ",", 
                col.names = c('LLH1','LLH2','LLH3','LLH4','LLH5','LLH6'))
  choiceLLH <- rbind(choiceLLH,x)
}

setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/ROI/ROI_0904/ROI_MLH/")
choiceMLH <- data.frame()
for (i in 1:10){
  #combine choice ROI for 10 EV
  FP_choice <- paste('ROI_MLH_0831_', as.character(i), '.csv', sep = "")
  x <- read.csv(FP_choice, header = F, sep = ",", 
                col.names = c('MLH1','MLH2','MLH3','MLH4','MLH5','MLH6','MLH7','MLH8','MLH9'))
  choiceMLH <- rbind(choiceMLH,x)
}

setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/ROI/ROI_0904/ROI_MMH/")
choiceMMH <- data.frame()
for (i in 1:10){
  #combine choice ROI for 10 EV
  FP_choice <- paste('ROI_MMH_0831_', as.character(i), '.csv', sep = "")
  x <- read.csv(FP_choice, header = F, sep = ",", 
                col.names = c('MMH1','MMH2','MMH3','MMH4','MMH5','MMH6','MMH7','MMH8','MMH9',
                              'MMH10','MMH11','MMH12','MMH13'))
  choiceMMH <- rbind(choiceMMH,x)
}

choiceTT <- cbind(choiceLLH,choiceMLH,choiceMMH) # Total ROI for PLML
choice <- dplyr::select(choiceTT, LLH1,LLH2,LLH4,MLH1,MLH5,MLH7,MLH8,MMH1,MMH8) # The significant ROI

Condition <- c('HH_H','HH_L','MH_H','MH_L','MM_H','MM_L','ML_H','ML_L','LL_H','LL_L')
condition <- rep(Condition, each = 57)
Prob <- factor(rep(c('HH','MH','MM','ML','LL'), each = 114), levels = c('LL', 'ML', 'MM', 'MH', 'HH'), order = T)
Mag <- factor(rep(c('H','L'), each = 57, times = 5), levels = c('L', 'H'), order = T)
ADHD <- rep('ADHD',29)
HC <- rep('HC',28)
Group <- factor(rep(c(ADHD,HC),10), levels = c('HC','ADHD'))
Subj <- factor(rep(seq(1,57,1), times = 10))
choice <- cbind(choice,condition,Group,Prob,Mag,Subj)


# choice plotting preparation (devide Magnitude and Probability)
choice_stat <- choice %>% group_by(condition,Group) %>% 
  summarise(MLLH1=mean(LLH1),MLLH2=mean(LLH2),MLLH4=mean(LLH4),MMLH1=mean(MLH1),MMLH5=mean(MLH5),
            MMLH7=mean(MLH7),MMLH8=mean(MLH8),MMMH1=mean(MMH1),MMMH8=mean(MMH8),
            SELLH1=se(LLH1),SELLH2=se(LLH2),SELLH4=se(LLH4),SEMLH1=se(MLH1),SEMLH5=se(MLH5),
            SEMLH7=se(MLH7),SEMLH8=se(MLH8),SEMMH1=se(MMH1),SEMMH8=se(MMH8)) %>% as.data.frame() 
Prob <- rep(c('HH','LL','MH','ML','MM'), each = 4)
Mag <- rep(c('H','L'), each = 2, times = 5)
choice_stat <- cbind(choice_stat,Prob,Mag)
choice_stat$Prob <- factor(choice_stat$Prob, levels = c('LL','ML','MM','MH','HH'))

# choice plotting preparation (combine Magnitude)
choice_stat2 <- choice %>% group_by(Prob,Group) %>% 
  summarise(MLLH1=mean(LLH1),MLLH2=mean(LLH2),MLLH4=mean(LLH4),MMLH1=mean(MLH1),MMLH5=mean(MLH5),
            MMLH7=mean(MLH7),MMLH8=mean(MLH8),MMMH1=mean(MMH1),MMMH8=mean(MMH8),
            SELLH1=se(LLH1),SELLH2=se(LLH2),SELLH4=se(LLH4),SEMLH1=se(MLH1),SEMLH5=se(MLH5),
            SEMLH7=se(MLH7),SEMLH8=se(MLH8),SEMMH1=se(MMH1),SEMMH8=se(MMH8)) %>% as.data.frame()
choice_stat2$Prob <- factor(choice_stat2$Prob, levels = c('LL','ML','MM','MH','HH'))

# Make the same result as choice_stat
choice_stat3 <- choice %>% group_by(Prob,Mag,Group) %>% 
  summarise(MLLH1=mean(LLH1),MLLH2=mean(LLH2),MLLH4=mean(LLH4),MMLH1=mean(MLH1),MMLH5=mean(MLH5),
            MMLH7=mean(MLH7),MMLH8=mean(MLH8),MMMH1=mean(MMH1),MMMH8=mean(MMH8),
            SELLH1=se(LLH1),SELLH2=se(LLH2),SELLH4=se(LLH4),SEMLH1=se(MLH1),SEMLH5=se(MLH5),
            SEMLH7=se(MLH7),SEMLH8=se(MLH8),SEMMH1=se(MMH1),SEMMH8=se(MMH8)) %>% as.data.frame()
choice_stat3$Prob <- factor(choice_stat3$Prob, levels = c('LL','ML','MM','MH','HH'))

# lmer ANOVA for quadratic effect (LLH1,LLH2,LLH4,MLH1,MLH5,MLH7,MLH8,MMH1,MMH8)
# lmer_data_LLH1 = data.frame(ROI = choice$LLH1, 
#                        Prob = factor(choice$P, levels = c('LL', 'ML', 'MM', 'MH', 'HH'), order = T), 
#                        Mag = factor(choice$M, levels = c('L', 'H'), order = T), 
#                        Group = factor(choice$Group, levels = c('HC','ADHD')),
#                        Subj = factor(rep(seq(1,57,1), times = 10)))
# lmerm_LLH1 = lmer(ROI~Prob*Mag*Group + (1|Subj), data = lmer_data_LLH1, 
#               contrasts = list(Prob = contr.poly(5)[,1:2], Mag = contr.poly(2)))
# summary(lmerm_LLH1)

lmerm_LLH1 = lmer(LLH1~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_LLH1)
lmerm_LLH2 = lmer(LLH2~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_LLH2)
lmerm_LLH4 = lmer(LLH4~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_LLH4)
lmerm_MLH1 = lmer(MLH1~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_MLH1)
lmerm_MLH5 = lmer(MLH5~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_MLH5)
lmerm_MLH7 = lmer(MLH7~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_MLH7)
lmerm_MLH8 = lmer(MLH8~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_MLH8)
lmerm_MMH1 = lmer(MMH1~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_MMH1)
lmerm_MMH8 = lmer(MMH8~P*M*Group + (1|Subj), data = choice, 
                  contrasts = list(P = contr.poly(5)[,1:2], M = contr.poly(2)));summary(lmerm_MMH8)

# ANOVA
aov.LLH1 <- lm(LLH1~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.LLH1, type = 3) # Frontal_Sup_Medial_R
aov.LLH2 <- lm(LLH2~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.LLH2, type = 3) # Frontal_Inf_Orb_R
aov.LLH4 <- lm(LLH4~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.LLH4, type = 3) # Frontal_Mid_R
aov.MLH1 <- lm(MLH1~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.MLH1, type = 3) # Frontal_Sup_Medial_L
aov.MLH5 <- lm(MLH5~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.MLH5, type = 3) # Temporal_Mid_R
aov.MLH7 <- lm(MLH7~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.MLH7, type = 3) # Hippocampus_L
aov.MLH8 <- lm(MLH8~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.MLH8, type = 3) # Insula_L
aov.MMH1 <- lm(MMH1~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.MMH1, type = 3) # Caudate_R
aov.MMH8 <- lm(MMH8~Group*P*M, choice, contrasts = list(Group=contr.sum, P=contr.sum, M=contr.sum))
Anova( aov.MMH8, type = 3) # Frontal_Mid_L


#t test for each condition by M
# t <- choice%>% 
#   group_by(P,Group)%>%
#   do(s=t.test(SMF_C~M,data=.))
# t %>% summarise(p = s$p.value)

# ADHDHHH <- filter(choice, Group == 'ADHD' & condition == 'HH_H') %>% select(FSM_R)
# HCHHH <- filter(choice, Group == 'HC' & condition == 'HH_H') %>% select(FSM_R)
# t.test(ADHDHHH, HCHHH)$p.value

# eval expression usage
# ggplot(choice_stat2, aes(x = P, y = eval(parse(text = "MR_VS"), envir = choice_stat2))) + geom_bar(stat = "identity") + facet_grid(. ~ Group)


# try for dividing the magnitude and group
ggplot(choice_stat3, aes(x=Prob, y=MMMH1, fill=Group, colour=Group)) +
  geom_errorbar(aes(ymin=MMMH1-SEMMH1, ymax=MMMH1+SEMMH1),width= 0.5, 
                position=position_dodge(1), size = 0.5) +
  geom_bar(position=position_dodge(1), stat="identity") +
  facet_wrap(~ Group+Mag) +
  theme(text = element_text(size = 10), legend.title=element_blank(), legend.position="none",
        axis.title = element_text(lineheight=.8, face="bold"), plot.title = element_text(lineheight=.8, face="bold")) +
  labs(x='Probability',y = 'Parameter Estimate (a.u.)') + 
  scale_y_continuous(limits=c(-5, 6)) +
  ggtitle('Neural response estimates across probability levels in Caudate_R') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 


# for loop for plotting
setwd('/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/ROI/ROI_0904/ROI_PLML/')
Nameplot <- c('Frontal_Sup_Medial_R.png','Frontal_Inf_Orb_R.png','Frontal_Mid_R.png',
              'Frontal_Sup_Medial_L.png','Temporal_Mid_R.png','Hippocampus_L.png',
              'Insula_L.png','Caudate_R.png','Frontal_Mid_L.png')
NameM <- c('MLLH1','MLLH2','MLLH4','MMLH1','MMLH5','MMLH7','MMLH8','MMMH1','MMMH8')
NameSE <- c('SELLH1','SELLH2','SELLH4','SEMLH1','SEMLH5','SEMLH7','SEMLH8','SEMMH1','SEMMH8')
Nametitle <- c('Frontal_Sup_Medial_R','Frontal_Inf_Orb_R','Frontal_Mid_R',
               'Frontal_Sup_Medial_L','Temporal_Mid_R','Hippocampus_L',
               'Insula_L.png','Caudate_R','Frontal_Mid_L')
ROIplot <- list()

for ( i in c(1:9)){
ROIplot[[i]] <- ggplot(choice_stat3, aes_string(x='Prob', y=NameM[i], fill='Group', colour='Group')) +
    geom_errorbar(aes_string(ymin= paste(NameM[i],'-',NameSE[i], sep = ''), 
                             ymax= paste(NameM[i],'+',NameSE[i], sep = '')),
                  width= 0.5, position=position_dodge(1), size = 0.5) +
    geom_bar(position=position_dodge(1), stat="identity") +
    facet_wrap(~ Group+Mag) +
    theme(text = element_text(size = 10), legend.title=element_blank(), legend.position="none",
          axis.title = element_text(lineheight=.8, face="bold"), plot.title = element_text(lineheight=.8, face="bold")) +
    labs(x='Probability',y = 'Parameter Estimate (a.u.)') + 
    # scale_y_continuous(limits=c(-8, 10)) +
    ggtitle(paste('Neural response estimates across probability levels in ',Nametitle[i],sep = '')) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) 
  ggsave(Nameplot[i], width = 150, height = 100, units = 'mm')
  
}
pdf('multiplotforROI_0906_PLML.pdf',30,25)
multiplot(ROIplot[[1]],ROIplot[[2]],ROIplot[[3]],ROIplot[[4]],ROIplot[[5]],ROIplot[[6]],
          ROIplot[[7]],ROIplot[[8]],ROIplot[[9]], cols = 3)
dev.off()





# preparing the datafram for each ROI (take R_VS as example)
lmer_data = data.frame(ROI = choice$LLH1, 
                       Prob = factor(choice$Prob, levels = c('LL', 'ML', 'MM', 'MH', 'HH'), order = T), 
                       Mag = factor(choice$Mag, levels = c('L', 'H'), order = T), 
                       Group = factor(choice$Group, levels = c('HC','ADHD')),
                       Subj = factor(rep(seq(1,57,1), times = 10)))

# predict the lmr with only linear and quadratic effect of Probability
lmerm2 = lmer(ROI~Prob*Mag*Group + (1|Subj), data = lmer_data, 
              contrasts = list(Prob = contr.poly(5)[,1:2], Mag = contr.poly(2)))
# summary(lmerm2)
new_lmerm2 = expand.grid(Prob = factor(c('LL', 'ML', 'MM', 'MH', 'HH'), 
                                       levels = c('LL', 'ML', 'MM', 'MH', 'HH'), order = T), 
                         Mag = factor(c('L', 'H'), levels = c('L', 'H'), order = T),
                         Group = factor(c('HC','ADHD'), levels = c('HC','ADHD')))
new_lmerm2$ROI = predict(lmerm2, newdata = new_lmerm2, re.form = NA)

ggplot(new_lmerm2, aes(x = Prob, y = ROI, color = Group)) + geom_point() +facet_grid(.~Mag)

### try to combine the two dataframe into one figure
ggplot(choice_stat3, aes(x=P, y=MMMH1, fill=Group, colour=Group)) +
  geom_errorbar(aes(ymin=MMMH1-SEMMH1, ymax=MMMH1+SEMMH1),width= 0.5, 
                position=position_dodge(1), size = 0.5) +
  geom_bar(position=position_dodge(1), stat="identity") +
  facet_wrap(~ Group+M) +
  theme(text = element_text(size = 10), legend.title=element_blank(), legend.position="none",
        axis.title = element_text(lineheight=.8, face="bold"), plot.title = element_text(lineheight=.8, face="bold")) +
  labs(x='Probability',y = 'Parameter Estimate (a.u.)') + 
  scale_y_continuous(limits=c(-5, 6)) +
  ggtitle('Neural response estimates across probability levels in Caudate_R') +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

ggplot() +
  geom_bar(data = choice_stat3, aes(x = Prob, y = MLLH1, fill = Group, color = Group), 
           position=position_dodge(1), stat="identity") +
  geom_point(data = new_lmerm2, aes(x = Prob, y = ROI, shape = Group)) +
  facet_wrap(~Group+Mag)


## replace the probability factor with the value predicted by lmr
lmer_data$pProb.L[lmer_data$Prob == 'HH'] = 0.6324555
lmer_data$pProb.L[lmer_data$Prob == 'MH'] = 0.3162278
lmer_data$pProb.L[lmer_data$Prob == 'MM'] = 0.0000000
lmer_data$pProb.L[lmer_data$Prob == 'ML'] = -0.3162278
lmer_data$pProb.L[lmer_data$Prob == 'LL'] = -0.6324555

lmer_data$pProb.Q[lmer_data$Prob == 'HH'] = 0.5345225
lmer_data$pProb.Q[lmer_data$Prob == 'MH'] = -0.2672612
lmer_data$pProb.Q[lmer_data$Prob == 'MM'] = -0.5345225
lmer_data$pProb.Q[lmer_data$Prob == 'ML'] = -0.2672612
lmer_data$pProb.Q[lmer_data$Prob == 'LL'] = 0.5345225

# Make probability as continuous variable for plotting the line
lmerm3 = lmer(ROI~pProb.L*Mag*Group + pProb.Q*Mag*Group + (1|Subj), data = lmer_data, 
              contrasts = list(Mag = contr.poly(2)))
# summary(lmerm3)
new_lmerm3 = expand.grid(pProb.L = seq(-0.6324555, 0.6324555, 0.001), 
                         Mag = factor(c('L', 'H'), levels = c('L', 'H'), order = T),
                         Group = factor(c('HC','ADHD'), levels = c('HC','ADHD')))
new_lmerm3$pProb.Q = scale(new_lmerm3$pProb.L^2)/2 # equation discovered by YS to make sure pProb.L^2=pProb.Q
new_lmerm3$ROI = predict(lmerm3, newdata = new_lmerm3, re.form = NA)

ggplot(new_lmerm3, aes(x = pProb.L, y = ROI, color = Mag)) + geom_line() +facet_grid(.~Group)

## try lmer3
ggplot() +
  geom_bar(data = choice_stat3, aes(x = Prob, y = MLLH1, fill = Group, color = Group), 
           position=position_dodge(1), stat="identity") +
  # geom_point(data = new_lmerm2, aes(x = Prob, y = ROI, shape = Group)) +
  geom_line(data = new_lmerm3, aes(x = pProb.L, y = ROI, color = Group)) +
  facet_wrap(~Group+Mag)


