rm(list = ls())
library(dplyr)
library(ggplot2)
library(car)
library(lme4)
library(lmerTest) # make summary of lmer test correct

setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/ROI/ROI_0822/")

# preparing choice data with 10 EV condition
choice <- data.frame()
for (i in 1:10){
  #combine choice ROI for 10 EV
  FP_choice <- paste('ROI_YS_0822_', as.character(i), '.csv', sep = "")
  x <- read.csv(FP_choice, header = F, sep = ",", 
                col.names = c('R_VS','R_MTL','VMPFC','L_DLPFC','R_DLPFC','DMPFC'))
  choice <- rbind(choice,x)
}

Condition <- c('HH_H','HH_L','MH_H','MH_L','MM_H','MM_L','ML_H','ML_L','LL_H','LL_L')
condition <- rep(Condition, each = 57)
P <- rep(c('HH','MH','MM','ML','LL'), each = 114)
M <- rep(c('H','L'), each = 57, times = 5)
ADHD <- rep('ADHD',29)
HC <- rep('HC',28)
Group <- rep(c(ADHD,HC),10)
choice <- cbind(choice,condition,Group,P,M)

# preparing the datafram for each ROI (take R_VS as example)
lmer_data = data.frame(ROI = choice$R_VS, 
                       Prob = factor(choice$P, levels = c('LL', 'ML', 'MM', 'MH', 'HH'), order = T), 
                       Mag = factor(choice$M, levels = c('L', 'H'), order = T), 
                       Group = factor(choice$Group, levels = c('HC','ADHD')),
                       Subj = factor(rep(seq(1,57,1), times = 10)))

# first try for establish linear mixed model for predicting ROI activation
lmerm1 = lmer(ROI~Prob*Mag*Group + (1|Subj), data = lmer_data)
summary(lmerm1)

new_lmerm1 = expand.grid(Prob = factor(c('LL', 'ML', 'MM', 'MH', 'HH'), 
                                     levels = c('LL', 'ML', 'MM', 'MH', 'HH'), order = T), 
                       Mag = factor(c('L', 'H'), levels = c('L', 'H'), order = T),
                       Group = factor(c('HC','ADHD'), levels = c('HC','ADHD')))
new_lmerm1$ROI = predict(lmerm1, newdata = new_lmerm1, re.form = NA)

ggplot(new_lmerm1, aes(x = Prob, y = ROI, color = Group)) + geom_point() +facet_grid(.~Mag)


# predict the lmr with only linear and quadratic effect of Probability
lmerm2 = lmer(ROI~Prob*Mag*Group + (1|Subj), data = lmer_data, 
              contrasts = list(Prob = contr.poly(5)[,1:2], Mag = contr.poly(2)))
summary(lmerm2)

new_lmerm2 = expand.grid(Prob = factor(c('LL', 'ML', 'MM', 'MH', 'HH'), 
                                       levels = c('LL', 'ML', 'MM', 'MH', 'HH'), order = T), 
                         Mag = factor(c('L', 'H'), levels = c('L', 'H'), order = T),
                         Group = factor(c('HC','ADHD'), levels = c('HC','ADHD')))
new_lmerm2$ROI = predict(lmerm2, newdata = new_lmerm2, re.form = NA)

ggplot(new_lmerm2, aes(x = Prob, y = ROI, color = Group)) + geom_point() +facet_grid(.~Mag)


## replace the probability factor with the value predicted by lmr 
## Using contr.poly(5) to find out the value of pProb.L and pProb.Q
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
summary(lmerm3)

new_lmerm3 = expand.grid(pProb.L = seq(-0.6324555, 0.6324555, 0.001), 
                         Mag = factor(c('L', 'H'), levels = c('L', 'H'), order = T),
                         Group = factor(c('HC','ADHD'), levels = c('HC','ADHD')))
new_lmerm3$pProb.Q = scale(new_lmerm3$pProb.L^2)/2 # equation discovered by YS to make sure pProb.L^2=pProb.Q
new_lmerm3$ROI = predict(lmerm3, newdata = new_lmerm3, re.form = NA)

ggplot(new_lmerm3, aes(x = pProb.L, y = ROI, color = Mag)) + geom_line() +facet_grid(.~Group)


