rm(list = ls())

library(ggplot2)
setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data")
ADHDDMTT <- read.csv('ADHDDMTT.csv', header = F, sep = ',', 
                     col.names = c("T", "C", "PB", "MG", "OC", "RP", "RT", "S", "EV", "Var", "FT", "FC", "Ses", "Subj"))

#logistic regression for all subject
DTcell <- data.frame()
for(i in 1:77){
  Allmodel <- glm(RP~EV, family = binomial(link = 'logit'), data = subset(ADHDDMTT, Subj==i))
  DT <- -(Allmodel$coefficients[1]/Allmodel$coefficients[2])
  DTcell[i,1] <- DT
}
colnames(DTcell) <- "DT"

#try for plot
ADHDMX <- c(3,5:10,13:16,18:20,26,28,32,38,40,43,49,57,58,60,61,67,68,69,70,75,76,77)
CNTLMX <- c(1,2,4,12,17,22:25,29,30,33,36,37,39,42,44:48,50:56,59,63,65,73)
SIBLMX <- c(11,21,27,31,34,35,41,62,64,66,71,72,74)
DTcell[ADHDMX, 2] <- "ADHD"
DTcell[CNTLMX, 2] <- "CNTL"
DTcell[SIBLMX, 2] <- "SIB"
DTcell[ADHDMX, 3] <- 1
DTcell[CNTLMX, 3] <- 2
DTcell[SIBLMX, 3] <- 3

#logistic regression for ADHD
ADHD <- subset(ADHDDMTT, Subj==3 | Subj==5 | Subj==6 | Subj==7 | Subj==8 | Subj==9 | Subj==10 | Subj==13
               | Subj==14 | Subj==15 | Subj==16 | Subj==18 | Subj==19 | Subj==20 | Subj==28 | Subj==32
               | Subj==38 | Subj==40 | Subj==43 | Subj==49 | Subj==57 | Subj==58 | Subj==60
               | Subj==61 | Subj==67 | Subj==68 | Subj==69 | Subj==70 | Subj==75 | Subj==76
               | Subj==77, select = c(RP, EV, Subj))
ADHDmodel <- glm(RP~EV, family = binomial(link = 'logit'), data = ADHD)
ADHDDT <- -(ADHDmodel$coefficients[1]/ADHDmodel$coefficients[2])
DTcell[2,4] <- ADHDDT

#logistic regression for control
Control <- subset(ADHDDMTT, Subj==1 | Subj==2 | Subj==4 | Subj==12 | Subj==17 | Subj==22 | Subj==23
                  | Subj==24 | Subj==25 | Subj==26  | Subj==29  | Subj==30 | Subj==33 | Subj==36 | Subj==37
                  | Subj==39 | Subj==42 | Subj==44 | Subj==45 | Subj==46 | Subj==47 | Subj==48 | Subj==50 
                  | Subj==51 | Subj==52 | Subj==53 | Subj==54 | Subj==55 | Subj==56 | Subj==59
                  | Subj==63 | Subj==65 | Subj==73, select = c(RP, EV, Subj))
Conmodel <- glm(RP~EV, family = binomial(link = 'logit'), data = Control)
ConDT <- -(Conmodel$coefficients[1]/Conmodel$coefficients[2])
DTcell[1,4] <- ConDT
colnames(DTcell) <- c('DT','Group','GN','GD')

#Have not excluded outlier
ADHD <- DTcell[ADHDMX,1]
CNTL <- DTcell[CNTLMX,1]
ALL <- rbind(ADHD,CNTL)
t.test(ADHD,CNTL)
t.test(ALL)
mean(DTcell$DT);mean(ADHD);mean(CNTL)
sd(DTcell$DT);sd(ADHD);sd(CNTL)

#Excluded outlier (DT<-40)
ADHD <- subset(DTcell[DTcell$DT > -40,], GN == 1 , select = DT)
CNTL <- subset(DTcell[DTcell$DT > -40,], GN == 2 , select = DT)
ALL <- subset(DTcell[DTcell$DT > -40,], GN == 1 | GN ==2 , select = DT)
t.test(ADHD,CNTL)
mean(ALL[,1]);mean(ADHD[,1]);mean(CNTL[,1])
sd(ALL[,1]);sd(ADHD[,1]);sd(CNTL[,1])

# write.csv(DTcell, file = "DTcell.csv")

#plot the histogram for DT distribution between the group

ggplot(data = subset(DTcell, GN == 1 | GN == 2), aes(x = DT, fill = Group)) +
  geom_histogram(binwidth=5, position = "dodge") +
  labs(title="Histogram of Decision threshold") +
  labs(x="DT", y="Count")
ggsave('Histogram of Decision threshold.png')

# ggplot(data = DTcell, aes(DT,GN)) +
#   geom_point(aes(colour = factor(GN)))


#plot the predicted line
NEWDATA <- data.frame(EV = seq(min(Control$EV, na.rm = TRUE), max(Control$EV, na.rm = TRUE), by = 0.01))
predicted <- predict(Conmodel, NEWDATA, type = "response", se.fit = TRUE)
with(Control, plot(EV, RP))
lines(NEWDATA$EV, predicted$fit, col = "red")

library(ggplot2)
g=ggplot(data=Control,aes(x=Control$EV , y=Control$RP))+
  stat_smooth(aes(y=Control$RP),method = "glm",method.args = list(family="binomial"));g


###try
ADHD3 <- subset(ADHDDMTT, Subj==3 , select = c(RP, EV, Subj))
ADHDmodel3 <- glm(RP~EV, family = binomial(link = 'logit'), data = ADHD3)
summary(ADHDmodel3)
ADHDDT3 <- -(ADHDmodel3$coefficients[1]/ADHDmodel3$coefficients[2])

library(ggplot2)
g=ggplot(data=ADHD3,aes(x=ADHD3$EV , y=ADHD3$RP))+
  stat_smooth(aes(y=ADHD3$RP),method = "glm",method.args = list(family="binomial"));g


# EV53 <- data.matrix(subset(ADHDDMTT, Subj == 53, select = c(EV)))
# RP53 <- data.matrix(subset(ADHDDMTT, Subj == 53, select = c(RP)))
# plot(EV53, RP53)
