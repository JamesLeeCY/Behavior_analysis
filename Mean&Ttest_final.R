rm(list=ls())
library(ggplot2)
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

setwd("/Users/chunyilee/Documents/GIBMS/research/ADHDDM_Taskfile_-MRI/Behavior data")
ADHDDMTT <- read.csv("ADHDDMTT.csv", header = F, sep = ",", col.names = c("T", "C", "PB", "MG", "OC", "RP", "RT", "S", "EV", "Var", "FT", "FC", "Ses", "Subj"))
ADHDDMAR <- read.csv("ADHDDMAR.csv", header = F, sep = ",", col.names = c("Subj", "HHH", "MHH", "MMH", "MLH", "LLH", "HHL", "MHL", "MML", "MLL", "LLL", "Condition"))
ADHDDMRT <- read.csv("ADHDDMRT.csv", header = F, sep = ",", col.names = c("Subj", "HHH", "MHH", "MMH", "MLH", "LLH", "HHL", "MHL", "MML", "MLL", "LLL", "Condition"))

Subj <- 77
ADHDMX <- c(3,5:10,13:16,18:20,26,28,32,38,40,43,49,57,58,60,61,67,68,69,70,75,76,77)
CNTLMX <- c(1,2,4,12,17,22:25,29,30,33,36,37,39,42,44:48,50:56,59,63,65,73)

for (k in Subj){
ADHDDMAR[k+1, 1] <- "MeanADHD"
ADHDDMRT[k+1, 1] <- "MeanADHD"
ADHDDMAR[k+2, 1] <- "MeanCon"
ADHDDMRT[k+2, 1] <- "MeanCon"
ADHDDMAR[k+3, 1] <- "SDADHD"
ADHDDMRT[k+3, 1] <- "SDADHD"
ADHDDMAR[k+4, 1] <- "SDCon"
ADHDDMRT[k+4, 1] <- "SDCon"
ADHDDMAR[k+5, 1] <- "SEMADHD"
ADHDDMRT[k+5, 1] <- "SEMADHD"
ADHDDMAR[k+6, 1] <- "SEMCon"
ADHDDMRT[k+6, 1] <- "SEMCon"
ADHDDMAR[k+7, 1] <- "T_pvalue"
ADHDDMRT[k+7, 1] <- "T_pvalue"
ADHDDMAR[k+8, 1] <- "T_statistic"
ADHDDMRT[k+8, 1] <- "T_statistic"

for (i in 2:11){
#mean
MARA <- mean(as.numeric(ADHDDMAR[ADHDMX, i]))
ADHDDMAR[k+1, i] <- MARA
MRTA <- mean(as.numeric(ADHDDMRT[ADHDMX, i]))
ADHDDMRT[k+1, i] <- MRTA
MARC <- mean(as.numeric(ADHDDMAR[CNTLMX, i]))
ADHDDMAR[k+2, i] <- MARC
MRTC <- mean(as.numeric(ADHDDMRT[CNTLMX, i]))
ADHDDMRT[k+2, i] <- MRTC
#standard deviation 
SDAA <- sd(as.numeric(ADHDDMAR[ADHDMX, i]))
ADHDDMAR[k+3, i] <- SDAA
SDRA <- sd(as.numeric(ADHDDMRT[ADHDMX, i]))
ADHDDMRT[k+3, i] <- SDRA
SDAC <- sd(as.numeric(ADHDDMAR[CNTLMX, i]))
ADHDDMAR[k+4, i] <- SDAC
SDRC <- sd(as.numeric(ADHDDMRT[CNTLMX, i]))
ADHDDMRT[k+4, i] <- SDRC
#standard error
SEMAA <- sd(as.numeric(ADHDDMAR[ADHDMX, i]))/sqrt(length(ADHDDMAR[ADHDMX, i]))
ADHDDMAR[k+5, i] <- SEMAA
SEMRA <- sd(as.numeric(ADHDDMRT[ADHDMX, i]))/sqrt(length(ADHDDMRT[ADHDMX, i]))
ADHDDMRT[k+5, i] <- SEMRA
SEMAC <- sd(as.numeric(ADHDDMAR[CNTLMX, i]))/sqrt(length(ADHDDMAR[CNTLMX, i]))
ADHDDMAR[k+6, i] <- SEMAC
SEMRC <- sd(as.numeric(ADHDDMRT[CNTLMX, i]))/sqrt(length(ADHDDMRT[CNTLMX, i]))
ADHDDMRT[k+6, i] <- SEMRC
#T.test
ART <-t.test(ADHDDMAR[ADHDMX, i], ADHDDMAR[CNTLMX, i], alternative = 'greater')
ADHDDMAR[k+7, i] <- ART$p.value
ADHDDMAR[k+8, i] <- ART$statistic
RTT <-t.test(ADHDDMRT[ADHDMX, i], ADHDDMRT[CNTLMX, i], alternative = 'greater')
ADHDDMRT[k+7, i] <- RTT$p.value
ADHDDMRT[k+8, i] <- RTT$statistic
}

AART <- ADHDDMAR[k+7, 2:11]
RRTT <- ADHDDMRT[k+7, 2:11]
TT <- rbind(AART, RRTT)

#Group differece
ARMD <- NULL
RTMD <- NULL
for (i in 2:11){
ARMD[i-1] <- as.numeric(ADHDDMAR[k+1, i]) - as.numeric(ADHDDMAR[k+2, i])
RTMD[i-1] <- as.numeric(ADHDDMRT[k+1, i]) - as.numeric(ADHDDMRT[k+2, i])
}

#set dataframe for plot AR and RT Means and SE vs condition
Pb <- c("HH", "MH", "MM", "ML", "LL", "HH", "MH", "MM", "ML", "LL", "HH", "MH", "MM", "ML", "LL", "HH", "MH", "MM", "ML", "LL")
Mg <- c("H", "H", "H", "H", "H", "L", "L", "L", "L", "L", "H", "H", "H", "H", "H", "L", "L", "L", "L", "L")
Gp <- c("ADHD", "ADHD", "ADHD", "ADHD", "ADHD", "ADHD", "ADHD", "ADHD", "ADHD", "ADHD", "Control","Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control", "Control")
MADHDAR <- as.numeric(ADHDDMAR[k+1, 2:11])
MConAR <- as.numeric(ADHDDMAR[k+2, 2:11])
MAR <- c(MADHDAR, MConAR)
MADHDRT <- as.numeric(ADHDDMRT[k+1, 2:11])
MConRT <- as.numeric(ADHDDMRT[k+2, 2:11])
MRT <- c(MADHDRT, MConRT)
SADHDAR <- as.numeric(ADHDDMAR[k+5, 2:11])
SConAR <- as.numeric(ADHDDMAR[k+6, 2:11])
SAR <- c(SADHDAR, SConAR)
SADHDRT <- as.numeric(ADHDDMRT[k+5, 2:11])
SConRT <- as.numeric(ADHDDMRT[k+6, 2:11])
SRT <- c(SADHDRT, SConRT)
dfMean <- data.frame(Gp, Pb, Mg, MAR, MRT, SAR, SRT)
}

write.csv(dfMean, 'MSE_of_ARRT.csv', row.names = F)

#Acceptance rate
dfMean2 <- dfMean
dfMean2$Pb <- factor(dfMean2$Pb, levels = c("LL", "ML", "MM", "MH", "HH"))
# label.df <- data.frame(Pb = c("MM", "ML"),
#                        MAR = c(0.7, 0.2))
# #pd <- position_dodge(0.1) # move them .05 to the left and right
p1 <- ggplot(data = subset(dfMean2, Mg == "H"), aes(x = Pb, y = MAR, group = Gp, colour = Gp))+
  geom_errorbar(aes(ymin=MAR-SAR, ymax=MAR+SAR), width=0, size =2) +
  geom_line(size = 2) +
  scale_color_manual(values=c('red','blue')) +
  geom_point(size = 7)+
  ylim(0, 1)+
  xlab("Probability")+
  ylab("p(Accept)")+
  ggtitle("High magnitude") +    
  theme(plot.margin=unit(c(1,1,1.5,0.1),"cm")) +
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 'MM', y = 0.75, label = "*", size = 10) +
  #annotate("text", x = 'MM', y = 0.75, label = "_") +
  annotate("text", x = 'ML', y = 0.20, label = "*", size = 10) +
  #annotate("text", x = 'ML', y = 0.20, label = "_") +
  annotate("text", x = 'LL', y = 0.15, label = "*", size = 10) +
  #annotate("text", x = 'LL', y = 0.15, label = "_") +
  theme(text = element_text(size = 20)) +
  geom_hline(yintercept = 0.5, linetype="dotted") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
        )
p2 <- ggplot(data = subset(dfMean2, Mg == "L"), aes(x = Pb, y = MAR, group = Gp, colour = Gp))+
  geom_errorbar(aes(ymin=MAR-SAR, ymax=MAR+SAR), width=0, size = 2) +
  geom_line(size = 2) +
  scale_color_manual(values=c('red','blue')) +
  geom_point(size = 7)+
  ylim(0, 1)+
  xlab("Probability")+
  ggtitle("Low magnitude") +  
  theme(plot.margin=unit(c(1,1,1.5,1.5),"cm"))+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  theme(text = element_text(size = 20)) +
  geom_hline(yintercept = 0.5, linetype="dotted") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
 

# png('AR_0823.png', width = 1500, height = 750, units = "px")
AR <- multiplot(p1, p2, cols = 2)
# dev.off()

#Reaction time
dfMean2 <- dfMean
dfMean2$Pb <- factor(dfMean2$Pb, levels = c("LL", "ML", "MM", "MH", "HH"))
p1 <- ggplot(data = subset(dfMean2, Mg == "H"), aes(x = Pb, y = MRT, group = Gp, colour = Gp))+
  geom_errorbar(aes(ymin=MRT-SRT, ymax=MRT+SRT), width=0, size = 2) +
  geom_line(size = 2) +
  scale_color_manual(values=c('red','blue')) +
  geom_point(size = 7)+
  ylim(1000, 1900)+
  xlab("Probability")+
  ylab("Reaction time")+
  ggtitle("High magnitude") +  
  theme(plot.margin=unit(c(1,1,1.5,0.1),"cm"))+
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 'ML', y = 1750, label = "*", size = 10) +
  #annotate("text", x = 'ML', y = 1750, label = "_") +
  annotate("text", x = 'LL', y = 1640, label = "*", size = 10) +
  #annotate("text", x = 'LL', y = 1640, label = "_") +
  theme(text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

p2 <- ggplot(data = subset(dfMean2, Mg == "L"), aes(x = Pb, y = MRT,  group = Gp, colour = Gp))+
  geom_errorbar(aes(ymin=MRT-SRT, ymax=MRT+SRT), width=0, size = 2) +
  geom_line(size = 2) +
  scale_color_manual(values=c('red','blue')) +
  geom_point(size = 7)+
  ylim(1000, 1900)+
  xlab("Probability")+
  ggtitle("Low magnitude") +    
  theme(plot.margin=unit(c(1,1,1.5,1.5),"cm"))+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 'HH', y = 1440, label = "*", size = 10) +
  #annotate("text", x = 'HH', y = 1440, label = "_") +
  annotate("text", x = 'ML', y = 1750, label = "*", size = 10) +
  #annotate("text", x = 'ML', y = 1750, label = "_") +
  theme(text = element_text(size = 20)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 

RT <- multiplot(p1, p2, cols = 2)

