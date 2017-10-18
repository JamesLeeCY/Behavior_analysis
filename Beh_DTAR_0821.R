rm(list = ls())

library(ggplot2)
library(dplyr)
library(scales)
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

DT <- read.csv('DTcell.csv')
AR <- read.csv('ADHDDMAR.csv', header = F)


DT <- DT[, -c(1,4)]
# adjusted negative sign of DT for square root
for (x in c(1:77)){
if(DT$DT[x]>0){
  DT[x,3] <- DT[x,1]^0.5
} else if (DT$DT[x]<0) {
  DT[x,3] <- -((-DT[x,1])^0.5)
}
}

colnames(DT) <- c('DT','Group','DTsq')
DTAC <- subset(DT, Group == 'ADHD' | Group == 'CNTL')
DTAC$Group <- factor(DTAC$Group, levels = c('ADHD','CNTL'))
write.csv(DT,'DT_sqrt_1015.csv')

DTn <- ggplot(DTAC, aes(Group, DT, colour=Group)) + 
   geom_violin(aes(fill = Group, color = Group), trim = F) + 
   geom_dotplot(binaxis='y', stackdir='center', dotsize=0.7, fill = 'white') +
   labs(x = '', y = '') +
   expand_limits(y=c(-75,25)) +
   scale_x_discrete(labels = c('ADHD','HC')) +
   scale_fill_discrete(name = "Group") +
   theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
# change the colour of violin plot
# DTN <- DTn + scale_fill_manual(values=c("#E69F00", "#3366CC")) + scale_colour_manual(values=c("#E69F00", "#3366CC"))
 ggsave('Probability density distribution of ADHD and HC decision threshold (DT).pdf', plot = DTn, 
        width = 100, height = 130, units = 'mm')
 
 
 DTsq <- ggplot(DTAC, aes(Group, DTsq, colour=Group)) + 
   geom_violin(aes(fill = factor(Group)),  trim = F) + 
   geom_dotplot(binaxis='y', stackdir='center', dotsize=0.9, fill = 'white') +
   labs(x = '', y = '') +
   expand_limits(y=c(-10,5)) +
   scale_x_discrete(labels = c('ADHD','HC')) +
   scale_fill_discrete(name = "Group") +
   theme(text = element_text(size = 15), panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black"), legend.position="none")
 
 ggsave('Probability density distribution of ADHD and HC adjusted decision threshold (adjusted DT).png', plot = DTsq, 
        width = 100, height = 130, units = 'mm')
 
 
# AR and RT
 
MSE <- read.csv('MSE_of_ARRT.csv')
MSE$Pb <- factor(MSE$Pb, levels = c("LL", "ML", "MM", "MH", "HH"))

# Acceptance rate
 p1 <- ggplot(data = subset(MSE, Mg == "H"), aes(x = Pb, y = MAR, group = Gp, colour = Gp))+
   geom_errorbar(aes(ymin=MAR-SAR, ymax=MAR+SAR), width=0, size =1) +
   geom_line(size = 1) +
   # scale_color_manual(values=c('red','blue')) +
   # geom_point(size = 2)+
   ylim(0, 1)+
   labs(x = 'Probability', y = 'p(Accept)') +
   ggtitle("High magnitude") +    
   theme(plot.margin=unit(c(1,1,1.5,0.1),"cm"), legend.position="none") +
   theme(plot.title = element_text(hjust = 0.5)) +
   annotate("text", x = 'MM', y = 0.75, label = "*", size = 5) +
   annotate("text", x = 'ML', y = 0.20, label = "*", size = 5) +
   annotate("text", x = 'LL', y = 0.15, label = "*", size = 5) +
   theme(text = element_text(size = 10)) +
   geom_hline(yintercept = 0.5, linetype="dotted") +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black")
   )
 p2 <- ggplot(data = subset(MSE, Mg == "L"), aes(x = Pb, y = MAR, group = Gp, colour = Gp))+
   geom_errorbar(aes(ymin=MAR-SAR, ymax=MAR+SAR), width=0, size =1) +
   geom_line(size = 1) +
   ylim(0, 1)+
   xlab("Probability")+
   ggtitle("Low magnitude") +  
   theme(plot.margin=unit(c(1,1,1.5,1.5),"cm"))+
   theme(axis.title.y=element_blank())+
   theme(legend.position="none")+
   theme(plot.title = element_text(hjust = 0.5)) + 
   theme(text = element_text(size = 10)) +
   geom_hline(yintercept = 0.5, linetype="dotted") +
   theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
         panel.background = element_blank(), axis.line = element_line(colour = "black")) 
pdf('AR_0825.pdf',10,5)
AR <- multiplot(p1, p2, cols = 2)
dev.off()


# Reaction time
p1 <- ggplot(data = subset(MSE, Mg == "H"), aes(x = Pb, y = MRT, group = Gp, colour = Gp))+
  geom_errorbar(aes(ymin=MRT-SRT, ymax=MRT+SRT), width=0, size =1) +
  geom_line(size = 1) +
  ylim(1000, 1900)+
  labs(x = 'Probability', y = 'Reaction time (ms)') +
  ggtitle("High magnitude") +    
  theme(plot.margin=unit(c(1,1,1.5,0.1),"cm"), legend.position="none") +
  theme(plot.title = element_text(hjust = 0.5)) +
  annotate("text", x = 'ML', y = 1750, label = "*", size = 5) +
  annotate("text", x = 'LL', y = 1640, label = "*", size = 5) +
  theme(text = element_text(size = 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")
  )
p2 <- ggplot(data = subset(MSE, Mg == "L"), aes(x = Pb, y = MRT, group = Gp, colour = Gp))+
  geom_errorbar(aes(ymin=MRT-SRT, ymax=MRT+SRT), width=0, size =1) +
  geom_line(size = 1) +
  ylim(1000, 1900)+
  xlab("Probability")+
  ggtitle("Low magnitude") +  
  theme(plot.margin=unit(c(1,1,1.5,1.5),"cm"))+
  theme(axis.title.y=element_blank())+
  theme(legend.position="none")+
  theme(plot.title = element_text(hjust = 0.5)) + 
  annotate("text", x = 'HH', y = 1440, label = "*", size = 5) +
  annotate("text", x = 'ML', y = 1750, label = "*", size = 5) +
  theme(text = element_text(size = 10)) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) 
pdf('RT_0825.pdf',10,5)
RT <- multiplot(p1, p2, cols = 2)
dev.off()


 