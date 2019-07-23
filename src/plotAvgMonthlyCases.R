rm(list=ls(all=TRUE))
library(ggplot2)
## plotAvgMonthlyCases
## period: 2005-2017
Case.Kao <- read.csv("../���/2005-2017�~�����U����f�Ҽ�.csv", header=T)
Temp.Kao <- read.csv("../���/�����ū�.csv", header=T)
Rain.Kao <- read.csv("../���/�����B�q.csv", header=T)
#Case.Kao <- read.csv("../���/1998-2017�����U����f�Ҹ��.csv", header=T)
#Temp.Kao <- read.csv("../���/1998-2017�����U����̧C�ū�.csv", header=T)
#Rain.Kao <- read.csv("../���/1998-2017�����U����̤j���B.csv", header=T)
#�h��2017�A�P�ɤ]���Ҽ{9��~12�몺�ūשM�B�q
#Temp.Kao <- Temp.Kao[-nrow(Temp.Kao),]
#Rain.Kao <- Rain.Kao[-nrow(Rain.Kao),] 

n <- nrow(Case.Kao)

#------------------------------------------------------
#Plot average dungue cases per month
#  scale_x_discrete(breaks=month.abb)
#Case.Kao_low <- Case.Kao[-c(nrow(Case.Kao)-1,nrow(Case.Kao)-2),]
#Case <- data.frame(1:12, apply(Case.Kao_low[,-1],2,mean),apply(Case.Kao_low[,-1],2,sd))

Case <- data.frame(1:12, apply(Case.Kao[,-1],2,mean),apply(Case.Kao[,-1],2,sd))

colnames(Case) <- c("month", "Case_mean","Case_sd")
ylab <- expression("Average mumber of Dengue cases per month") 
ggplot(Case, aes(x = month)) + 
  geom_point(aes(y = Case_mean, group=1, colour="red")) +
  geom_line(aes(y = Case_mean, group=1, colour="red")) +
  geom_errorbar(aes(ymin=Case_mean-Case_sd/sqrt(n), ymax=Case_mean+Case_sd/sqrt(n)), group=1, width=.1) +
  labs(y=ylab) +
  xlab("Month") +
  scale_x_continuous(breaks=1:12,labels=month.abb) +
  theme(panel.grid.minor = element_blank(),legend.position="none") + #remove legend
  scale_y_log10(breaks = c( 1, 10, 100, 1000, 10000, 20000))
#  ggsave("denguecases_avgmonthly.tiff", units="in", width=6.2, height=4, dpi=300, compression = 'lzw')