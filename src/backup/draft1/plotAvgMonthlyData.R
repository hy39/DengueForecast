rm(list=ls(all=TRUE))
library(ggplot2)
## 2005-2017之資料整理
## 每年病例計算從當年Apr1-隔年Mar31
#Case.Kao <- read.csv("../資料/2005-2017年高雄各月份病例數.csv", header=T)
#Temp.Kao <- read.csv("../資料/高雄溫度.csv", header=T)
#Rain.Kao <- read.csv("../資料/高雄雨量.csv", header=T)
Case.Kao <- read.csv("../資料/1998-2017高雄各月份病例資料.csv", header=T)
Temp.Kao <- read.csv("../資料/1998-2017高雄各月份最低溫度.csv", header=T)
Rain.Kao <- read.csv("../資料/1998-2017高雄各月份最大降雨.csv", header=T)
#去掉2017，同時也不考慮9月~12月的溫度和雨量
Case.Kao <- Case.Kao[-nrow(Case.Kao),] 
Temp.Kao <- Temp.Kao[-nrow(Temp.Kao),]
Rain.Kao <- Rain.Kao[-nrow(Rain.Kao),] 

n <- nrow(Case.Kao)
#Calculate column mean
Weather <- data.frame(1:12, apply(Temp.Kao[,-1],2,mean),apply(Temp.Kao[,-1],2,sd),apply(Rain.Kao[,-1],2,mean),apply(Rain.Kao[,-1],2,sd))
colnames(Weather) <- c("month", "Temp_mean", "Temp_sd", "Rain_mean", "Rain_sd")
ylab <- expression("Min Temperature"(degree*C)) 
ggplot(Weather, aes(x = month)) + 
  geom_point(aes(y = Temp_mean, group=1, colour="red")) +
  geom_line(aes(y = Temp_mean, group=1, colour="red")) +
  geom_errorbar(aes(ymin=Temp_mean-Temp_sd/sqrt(n), ymax=Temp_mean+Temp_sd/sqrt(n)), group=1, width=.1) +
  labs(y=ylab) +
  geom_point(aes(y=Rain_mean/10, colour='blue')) +
  geom_line(aes(y=Rain_mean/10, colour='blue')) +
  geom_errorbar(aes(ymin=(Rain_mean-Rain_sd/sqrt(n))/10, ymax=(Rain_mean+Rain_sd/sqrt(n))/10), width=.1) +
  scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Max Rainfall(mm)"),limits = c(0, 30),minor_breaks = seq(0, 30, 5),breaks = seq(0, 30, 10)) +
  xlab("Month") +
  scale_x_continuous(breaks=1:12,labels=month.abb) +
  scale_color_manual(labels = c("Max Rainfall","Min Temperature"), values = c("red", "blue")) +
  theme(panel.grid.minor = element_blank(), legend.title=element_blank(), legend.position=c(0.85, 0.9))
  #theme(panel.grid.minor = element_blank(),legend.title=element_blank(), legend.position="center")
  ggsave("weathermonth.tiff", units="in", width=6.2, height=4, dpi=300, compression = 'lzw')

  
#------------------------------------------------------
#Plot average dungue cases per month
#  scale_x_discrete(breaks=month.abb)
Case.Kao <- Case.Kao[-c(nrow(Case.Kao)-1,nrow(Case.Kao)-2),]
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
  theme(panel.grid.minor = element_blank(),legend.position="none") #remove legend 
  ggsave("denguemonth.tiff", units="in", width=6.2, height=4, dpi=300, compression = 'lzw')
