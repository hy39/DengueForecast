rm(list=ls(all=TRUE))
library(ggplot2)
## plotAvgMonthlyTemp
## 2005-2017之資料整理
## 畫出每月最大和平均雨量的變異
## Figure1 in Draft2
## Source codes need to be cleaned


Case.Kao <- read.csv("../資料/2005-2017年高雄各月份病例數.csv", header=T)
#Temp.Kao <- read.csv("../資料/高雄溫度.csv", header=T)
#Temp.Avg.Kao <- round(read.csv("../資料/2005-2017年高雄各月份平均溫度.csv", header=T),1)
Rain.Kao <- read.csv("../資料/高雄雨量.csv", header=T)
#r <- read.csv("../資料/2005-2017年高雄各月份平均雨量.csv", header=T) # averaged hourly data
r <- read.csv("../資料/2002-2017年高雄各月份平均雨量(日總合).csv", header=T) # averaged hourly data

#w <- read.csv("../資料/monthdays.csv", header=F)
Rain.Avg.Kao <- r*1 # Daily accimulated rainfall
#m <- matrix(c(1, 0, 1, 5, -3, 1, 2, 4, 7), nrow = 3))
### 為何最大降雨量比每月累積雨量還大???
### 如何計算每日最大相雨量? 平均各地?

#去掉2017，同時也不考慮9月~12月的溫度和雨量
Rain.Kao <- Rain.Kao[-nrow(Rain.Kao),] 

n <- nrow(Case.Kao)
#Calculate column mean


weather_df<-data.frame()
for (row in 1:nrow(Rain.Kao)) {
for (month in 1:12) {
  print(paste("The month is", month))
  rain <- Rain.Kao[row,paste(c('R',month),collapse = "")]
  print(paste("The minimum rainfall is", Rain.Kao[row,paste(c('R',month),collapse = "")]))
  print(paste("The average rainfall is", Rain.Avg.Kao[row,paste(c('R',month),collapse = "")]))
  
  #month_char <-  sprintf("%02d", as.numeric(month))
  newrow = c(Rain.Kao[row,'YEAR'],month,rain,0)
  weather_df <- rbind(weather_df, newrow)
  
  rain <- Rain.Avg.Kao[row,paste(c('R',month),collapse = "")]
  newrow = c(Rain.Kao[row,'YEAR'],month,rain,1)
  weather_df <- rbind(weather_df, newrow)
}
}



names(weather_df)<-c("Year","Month","Rain","Category") #Category=0 for min temperature, =1 for avg temperature


weather_df$Month_txt <- month.abb[weather_df$Month]
#weather_df$Month_n[weather_df$Category==0] <- weather_df$Month[weather_df$Category==0]-0.2
#weather_df$Month_n[weather_df$Category==1] <- weather_df$Month[weather_df$Category==1]+0.2
ggplot(data=weather_df, aes(x=Month_txt,  y=Rain, fill = factor(Category,labels=c("max","acc")))) + 
  
  geom_boxplot(aes(Month_txt,Rain),outlier.size=0) +
  geom_point(position=position_dodge(width=0.75),size=1.5) +
  xlab("Month") +
  ylab(expression("Rainfall (mm)")) +
  #scale_x_continuous(breaks=1:12,labels=month.abb) +
  scale_x_discrete(limits=month.abb) +
  #scale_color_manual(labels = c("Max Rainfall","Min Temperature"), values = c("red", "blue")) +
  theme(panel.grid.minor = element_blank(), legend.title=element_blank())
  #ggsave("rainfall_avgmonthly", units="in", width=6.2, height=4, dpi=300, compression = 'lzw')

  

Ra <- Rain.Kao[,2:13]
SDa <- sum(apply(Ra, 2, sd))
xa <- apply(Ra, 2, sd)/SDa
print(paste("Total SD of max rain", SDa))

Rb <- Rain.Avg.Kao[1:12,2:13]
SDb <- sum(apply(Rb, 2, sd))
xb <- apply(Rb, 2, sd)/SDb


