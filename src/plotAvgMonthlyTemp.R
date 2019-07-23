rm(list=ls(all=TRUE))
library(ggplot2)
## plotAvgMonthlyTemp
## 2005-2017����ƾ�z
## �e�X�̧C�ūשM�����ūת��ܲ�
## Figure1 in Draft2
## Source codes need to be cleaned


Case.Kao <- read.csv("../���/2005-2017�~�����U����f�Ҽ�.csv", header=T)
Temp.Kao <- read.csv("../���/�����ū�.csv", header=T)
Temp.Avg.Kao <- round(read.csv("../���/2005-2017�~�����U��������ū�.csv", header=T),1)
Rain.Kao <- read.csv("../���/�����B�q.csv", header=T)

#�h��2017�A�P�ɤ]���Ҽ{9��~12�몺�ūשM�B�q
Temp.Kao <- Temp.Kao[-nrow(Temp.Kao),]
Rain.Kao <- Rain.Kao[-nrow(Rain.Kao),] 

n <- nrow(Case.Kao)
#Calculate column mean
Temp.Kao[,paste(c('T','3'),collapse = "")]
#^^^^^^^^^^^^^^

weather_df<-data.frame()
for (row in 1:nrow(Temp.Kao)) {
for (month in 1:12) {
  print(paste("The month is", month))
  temp <- Temp.Kao[row,paste(c('T',month),collapse = "")]
  print(paste("The minimum temperature is", Temp.Kao[row,paste(c('T',month),collapse = "")]))
  print(paste("The average temperature is", Temp.Avg.Kao[row,paste(c('T',month),collapse = "")]))
  
  #month_char <-  sprintf("%02d", as.numeric(month))
  newrow = c(Temp.Kao[row,'YEAR'],month,temp,0)
  weather_df <- rbind(weather_df, newrow)
  
  temp <- Temp.Avg.Kao[row,paste(c('T',month),collapse = "")]
  newrow = c(Temp.Kao[row,'YEAR'],month,temp,1)
  weather_df <- rbind(weather_df, newrow)
}
}



names(weather_df)<-c("Year","Month","Temp","Category") #Category=0 for min temperature, =1 for avg temperature


weather_df$Month_txt <- month.abb[weather_df$Month]
#weather_df$Month_n[weather_df$Category==0] <- weather_df$Month[weather_df$Category==0]-0.2
#weather_df$Month_n[weather_df$Category==1] <- weather_df$Month[weather_df$Category==1]+0.2
ggplot(data=weather_df, aes(x=Month_txt,  y=Temp, fill = factor(Category,labels=c("min","avg")))) + 
  
  geom_boxplot(aes(Month_txt,Temp),outlier.size=0) +
  geom_point(position=position_dodge(width=0.75),size=1.5) +
  xlab("Month") +
  ylab(expression("Temperature " ( degree*C))) +
  #scale_x_continuous(breaks=1:12,labels=month.abb) +
  scale_x_discrete(limits=month.abb) +
  #scale_color_manual(labels = c("Max Rainfall","Min Temperature"), values = c("red", "blue")) +
  theme(panel.grid.minor = element_blank(), legend.title=element_blank())

  ggsave("temperature_avgmonthly", units="in", width=6.2, height=4, dpi=300, compression = 'lzw')

  