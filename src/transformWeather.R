rm(list=ls(all=TRUE))
library(ggplot2)
## 2005-2017之資料整理
## 每年病例計算從當年Apr1-隔年Mar31
Case.Kao <- read.csv("../資料/2005-2017年高雄各月份病例數.csv", header=T)
Temp.Kao <- read.csv("../資料/高雄溫度.csv", header=T)
Temp.Avg.Kao <- round(read.csv("../資料/2005-2017年高雄各月份平均溫度.csv", header=T),1)
Rain.Kao <- read.csv("../資料/高雄雨量.csv", header=T)

#去掉2017，同時也不考慮9月~12月的溫度和雨量
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



names(weather_df)<-c("Year","Month","Temp","Category")


#ggplot(data = mpg, aes(x = class, y = hwy, fill = factor(year))) +
ggplot(data = mpg, aes(x = class, y = hwy)) +
  geom_boxplot() 


#Weather <- data.frame(1:12, apply(Temp.Kao[,-1],2,mean),apply(Temp.Kao[,-1],2,sd),apply(Rain.Kao[,-1],2,mean),apply(Rain.Kao[,-1],2,sd))
#colnames(Weather) <- c("month", "Temp_mean", "Temp_sd", "Rain_mean", "Rain_sd")
#ylab <- expression("Min Temperature"(degree*C)) 

# how to plot two boxplots??? fill??
# format(as.Date(paste0("2015-", weather_df$Month, "-01"), format="%Y-%m-%d"),"%B")
# save to 01 02 03... 10 11 12
weather_df$Month_txt <- month.abb[weather_df$Month]
weather_df$Month_n[weather_df$Category==0] <- weather_df$Month[weather_df$Category==0]-0.2
weather_df$Month_n[weather_df$Category==1] <- weather_df$Month[weather_df$Category==1]+0.2
ggplot(data=weather_df, aes(x=month.text2,  y=Temp, fill = as.factor(Category))) + 
  
  geom_boxplot(aes(month.text2,Temp),outlier.size=0) +
  #geom_jitter(aes(Month_n,Temp),
  #            position=position_jitter(width=0.1,height=0),
  #            alpha=0.6,
  #            size=1,
  #            show_legend=FALSE) +
  geom_point(position=position_dodge(width=0.75),size=1.5) +
  #scale_fill_discrete(name="Month") +
  #geom_point(aes(y = Temp_mean, group=1, colour="red")) +
  #geom_line(aes(y = Temp_mean, group=1, colour="red")) +
  #geom_errorbar(aes(ymin=Temp_mean-Temp_sd/sqrt(n), ymax=Temp_mean+Temp_sd/sqrt(n)), group=1, width=.1) +
  #labs(y=ylab) +
  #geom_point(aes(y=Rain_mean/10, colour='blue')) +
  #geom_line(aes(y=Rain_mean/10, colour='blue')) +
  #geom_errorbar(aes(ymin=(Rain_mean-Rain_sd/sqrt(n))/10, ymax=(Rain_mean+Rain_sd/sqrt(n))/10), width=.1) +
  #scale_y_continuous(sec.axis = sec_axis(~.*10, name = "Max Rainfall(mm)"),limits = c(0, 30),minor_breaks = seq(0, 30, 5),breaks = seq(0, 30, 10)) +
  xlab("Month") +
  #scale_x_continuous(breaks=1:12,labels=month.abb) +
  scale_x_discrete(limits=month.abb) +
  #scale_color_manual(labels = c("Max Rainfall","Min Temperature"), values = c("red", "blue")) +
  theme(panel.grid.minor = element_blank(), legend.title=element_blank(), legend.position=c(0.85, 0.9))

  #ggsave("weathermonth.tiff", units="in", width=6.2, height=4, dpi=300, compression = 'lzw')

  