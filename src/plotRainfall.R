library(ggplot2)
# Print mean precipitation from 2005-2016
RAvg <- read.csv("../資料/2005-2017年高雄各月份平均雨量.csv", header=T)
R <- t(RAvg)
R <- R[,-13] #remove 2017 record
R <- R[-1,]
Ravg <- 0
Rmax <- 0

colnames(R) <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016') 
for (i in 1:length(R)) 
{
   Ravg[i] <- R[i] 
}

RMax <- read.csv("高雄雨量.csv", header=T)
R <- t(RMax)
R <- R[,-13]
R <- R[-1,]
colnames(R) <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016') 
for (i in 1:length(R)) 
{
   Rmax[i] <- R[i] 
}

date <- seq(as.Date("2005-01-01"), by="1 month", length.out=144)
D <- data.frame(date, Rmax, Ravg)
ylab <- expression("Daily maximum rainfall") 
library(scales)
ggplot(D, aes(date)) + 
  geom_line(aes(y = Rmax, colour = "Rmax")) +
  geom_line(aes(y = Ravg*100, colour = "Ravg")) + 
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", limits = c(as.Date("2005-01-01"), as.Date("2016-12-01")) ) +
  scale_y_continuous(sec.axis = sec_axis(~./100, name="Daily average rainfall")) +
  xlab('Year') +
  labs(y=ylab) +
  theme(legend.position="none")
#ggsave("rainfall_avg.tiff", units="in", width=6, height=2.5, dpi=300, compression = 'lzw')
