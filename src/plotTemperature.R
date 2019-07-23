rm(list = ls())
library(ggplot2)
# Print mean precipitation from 2005-2016
#TAvg <- read.csv("../戈/2005-2017蔼动るキА放.csv", header=T)
#colnames(T) <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016') 

TAvg <- read.csv("../戈/1998-2017蔼动る程放.csv", header=T)
colnames(T) <- c('1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016') 
T <- t(TAvg)
T <- T[,-ncol(T)] #remove 2017 record
T <- T[-1,]
Tavg <- 0
Tmin <- 0

for (i in 1:length(T)) 
{
   Tavg[i] <- T[i] 
}
date <- seq(as.Date("1998-01-01"), by="1 month", length.out=length(T))
#date <- melt(date)
#var0 <- 100 + c(0, cumsum(runif(49, -20, 20)))
#D <- data.frame(date, Tavg)

TMin <- read.csv("../戈/1998-2017蔼动る程放.csv", header=T)
#TMin <- read.csv("蔼动放.csv", header=T)
T <- t(TMin)
T <- T[,-ncol(T)]
T <- T[-1,]
colnames(T) <- c('1998','1999','2000','2001','2002','2003','2004','2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016') 

for (i in 1:length(T)) 
{
   Tmin[i] <- T[i] 
}
date <- seq(as.Date("1998-01-01"), by="1 month", length.out=length(T))
#var0 <- 100 + c(0, cumsum(runif(49, -20, 20)))
D <- data.frame(date, Tavg, Tmin)
ylab <- expression("Temperature " ( degree*C)) 
library(scales)
ggplot(D, aes(date)) + 
  #geom_line(aes(y = Tavg, colour = "Tavg"), linetype = "dashed") + 
  geom_line(aes(y = Tavg, colour = "Tavg")) +
    geom_line(aes(y = Tmin, colour = "Tmin")) +

  #scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = ) +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", limits = c(as.Date(date[1]-30), as.Date("2016-12-01")) ) +
  xlab('Year') +
  labs(y=ylab) +
  theme(legend.position="none")
  #theme(legend.justification=c(1,0), legend.position=c(1,0))

  ggsave("temperature.tiff", units="in", width=6, height=2.5, dpi=300, compression = 'lzw')
#================================================================





#ggplot(D, aes(date)) + 
#  geom_line(aes(y = var1, colour = "var1"))



