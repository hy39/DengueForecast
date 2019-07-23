library(ggplot2)
CasesMonth <- read.csv("../資料/2005-2017年高雄各月份病例數.csv", header=T)
C <- t(CasesMonth)
C <- C[-1,]
cases <- 0
colnames(C) <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016') 

for (i in 1:length(C)) 
{
  cases[i] <- C[i] 
}
date <- seq(as.Date("2005-01-01"), by="1 month", length.out=144)
#var0 <- 100 + c(0, cumsum(runif(49, -20, 20)))
D <- data.frame(date, cases)
library(scales)
ggplot(D, aes(date)) + 
  geom_line(aes(y = cases)) + 
  scale_x_date(date_breaks = "2 year", date_labels = "%Y", limits = c(as.Date("2005-01-01"), as.Date("2016-12-01"))) +
  xlab('Year') +
  ylab('Monthly Case number')
#ggsave("denguecase.tiff", units="in", width=6, height=2.5, dpi=300, compression = 'lzw')