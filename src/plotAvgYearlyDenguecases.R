rm(list=ls(all=TRUE))
library(ggplot2)
CasesMonth <- read.csv("../資料/2005-2017年高雄各月份病例數.csv", header=T)
C <- t(CasesMonth)
C <- C[-1,]
cases <- 0
years <- c('2005','2006','2007','2008','2009','2010','2011','2012','2013','2014','2015','2016')
colnames(C) <- years
cases_Obs <- rowSums(CasesMonth)
load("DenguePrediction.RData") 
load("DenguePredictionCI.RData") 
casedf <- data.frame(years, y$y_hat, y_ci$y_hat_lwr, y_ci$y_hat_upr, y$y_actual)
colnames(casedf) <- c('years','estimated','lb','ub','actual')
  
for (i in 1:length(C)) 
{
  cases[i] <- C[i] 
}
#date <- seq(as.Date("2005-01-01"), by="1 month", length.out=144)
#D <- data.frame(date, cases)
#library(scales)

limits <- aes(ymax = casedf$ub, ymin = casedf$lb)
cols <- c("Estimated"="#3591d1","Observed"="#f04546")

ggplot(data=casedf, aes(x=years)) + 
   

  #scale_x_date(date_breaks = "2 year", date_labels = "%Y", limits = c(as.Date("2005-01-01"), as.Date("2016-12-01"))) +
  xlab('Years') +
  ylab('Monthly Case number') +

  geom_bar(aes(y=estimated,fill = "Estimated"),colour="#333333",width=.8,alpha = 0.8,stat="identity") +
  geom_errorbar(limits, width = 0.45) +

  #geom_line(aes(y=actual,group=1,colour="Observed")) + 
  geom_point(aes(y=actual,colour="Observed"), size=2) +
  
  scale_fill_manual(name="",values=cols) + scale_colour_manual(name="",values=cols) +
  scale_x_discrete(breaks=years) +
  #scale_y_continuous(breaks = c(seq(0, 20000, by = 5000)), labels = c(0, 500, 1000, 15000, 20000))
  #theme(axis.text.x=element_blank()) + 
  scale_y_log10(breaks = c( 1, 10, 100, 1000, 10000, 20000))
#ggsave("denguecase.tiff", units="in", width=6, height=2.5, dpi=300, compression = 'lzw')