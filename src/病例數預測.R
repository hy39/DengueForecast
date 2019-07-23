rm(list=ls(all=TRUE))
library('MASS')
source('stepAICc.R')
startyear <- 2005
## 2005-2017ぇ戈俱瞶
## –痜ㄒ璸衡眖讽Apr1-筳Mar31
## ㄏノ环ǎ程放㎝程獴秖

Case.Kao <- read.csv("../戈/蔼动痜ㄒ.csv", header=T)
Temp.Kao <- read.csv("../戈/蔼动放.csv", header=T)
Rain.Kao <- read.csv("../戈/蔼动獴秖.csv", header=T)

#Case.Kao <- read.csv("../戈/1998-2017蔼动祅荐痜ㄒ计.csv", header=T)
#Temp.Kao <- read.csv("../戈/1998-2017蔼动る程放.csv", header=T)
#Rain.Kao <- read.csv("../戈/1998-2017蔼动る程獴.csv", header=T)
if (startyear == 2002) {
  Case.Kao <- read.csv("../戈/2002-2017蔼动祅荐痜ㄒ计.csv", header=T)
  Temp.Kao <- read.csv("../戈/2002-2017蔼动る程放.csv", header=T)
  Rain.Kao <- read.csv("../戈/2002-2017蔼动る程獴.csv", header=T)
}
#奔2017ぃσ納9る~12る放㎝獴秖
Kao <- cbind(Case.Kao[-nrow(Case.Kao),], Temp.Kao[-nrow(Case.Kao),2:9], Rain.Kao[-nrow(Case.Kao),2:9]) 
Kao2017 <- cbind(1, Temp.Kao[nrow(Case.Kao),2:9], Rain.Kao[nrow(Case.Kao),2:9]) 
n <- dim(Kao)[1]
k <- dim(Kao)[2] - 3

##use "step function"(AIC) to do forward selection:
glm_fin <- stepAICc(glm(CASE3 ~ 1, family = poisson, data = Kao),
                scope = CASE3 ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8,
                #scope = CASE3 ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + R3 + R4 + R5 + R6 + R7 + R8,
                direction = "forward")
fit.glm <- glm_fin

##ノ"step function"(AIC)┮匡家и︳璸2017箇代痜ㄒ计
#fit.glm <- glm(CASE3 ~ T6 + R5 + R1 + R8 + R2 + R4 + T7 + T5 + T2,
#               family = poisson(log), 
#               data = Kao)
par <- rownames(as.data.frame(fit.glm$coeff))
par <- par[2:length(par)]

newdata <- c(1,T6=Temp.Kao[13,7], 
             R5=Rain.Kao[13,6],
             R1=Rain.Kao[13,2],
             R8=Rain.Kao[13,9],
             R2=Rain.Kao[13,3], 
             R4=Rain.Kao[13,5],
             T7=Temp.Kao[13,8],
             T5=Temp.Kao[13,6],
             T2=Temp.Kao[13,3] )
#             T3=Temp.Kao[13,4])
if (startyear == 2002) {
newdata <- c(1,T6=Temp.Kao[13,7], 
             R5=Rain.Kao[13,6],
             R1=Rain.Kao[13,2],
             R8=Rain.Kao[13,9],
             R2=Rain.Kao[13,3], 
             R4=Rain.Kao[13,5],
             R7=Rain.Kao[13,8],
             R6=Rain.Kao[13,7],
             T5=Temp.Kao[13,6],
             T3=Temp.Kao[13,4],
             T8=Temp.Kao[13,9]
             )
}

pred2017 <- exp(sum(fit.glm$coeff*cbind(1,Kao2017[par])))
#pred2017 <- exp(sum(fit.glm$coeff *newdata))
pred2017 #2017痜ㄒ计箇代
round(fit.glm$coeff,3) #癹耴把计︳璸


##痷龟祇痜计籔︳璸祇痜计ゑ耕
y_actual <- Kao$CASE3
y_hat <- round(glm_fin$fitted)
y <- data.frame(y_actual,y_hat)
save(y, file = "DenguePrediction.RData")