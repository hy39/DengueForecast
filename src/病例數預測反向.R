rm(list=ls(all=TRUE))
library(pscl)
## 2005-2017ぇ戈俱瞶
## –痜ㄒ璸衡眖讽Apr1-筳Mar31
Case.Kao <- read.csv("蔼动痜ㄒ.csv", header=T)
Temp.Kao <- read.csv("蔼动放.csv", header=T)
Rain.Kao <- read.csv("蔼动獴秖.csv", header=T)
#Temp.Kao <- read.csv("../戈/2005-2017蔼动るキА放.csv", header=T)
#Rain.Kao <- read.csv("../戈/2005-2017蔼动るキА獴秖.csv", header=T)
#奔2017ぃσ納9る~12る放㎝獴秖
Kao <- cbind(Case.Kao[-13,], Temp.Kao[-13,2:9], Rain.Kao[-13,2:9]) 
n <- dim(Kao)[1]
k <- dim(Kao)[2] - 3

##use "step function"(AIC) to do forward selection:
#fullmodel <- CASE3 ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8
fullmodel <- glm(CASE3 ~ R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8, family = poisson, data = Kao) 
#fit1 <- lm(crew ~ age + tonnage + passengers + length + cabins + passdens)
#fit2 <- lm(crew ~ 1)
#glm_fin <- step(glm(fullmodel, family = poisson, data = Kao), 
#                direction = "backward")
sp1.back<-stepAIC(fullmodel, trace=TRUE) 

##ノ"step function"(AIC)┮匡家и︳璸2017箇代痜ㄒ计
fit.glm <- glm(CASE3 ~ T6 + R5 + R1 + R8 + R2 + R4 + T7 + T5 + T2 + T3,
               family = poisson(log), 
               data = Kao)
newdata <- c(1,T6=Temp.Kao[13,7], 
             R5=Rain.Kao[13,6],
             R1=Rain.Kao[13,2],
             R8=Rain.Kao[13,9],
             R2=Rain.Kao[13,3], 
             R4=Rain.Kao[13,5],
             T7=Temp.Kao[13,8],
             T5=Temp.Kao[13,6],
             T2=Temp.Kao[13,3],
             T3=Temp.Kao[13,4])
pred2017 <- exp(sum(fit.glm$coeff *newdata))
pred2017 #2017痜ㄒ计箇代
round(fit.glm$coeff,3) #癹耴把计︳璸


##痷龟祇痜计籔︳璸祇痜计ゑ耕
y_actual <- Kao$CASE3
y_hat <- round(glm_fin$fitted)
y <- data.frame(y_actual,y_hat)
save(y, file = "DenguePrediction.RData")