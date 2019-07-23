rm(list=ls(all=TRUE))
library('MASS')
source('stepAICc.R')
## 2005-2017之資料整理
## 每年病例計算從當年Apr1-隔年Mar31
Case.Kao <- read.csv("高雄病例.csv", header=T)
Temp.Kao <- read.csv("高雄溫度.csv", header=T)

#Rain.Kao <- read.csv("高雄雨量.csv", header=T)
#Rain.Kao <- read.csv("高雄雨量(2).csv", header=T) % Both average and maximum rainfall
Rain.Kao <- read.csv("../資料/2005-2017年高雄各月份平均雨量.csv", header=T)
#Temp.Kao <- read.csv("../資料/2005-2017年高雄各月份平均溫度.csv", header=T)

#RainA.Kao <- read.csv("../資料/2005-2017年高雄各月份平均雨量(2).csv", header=T)
#去掉2017，同時也不考慮9月~12月的溫度和雨量
pred_year_row <- nrow(Case.Kao)
Kao <- cbind(Case.Kao[-pred_year_row,], Temp.Kao[-pred_year_row,2:9], Rain.Kao[-pred_year_row,2:9]) 
Kao2017 <- cbind(1, Temp.Kao[pred_year_row,2:9], Rain.Kao[pred_year_row,2:9]) 
#Kao <- cbind(Case.Kao[-13,], Temp.Kao[-13,2:9], Rain.Kao[-13,2:9], RainA.Kao[-13,2:9]) 
n <- dim(Kao)[1]
k <- dim(Kao)[2] - 3

##use "step function"(AIC) to do forward selection:
#full parameter sets: T, R and AR
#glm_fin <- stepAICc(glm(CASE3 ~ 1, family = poisson, data = Kao),
#                scope = CASE3 ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8 + AR1 + AR2 + AR3 + AR4 + AR5 + AR6 + AR7 + AR8, 
#                direction = "both")
#T and R
glm_fin <- stepAICc(glm(CASE3 ~ 1, family = poisson, data = Kao),
                scope = CASE3 ~ T1 + T2 + T3 + T4 + T5 + T6 + T7 + T8 + R1 + R2 + R3 + R4 + R5 + R6 + R7 + R8, 
                direction = "forward")

##利用"step function"(AIC)所選出的模型，我們可以估計2017年預測病例數
fit.glm <- glm_fin
par <- rownames(as.data.frame(fit.glm$coeff))
par <- par[2:length(par)]
pred2017 <- exp(sum(fit.glm$coeff*cbind(1,Kao2017[par])))

##真實發病數與估計發病數的比較
y_actual <- Kao$CASE3
y_hat <- round(glm_fin$fitted)
y <- data.frame(y_actual,y_hat)
#save(y, file = "DenguePrediction.RData")