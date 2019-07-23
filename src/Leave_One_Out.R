## l-o-o cross validation
n <- dim(Kao)[1]
p <- length(glm_fin$coeff)-1
  
loo_pred <- rep(NA, n)
loo_fitted <- matrix(NA, n, n)
loo_train_mse <- rep(NA, n)
MSE_Tr <- 0
MSE_Va <- 0
MSE_Ratio <- 0
Expected_Ratio <-0

for(i in 1:n)
{
  glm_i <- glm(glm_fin$formula, family = poisson, data = Kao[-i,])
  loo_pred[i] <- round(predict(glm_i, type = "response", newdata = Kao[i,]))
  loo_fitted[-i, i] <- round(glm_i$fitted)
  loo_fitted[i,i] <- loo_pred[i]
  loo_train_mse[i] <- mean((glm_i$y - glm_i$fitted) ^ 2)
}
MSE_Tr <- mean(loo_train_mse)

loo_matrix <- t(loo_fitted)
diag(loo_matrix) <- loo_pred
write.csv(loo_matrix, file = "loo_matrix.csv", row.names = FALSE)

length(glm_fin$coeff)
##MSE for validating set
loo_mse <- mean((y_actual - loo_pred) ^ 2)
MSE_Va <- loo_mse

MSE_Ratio <- MSE_Tr/MSE_Va
Expected_Ratio <- (n-p-1)/(n+p+1)

##痷龟祇痜计家︳璸祇痜计籔leave-one-oout︳璸祇痜计ゑ耕
Y_Pred <- data.frame(y_actual,y_hat,loo_pred) 
Validate <- data.frame(MSE_Ratio, Expected_Ratio <- (n-p-1)/(n+p+1))
