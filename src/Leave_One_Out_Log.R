## l-o-o cross validation

rem_id <- nrow(Kao) #ignore 2016 in Kao because case number is too low
#rem_id <- 0        #include every years
Kao_new <- Kao 
n <- dim(Kao_new)[1]
p <- length(glm_fin$coeff)-1
  
loo_pred <- rep(NA, n)
loo_fitted <- matrix(NA, n, n)
loo_train_mse <- rep(NA, n)

#log scale
log_loo_fitted <- matrix(NA, n, n)

MSE_Tr <- 0
MSE_Va <- 0
MSE_Ratio <- 0
Expected_Ratio <-0

for(i in 1:n) 
   {
   print(i)
   glm_i <- glm(glm_fin$formula, family = poisson, data = Kao_new[-i,])
   loo_pred[i] <- predict(glm_i, type = "response", newdata = Kao_new[i,])
   fitted <- glm_i$fitted
   loo_fitted[-i, i] <- fitted
   loo_fitted[i,i] <- loo_pred[i]
   
   log_loo_fitted[-i, i] <- log(fitted)
   log_loo_fitted[i,i] <- log(loo_pred[i])
    #loo_pred[i] <- round(predict(glm_i, type = "response", newdata = Kao[i,]))
   #loo_fitted[-i, i] <- round(glm_i$fitted)
   
#   loo_train_mse[i] <- mean((log(glm_i$y) - log(fitted))^2)
}
#log_loo_fitted_new <- log_loo_fitted[-rem_id, -rem_id]
if (rem_id == 0) {
  log_loo_fitted_new <- log_loo_fitted
} else {
log_loo_fitted_new <- log_loo_fitted[-rem_id, -rem_id]
}
##MSE for training set
for(i in 1:nrow(log_loo_fitted_new)) #ignore 2016 because case number is too low
{
  log_fitted <- log_loo_fitted_new[,i]
  loo_train_mse[i] <- mean((log(glm_i$y[-i]) - log_fitted[-i])^2)
}
if (rem_id > 0) {
loo_train_mse <- loo_train_mse[-rem_id]
}
MSE_Tr <- mean(loo_train_mse)


loo_matrix <- t(loo_fitted)
diag(loo_matrix) <- loo_pred
#write.csv(loo_matrix, file = "loo_matrix.csv", row.names = FALSE)

length(glm_fin$coeff)
##MSE for validating set
if (rem_id > 0) {
  loo_mse <- mean((log(y_actual[1:(n-1)]) - log(loo_pred[-rem_id])) ^ 2)
} else {
  loo_mse <- mean((log(y_actual[1:n]) - log(loo_pred)) ^ 2)
}
MSE_Va <- loo_mse

MSE_Ratio <- MSE_Tr/MSE_Va
if (rem_id>0) {
  Expected_Ratio <- ((n-1)-p-1)/((n-1)+p+1)
} else {
  Expected_Ratio <- (n-p-1)/(n+p+1)
}
##痷龟祇痜计家︳璸祇痜计籔leave-one-oout︳璸祇痜计ゑ耕
Y_Pred <- data.frame(y_actual,y_hat,loo_pred) 
Validate <- data.frame(MSE_Ratio, Expected_Ratio <- (n-p-1)/(n+p+1))
