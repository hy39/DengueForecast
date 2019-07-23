##confidence interval
n_rep <- 1000
y_hat_pool <- matrix(0, n, n_rep)
#ignore year 2017 and climatic data from September to December
new.Kao <- cbind(Temp.Kao[1:12,2:9], Rain.Kao[1:12,2:9])
for(i in 1:n_rep)
   {
   y_sim <- rpois(n, y_hat)
   #y_sim <- rpois((n+1), c(y_hat,pred2017))
   glm_i <- glm(y_sim ~ T6 + R5 + R1 + R8 + R2 + R4 + T7 + T5 + T2,
   family = poisson(log), 
   data=new.Kao)
   y_hat_pool[, i] <- glm_i$fitted
   }
y_hat_lwr <- round(apply(y_hat_pool, 1, quantile, probs = 0.025))
y_hat_upr <- round(apply(y_hat_pool, 1, quantile, probs = 0.975))
y_ci <- data.frame(y_hat_lwr,y_hat_upr)
save(y_ci, file = "DenguePrediction.RData")