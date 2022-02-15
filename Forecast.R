my_data <- read.delim("dataco2.txt")
trainset=my_data[1:38,]
validset=my_data[39:59,]
timeserie=ts(data=trainset,start=1960)

# étape 1 : Identification
# représentation graphique
plot(timeserie)

#tendance lineaire
annee=1960:1997
model=lm(trainset~annee)
model
abline(model,col="blue")

#correlogramme simple d'ordre 36
acf(timeserie,lag.max=36,plot=TRUE)

#correlogramme partiel d'ordre 36
pacf(timeserie,lag.max=36,plot=TRUE)


# plot la serie apres differenciation
gnpgr = diff(diff(trainset)) 
main <- "Emission de CO2"
plot(gnpgr, main = main, ylab = "CO2", type="l")


# ACF et PACF apres differenciation
par(mfrow = c(2,1)) 
acf(diff (diff(timeserie)), 36, main = "ACF for First Difference ", 
    panel.first = grid ())
pacf(diff (diff(timeserie)), 36, main = "PACF for First Difference ", 
     panel.first = grid ())

# étape 2 : estimation
modelTs1 <- arima (timeserie, order = c(1, 2, 0))
modelTs4 <- arima (timeserie, order = c(4, 2, 0))
modelTs4

n <- length(timeserie)
se <- sqrt(diag(vcov(modelTs4)))
tstat = abs(sqrt(n - 1)*coef(modelTs4)/se)
tstat

# étape 3 : validation
modelTs1
modelTs4
Box.test(modelTs4$residuals, lag = 6, type = "Ljung-Box")

# étape 4 : Prévision
library(forecast)
modelTs4.for <- predict(modelTs4, n.ahead = 21)

par (mfrow = c(1,1)) 
ts.plot (timeserie, modelTs4.for$pred, 
         ylab = "Prediction", col = c("blue", "red"))
grid ()
U.for = modelTs4.for$pred + modelTs4.for$se
L.for = modelTs4.for$pred - modelTs4.for$se
xx.for = c(time (U.for), rev (time (U.for)))
yy.for = c(L.for, rev(U.for))
polygon (xx.for, yy.for, border = 8, col = gray (0.6, alpha =0.2))
lines (modelTs4.for$pred, type = "p", col = "red")

# évaluation du modele
getPerformance = function(pred, val) {
  res = pred - val
  MAE = sum(abs(res))/length(val)
  RSS = sum(res^2)
  MSE = RSS/length(val)
  RMSE = sqrt(MSE)
  perf = data.frame(MAE, RSS, MSE, RMSE)
}

eval = getPerformance(modelTs4.for$pred, validset)
eval