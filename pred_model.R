library(quantmod)
library(nnet)


data <- read.csv("EURUSD_M30_2.csv", header = F, sep = ";")
data[,2] <- NULL
data2 <- data[13754:23753,]
colnames(data) <- c("Date", "Time", "Open", "High", "Low", "Close", "Volume")
data_xts <- xts(data[,c(-1, -2)], order.by = as.POSIXct(paste(data[,c(1)], data[,c(2)]), format = "%Y.%m.%d %H:%M:%S"))

data_xts$rsi <- RSI(data_xts$Close)
data_xts$MACD <- MACD(data_xts$Close)
data_xts$will <- williamsAD(data_xts[,2:4])
data_xts$cci <-  CCI(data_xts[,2:4])
data_xts$STOCH <- stoch(data_xts[,2:4])
data_xts$Aroon <- aroon(data_xts[, 2:3])
data_xts$ATR <- ATR(data_xts[, 2:4])


data_xts$H_Return <- diff(log(data_xts$High))
data_xts$L_Return <- diff(log(data_xts$Low))
data_xts$C_Return <- diff(log(data_xts$Close))


for(i in 1:(nrow(data_xts)-1))
{
  data_xts[i,"H_Return"] <- data_xts[i + 1,"H_Return"]
  data_xts[i,"L_Return"] <- data_xts[i + 1,"L_Return"]
  data_xts[i,"C_Return"] <- data_xts[i + 1,"C_Return"]
}

s <- 1045

for( i in (s:nrow(data_xts)))
{
  
  model_high <- nnet(H_Return ~ Open + High + Low + Close + Volume + rsi + macd + MACD + will + cci + 
                       fastK + fastD + STOCH + aroonUp + aroonDn + Aroon + tr + atr + trueHigh + ATR, 
                     data = data_xts[((i-1000):(i-2)),], maxit = 5000, size = 20, decay = 0.01, linout = 1)
  
  model_low <- nnet(L_Return ~ Open + High + Low + Close + Volume + rsi + macd + MACD + will + cci + 
                      fastK + fastD + STOCH + aroonUp + aroonDn + Aroon + tr + atr + trueHigh + ATR, 
                    data = data_xts[((i-1000):(i-2)),], maxit = 5000, size = 20, decay = 0.01, linout = 1)
  
  model_close <- nnet(C_Return ~ Open + High + Low + Close + Volume + rsi + macd + MACD + will + cci + 
                        fastK + fastD + STOCH + aroonUp + aroonDn + Aroon + tr + atr + trueHigh + ATR, 
                      data = data_xts[((i-1000):(i-2)),], maxit = 5000, size = 20, decay = 0.01, linout = 1)
  
  
  
  
  pred_h <- predict(model_high, newdata = data_xts[i-1,(1:20)])  
  data[i,"H_Prediction"] <- exp(pred_h[1]) * data[i-1, "High"]
  
  pred_l <- predict(model_low, newdata = data_xts[i-1,(1:20)])  
  data[i,"L_Prediction"] <- exp(pred_l[1]) * data[i-1, "Low"]
  
  pred_c <- predict(model_close, newdata = data_xts[i-1,(1:20)])  
  data[i,"C_Prediction"] <- exp(pred_c[1]) * data[i-1, "Close"]
  
}

data_write <-  data[1044:4486,]
write.csv(data_write, "pred_data3_30.csv")

for(i in (2:nrow(data_final)))
{
  if(  ((data_final[i,2] > data_final[(i-1), 1])&&(data_final[i,1] > data_final[(i-1), 1])) || 
       ((data_final[i,2] < data_final[(i-1), 1])&&(data_final[i,1] < data_final[(i-1), 1])))
  {
    data_final[i, "Move"] <- "Correct"
  }
  else{
    data_final[i, "Move"] <- "Wrong"
  }
}

sum(data_final[-1,"Move"] == "Correct")/nrow(data_final[-1,])
cor(data_final$Close,data_final$C_Prediction)^2

plus <- 0
minus <- 0
for(i in 2:nrow(data_final))
{
  if(data_final[i, "Move"] == "Correct")
  {
    plus <- plus + abs(data_final[i, "Close"] - data_final[(i-1), "Close"])
  }
  else{
    minus <- minus + (abs(data_final[i, "Close"] - data_final[(i-1), "Close"]))
  }
}
((plus - minus)/0.0001)*10 #profit/loss

data3 <- data2[1045:1652,] 
rownames(data3) <- NULL
write.csv(data3, "pred_data_30.csv")



