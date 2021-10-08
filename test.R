library(rzmq)
library(xts)
library(quantmod)
library(nnet)


RRR <- 3      # Reward:Risk 
HL <- 2        # High:Low 
risk <- 0.01     # % risk per trade

# Function to send commands to ZeroMQ MT4 EA
remote.send <- function(rSocket,data) {
  send.raw.string(rSocket, data)
   msg <- receive.string(rSocket)
  
   # print(msg)
}

# Function to PULL data from ZeroMQ MT4 EA PUSH socket.
remote.pull <- function(pSocket) {
  
  msg <- receive.socket(pSocket)
  
  if(!is.null(msg)) {
    msg <- rawToChar(msg)
  }
  
  return(msg)
}

# Dataframe creation function
create_df <- function(data)
{
  df <- data.frame()
  first_split <- strsplit(data, "||", T)
  count <- nlevels(as.factor(first_split[[1]]))
  
  for(i in (1:count))
  {
    splt <- strsplit(as.character(as.factor(first_split[[1]])[i]), "|", T)
    df[i, "Date"] <- as.POSIXct(splt[[1]][1], format = "%Y.%m.%d %H:%M")
    df[i, "Open"] <- as.numeric(splt[[1]][2])
    df[i, "High"] <- as.numeric(splt[[1]][3])
    df[i, "Low"] <- as.numeric(splt[[1]][4])
    df[i, "Close"] <- as.numeric(splt[[1]][5])
    df[i, "Volume"] <- as.numeric(splt[[1]][6])
  }
  
  return(df)
}

# CREATE ZeroMQ Context
context = init.context()

# Initialize ZeroMQ REQ Socket
reqSocket = init.socket(context,"ZMQ_REQ")

# Initialize ZeroMQ PULL Socket
pullSocket = init.socket(context, "ZMQ_PULL")

# Connect to REQ Socket on port 5555
connect.socket(reqSocket,"tcp://localhost:5555")

# Connect to PULL Socket on port 5556
connect.socket(pullSocket,"tcp://localhost:5556")

# main cycle
while(T)
{
  print("No active positions, ready to continue..")
  
  remote.send(reqSocket, "time")
  time <- receive.string(pullSocket)
  time <- align.time(as.POSIXct(time, format = "%Y.%m.%d %H:%M:%S"), 1800)
  print("This is a rounded time")
  print(time)
  
  remote.send(reqSocket, "time")
  time2 <- receive.string(pullSocket)
  time2 <- as.POSIXct(time2, format = "%Y.%m.%d %H:%M:%S")
  print("This is a current time")
  print(time2)
  
  while (time >= time2)
  {
    remote.send(reqSocket, "time")
    time2 <- as.POSIXct(receive.string(pullSocket), format = "%Y.%m.%d %H:%M:%S")
    
  }
  remote.send(reqSocket, "time")
  time3 <- as.POSIXct(receive.string(pullSocket), format = "%Y.%m.%d %H:%M:%S")
  
  print("This is the time, the algo has been waiting for")
  print(time2)
  print("This is a current time")
  print(time3)
  
  remote.send(reqSocket, "data")
  rec_data <- receive.string(pullSocket)
  df <- create_df(rec_data)
  
  close_t1 <- df[nrow(df), "Close"]
  predicted_values <- neural_net(df)
  
  request <- management_system(close_t1, predicted_values[1], predicted_values[2], predicted_values[3])
  if(!is.null(request))
  {
    remote.send(reqSocket, request)
  }
  else
  {
    print("Nothing to buy or sell")
  }
  
  print("----------------------------------------")
  print("")
}

# side cycles
while(T)
{
  remote.send(reqSocket, "time")
  time <- receive.string(pullSocket)
  time <- align.time(as.POSIXct(time, format = "%Y.%m.%d %H:%M:%S"), 1800)
  print("This is a rounded time")
  print(time)
  
  remote.send(reqSocket, "time")
  time2 <- receive.string(pullSocket)
  time2 <- as.POSIXct(time2, format = "%Y.%m.%d %H:%M:%S")
  print("This is a current time")
  print(time2)
  
  while (time >= time2)
  {
    remote.send(reqSocket, "time")
    time2 <- as.POSIXct(receive.string(pullSocket), format = "%Y.%m.%d %H:%M:%S")
    
  }
  remote.send(reqSocket, "time")
  time3 <- as.POSIXct(receive.string(pullSocket), format = "%Y.%m.%d %H:%M:%S")
  
  print("This is the time, the algo has been waiting for")
  print(time2)
  print("This is a current time")
  print(time3)
  
  remote.send(reqSocket, "data")
  rec_data <- receive.string(pullSocket)
  df <- create_df(rec_data)
  
  close_t1 <- df[nrow(df), "Close"]
  break
}

neural_net <- function(df) 
{
  df_xts <- xts(df[,c(-1)], order.by = as.POSIXct(paste(df[,c(1)]), format = "%Y-%m-%d %H:%M:%S"))
  
  df_xts$rsi <- RSI(df_xts$Close)
  df_xts$MACD <- MACD(df_xts$Close)
  df_xts$will <- williamsAD(df_xts[,2:4])
  df_xts$cci <-  CCI(df_xts[,2:4])
  df_xts$STOCH <- stoch(df_xts[,2:4])
  df_xts$Aroon <- aroon(df_xts[, 2:3])
  df_xts$ATR <- ATR(df_xts[, 2:4])
  
  df_xts$H_Return <- diff(log(df_xts$High))
  df_xts$L_Return <- diff(log(df_xts$Low))
  df_xts$C_Return <- diff(log(df_xts$Close))
  
  for(i in 1:(nrow(df_xts)-1))
  {
    df_xts[i,"H_Return"] <- df_xts[i + 1,"H_Return"]
    df_xts[i,"L_Return"] <- df_xts[i + 1,"L_Return"]
    df_xts[i,"C_Return"] <- df_xts[i + 1,"C_Return"]
  }
  
  
  model_high <- nnet(H_Return ~ Open + High + Low + Close + Volume + rsi + macd + MACD + will + cci + 
                        fastK + fastD + STOCH + aroonUp + aroonDn + Aroon + tr + atr + trueHigh + ATR, 
                       data = df_xts[1:(nrow(df_xts)-1)], maxit = 5000, size = 20, decay = 0.01, linout = 1)
    
  model_low <- nnet(L_Return ~ Open + High + Low + Close + Volume + rsi + macd + MACD + will + cci + 
                      fastK + fastD + STOCH + aroonUp + aroonDn + Aroon + tr + atr + trueHigh + ATR, 
                      data = df_xts[1:(nrow(df_xts)-1)], maxit = 5000, size = 20, decay = 0.01, linout = 1)
    
  model_close <- nnet(C_Return ~ Open + High + Low + Close + Volume + rsi + macd + MACD + will + cci + 
                        fastK + fastD + STOCH + aroonUp + aroonDn + Aroon + tr + atr + trueHigh + ATR, 
                        data = df_xts[1:(nrow(df_xts)-1)], maxit = 5000, size = 20, decay = 0.01, linout = 1)
    
  
  pred_h <- predict(model_high, newdata = df_xts[nrow(df_xts),(1:20)])  
  h_prediction <- exp(pred_h[1]) * df[nrow(df), "High"]
    
  pred_l <- predict(model_low, newdata = df_xts[nrow(df_xts),(1:20)])  
  l_prediction <- exp(pred_l[1]) * df[nrow(df), "Low"]
    
  pred_c <- predict(model_close, newdata = df_xts[nrow(df_xts),(1:20)])  
  c_prediction <- exp(pred_c[1]) * df[nrow(df), "Close"]
    
  pred_values <- c(h_prediction, l_prediction, c_prediction)
  
  return(pred_values)
}

management_system <- function(close_t1, p_high, p_low, p_close)
{
  mess <- ""
  
  if(abs(close_t1 - p_high) > (HL * abs(close_t1 - p_low)))
  {
    order_type <- "LONG"
    size <- get_position_size(order_type, p_high, p_low)
    sl <- p_low
    tp <- p_high

    mess <- paste("BUY", size, sl, tp, sep = "|")
  }
  else if (HL * abs(close_t1 - p_high) < (abs(close_t1 - p_low)))
  {
    order_type <- "SHORT"
    size <- get_position_size(order_type, p_high, p_low)
    sl <- p_high
    tp <- p_low
    
    mess <- paste("SELL", size, sl, tp, sep = "|")
  }
  
  return(mess)
}

get_position_size <- function(order_type, p_high, p_low)
{
  remote.send(reqSocket, "balance")
  balance <- as.numeric(receive.string(pullSocket))
  
  remote.send(reqSocket, "actual_price")
  actual_price <- as.numeric(receive.string(pullSocket))
  
  position_size <- 0
  
  if(order_type == "LONG")
  {
    dollar_risk <- balance * risk
    pips_sl <- (actual_price - p_low)/0.0001
    pips_tp <- (p_high - actual_price)/0.0001
    
    position_size_sl <- dollar_risk/(pips_sl * 10) 
    position_size_tp <- (dollar_risk * RRR)/(pips_tp * 10)
    
    position_size <- min(position_size_sl, position_size_tp)
    
  }
  else if(order_type == "SHORT")
  {
    dollar_risk <- balance * risk
    pips_sl <- (p_high - actual_price)/0.0001
    pips_tp <- (actual_price - p_low)/0.0001
    
    position_size_sl <- dollar_risk/(pips_sl * 10) 
    position_size_tp <- (dollar_risk * RRR)/(pips_tp * 10)
    
    position_size <- min(position_size_sl, position_size_tp)
  }
  
  return(position_size)
}

remote.send(reqSocket, "total_positions")
total_positions <- as.numeric(receive.string(pullSocket))

while(total_positions > 0)
{
  remote.send(reqSocket, "total_positions")
  total_positions <- as.numeric(receive.string(pullSocket))
  print("Waiting for closing current position")
}
