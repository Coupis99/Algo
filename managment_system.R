library(quantmod)
library(ggplot2)
library(plotly)

data <- read.csv("pred_data3_30.csv", sep = ",", header = T)
data[,1] <- NULL
data[1,"Balance"] <- 1000

data_m1 <- read.csv("EURUSD_M1_3.csv", header = F, sep = ";")
colnames(data_m1) <- c("Date", "Time", "Open", "High", "Low", "Close", "Volume")

data_m1_xts <- xts(data_m1[,c(-1, -2)], order.by = as.POSIXct(paste(data_m1[,c(1)], data_m1[,c(2)]), format = "%Y.%m.%d %H:%M:%S"))

balance <- 1000  # in USD
RRR <- 5      # Reward:Risk 
HL <- 2        # High:Low 
risk <- 0.01     # % risk na trade
leverage <- 3000   # leverage 1:x
position_size <- 0.1  # v lotech (nahrazeno funkcí)

data$Volatility <- volatility(data[,(3:6)])
data[1:9, "Volatility"] <- 0

###########################################

#Trading cycle
for (i in (2:nrow(data)))
{
  if (abs((data[i, "H_Prediction"] - data[(i-1), "Close"])) > (HL * abs((data[i, "L_Prediction"] - data[(i-1), "Close"]))) &&
      data[(i), "Volatility"] >= 0.01)
      
  {
    data[i, "Info"] <- "Buy"
    data <-  trade_buy(data, i)
    
    
  }
  else if (abs((data[i, "L_Prediction"] - data[(i-1), "Close"])) > (HL * abs((data[i, "H_Prediction"] - data[(i-1), "Close"]))) &&
           data[(i), "Volatility"] >= 0.01) 
          
  {
    data[i, "Info"] <- "Sell"
    data <-  trade_sell(data, i)
    
  }
  else
  {
    data[i, "Info"] <- "Nothing"
    data[i, "Balance"] <- data[(i-1), "Balance"]
  }
}

###########################################

trade_buy <- function(data, i)
{
  p_s <- NULL
  if (position_size_buy(data, i)[1] <= position_size_buy(data, i)[2])
  {
    data[i, "Leverage"] <- "Passed"
    p_s <- position_size_buy(data, i)[1]
  }
  else 
  {
    data[i, "Leverage"] <- paste("Failed (",position_size_buy(data, i)[2], ",", position_size_buy(data, i)[1], ")")
    p_s <- position_size_buy(data, i)[2]
  }
  
  open <- data[i, "Open"]
  sl <- open - (0.0001 * ((data[(i-1), "Balance"] * risk)/(p_s * 10)))
  tp <- open + RRR * ((0.0001 * ((data[(i-1), "Balance"] * risk)/(p_s * 10))))
  
  start_date <- data[i, "Date"]
  start_time <- data[i, "Time"]
  start_all <- paste(toString(as.Date(start_date, format = "%Y.%m.%d")), toString(start_time))
  end_all <- toString(as.POSIXct(start_all) + 1799)
  sub_data_m1_xts <- data_m1_xts[paste(start_all, "/", end_all, sep = "")]
  
  data[i, "Position_size"] <- p_s

  
  for (j in (1:nrow(sub_data_m1_xts)))
  {
      if(((sub_data_m1_xts[j,"Open"] >= tp)||(sub_data_m1_xts[j,"High"] >= tp)||(sub_data_m1_xts[j,"Low"] >= tp)||(sub_data_m1_xts[j,"Close"] >= tp)) 
         && ((sub_data_m1_xts[j,"Open"] <= sl)||(sub_data_m1_xts[j,"High"] <= sl)||(sub_data_m1_xts[j,"Low"] <= sl)||(sub_data_m1_xts[j,"Close"] <= sl)))
      {
        data[i,"Status"] <- "Cannot decide"
        data[i, "Balance"] <- data[(i-1), "Balance"]
        break
      }
      if((sub_data_m1_xts[j,"Open"] >= tp)||(sub_data_m1_xts[j,"High"] >= tp)||(sub_data_m1_xts[j,"Low"] >= tp)||(sub_data_m1_xts[j,"Close"] >= tp))
      {
        data[i,"Status"] <- "Win"
        data[i, "Trade_time"] <- index(sub_data_m1_xts[j])
        data[i, "Balance"] <- data[(i-1), "Balance"] + (((tp - open)/0.0001) * data[i, "Position_size"] * 10)
        data[i, "Growth_rate"] <- growth_function(data, i, (tp - open))[1]
        data[i, "Pips"] <- growth_function(data, i, (tp - open))[2]
        break
      }
      if((sub_data_m1_xts[j,"Open"] <= sl)||(sub_data_m1_xts[j,"High"] <= sl)||(sub_data_m1_xts[j,"Low"] <= sl)||(sub_data_m1_xts[j,"Close"] <= sl))
      {
        data[i,"Status"] <- "Loss"
        data[i, "Trade_time"] <- index(sub_data_m1_xts[j])
        data[i, "Balance"] <- data[(i-1), "Balance"] - (((open - sl)/0.0001) * data[i, "Position_size"] * 10)
        data[i, "Growth_rate"] <- growth_function(data, i, (sl - open))[1]
        data[i, "Pips"] <- growth_function(data, i, (sl - open))[2]
        break
      }
      if(j == nrow(sub_data_m1_xts))
      {
        if((data[i,"Close"] - open) > 0)
        {
          data[i,"Status"] <- "Trade to close - w"
          data[i, "Balance"] <- data[(i-1), "Balance"] + (((data[i,"Close"] - open)/0.0001) * data[i, "Position_size"] * 10)
          data[i, "Growth_rate"] <- growth_function(data, i, (data[i,"Close"] - open))[1]
          data[i, "Pips"] <- growth_function(data, i, (data[i,"Close"] - open))[2]
          break
        }
        if((data[i,"Close"] - open) <= 0)
        {
          data[i,"Status"] <- "Trade to close - l"
          data[i, "Balance"] <- data[(i-1), "Balance"] + (((data[i,"Close"] - open)/0.0001) * data[i, "Position_size"] * 10)
          data[i, "Growth_rate"] <- growth_function(data, i, (data[i,"Close"] - open))[1]
          data[i, "Pips"] <- growth_function(data, i, (data[i,"Close"] - open))[2]
          break
        }
      }
  }
  
  return(data)
}

###########################################

trade_sell <- function(data, i)
{
  p_s <- NULL
  if (position_size_sell(data, i)[1] <= position_size_sell(data, i)[2])
  {
    data[i, "Leverage"] <- "Passed"
    p_s <- position_size_sell(data, i)[1]
  }
  else 
  {
    data[i, "Leverage"] <- paste("Failed (",position_size_sell(data, i)[2], ",", position_size_sell(data, i)[1], ")")
    p_s <- position_size_sell(data, i)[2]
  }
  
  open <- data[i, "Open"]
  sl <- open + (0.0001 * ((data[(i-1), "Balance"] * risk)/(p_s * 10)))
  tp <- open - RRR * ((0.0001 * ((data[(i-1), "Balance"] * risk)/(p_s * 10))))
  
  start_date <- data[i, "Date"]
  start_time <- data[i, "Time"]
  start_all <- paste(toString(as.Date(start_date, format = "%Y.%m.%d")), toString(start_time))
  end_all <- toString(as.POSIXct(start_all) + 1799)
  sub_data_m1_xts <- data_m1_xts[paste(start_all, "/", end_all, sep = "")]
  
  data[i, "Position_size"] <- p_s
  
  for (j in (1:nrow(sub_data_m1_xts)))
  {
    if(((sub_data_m1_xts[j,"Open"] <= tp)||(sub_data_m1_xts[j,"High"] <= tp)||(sub_data_m1_xts[j,"Low"] <= tp)||(sub_data_m1_xts[j,"Close"] <= tp)) 
       && ((sub_data_m1_xts[j,"Open"] >= sl)||(sub_data_m1_xts[j,"High"] >= sl)||(sub_data_m1_xts[j,"Low"] >= sl)||(sub_data_m1_xts[j,"Close"] >= sl)))
    {
      data[i,"Status"] <- "Cannot decide"
      data[i, "Balance"] <- data[(i-1), "Balance"]
      break
    }
    if((sub_data_m1_xts[j,"Open"] <= tp)||(sub_data_m1_xts[j,"High"] <= tp)||(sub_data_m1_xts[j,"Low"] <= tp)||(sub_data_m1_xts[j,"Close"] <= tp))
    {
      data[i,"Status"] <- "Win"
      data[i, "Trade_time"] <- index(sub_data_m1_xts[j])
      data[i, "Balance"] <- data[(i-1), "Balance"] + (((open - tp)/0.0001) * data[i, "Position_size"] * 10)
      data[i, "Growth_rate"] <- growth_function(data, i, (open - tp))[1]
      data[i, "Pips"] <- growth_function(data, i, (open - tp))[2]
      break
    }
    if((sub_data_m1_xts[j,"Open"] >= sl)||(sub_data_m1_xts[j,"High"] >= sl)||(sub_data_m1_xts[j,"Low"] >= sl)||(sub_data_m1_xts[j,"Close"] >= sl))
    {
      data[i,"Status"] <- "Loss"
      data[i, "Trade_time"] <- index(sub_data_m1_xts[j])
      data[i, "Balance"] <- data[(i-1), "Balance"] - (((sl - open)/0.0001) * data[i, "Position_size"] * 10)
      data[i, "Growth_rate"] <- growth_function(data, i, (open - sl))[1]
      data[i, "Pips"] <- growth_function(data, i, (open - sl))[2]
      break
    }
    if(j == nrow(sub_data_m1_xts))
    {
      if((open - data[i, "Close"]) > 0)
      {
        data[i,"Status"] <- "Trade to close - w"
        data[i, "Balance"] <- data[(i-1), "Balance"] + (((open - data[i, "Close"])/0.0001) * data[i, "Position_size"] * 10)
        data[i, "Growth_rate"] <- growth_function(data, i, (open - data[i, "Close"]))[1]
        data[i, "Pips"] <- growth_function(data, i, (open - data[i, "Close"]))[2]
        break
      }
      if((open - data[i, "Close"]) <= 0)
      {
        data[i,"Status"] <- "Trade to close - l"
        data[i, "Balance"] <- data[(i-1), "Balance"] + (((open - data[i, "Close"])/0.0001) * data[i, "Position_size"] * 10)
        data[i, "Growth_rate"] <- growth_function(data, i, (open - data[i, "Close"]))[1]
        data[i, "Pips"] <- growth_function(data, i, (open - data[i, "Close"]))[2]
        break
      }
    }
  }
  
  return(data)
}

###########################################

# funkce na výpočet velikosti ideální pozice

position_size_buy <- function(data, i)
{
  low <- data[i, "L_Prediction"]
  high <- data[i, "H_Prediction"]
  
  dollar_risk <- data[(i-1), "Balance"] * risk
  low_pips <- (data[(i-1), "Close"] - data[i, "L_Prediction"])/0.0001
  
  position_size_1 <- round(dollar_risk/(low_pips * 10), 2)
  
  dollar_gain <- data[(i-1), "Balance"] * risk * RRR
  high_pips <- (data[i,"H_Prediction"] - data[(i-1), "Close"])/0.0001
  
  position_size_2 <- round(dollar_gain/(high_pips * 10), 2)
  
  if(position_size_1 < position_size_2)
  {
    return (c(position_size_1, round(leverage_function(data, i), 2)))
  }
  else{
    return (c(position_size_2, round(leverage_function(data, i), 2)))
  }
  
}

###########################################

position_size_sell <- function(data, i)
{
  low <- data[i, "L_Prediction"]
  high <- data[i, "H_Prediction"]
  
  dollar_risk <- data[(i-1), "Balance"] * risk
  low_pips <- ( data[i, "H_Prediction"] - data[(i-1), "Close"])/0.0001
  
  position_size_1 <- round(dollar_risk/(low_pips * 10), 2)
  
  dollar_gain <- data[(i-1), "Balance"] * risk * RRR
  high_pips <- (data[(i-1), "Close"] - data[i,"L_Prediction"])/0.0001
  
  position_size_2 <- round(dollar_gain/(high_pips * 10), 2)
  
  if(position_size_1 < position_size_2)
  {
    return (c(position_size_1, round(leverage_function(data, i), 2)))
  }
  else{
    return (c(position_size_2, round(leverage_function(data, i), 2)))
  }
  
}

###########################################

# leverage funkce

leverage_function <- function(data ,i)
{
  buy_power <- data[(i-1), "Balance"] * leverage
  max_size <- buy_power/100000
  
  return(max_size)
}

###########################################

stat <- function(data)
{
  return <- (data[nrow(data), "Balance"] - balance)/balance
  max_value <- max(data$Balance)
  min_value <- min(data$Balance)
  max_growth <- max(diff(data$Balance))
  min_growth <- min(diff(data$Balance))
  std <- sd(data$Balance)
  last_value <- data[nrow(data), "Balance"]
  
  buy_percentage <- (nrow(na.trim(data[data$Info == "Buy",c("Info","Open")])))/ ((nrow(na.trim(data[data$Info == "Buy",c("Info","Open")]))) 
                                                                                 + (nrow(na.trim(data[data$Info == "Sell",c("Info","Open")]))) 
                                                                                 + (nrow(na.trim(data[data$Info == "Nothing",c("Info","Open")]))))
  
  sell_percentage <- (nrow(na.trim(data[data$Info == "Sell",c("Info","Open")])))/ ((nrow(na.trim(data[data$Info == "Buy",c("Info","Open")]))) 
                                                                                   + (nrow(na.trim(data[data$Info == "Sell",c("Info","Open")]))) 
                                                                                   + (nrow(na.trim(data[data$Info == "Nothing",c("Info","Open")]))))
  
  nothing_percentage <- (nrow(na.trim(data[data$Info == "Nothing",c("Info","Open")])))/ ((nrow(na.trim(data[data$Info == "Buy",c("Info","Open")]))) 
                                                                                         + (nrow(na.trim(data[data$Info == "Sell",c("Info","Open")]))) 
                                                                                         + (nrow(na.trim(data[data$Info == "Nothing",c("Info","Open")]))))
  
  w_l <- nrow(data[(data$Status == "Win") & (!is.na(data$Status)),]) / (nrow(data[(data$Status == "Win") & (!is.na(data$Status)),]) 
                                                                        + nrow(data[(data$Status == "Loss") & (!is.na(data$Status)),]))
  
  ttc_w <- nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),])/(nrow(data[(data$Status == "Win") & (!is.na(data$Status)),])
                                                                                 + nrow(data[(data$Status == "Loss") & (!is.na(data$Status)),])
                                                                                 + nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),])
                                                                                 + nrow(data[(data$Status == "Trade to close - l") & (!is.na(data$Status)),])
                                                                                 + nrow(data[(data$Status == "Cannot decide") & (!is.na(data$Status)),]))
  
  ttc_l <- nrow(data[(data$Status == "Trade to close - l") & (!is.na(data$Status)),])/(nrow(data[(data$Status == "Win") & (!is.na(data$Status)),])
                                                                                       + nrow(data[(data$Status == "Loss") & (!is.na(data$Status)),])
                                                                                       + nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),])
                                                                                       + nrow(data[(data$Status == "Trade to close - l") & (!is.na(data$Status)),])
                                                                                       + nrow(data[(data$Status == "Cannot decide") & (!is.na(data$Status)),])) 
  
  ttc_both <- (nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),]) + nrow(data[(data$Status == "Trade to close - l") & (!is.na(data$Status)),]))/(nrow(data[(data$Status == "Win") & (!is.na(data$Status)),])
                                                                                       + nrow(data[(data$Status == "Loss") & (!is.na(data$Status)),])
                                                                                       + nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),])
                                                                                       + nrow(data[(data$Status == "Trade to close - l") & (!is.na(data$Status)),])
                                                                                       + nrow(data[(data$Status == "Cannot decide") & (!is.na(data$Status)),])) 
  
  ttc_w_l <- nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),])/(nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),])
                                                                                           + (nrow(data[(data$Status == "Trade to close - l") & (!is.na(data$Status)),])))
  
  cnd <- nrow(data[(data$Status == "Cannot decide") & (!is.na(data$Status)),])/(nrow(data[(data$Status == "Win") & (!is.na(data$Status)),])
                                                                                 + nrow(data[(data$Status == "Loss") & (!is.na(data$Status)),])
                                                                                 + nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),])
                                                                                 + nrow(data[(data$Status == "Trade to close - l") & (!is.na(data$Status)),])
                                                                                 + nrow(data[(data$Status == "Cannot decide") & (!is.na(data$Status)),])) 
  
  w_l_both <- (nrow(data[(data$Status == "Win") & (!is.na(data$Status)),]) + nrow(data[(data$Status == "Loss") & (!is.na(data$Status)),])) /(nrow(data[(data$Status == "Win") & (!is.na(data$Status)),])
                                                                                                                                            + nrow(data[(data$Status == "Loss") & (!is.na(data$Status)),])
                                                                                                                                            + nrow(data[(data$Status == "Trade to close - w") & (!is.na(data$Status)),])
                                                                                                                                            + nrow(data[(data$Status == "Trade to close - l") & (!is.na(data$Status)),])
                                                                                                                                            + nrow(data[(data$Status == "Cannot decide") & (!is.na(data$Status)),])) 
  
  pass_percentage <- nrow(data[(data$Leverage == "Passed") & (!is.na(data$Status)),])/(nrow(data[(data$Leverage == "Passed") & (!is.na(data$Status)),]) + nrow(data[(data$Leverage != "Passed") & (!is.na(data$Status)),]))
  
  r_list <- list("Max value" = max_value, "Min value" = min_value,"Last value" = last_value, "Max growth" = max_growth, "Min growth" = min_growth, 
                 "Standard deviation" = std,  "Buy percentage" = buy_percentage, "Sell percentage" = sell_percentage, "Nothing percentage" = nothing_percentage,
                 "Win/Loss percentage" = w_l, "Trade to close win percentage" = ttc_w, "Trade to close loss percentage" = ttc_l,
                 "Trade to close win&loss percentage" = ttc_both, "trade to close win vs loss" = ttc_w_l, "Cannot decide percentage" = cnd, "Win + loss / others" = w_l_both, "Passed percentage" = pass_percentage)
  
  return(r_list)
}

###########################################

growth_function <- function(data, i, n_pips)
{
  g_rate <- paste(round(((data[i, "Balance"]/data[(i-1), "Balance"]) - 1) * 100, 2), "% (", round(data[i, "Balance"] - data[(i-1), "Balance"], 2), "$)")
  pips <- round(n_pips/0.0001, 2)
  
  return(c(g_rate, pips))
  

}

volatility_function <- function(data, i)
{
  
}

stat(data)
plot(data$Balance, type = "line")
plot(data$Close, type = "line")

fig <- data %>% plot_ly(x = ~index(test_xts), type="candlestick",
                      open = ~Open, close = ~Close,
                      high = ~High, low = ~Low) 
fig

test_xts <- xts(data[,3:6], order.by = as.POSIXct(paste(data[,c(1)], data[,c(2)]), format = "%Y.%m.%d %H:%M:%S"))

median(volatility(data[,(3:6)]), na.rm = T)
median(abs(as.numeric(data$Pips)), na.rm = T)




