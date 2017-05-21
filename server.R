# server.R

library(quantmod)
library(plotly)
library(dplyr)
library(forecast)
library(scales)
library(DT)
source("helpers.R")

shinyServer(function(input, output) {

  
dataInput <- reactive({
  getSymbols(input$symb, src = "google", 
  from = input$dates[1],
  to = input$dates[2],
  auto.assign = FALSE)
})
  
finalInput <- reactive({
  if(!input$adjust) return(dataInput())
  adjust(dataInput())
})

output$p <- renderPlot({
  data <- finalInput() 
  data <- data[,4]
  colnames(data) <- "Stock_price"
  stock <- diff(log(data$Stock_price),1) 
  stock <- stock[!is.na(stock)]
  breakpoint <- floor(input$portion*nrow(stock))
  train <- stock[1:breakpoint,1]
  test <- stock[-nrow(train),1]
  acf.train = acf(train,main = 'ACF Plot', lag.max = 100)
})

output$q <- renderPlot({
  data <- finalInput() 
  data <- data[,4]
  colnames(data) <- "Stock_price"
  stock <- diff(log(data$Stock_price),1) 
  stock <- stock[!is.na(stock)]
  breakpoint <- floor(input$portion*nrow(stock))
  train <- stock[1:breakpoint,1]
  test <- stock[-nrow(train),1]
  pacf.train = pacf(train,main = "PACF plot", lag.max = 100)
})

price <- function(return, pre_price){
  round(exp(return)*pre_price,2)
}

output$company <- DT::renderDataTable({
  Allsymbols
})

output$forecast_price <- DT::renderDataTable({
  data <- finalInput() 
  data <- data[,4]
  colnames(data) <- "Stock_price"
  stock_price <- xts(data,index(data)) 
  stock_price <- stock_price[-1]
  stock <- xts(diff(log(data$Stock_price),1),index(data)) 
  stock <- stock[!is.na(stock)]
  breakpoint <- floor(input$portion*nrow(stock))
  train <- stock[1:breakpoint,1]
  test <- stock[-nrow(train),1]
  #Khởi tạo Actual_series
  Actual_series = xts(0,as.Date("2007-01-01","%Y-%m-%d"))
  #Khởi tạo dataframe của chuỗi forecast
  forecasted_series = data.frame(Forecasted = numeric(),Upper_Forecasted = numeric(),Lower_Forecasted = numeric())
  for(b in breakpoint:nrow(stock)-1){
    stock_train = stock[1:b,]
    stock_test = stock[-row(stock_train),]
    #Summary ARIMA model
    fit = arima(stock_train,order = c(input$p,0,input$q),include.mean = FALSE)
    #summary(fit$model)
    #Plotting residual plot
    #acf(fit$residuals,main="Residuals plot")
    #Forecast log returns
    #library(forecast)
    arima.forecast = forecast.Arima(fit,h=1,level = 95)
    #summary(arima.forecast$mean)
    #print(arima.forecast$mean)
    #Thêm giá trị dự báo vào chuỗi forecasted_series
    #date <- as.Date(index(stock[b+1,],"%Y-%m-%d"))
    Actual_Price <- data.frame(stock_price[b+1,])
    
    newforecast <- c(Forecasted = price(arima.forecast$mean[1],Actual_Price$Stock_price),
                     price(arima.forecast$upper,Actual_Price$Stock_price),
                     price(arima.forecast$lower,Actual_Price$Stock_price))
    
    forecasted_series <- rbind(forecasted_series,newforecast)
    xts.Actual_price <- stock_price[b+1,]
    Actual_series <- c(Actual_series,xts(xts.Actual_price))
    rm(Actual_Price)
    rm(xts.Actual_price)
  }
  
  colnames(forecasted_series)=c("Forecasted","Upper_Forecasted","Lower_Forecasted")
  #forecasted_series <- forecasted_series[order(-forecasted_series[,1]),]
  Actual_series = Actual_series[-1]
  forecasted_series = xts(forecasted_series,index(Actual_series))
  comparision = merge(Actual_series,forecasted_series) 
  comparision <- data.frame(Date = as.character.Date(index(comparision),"%Y-%m-%d"),comparision) %>% 
    arrange(Date %>% desc()) 
  
  comparision
  
})

output$forecast <- DT::renderDataTable({
  data <- finalInput() 
  data <- data[,4]
  colnames(data) <- "Stock_price"
  stock <- diff(log(data$Stock_price),1) 
  stock <- stock[!is.na(stock)]
  breakpoint <- floor(input$portion*nrow(stock))
  train <- stock[1:breakpoint,1]
  test <- stock[-nrow(train),1]
  #Khởi tạo Actual_series
  Actual_series = xts(0,as.Date("2007-01-01","%Y-%m-%d"))
  #Khởi tạo dataframe của chuỗi forecast
  forecasted_series = data.frame(Forecasted = numeric(),Upper_Forecasted = numeric(),Lower_Forecasted = numeric())
  for(b in breakpoint:nrow(stock)-1){
    stock_train = stock[1:b,]
    stock_test = stock[-row(stock_train),]
    #Summary ARIMA model
    fit = arima(stock_train,order = c(input$p,0,input$q),include.mean = FALSE)
    #summary(fit$model)
    #Plotting residual plot
    #acf(fit$residuals,main="Residuals plot")
    #Forecast log returns
    #library(forecast)
    arima.forecast = forecast.Arima(fit,h=1,level = 95)
    #summary(arima.forecast$mean)
    #print(arima.forecast$mean)
    #Thêm giá trị dự báo vào chuỗi forecasted_series
    date <- as.Date(index(stock[b+1,],"%Y-%m-%d"))
  
    newforecast <- c(Forecasted = round(arima.forecast$mean[1],4),
                     round(arima.forecast$upper,4),
                     round(arima.forecast$lower,4))
    forecasted_series <- rbind(forecasted_series,newforecast)
    Actual_return = round(stock[(b+1),],4)
    Actual_series <- c(Actual_series,xts(Actual_return))
    rm(Actual_return)
  }
  
  colnames(forecasted_series)=c("Forecasted","Upper_Forecasted","Lower_Forecasted")
  #forecasted_series <- forecasted_series[order(-forecasted_series[,1]),]
  Actual_series = Actual_series[-1]
  forecasted_series = xts(forecasted_series,index(Actual_series))
  comparision = merge(Actual_series,forecasted_series) 
  comparision <- data.frame(Date = as.character.Date(index(comparision),"%Y-%m-%d"),comparision) %>% 
                 arrange(Date %>% desc()) %>% 
                 mutate(Comparision = ifelse(Actual_series*Forecasted >= 0,1,0)) %>% 
                 mutate(Percent_Exactly_predict = percent(sum(Comparision)/nrow(comparision)))
  
  comparision
  
})


output$table <- DT::renderDataTable({
  data <- data.frame(Date = as.character.Date(index(finalInput()),"%Y-%m-%d"),finalInput()) %>% 
         arrange(Date %>% desc())
  
  colnames(data) = c("Date","Open","High","Low","Close","Volume")

  data
})

output$plotCandle  <- renderPlot({
  Stock_Price <- finalInput()
  candleChart(Stock_Price, theme = chartTheme("white"), 
              multi.col = TRUE, TA = NULL, title = "Stock price")
  addBBands()
  addMACD()
})


})