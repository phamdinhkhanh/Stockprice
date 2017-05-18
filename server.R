# server.R

library(quantmod)
library(plotly)
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

output$plotCandle  <- renderPlot({
  Stock_Price <- finalInput()
  candleChart(Stock_Price, theme = chartTheme("white"), 
              multi.col = TRUE, TA = NULL, title = "Stock price")
  addBBands()
  addMACD()
})
})