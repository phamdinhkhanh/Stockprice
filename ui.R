library(shiny)

shinyUI(fluidPage(
  titlePanel("CHỨNG KHOÁN THẾ GIỚI"),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Việc của bạn là chọn một mã chứng khoán,
               Tôi sẽ trả kết quả cho bạn"),
    
      textInput("symb", "Symbol", "AAPL"),
    
      dateRangeInput("dates", 
        "Date range",
        start = "2013-01-01", 
        end = as.character(Sys.Date())),
      
      br(),
      br(),
      
      checkboxInput("adjust", 
        "Điều chỉnh lạm phát", value = FALSE)
    ),
    
    mainPanel(plotOutput("plotCandle"))
  )
))