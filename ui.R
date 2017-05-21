library(shiny)
library(DT)
source("helpers.R")

shinyUI(
navbarPage("STOCK PREDICTION",
 tabPanel("BẢNG GIÁ",
    titlePanel("CHỨNG KHOÁN THẾ GIỚI"),
    sidebarLayout(
      sidebarPanel(
        helpText("Việc của bạn là chọn một mã chứng khoán,
                 Tôi sẽ trả kết quả cho bạn"),
        
        selectInput("symb", "Symbol", choices = Allsymbols[,1], selected = "AAPL", multiple = FALSE),
        
        dateRangeInput("dates", 
                       "Date range",
                       start = "2013-01-01", 
                       end = as.character(Sys.Date())),
        
        br(),
        br(),
        
        checkboxInput("adjust", 
                      "Điều chỉnh lạm phát", value = FALSE),
        
        numericInput(inputId = "portion",
                     label = "Tỷ lệ chia mẫu",
                     value = 0.9,
                     min = 0.5, max = 0.95),
        
        numericInput(inputId = "p", value = 1,
                     min = 1, max = 4, 
                     label = "Bậc tự tương quan q"),
        
        numericInput(inputId = "q", value = 1,
                     min = 1, max = 4,
                     label = "Bậc tự tương quan riêng phần p")
        ),
      mainPanel(
        plotOutput("plotCandle"),
        br(),
        br(),
        strong("Bảng giá tuần gần nhất"),
        DT::dataTableOutput("table")
        #tableOutput("table")
      )
    )
  ),
 
 tabPanel("DỰ BÁO GIÁ",
          DT::dataTableOutput("forecast_price")
          #tableOutput("forecast")    
 ),
 
 tabPanel("DỰ BÁO LỢI SUẤT",
    DT::dataTableOutput("forecast")
    #tableOutput("forecast")    
 ),
 
 tabPanel("TỰ TƯƠNG QUAN",
    strong("Đồ thị tự tương quan q"),
    plotOutput("p"),
    br(),
    strong("Đồ thị tự tương quan riêng phần p"),
    plotOutput("q")
 ),
navbarMenu("MORE!",
 tabPanel("THÔNG TIN CÔNG TY",
          DT::dataTableOutput("company"))            
 )
)
)
