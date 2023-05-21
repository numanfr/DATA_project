#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(markdown)
library(quantmod)
library(DT)
library(shinythemes)
library(forecast)
library(ggplot2)


# Define UI
ui <- shinyUI(fluidPage(
  
  navbarPage(

    tabPanel("Model", 
             p("Model"),
             
    )
  ),
  
  fileInput('target_upload', 'Choose file to upload',
            accept = c(
              'text/csv',
              'text/comma-separated-values',
              '.csv'
            )),
  radioButtons("separator","Separator: ",choices = c(";",",",":"), selected=",",inline=TRUE),
  DT::dataTableOutput("sample_table"),
  
  theme = shinytheme("cerulean"),
  titlePanel("Stock analysis applications"),
  sidebarLayout(
    sidebarPanel(
      #uiOutput("codePanel"),
      br(),
      sliderInput("n", "Forecast future days：", min = 1, max = 30, value = 5)
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Historical data", plotOutput("plot")),
        tabPanel("Predict the outcome", plotOutput("forecast"))
      )
    )
  )
  
  
)
)






# Define server logic
server <- shinyServer(function(input, output) {
  
  df_products_upload <- reactive({
    inFile <- input$target_upload
    if (is.null(inFile))
      return(NULL)
    df <- read.csv(inFile$datapath, header = TRUE,sep = input$separator)
    return(df)
    
    filt <- selectInput("codeInput",label ="Choose stock ID",
                                            choices = as.list(df_products_upload$X))
  })
  
  #filt <- selectInput("codeInput",label ="Choose stock ID",
  #                    choices = as.list(df_products_upload$X))
  
  df_subset <- reactive({
    a = subset(df_products_upload, category = input$state)
    return(a)
  })
  
  output$sample_table<- DT::renderDataTable({
    df <- df_products_upload()
    DT::datatable(df)
  })
  
  
  stock_data <- reactive({
    getSymbols(input$symbol, auto.assign = FALSE)
  })
  
  
  output$plot <- renderPlot({
    if (!is.null(df_products_upload())) {
    
      ggplot(df_products_upload(),aes(x=X,y=DOM))+geom_point()
    }
  })
  
  
  output$forecast <- renderPlot({
    
    if (!is.null(df_products_upload())) {
      
      ggplot(df_products_upload(),aes(x=X,y=spreads))+geom_point()
    }
  })
  
}
)



             
             
             
   











# Run the app ----
shinyApp(ui = ui, server = server)


