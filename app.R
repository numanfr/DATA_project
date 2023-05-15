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

# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme
  theme = bs_theme(version = 4, bootswatch = "minty"),
  
  #add main button
  navbarPage("Optive7",
             
             
             #first
             tabPanel("Introduction",
                      navlistPanel(tabPanel("Aim",
                                           p("The purpose of this project is to build a model that predicts future volatility.

")),
                                   tabPanel("For the crowed"))),
             
             
             
             
             
             #second
             tabPanel("Data processing",
                      navlistPanel(tabPanel("Data cleaning"),
                                   tabPanel("Calculate"))),
             
             
             
             
             
             #third
             #add main button and sub-button
             tabPanel("Model",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("radio",
                                         label = "Model selection",
                                         choices = list("Model1" = 1,
                                                     "Model2" = 2,
                                                     "Model3" = 3),
                                         selected = 1),
                          dateRangeInput("dates",
                                    label = "Date range")
                          
                        ),
                        
                        
                      
                        mainPanel()
                        )
                      )
             
             
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dataInput <- reactive({
    getSymbols(input$symb, src = "yahoo",
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({   
    data <- dataInput()
    if (input$adjust) data <- adjust(dataInput())
    
    chartSeries(data, theme = chartTheme("white"),
                type = "line", log.scale = input$log, TA = NULL)
  })
}



             
             
             
   











# Run the app ----
shinyApp(ui = ui, server = server)


