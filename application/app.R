#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)



library(tidyverse) # Data management and manipulation
library(ggplot2) # Visuals


#for k means
library(ggthemes)
library(ggpubr)
library(grid)




rsconnect::setAccountInfo(name='optiver7', token='6564E2328ECEE76928F604503D08BDE9', secret='a/xf+Z7jQ2GXeiOKs1J5PQQBK2ckHYIW99Eb3hYL')


data1 <- read.csv("lightgbm_baseline_predict.csv")
data2 <- read.csv("all_times.csv")







# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "superhero"),
  
  #add main button
  navbarPage("Optiver7",
             
             
             
             #first
             tabPanel("Introduction",
                      navlistPanel(tabPanel("Aim",
                                           p("We sought to capture volatility across 126 different stocks in different time intervals, predicting volatility through a range of time intervals. 
                                           We visualised our results in the app to show how we can predict volatility of these stocks in a certain period of time.
                                           We optimised our clustering through a tradeoff between accuracy and explainability to create our features.
                                           We represented communication through displaying both the stock features and their volatility predictions in the Shiny App.
                                           This app will show traders how different models and tools have been used to generate our predictions.

")),
                                   tabPanel("Target Audience",
                                            h3("Who is for?"),
                                            HTML("Our main target audience is those engaged in the financial services industry with our tool being useful for a range of people in different roles.
                                            These roles include trading, asset management, hedge fund managers, equity research, brokerage and financial consultants.
                                            Our app is also applicable for the use of academics and students, and for anyone who is looking into the role of data science in trading.
"),
                                            h3("How to use this app?"),
                                            HTML("Since our audience is quite broad, our expectation for the app is to be as simple and convenient as possible, so that users can search for effective information quickly. The format is simple and easier for users to use.
                                                 "),
                                            h3("Motivation"),
                                            HTML("Our motivation stemmed from our financial understanding of volatility and using this to guide the features of a stock which we believed would best represent its volatility. 
                                                 "),
                                            ),
                                   
                                   )),
             
             
             
             
             
             #second
             tabPanel("Data processing",
                      fluidRow(
                        column(4, selectInput("time_id", "TIME ID",  unique(data2$time_id))),
                        column(4, selectInput("stock_id", "STOCK ID", unique(data2$stock_id)))
                        
                        ),
                      submitButton("Submit",width = "750px"),
                      
                      fluidRow(
                        column(3, h6("Beta", textOutput("beta"))),
                        column(3, h6("Spread", textOutput("spread"))),
                        column(3, h6("DOM", textOutput("dom")))),
                      
                      br(),
                      p("Beta, DOM (market depth), and spread are financial indicators that provide different information about a stock or financial market:"),
                      strong("Beta:"),
                      p("Beta is a measure of a stock's risk relative to the market. A beta of 1 indicates that the price of a stock moves with the market. A beta coefficient of less than 1 means a stock is less volatile than the market, while a beta of more than 1 means a stock is more volatile. Beta is very useful in portfolio construction, where one can balance between high beta stocks and low beta stocks to achieve an acceptable level of market risk."),
                      strong("DOM (Market depth): "),
                      p("Market depth is a measure of security supply and demand. It usually shows the number of open buy and sell orders for a security at different prices. The depth of market data can provide information about the liquidity of securities, and securities with large buy and sell orders at many price levels are considered more liquid. Liquidity is an important characteristic of traders because it describes the extent to which assets can be bought and sold quickly without affecting the price of the asset."),
                      strong("Spread: "),
                      p("The spread is the difference between the bid price (the highest price a buyer is willing to pay for an asset) and the ask price (the lowest price a seller is willing to accept). A narrower spread indicates a more liquid market, while a wider spread indicates a less liquid market. Spreads affect transaction costs (transaction costs), so they are of great interest to traders and investors." )
                      ),
                      
                      
             #third
             navbarMenu("LightGBM",
                      tabPanel("Model Feature Importance",
                               sidebarLayout(
                                 sidebarPanel(HTML("Model Feature Importance"),width = 3),
                                 mainPanel(fluidRow(
                                   column(7,imageOutput("photo")),
                                   column(5,imageOutput("photo1")),
                                   width = 9
                                 ))
                               )),
                      tabPanel("Top 25 Important Features",
                               sidebarLayout(
                                 sidebarPanel(HTML("Top 25 Important Features"),width = 3
                                              ),
                                 mainPanel(
                                   fluidRow(
                                     column(7,imageOutput("photo2")),
                                     column(5,imageOutput("photo3")),
                                     width = 9
                                   )
                                 )
                               )
                      ),
                      tabPanel("Top 40 Important Features",
                               sidebarLayout(
                                 sidebarPanel(HTML("Top 40 Important Features"),width = 3
                                 ),
                                 mainPanel(
                                   fluidRow(
                                     column(7,imageOutput("photo4")),
                                     column(5,imageOutput("photo5")),
                                     width = 9
                                   )
                                 )
                               )
                      )
                      ),
             
             
             #fourth
             tabPanel("Model",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("stock", "Stock ID", choices = unique(data1$stock_id)),
                          #numericRangeInput("range","Range",min = min(unique(data1$time_id)), max = max(unique(data1$time_id)),
                                      #value = c(min(unique(data1$time_id)),max(unique(data1$time_id)))),
                          numericInput("start","Start",value = 5, min = min(unique(data1$time_id)), max = max(unique(data1$time_id)),step = 3),
                          numericInput("end","End",value = 725, min = min(unique(data1$time_id)),max = max(unique(data1$time_id)),step = 3),
                          submitButton("ok", "Show")
                         
                          
                          
                        ),
                        mainPanel(
                          plotOutput("plot",height = "600px")
                          
                        )
                      )
               
             )
                      
      
             

              
             
             
             

             
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  output$photo <- renderImage({
    list(
      src = "1.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  
  
  output$photo1 <- renderImage({
    list(
      src = "2.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo2 <- renderImage({
    list(
      src = "3.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo3 <- renderImage({
    list(
      src = "4.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo4 <- renderImage({
    list(
      src = "5.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo5 <- renderImage({
    list(
      src = "6.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)

  
   
  output$beta <- renderText({
    fildata <- data2 %>% filter(stock_id == input$stock_id & time_id == input$time_id)
    b <- fildata$beta
  })
  
  output$spread <- renderText({
    fildata <- data2 %>% filter(stock_id == input$stock_id & time_id == input$time_id)
    b <- fildata$spread
  })
 
  output$dom <- renderText({
    fildata <- data2 %>% filter(stock_id == input$stock_id & time_id == input$time_id)
    b <- fildata$dom
  })
  
  
  
  
  output$plot <- renderPlot({
    start_time <- input$start
    end_time <- input$end
    #selet_x <- input$range
    m <- data1 %>% filter(stock_id == input$stock) 
    a <- m[m$time_id >= start_time & m$time_id <= end_time,]
    
    
    #a <- subset(m, start_time > end_time) 
    #plot(a$time_id, a$predicted_volatility)
    #subset_time <- subset(m, time_id %in% selet_x)
    p1 = ggplot(a,aes(time_id,predicted_volatility))+geom_line(color= "#FF0000") + xlab("Time") + ylab("Predicted volatility") + ggtitle("Predicted volatility")
    p2 = ggplot(a,aes(time_id,target_realized_volatility))+geom_line(color = "#0000FF") + xlab("Time") + ylab("Target realized volatility") + ggtitle("Target realized volatility")
    grid.arrange(p1, p2)
  })
  
  
  
  
  
}



             
             
  


# Run the app ----
shinyApp(ui = ui, server = server)


