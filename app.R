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
library(gridExtra)
library(RColorBrewer)




rsconnect::setAccountInfo(name='optiver7', token='B7D2C396DC6FCD9C5055C223F7C9E4DF', secret='1alZ5PsFaC0pckcgXQ0szSZ+w7VboQ6CstLiSlg/')




data1 <- read.csv("lightgbm_baseline_predict.csv")
data2 <- read.csv("all_times.csv")
data3<- read.csv("final-ensemble.csv")







# Define UI for application that draws a histogram
ui <- fluidPage(
  
  theme = bslib::bs_theme(bootswatch = "superhero"),
  navbarPage(
    "Optiver7",
    #first
    navbarMenu("Guide", 
               tabPanel("Introduction",
                        div(style='width:1400px; height:300px',
                            br(),
                            
                            titlePanel("Welcome to Optiver Dashboard!"),
                            br(),
                            h3("Motivation"),
                            HTML("Our motivation stemmed from our financial understanding of volatility and using this to guide the features of a stock <br>which we believed would best represent its volatility." ),
                            br(),
                            h3("Who is for?"),
                            HTML("Our main target audience is those engaged in the financial services industry with our tool being useful for a range of <br> people in different roles.
                                            These roles include trading, asset management, hedge fund managers, equity research, brokerage <br> and financial consultants.
                                            Our app is also applicable for the use of academics and students, and for anyone who is looking into <br> the role of data science in trading."),
                            br(),
                            h3("How to use this app?"),
                            HTML("Since our audience is quite broad, our expectation for the app is to be as simple and convenient as possible, so that users can <br> search for effective information quickly. The format is simple and easier for users to use.")
                            
                            
                            
                        )
                        
               ),    
               tabPanel("Aim",
                        br(),
                        br(),
                        tags$p("We sought to capture volatility across 126 different stocks in different time intervals, predicting volatility through a range of time intervals. 
                                           We visualised our results in the app to show how we can predict volatility of these stocks in a certain period of time.
                                           We optimised our clustering through a tradeoff between accuracy and explainability to create our features.
                                           We represented communication through displaying both the stock features and their volatility predictions in the Shiny App.
                                           This app will show traders how different models and tools have been used to generate our predictions.",style = "font-size: 20px; "),
                        imageOutput("photo1")
                        
                        
               )
    ),
    
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
               column(3, h6("Dom", textOutput("dom")))),
             
             br(),
             p("Beta, DOM(market depth), and spread are financial indicators that provide different information about a stock or financial market:"),
             strong("Beta:"),
             p("Beta is a measure of a stock's risk relative to the market. A beta of 1 indicates that the price of a stock moves with the market. A beta coefficient of less than 1 means a stock is less volatile than the market, while a beta of more than 1 means a stock is more volatile. Beta is very useful in portfolio construction, where one can balance between high beta stocks and low beta stocks to achieve an acceptable level of market risk."),
             strong("DOM(Market depth): "),
             p("Market depth is a measure of security supply and demand. It usually shows the number of open buy and sell orders for a security at different prices. The depth of market data can provide information about the liquidity of securities, and securities with large buy and sell orders at many price levels are considered more liquid. Liquidity is an important characteristic of traders because it describes the extent to which assets can be bought and sold quickly without affecting the price of the asset."),
             strong("Spread: "),
             p("The spread is the difference between the bid price (the highest price a buyer is willing to pay for an asset) and the ask price (the lowest price a seller is willing to accept). A narrower spread indicates a more liquid market, while a wider spread indicates a less liquid market. Spreads affect transaction costs (transaction costs), so they are of great interest to traders and investors.)" )
    ),
    
    
    
    
    #third
    navbarMenu("LightGBM",
               tabPanel("Forecasting",
                        fluidRow(column(4,selectInput("t", "Time ID", unique(data1$time_id))),
                                 column(4,selectInput("s", "Stock ID", unique(data1$stock_id)))),
                        submitButton("Submit",width = "750px"),
                        fluidRow(column(4,h6("Target realized volatility", textOutput("target"))),
                                 column(4,h6("Predicted volatility", textOutput("predict"))),
                                 column(4,h6("RMSE", textOutput("rmse")))),
                        br(),
                        br(),
                        strong("Light GBM (Light Gradient Boosting Machine)"),
                        HTML(" is a common gradient boosting framework, which is used to solve supervised machine learning tasks. The images below show the top 25 feature importance and rank them in descending order of importance to help users analyze which features contribute the most to the prediction model. By analyzing feature importance, feature selection, dimension reduction or feature engineering can be further optimized."),
                        br(),
                        br(),
                        HTML("Evaluating the performance of a Light GBM model usually involves evaluating the model using appropriate evaluation metrics. For classification problems, commonly used indicators include accuracy rate, accuracy rate, recall rate, F1 score, etc. For regression problems, indexes such as mean square error (MSE), root mean square error (RMSE) and mean absolute error (MAE) can be used. The performance of the Light GBM model on a given task can be evaluated by comparing it with other models or cross-verifying it.")
               ),
               
               tabPanel("Model Feature Importance",
                        HTML("This figure illustrates the important features of the model by using lightBGM"),
                        br(),
                        fluidRow(
                          column(7,imageOutput("photo2")),
                          column(5,imageOutput("photo3")))),
               
               tabPanel("Top 25 Important Features",
                        fluidRow(
                          column(7,imageOutput("photo4")),
                          column(5,imageOutput("photo5")))),
               
               tabPanel("Top 40 Important Features",
                        fluidRow(
                          column(7,imageOutput("photo6")),
                          column(5,imageOutput("photo7"))))
    ),    
    
    
    
    
    
    
    
    tabPanel(
      "Forecasting",
      sidebarLayout(
        sidebarPanel(
          selectInput("tt", "Time ID", choices = unique(data3$time_id)),
          selectInput("ss", "Stock ID", choices = unique(data3$stock_id)),
          selectInput("model", "Select model to show", choices = c("LGBM", "POLY", "ENSEMBLES")),
          uiOutput("select"),
          h3("Predict"),
          verbatimTextOutput("value"),
          h3("RMSPE"),
          verbatimTextOutput("rmspe")
          
        ),
        mainPanel(
          tabsetPanel(
            id = "tabset",
            tabPanel("panel 1", plotOutput("p1",width = "500px", height = "600px")),
            tabPanel("panel 2", plotOutput("p2",width = "500px", height = "600px")),
            tabPanel("panel 3", plotOutput("p3",width = "500px", height = "600px"))
            
          )
        )
        
        
      )
    )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

  output$photo1 <- renderImage({
    list(
      src = "1.png",
      filetype = "image/png",
      width = 700,
      height = 300,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  
  
  
  output$photo2 <- renderImage({
    list(
      src = "2.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo3 <- renderImage({
    list(
      src = "3.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  
  
  output$photo4 <- renderImage({
    list(
      src = "4.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo5 <- renderImage({
    list(
      src = "5.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  output$photo6 <- renderImage({
    list(
      src = "6.png",
      filetype = "image/png",
      width = 450,
      height = 600,
      alt = "This is a chainring"
    )
    
  }, deleteFile = FALSE)
  
  
  output$photo7 <- renderImage({
    list(
      src = "7.png",
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
  
  output$target <- renderText({
    fildata <- data1 %>% filter(stock_id == input$s & time_id == input$t)
    b <- fildata$target_realized_volatility
  })
  
  output$predict <- renderText({
    fildata <- data1 %>% filter(stock_id == input$s & time_id == input$t)
    b <- fildata$target_realized_volatility
  })
  
  output$rmse <- renderText({
    fildata <- data1 %>% filter(stock_id == input$s & time_id == input$t)
    target_data <- fildata$target_realized_volatility
    predict_data <- fildata$predicted_volatility
    rm <- sqrt(mean((target_data - predict_data )^2))
  })
  
  
  
  output$select <- renderUI({
    fildata <- data3 %>% filter(stock_id == input$ss, time_id == input$tt)
    
    if (input$model == "LGBM") {
      checkboxGroupInput(
        "a",
        label = h3("Model"),
        list("Normal" = round(fildata$lgbm.norm,4), "Simp" = round(fildata$lgbm.simp,4), "Ext" = round(fildata$lgbm.ext,4)),
        verbatimTextOutput("value")
        
      )
      
    } else if (input$model == "POLY") {
      checkboxGroupInput(
        "a",
        label = h3("Model"),
        list("Regression" = round(fildata$poly.reg, 4)),

        verbatimTextOutput("value")
      )
      
    } else if (input$model == "ENSEMBLES") {
      checkboxGroupInput(
        "a",
        label = h3("Model"),
        list("Average" = round(fildata$ensemble.average,4), "Weighted" = round(fildata$ensemble.weighted,4)),

        verbatimTextOutput("value")
        )
      
    }
  })
  
  
  
  output$value <- renderPrint({ 
    input$a
    })
    
  
    
  
  output$rmspe <- renderPrint({ 
    
    fildata <- data3 %>% filter(stock_id == input$ss, time_id == input$tt)
    rmspe_values <- c()
    
  
    if (round(fildata$poly.reg, 4 ) %in% input$a && input$model == "POLY") {
      # Do something if necessary
    }
    
    if (round(fildata$ensemble.average,4 ) %in% input$a && input$model == "ENSEMBLES") {
      rmspe_values <- c(rmspe_values, round(fildata$rmspe.average,4))
    }
    
    if (round(fildata$ensemble.weighted,4 ) %in% input$a && input$model == "ENSEMBLES") {
      rmspe_values <- c(rmspe_values, round(fildata$rmspe.weighted,4))
    }
    
    if (round(fildata$lgbm.norm,4 ) %in% input$a && input$model == "LGBM") {
      rmspe_values <- c(rmspe_values, round(fildata$rmspe.norm,4))
    }
    
    if (round(fildata$lgbm.simp, 4) %in% input$a && input$model == "LGBM") {
      rmspe_values <- c(rmspe_values, round(fildata$rmspe.simp,4))
    }
    
    if (round(fildata$lgbm.ext, 4) %in% input$a && input$model == "LGBM") {
      rmspe_values <- c(rmspe_values, round(fildata$rmspe.ext,4))
    }
    
    rmspe_values
  })
  
  
  
  output$p1 <- renderPlot({
    f <- data3 %>% filter(stock_id == input$ss)
    s <- f[, c("rmspe.weighted","rmspe.average","rmspe.poly","rmspe.ext","rmspe.simp","rmspe.norm")]
    boxplot( s, main = "RMSPE distribution of different model predicted values",ylab = "volatility",fill = "PuRd" ,las =2)
    
    
    
  })
  
  
  
  output$p2 <- renderPlot({
    
    f <- data3 %>% filter(stock_id == input$ss)
    
  
    actual <- f$realized_volatility
    predictedn <- f$lgbm.norm
    predicteds <- f$lgbm.simp
    predictede <- f$lgbm.ext
    
    
    plot1 = ggplot(f,aes(x = 1:length(actual))) +
      geom_point(aes(y = actual), color = "red") +
      geom_point(aes(y = predictedn), color = "blue") +
      labs(title = "LGBM-norm Actual vs Predicted", x = "Predicted Value", y = "Actual Value")
    
    
    plot2 = ggplot(f,aes(x = 1:length(actual))) +
      geom_point(aes(y = actual), color = "red") +
      geom_point(aes(y = predicteds), color = "blue") +
      labs(title = "LGBM-norm Actual vs Predicted", x = "Predicted Value", y = "Actual Value")
    
    plot3 = ggplot(f,aes(x = 1:length(actual))) +
      geom_point(aes(y = actual), color = "red") +
      geom_point(aes(y = predictede), color = "blue") +
      labs(title = "LGBM-ext Actual vs Predicted", x = "Predicted Value", y = "Actual Value")
   
    grid.arrange(plot1,plot2,plot3)
 
  })
  
  
  output$p3 <- renderPlot({
    
    f <- data3 %>% filter(stock_id == input$ss)
    
    actual <- f$realized_volatility
    predicteda <- f$lgbm.average
    predictedw <- f$lgbm.weighted
    predictedr <- f$lgbm.reg
    
    plot1 = ggplot(f,aes(x = 1:length(actual))) +
      geom_point(aes(y = actual), color = "red") +
      geom_point(aes(y = predicteda), color = "blue") +
      labs(title = "ensemble.avarge Actual vs Predicted", x = "Predicted Value", y = "Actual Value")
    
    plot2 = ggplot(f,aes(x = 1:length(actual))) +
      geom_point(aes(y = actual), color = "red") +
      geom_point(aes(y = predictedw), color = "blue") +
      labs(title = "ensemble.weighted Actual vs Predicted", x = "Predicted Value", y = "Actual Value")
    
    
    plot3 = ggplot(f,aes(x = 1:length(actual))) +
      geom_point(aes(y = actual), color = "red") +
      geom_point(aes(y = predictedr), color = "blue") +
      labs(title = "poly.reg Actual vs Predicted", x = "Predicted Value", y = "Actual Value")
    
    
    
    
    
    grid.arrange(plot1,plot2,plot3)
  })
  
 
  
}



             
             
  


# Run the app ----
shinyApp(ui = ui, server = server)


