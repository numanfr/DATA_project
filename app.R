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
library(dplyr)
library(correlation)
library(ggplot2)




data <- read.csv("merged.csv")
data2 <- read.csv("all_times.csv")

# Generate sample data for demonstration
set.seed(123)
actual_vol <- rnorm(100, mean = 0, sd = 1)
pred_vol <- actual_vol + rnorm(100, mean = 0, sd = 0.5)

# Calculate Pearson correlation coefficient between actual and predicted volatilities
correlation <- cor(actual_vol, pred_vol, method = "pearson")

# Combine actual and predicted volatilities into a data frame
vol_comparison <- data.frame(actual_vol = actual_vol, pred_vol = pred_vol)

# Create a scatter plot of actual vs predicted volatilities
scatter_plot <- ggplot(vol_comparison, aes(x = actual_vol, y = pred_vol)) +
  geom_point(alpha = 0.5) +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed") +
  theme_minimal() +
  labs(x = "Actual Volatility", y = "Predicted Volatility", 
       title = "Scatter plot of Actual vs Predicted Volatility") +
  annotate("text", x = 1, y = 1.5, label = paste("Correlation =", round(correlation, 2)))

# Create a line plot of actual vs predicted volatilities
line_plot <- ggplot(vol_comparison, aes(x = 1:length(actual_vol))) +
  geom_line(aes(y = actual_vol, color = "Actual Volatility")) +
  geom_line(aes(y = pred_vol, color = "Predicted Volatility")) +
  theme_minimal() +
  labs(x = "Time", y = "Volatility", 
       title = "Line plot of Actual vs Predicted Volatility") +
  scale_color_manual(values = c("Actual Volatility" = "blue", "Predicted Volatility" = "red"))

# Display the plots
scatter_plot
line_plot







# Define UI for application that draws a histogram
ui <- fluidPage(
  #theme
  theme = bs_theme(version = 4, bootswatch = "solar"),
  
  #add main button
  navbarPage("Optive7",
             
             
             #first
             tabPanel("Introduction",
                      navlistPanel(tabPanel("Aim",
                                           p("We want to develop an app for optiver that can accurately predict the volatility of financial products. Users can use this app to predict the volatility of more than 100 stocks in a certain period of time.

")),
                                   tabPanel("For the crowed",
                                            h3("Who is for?"),
                                            HTML("Our main target audience is those engaged in the financial industry and those engaged in property management consulting. But anyone interested in investing in stocks can use this app and get information from it.
"),
                                            h3("How to use this app?"),
                                            HTML("Since our audience is quite broad, our expectation for the app is to be as simple and convenient as possible, so that users can search for effective information quickly. The format is simple and easier for users to use.")
                                            ))),
             
             
             
             
             
             #second
             tabPanel("Data processing",
                      fluidRow(
                        column(4, selectInput("time_id", "time id",  unique(data2$time_id))),
                        column(4, selectInput("stock_id", "stock id", unique(data2$stock_id)))
                        ),
                      fluidRow(
                        column(3, h6("Beta", textOutput("beta"))),
                        column(3, h6("Spred", textOutput("spread"))),
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
                      ),
             
              
             
             
             
             tabPanel("Study",
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("stock", "Stock ID", unique(data$stock_id)),
                          sliderInput("time", "Time ID", 
                                      value = min(unique(data$time_id)),
                                      min = min(unique(data$time_id)),
                                      max = max(unique(data$time_id)))
                        
                        ),
                        mainPanel(plotOutput("model"))
                      )
                      
             )
             
  )
)





# Define server logic required to draw a histogram
server <- function(input, output) {
  output$model <- renderPlot({
    line_plot
    })
  

  
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
  
  
}



             
             
             
   











# Run the app ----
shinyApp(ui = ui, server = server)


