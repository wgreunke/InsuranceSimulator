#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Insurance Simulator"),

  
  fluidRow(sliderInput("bins",
                       "Number of bins:",
                       min = 1,
                       max = 50,
                       value = 30),  
           #Start off with higher price to get some customers
           numericInput("monthly_price", "Monthly Policy Price:", 1000),
           actionButton("add_week", "Advance the simulation one week"),
  ),
  
#dump the data table for testing

fluidRow(column(12, tableOutput("table"))), #end fluid row
  
   fluidRow(
      column(6,      plotOutput("policy_plot")

      ),
      column(6,             plotOutput("claims_plot")      )
    
  ),# end fluidrow
  
 #Second row financial data and performance metrics
 fluidRow(
   column(6,      plotOutput("financial_plot")
          
   ),
   column(6,             plotOutput("metrics_plot")      )
   
 )# end fluidrow
 
)#end fluid page
#**************************Server **********************************************


# Define server logic required to draw a histogram
server <- function(input, output) {
  #Create the sample datafame that will hold the financial data

  output$table <- renderTable(iris)  
#company_data <- data.frame("num_customers" = c(7,9), "new_customers"=1:2, "new_claims" = c(0,1),"week"=(1:2))
current_week=1
  week=(1:2)
total_customers=(4:5)
new_claims=c(5,9)
company_data=data.frame(week,total_customers,new_claims)



    print(company_data)
  
#Policy plot is total number policy holders and policy price history.

  output$policy_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggplot(company_data, aes(x=week,y=total_customers))+geom_line()
  })
  
  #Claims output is number of claims in week and average value of those claims.
  
  output$claims_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggplot(company_data, aes(x=week,y=total_customers))+geom_line()
  })
  
  #This shows the financial data
  #Main rows - DWP, Claims Payout, and Surplus?
  
  output$financial_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggplot(company_data, aes(x=week,y=total_customers))+geom_line()
  })

  #Major metric is Loss Ratio
  
  output$metrics_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    ggplot(company_data, aes(x=week,y=total_customers))+geom_line()
  })
  
    
}


# Run the application 
shinyApp(ui = ui, server = server)