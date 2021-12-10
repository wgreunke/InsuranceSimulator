
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
#  https://stackoverflow.com/questions/68405253/r-shiny-variable-increases-by-1-each-time-actionbutton-is-clicked
#create the reactive value object.
rv=reactiveValues( company_df=data.frame("num_customers" = c(7,9), "new_customers"=1:2, "new_claims" = c(0,1),"week"=(1:2),
                        policy_df=data.frame("week"=0,"num_customers"=0),
                        claims_df=data.frame("week"=0,"num_claims"=0,"avg_claim"=0))
                        )#end reactive values
#Do the work with the reactive value when the button is pushed

#Advance the cycle one week
observeEvent(input$add_week, {
  rv$company_df=rv$company_df+1
  rv$policy_df=rv$policy_df+1
  rv$claims_df=rv$claims_df+1
}) # End observeEvent


#rv$company_data<- data.frame("num_customers" = c(7,9), "new_customers"=1:2, "new_claims" = c(0,1),"week"=(1:2))
#observeEvent(input$add_week,{react_values$week=react_values$week+1})
  


policy_data=data.frame("week"=0,"customer_id" =0, "annual_premium"=0 )
#output$table <- renderTable(iris)  
#output$table=renderTable(policy_data)
    print(company_data)
  

output$table=renderTable(rv$policy_df)

    
#Policy plot is total number policy holders and policy price history.
  output$policy_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #ggplot(company_data, aes(x=week,y=total_customers))+geom_line()
    ggplot(rv$policy_df, aes(x=week,y=num_customers))+geom_line()
  })
  
  #Claims output is number of claims in week and average value of those claims.
  output$claims_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2]
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #plot(rv$company_df$week,rv$company_df$num_customers)
    
    #ggplot(company_data, aes(x=week,y=total_customers))+geom_line()
    ggplot(rv$company_df, aes(x=week,y=num_customers))+geom_line()
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
  
} #End Server


# Run the application 
shinyApp(ui = ui, server = server)