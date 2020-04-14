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
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Create the sample datafame that will hold the financial data
  
#company_data <- data.frame("num_customers" = c(7,9), "new_customers"=1:2, "new_claims" = c(0,1),"week"=(1:2))
week=(1:2)
total_customers=(4:5)
new_claims=c(5,9)
company_data=data.frame(week,total_customers,new_claims)



    print(company_data)
  
  output$distPlot <- renderPlot({
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