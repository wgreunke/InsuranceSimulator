
library(shiny)
library(ggplot2)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Insurance Simulator"),

  
  fluidRow(
           #Start off with higher price to get some customers
           numericInput("annual_premium_input", "Enter the annual premium - Start with 100:", 1000),
           actionButton("use_new_price", "Update Price")
           #actionButton("add_week", "Advance the simulation one week")
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
server <- function(input, output, session) {
#  https://stackoverflow.com/questions/68405253/r-shiny-variable-increases-by-1-each-time-actionbutton-is-clicked

#Variables - Policy
target_policy_price=100.0001 #This is what the customer is willing to pay (annual), it has to be discovered
target_policy_std=0
#displayed_price = 6 #What price is advertised to customer.  Will be controlled by user.
#num_customers_shopping=100 #How many customers are shopping for a policy?
#num_customer_shopping_std=100
trials_per_week=1000

#Variables - Claims
claim_rate=.02
claim_rate_std=0
avg_claim_amount=target_policy_price/claim_rate
claim_amount_std=1


#create the reactive value object
rv=reactiveValues( financial_df=data.frame("week"=0, "premium_earned" = 0, "loss_paid"=0, "underwriting_gain" =0,"policyholder_surplus"=0 ),
                  policy_df=data.frame("week"=0,"policy_price"=0,"new_customers_added"=0,"total_customers"=0),
                  claims_df=data.frame("week"=0,"num_claims"=0,"avg_claim"=0,"total_claim_amount"=0),
                  current_week=0L,
                  new_annual_premium=1000 #Need to fix this so it pulls the origional value from the annual premium
                  )#end reactive values

#Do the work with the reactive value when the button is pushed
#Advance the cycle one week

#Updating the price when button is pushed
observeEvent(input$use_new_price,{
  rv$new_annual_premium=as.numeric(input$annual_premium_input)
})

observe ({
#observe(input$add_week, {
  invalidateLater(500)  #1000 miliseconds equals 1 second
  isolate({
  rv$current_week=rv$current_week+1
  #rv$policy_df=rv$policy_df+1

  #shop for policy, only buy if less than target.
  #Add a loop to get mulitple customers in a week.
  
  
  #Premiums are collected from customers active in the prior week
  temp_premium_earned=sum(rv$policy_df$policy_price*rv$policy_df$new_customers_added/52)
  
  temp_customers_to_add=0 #This will be used when there is a loop to shop multiple customers each week.
  for (i in 1:trials_per_week)
  {
    temp_random_policy_target=abs(rnorm(1,target_policy_price,target_policy_std))
    temp_total_customers=sum(rv$policy_df$new_customers_added)
    
    if (rv$new_annual_premium<temp_random_policy_target) #Create a normal distribution about the target price.
      {
      #Buy the policy, add a row
      temp_customers_to_add=temp_customers_to_add+1 # Increment the counter for each signup
      }#End if
  } #end for
  print(temp_random_policy_target)
  
    #For a given week you will add the number of customers that signed up and each will have the same policy price.
  new_customer_row=c(rv$current_week,rv$new_annual_premium,temp_customers_to_add,(temp_total_customers+temp_customers_to_add))
  #May be problems if there are no customers for the week.  If so may have to add blank row for the week.
  rv$policy_df=rbind(rv$policy_df,new_customer_row)
  
  
  
  #Claims
  #For each week, multiply the claimrate /52 times # of customers
  #Record a random average claim amount.
  #Use the max number of customers colum for total customer count
  #temp_num_claims=as.integer(abs(rv$policy_df[rv$current_week,"total_customers"]*(rnorm(1,claim_rate,claim_rate_std)/52)))  #Not sure if this is firing enough
  temp_num_claims=round(abs(max(rv$policy_df[,"total_customers"])*(rnorm(1,claim_rate,claim_rate_std)))/52)  #Not sure if this is firing enough
  
  temp_avg_claim_amount=rnorm(1,avg_claim_amount,claim_amount_std)

  #Only policyholders who were active last week are eligble to submit a claim
  temp_new_claim_row=c(rv$current_week-1,temp_num_claims,temp_avg_claim_amount,temp_num_claims*temp_avg_claim_amount)
  temp_loss_paid=temp_num_claims*temp_avg_claim_amount
  rv$claims_df=rbind(rv$claims_df,temp_new_claim_row)
  #rv$company_df=rv$company_df+1
  
  #Financials
  #premiums_earned-loss=underwriting_gain
  #for reference - financial_df=data.frame("week"=0, "premium_earned" = 0, "loss_paid"=0, "underwriting_gain" =0,"policyholder_surplus"=0 )
  
  last_polciy_holder_surplus=rv$financial_df[rv$current_week,"policyholder_surplus"] #Get the last surplus and add it to current surplus - add 1 to the row.
  temp_financial_row=c(rv$current_week-1,temp_premium_earned,temp_loss_paid,temp_premium_earned-temp_loss_paid,temp_premium_earned-temp_loss_paid+last_polciy_holder_surplus)
  rv$financial_df=rbind(rv$financial_df,temp_financial_row)
  
  browser()
  
}) #End isolate
}) # End observeEvent


#rv$company_data<- data.frame("num_customers" = c(7,9), "new_customers"=1:2, "new_claims" = c(0,1),"week"=(1:2))
#observeEvent(input$add_week,{react_values$week=react_values$week+1})
  
#output$table=renderTable(rv$company_df)
#output$table=renderTable(rv$policy_df)
#output$table=renderTable(rv$claims_df)
#output$table=renderTable(rv$financial_df)

#Policy plot is total number policy holders and policy price history
  output$policy_plot <- renderPlot({
  
    ggplot(rv$policy_df, aes(x=week,y=total_customers))+geom_line()
  })
  
  #Claims output is number of claims in week and average value of those claims.
  output$claims_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    #ggplot(rv$company_df, aes(x=week,y=num_customers))+geom_line()
    ggplot(rv$claims_df,aes(x=week,y=num_claims)) +geom_line()
    #ggplot(rv$claims_df, aes(x=week,y=num_claims))+geom_line()
      })
  
  #This shows the financial data
  #Main rows - DWP, Claims Payout, and Surplus?
  
  output$financial_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #ggplot(company_data, aes(x=week,y=total_customers))+geom_line()
    ggplot(rv$financial_df, aes(x=week,y=policyholder_surplus))+geom_line()
    
    })

  #Major metric is Loss Ratio
  
  output$metrics_plot <- renderPlot({
    # generate bins based on input$bins from ui.R
    
    # draw the histogram with the specified number of bins
    #hist(x, breaks = bins, col = 'darkgray', border = 'white')
    #ggplot(company_data, aes(x=week,y=total_customers))+geom_line()
  })
  
} #End Server


# Run the application 
shinyApp(ui = ui, server = server)

#Reference
#https://www.investopedia.com/terms/p/policyholder-surplus.asp
