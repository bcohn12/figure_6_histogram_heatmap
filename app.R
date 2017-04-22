#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
read.csv("")
library(shiny)
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Old Faithful Geyser Data"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         numericInput("task_intensity",
                     "Task Intensity, where 100 is 10%",
                     min = 1,
                     max = 1000,
                     value = 10),
         numericInput("muscle",
                     "Task Intensity, where 100 is 10%",
                     min = 1,
                     max = 1000,
                     value = 10)
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("distPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
      # generate bins based on input$bins from ui.R
      #x    <- faithful[, 2] 
      #bins <- seq(min(x), max(x), length.out = input$bins + 1)
     
     hist(x[[]])
      
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

