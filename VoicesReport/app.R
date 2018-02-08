#
library(shiny)
source("../VoicesDataAnalysis.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Voices Report"),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
        checkboxGroupInput(inputId = "year",
                           label = "Year:",
                           choices = c("Voices 2015", "Voices 2016"),
                           selected = "Voices 2015")
      ),
      
      # Show a plot of the generated distribution
      mainPanel(
         plotOutput("meansPlot")
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$meansPlot <- renderPlot({
      if(input$year == "Voices 2015"){
        plot <- barplot(c(avg.absent_14.15, avg.absent_15.16), 
                width = 0.3, 
                ylim = c(0, 10),
                ylab = "Average Days Absent",
                col = c("slategray1", "gray"), 
                names.arg = c("Mean Absence 14-15 (Pre)", "Mean Absence 15-16 (Post)")) 
        legend("topright", legend = c("Control (Pre-Voices)", "Treatment (Post-Voices)"), fill = c("slategray1", "gray"))
        text(x = plot, y = c(avg.absent_14.15, avg.absent_15.16), label = c(avg.absent_14.15, avg.absent_15.16), pos = 1, cex = 1, col = "red")
      
      }else if(input$year == "Voices 2016"){
       plot <- barplot(c(avg2016.absent_15.16, avg2016.absent_16.17), 
               width = 0.3, 
               col = c("slategray1", "gray"), 
               ylim = c(0, 10),
               ylab = "Average Days Absent",
               names.arg = c("Mean Absence 15-16", "Mean Absence 16-17"))
       legend("topright", legend = c("Control (Pre-Voices)", "Treatment (Post-Voices)"), fill = c("slategray1", "gray"))
       text(x = plot, y = c(avg2016.absent_15.16, avg2016.absent_16.17), label = c(avg2016.absent_15.16, avg2016.absent_16.17), pos = 1, cex = 1, col = "red")
     }
   })
}

# Run the application 
shinyApp(ui = ui, server = server)

