#Shiny Visual application Code
library(shiny)
#https://rdivanji.shinyapps.io/VoicesReport/

source("Data/VoicesDataAnalysis.R")
# Define UI for application that draws a histogram
ui <- fluidPage(
  navbarPage("Voices From The Field 2015 & 2016 Findings",
             #Attendance
             tabPanel("Attendance",
                      titlePanel("Attendance Data"),
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "year",
                                             label = "Year:",
                                             choices = c("Voices 2015", "Voices 2016"),
                                             selected = "Voices 2016")

                        ),
                        mainPanel(
                          plotOutput("meansPlot"),
                          tableOutput("ResultsTable")
                        )
                      )
             ),
             
             #Math
             tabPanel("Math",
                      sidebarLayout(
                        sidebarPanel(

                        ),
                        mainPanel(

                        )
                      )
             ),
             
             #Reading
             tabPanel("Reading",
                      sidebarLayout(
                        sidebarPanel(
                          checkboxGroupInput(inputId = "type",
                                             label = "Type:",
                                             choices = c("bike", "rack"),
                                             selected = "bike")
                        ),
                        mainPanel(

                        )
                      )
             )
             
             
  )

)

# Define server logic required to draw a barplot
server <- function(input, output) {
   #Barplot code
   output$meansPlot <- renderPlot({
      #drawing Voices 2015 barplot
      if(input$year == "Voices 2015"){
        plot <- barplot(c(avg.absent_14.15, avg.absent_15.16), 
                width = 0.3, 
                col = c("slategray1", "gray"), 
                main = "Attendance Pre and Post Voices 2015 Intervention",
                ylim = c(0, 10),
                ylab = "Average Days Absent in School Year",
                names.arg = c("Mean Absence 14-15 School Year", "Mean Absence 15-16 School Year")) 
        legend("topleft", legend = c("Control (Pre-Voices)", "Treatment (Post-Voices)"), fill = c("slategray1", "gray"))
        text(x = plot, y = c(avg.absent_14.15, avg.absent_15.16), label = c(avg.absent_14.15, avg.absent_15.16), pos = 1, cex = 1, col = "red")
      
      }else if(input$year == "Voices 2016"){
       plot <- barplot(c(avg2016.absent_15.16, avg2016.absent_16.17), 
               width = 0.3, 
               col = c("slategray1", "gray"), 
               main = "Attendance Pre and Post Voices 2016 Intervention",
               ylim = c(0, 10),
               ylab = "Average Days Absent in School Year",
               names.arg = c("Mean Absence 15-16 School Year", "Mean Absence 16-17 School Year"))
       legend("topleft", legend = c("Control (Pre-Voices)", "Treatment (Post-Voices)"), fill = c("slategray1", "gray"))
       text(x = plot, y = c(avg2016.absent_15.16, avg2016.absent_16.17), label = c(avg2016.absent_15.16, avg2016.absent_16.17), pos = 1, cex = 1, col = "red")
     }
   })
   
   #Statistics data table code
   output$ResultsTable <- renderTable({
     #2015 attendance table
     if(input$year == "Voices 2015"){
       return(results2015.df)
     #2016 attendance table
     }else if(input$year == "Voices 2016"){
       return(results2016.df)
     }
      
   })

}

# Run the application 
shinyApp(ui = ui, server = server)

