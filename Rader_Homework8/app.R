############################
# Homework 8
#Due 11/1/2019
#Creating a Shiny Web Application with mtcars dataset
#Must have a range slider
#Allows user to select x and y variables
#Doesn't create plot until GO button is toggled
############################


#Loading in packages
library(shiny)
library(tidyverse)

#Assigning min and max variables from cyl
min.cyl <- min(mtcars$cyl)
max.cyl <- max(mtcars$cyl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mtcars Data Set Viewer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("cyl.adjuster",
                        "Number of Cylinders",
                        min.cyl,
                        max.cyl,
                        value = c(min.cyl, max.cyl)),
            actionButton ("goButton",
                          "Go")
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
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
