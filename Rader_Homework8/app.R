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


#Assigning min and max variables for slider
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
           plotOutput("mtcars_plot")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {

    #Filtering mtcars and making it reactive
    d_filt <- reactive({
        mtcars %>%
            filter(cyl >= min(input$cyc.adjuster)) %>%
            filter(cyc <= max(input$cyc.adjuster))
    })
    
    
    #Building a plot
    plot_mtcars <- eventReactive(input$goButton, {
        ggplot(d_filt(), aes_string(x = "mpg", y = "hp", color = "gear")) +
            geom_point()
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
