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


#Creating a vector of axis variables
axis_variables <- names(mtcars)


#Creating a character vector for the columns
factor.indices <- vapply(mtcars, is.factor, TRUE)
factor.columns <- axis_variables[factor.indices]


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Mtcars Data Set Viewer"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            #Adding in a range slider
            sliderInput("cylrange",
                        "Number of Cylinders",
                        min = min.cyl,
                        max = max.cyl,
                        value = c(min.cyl, max.cyl)),
            
            #Selecting x variable
            selectInput(inputId = "xvar",
                        label = "X Axis",
                        choices = axis_variables,
                        selected = "x"),
            
            #Selecting y variable
            selectInput(inputId = "yvar",
                        label = "Y Axis",
                        choices = axis_variables,
                        selected = "y"),
            
            #Adding in a go button
            actionButton ("goButton",
                          "Go")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("mtcars_plot")
        )
    )
)


# Define server logic required to draw a plot
server <- function(input, output) {

    #Filtering mtcars and making it reactive
    d_filt <- reactive({
        mtcars %>%
            filter(cyl >= min(input$cylrange)) %>%
            filter(cyl <= max(input$cylrange))
    })
    
    
    #Building a plot
    plot_mtcars <- eventReactive(input$goButton, {
        ggplot(d_filt(), aes_string(x = input$xvar, y = input$yvar, colour = "gear")) +
            geom_point()
    })
    
    #Creating a dynamic plot that plots the output
    output$mtcars_plot <- renderPlot(
        plot_mtcars()
    )
}

# Run the application 
shinyApp(ui = ui, server = server)
