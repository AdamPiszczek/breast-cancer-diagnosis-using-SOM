library(shiny)
library(dplyr)
library(ggplot2)
library(kohonen)

load(file="./data/processed_dataset.RData") # loading previously processed dataset

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Presentation of breast cancer diagnosis in mammography using the self-organizing SOM network based on the Mammographic Mass_MLR dataset"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30),
            numericInput("BI.RADS", 
                         h3("BI.RADS"), 
                         value = 1),
            numericInput("Age", 
                         h3("Age"), 
                         value = 1),
            numericInput("Margin", 
                         h3("Margin"), 
                         value = 1),
            numericInput("Shape", 
                         h3("Shape"), 
                         value = 1),
            numericInput("Density", 
                         h3("Density"), 
                         value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           #plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        # x    <- faithful[, 2]
        # bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        # hist(x, breaks = bins, col = 'darkgray', border = 'white')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
