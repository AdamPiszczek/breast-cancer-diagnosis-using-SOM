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
            hr(),
            HTML("<h2>Patient's parameters:</h2>"),
            numericInput("BI.RADS", 
                         h3("BI.RADS"),
                         min = 1,
                         max = 5,
                         value = 1),
            numericInput("Age", 
                         h3("Age"),
                         min = 18,
                         max = 120,
                         value = 18),
            numericInput("Margin", 
                         h3("Margin"),
                         min = 1,
                         max = 5,
                         value = 1),
            numericInput("Shape", 
                         h3("Shape"),
                         min = 1,
                         max = 5,
                         value = 1),
            numericInput("Density", 
                         h3("Density"),
                         min = 1,
                         max = 5,
                         value = 1),
            actionButton("predict","Predict")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("changes"),
           plotOutput("count"),
           plotOutput("mapping"),
           plotOutput("quality"),
           plotOutput("neighbours"),
           plotOutput("codes"),
           plotOutput("clusters"),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$changes <- renderPlot({
      plot(som_model, type="changes")
    })
    output$count <- renderPlot({
      plot(som_model, type="count")
    })
    output$mapping <- renderPlot({
      plot(som_model, type="mapping")
    })
    output$quality <- renderPlot({
      plot(som_model, type="quality")
    })
    output$neighbours <- renderPlot({
      plot(som_model, type="dist.neighbours")
    })
    output$codes <- renderPlot({
      plot(som_model, type="codes")
    })
    output$clusters <- renderPlot({
      coolBlueHotRed <- function(n, alpha = 1) {rainbow(n, end=4/6, alpha=alpha)[n:1]}
      pretty_palette <- c("#1f77b4","#ff7f0e","#2ca02c", "#d62728","#9467bd","#8c564b","#e377c2")
      
      c <- som_model$codes 
      som_cluster <- cutree(hclust(dist(som_model$codes[[1]])), 2)
      plot(som_model, type="mapping", bgcol = pretty_palette[som_cluster], main = "Clusters") 
      add.cluster.boundaries(som_model, som_cluster)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
