library(shiny)
library(dplyr)
library(ggplot2)
library(kohonen)

load(file="./data/processed_dataset.RData") # loading previously processed dataset
trainingdata <- list(measurements = as.matrix(traingingData[,1:5]),
                     severity = as.matrix(traingingData[,6]))

# set the appropriate SOM parameters
set.seed(303803)
gridNumOfRow <- 4
gridNumOfCol <- 4
numberOfIterations <- 1000
learningRate <- c(0.05,0.001)
topology <- c("rectangular", "hexagonal")
data_train_matrix <- as.matrix(traingingData)
som_grid <- somgrid(xdim = gridNumOfRow, ydim=gridNumOfCol, topo="rectangular", neighbourhood.fct = "gaussian")
som_model <- supersom(trainingdata, 
                      grid=som_grid, 
                      rlen=numberOfIterations, 
                      alpha=learningRate, 
                      keep.data = TRUE)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Presentation of breast cancer diagnosis in mammography using the self-organizing SOM network based on the Mammographic Mass_MLR dataset"),

    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
          column(5,
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
          ),
          column(5,
            HTML("<h2>SOM parameters:</h2>"),
            numericInput("somrows", 
                         h3("Number of rows (neurons)"),
                         min = 1,
                         max = 100,
                         value = 5),
            numericInput("somcols", 
                         h3("Number of columns (neurons)"),
                         min = 1,
                         max = 100,
                         value = 5),
            radioButtons("topology", h3("Mesh type"),
                         choices = list("rectangular" = "rectangular", "hexagonal" = "hexagonal"),selected = "rectangular"),
            numericInput("numofiterations", 
                         h3("Number of iterations"),
                         min = 1,
                         max = 20000,
                         value = 1000)
        )),
        
        # Show a plot of the generated distribution
        mainPanel(
           actionButton("predict","Predict"),
           htmlOutput("text"),
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
  
    observeEvent(input$predict, {
      # create data that will be marked separately
      trainingdata <- list(measurements = as.matrix(traingingData[,1:5]),
                           severity = as.matrix(traingingData[,6]))
      
      # set the appropriate SOM parameters
      set.seed(303803)
      gridNumOfRow <- round(input$somrows)
      gridNumOfCol <- round(input$somcols)
      numberOfIterations <- input$numofiterations
      learningRate <- c(0.05,0.001)
      topology <- c("rectangular", "hexagonal")
      data_train_matrix <- as.matrix(traingingData)
      som_grid <- somgrid(xdim = gridNumOfRow, ydim=gridNumOfCol, topo=input$topology, neighbourhood.fct = "gaussian")
      som_model <- supersom(trainingdata, 
                            grid=som_grid, 
                            rlen=numberOfIterations, 
                            alpha=learningRate, 
                            keep.data = TRUE)
      userdata <- c(input$BI.RADS,input$Age,input$Margin,input$Shape,input$Density)
      userdatamatrix <- matrix(userdata,nrow=1,ncol=5,byrow=TRUE)
      # data must be normalized before prediction
      for (i in 1:5){
        userdatamatrix[1,i] <- (userdatamatrix[1,i]-min(dataset[,i]))/(max(dataset[,i])-min(dataset[,i]))
      }
      customUserdata <- list(measurements = userdatamatrix)
      som.predictionUser <- predict(som_model, newdata = customUserdata)
      
      if (som.predictionUser$predictions[["severity"]][1] == 1){
        output$text <- renderText({paste("<center><font color=\"#FF0000\", font size=10><b>The detected lesion is malignant</b></font></center>")})
        
      } else{
        output$text <- renderText({paste("<center><font color=\"#2986CC\", font size=10><b>The detected lesion is benign</b></font></center>")})
      }
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
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
