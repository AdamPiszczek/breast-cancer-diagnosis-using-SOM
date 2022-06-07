library(shiny)
library(dplyr)
library(ggplot2)
library(kohonen)

load(file="./data/processed_dataset.RData") # loading previously processed data set


# Define UI for application that displays the necessary buttons and results
ui <- fluidPage(

    # Application title
    titlePanel("Presentation of breast cancer diagnosis in mammography using the self-organizing SOM network based on the Mammographic Mass_MLR dataset"),

    # Sidebar with a slider input
    sidebarLayout(
        sidebarPanel(
          column(5,
            HTML("<h2>Patient's parameters:</h2>"),
            sliderInput("BI.RADS", 
                         h3("BI.RADS"),
                         min = 1,
                         max = 5,
                         value = 1),
            sliderInput("Age", 
                         h3("Age"),
                         min = 18,
                         max = 96,
                         value = 18),
            sliderInput("Margin", 
                         h3("Margin"),
                         min = 1,
                         max = 5,
                         value = 1),
            sliderInput("Shape", 
                         h3("Shape"),
                         min = 1,
                         max = 4,
                         value = 1),
            sliderInput("Density", 
                         h3("Density"),
                         min = 1,
                         max = 4,
                         value = 1),
            hr(),
            actionButton("predict","Predict")
          ),
          column(5,
            HTML("<h2>SOM parameters:</h2>"),
            sliderInput("somrows", 
                         h3("Number of rows (neurons)"),
                         min = 1,
                         max = floor(sqrt(dim(trainingData)[1])),
                         value = 5),
            sliderInput("somcols", 
                         h3("Number of columns (neurons)"),
                         min = 1,
                         max = floor(sqrt(dim(trainingData)[1])),
                         value = 5),
            radioButtons("topology", h3("Mesh type"),
                         choices = list("rectangular" = "rectangular", "hexagonal" = "hexagonal"),selected = "rectangular"),
            sliderInput("numofiterations", 
                         h3("Number of iterations"),
                         min = 1,
                         max = 20000,
                         value = 1000),
            hr(),
            actionButton("learn","Learn & Predict")
        )),
        
        # Show a plot of the generated distribution
        mainPanel(
           htmlOutput("text"),
           htmlOutput("text2"),
           htmlOutput("text3"),
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

# Define server logic required to teach neural network
server <- function(input, output) {
    SOMlist <- reactiveVal() # creating global variable
  
    observeEvent(input$learn, {
      # create data that will be marked separately
      trainingdata <- list(severity = as.matrix(trainingData[,6]),measurements = as.matrix(trainingData[,1:5]))
      
      # set the appropriate SOM parameters
      set.seed(303803)
      gridNumOfRow <- round(input$somrows)
      gridNumOfCol <- round(input$somcols)
      numberOfIterations <- input$numofiterations
      learningRate <- c(0.05,0.001)
      topology <- c("rectangular", "hexagonal")
      data_train_matrix <- as.matrix(trainingData)
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
      testingdata <- list(measurements = as.matrix(testingData[,1:5]))
      som.prediction <- predict(som_model, newdata = testingdata)
      truthTable <- table(testingData[,6],som.prediction$predictions[["severity"]])
      output$text2 <- renderText({paste0("<center><font size=10><b>Sensivity: ", signif(truthTable[2,2] / (truthTable[2,2] + truthTable[1,2])*100,digits=4),"%</b></font></center>")})
      output$text3 <- renderText({paste0("<center><font size=10><b>Specificity: ", signif(truthTable[1,1] / (truthTable[1,1] + truthTable[2,1])*100,digits=4),"%</b></font></center>")})
      
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
      SOMlist(list(som_model,truthTable)) # saving results of learning to global variable
    })
  observeEvent(input$predict, {
    
    # getting learned data from a global variable
    som_model <- SOMlist()[[1]]
    truthTable <- SOMlist()[[2]]
    
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
    output$text2 <- renderText({paste0("<center><font size=10><b>Sensivity: ", signif(truthTable[2,2] / (truthTable[2,2] + truthTable[1,2])*100,digits=4),"%</b></font></center>")})
    output$text3 <- renderText({paste0("<center><font size=10><b>Specificity: ", signif(truthTable[1,1] / (truthTable[1,1] + truthTable[2,1])*100,digits=4),"%</b></font></center>")})
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
