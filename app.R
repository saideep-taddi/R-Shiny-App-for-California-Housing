library(shiny)
library(tidyverse)
library(caret)
library(randomForest)
library(ggplot2)
library(mice)
library(leaflet)
library(shinydashboard)

# Load the dataset
house_data <- read.csv("./california_housing.csv")


ui <- dashboardPage(
  dashboardHeader(title = "House Price Prediction"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Data Exploration", tabName = "dataExploration", icon = icon("chart-line")),
      menuItem("House Price Map", tabName = "priceMap", icon = icon("globe")),
      menuItem("Model Training", tabName = "modelTraining", icon = icon("cogs")),
      menuItem("Model Insights", tabName = "modelInsights", icon = icon("info-circle")),
      menuItem("Predictions", tabName = "predictions", icon = icon("calculator"))
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "modelTraining",
              fluidRow(
                box(title = "Select Model", status = "primary", solidHeader = TRUE,
                    selectInput("modelType", "Model Type:", choices = c("Random Forest", "Linear Regression")),
                    numericInput("trainSize", "Training Set Size (%):", 70, min = 50, max = 90),
                    actionButton("trainModel", "Train Model", class = "btn-success")
                )
              )),
      tabItem(tabName = "dataExploration",
              fluidRow(
                box(title = "Choose Variable", status = "warning", solidHeader = TRUE,
                    selectInput("edaSelect", "Select a variable for EDA:", choices = names(house_data)),
                    plotOutput("edaPlot")
                )
              )),
      tabItem(tabName = "priceMap",
              fluidRow(
                box(title = "House Price Map", status = "info", solidHeader = TRUE,
                    leafletOutput("housePriceMap", height = 450)
                )
              )),
      tabItem(tabName = "modelInsights",
              fluidRow(
                box(title = "Model Summary", status = "primary", solidHeader = TRUE, verbatimTextOutput("modelSummary")),
                box(title = "Model Performance", status = "primary", solidHeader = TRUE, tableOutput("modelPerformance")),
                box(title = "Model Plots", status = "primary", solidHeader = TRUE, plotOutput("modelPerformancePlot"))
              )),
      tabItem(tabName = "predictions",
              fluidRow(
                box(title = "Make a Prediction", status = "success", solidHeader = TRUE,
                    numericInput("housing_median_age", "Housing Median Age:", value = 41),
                    numericInput("total_rooms", "Total Rooms:", value = 880),
                    numericInput("total_bedrooms", "Total Bedrooms:", value = 129),
                    numericInput("population", "Population:", value = 322),
                    numericInput("households", "Households:", value = 126),
                    numericInput("median_income", "Median Income:", value = 8.3252),
                    selectInput("ocean_proximity", "Ocean Proximity:", choices = unique(house_data$ocean_proximity)),
                    actionButton("predictButton", "Predict House Value", class = "btn-success"),
                    verbatimTextOutput("predictionOutput")
                )
              ))
    )
  )
)



server <- function(input, output) {
  # EDA Plot
  output$edaPlot <- renderPlot({
    req(input$edaSelect)
    data <- house_data[[input$edaSelect]]
    
    if (is.numeric(data)) {
      ggplot(house_data, aes_string(x = input$edaSelect)) +
        geom_histogram(bins = 30, fill = 'blue') +
        theme_minimal()
    } else {
      ggplot(house_data, aes_string(x = input$edaSelect)) +
        geom_bar(fill = 'blue') +
        theme_minimal()
    }
  })

  # Leaflet Map for House Price Visualization
  output$housePriceMap <- renderLeaflet({
    req(input$edaSelect)
    
    colorPalette <- colorNumeric(palette = "viridis", domain = house_data$median_house_value)
    
    leaflet(house_data) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = ~longitude, lat = ~latitude,
        color = ~colorPalette(median_house_value),
        radius = 5, opacity = 1, fillOpacity = 0.8,
        popup = ~paste("Median House Value:", median_house_value)
      )
  })
  
  
  # Reactive value for trained model
  trainedModel <- reactiveVal()
  
  # Observe event for model training
  observeEvent(input$trainModel, {
    req(input$modelType)
    set.seed(123)  # For reproducibility
    
    house_data_proc <- house_data %>%
      mutate(across(where(is.factor), as.character)) %>%
      mutate(across(where(is.character), as.factor)) %>%
      select(-longitude, -latitude)
      
    trainIndex <- createDataPartition(house_data_proc$median_house_value, p = input$trainSize / 100, list = FALSE)[,1]
    trainData <- house_data_proc[trainIndex, ]
    testData <- house_data_proc[-trainIndex, ]
    
    trainData <- na.omit(trainData)
    testData <- na.omit(testData)
    
    # Perform imputation here
    tempData <- rbind(trainData, testData)
    tempData <- mice(tempData, m=1, method='pmm', maxit = 5)$data
    
    trainData <- tempData[1:nrow(trainData), ]
    testData <- tempData[(nrow(trainData) + 1):nrow(tempData), ]
    
    
    
    
    if (input$modelType == "Random Forest") {
      model <- randomForest(median_house_value ~ ., data = trainData, ntree = 100)
    } else if (input$modelType == "Linear Regression") {
      model <- lm(median_house_value ~ ., data = trainData)
    }
    
    trainedModel(list(model = model, testData = testData))
  })
  
  # Model summary and performance
  output$modelSummary <- renderPrint({
    modelInfo <- trainedModel()
    req(modelInfo)
    summary(modelInfo$model)
  })
  
  output$modelPerformance <- renderTable({
    modelInfo <- trainedModel()
    req(modelInfo)
    pred <- predict(modelInfo$model, modelInfo$testData)
    data.frame(
      RMSE = RMSE(pred, modelInfo$testData$median_house_value),
      Rsquared = R2(pred, modelInfo$testData$median_house_value)
    )
  })
  
  # Predictions
  prediction <- eventReactive(input$predictButton, {
    modelInfo <- trainedModel()
    req(modelInfo)
    
    new_data <- data.frame(
      housing_median_age = input$housing_median_age,
      total_rooms = input$total_rooms,
      total_bedrooms = input$total_bedrooms,
      population = input$population,
      households = input$households,
      median_income = input$median_income,
      ocean_proximity = input$ocean_proximity
    )
    
    predict(modelInfo$model, new_data)
  })
  
  output$predictionOutput <- renderPrint({
    prediction_result <- prediction()
    if (length(prediction_result) > 0) {
      paste("Predicted House Value:", round(prediction_result, 2))
    } else {
      "Prediction not available"
    }
  })
  
  output$modelPerformancePlot <- renderPlot({
    modelInfo <- trainedModel()
    req(modelInfo)
    
    if (input$modelType == "Linear Regression") {
      # Diagnostic plots for linear regression
      par(mfrow = c(2, 2))
      plot(modelInfo$model)
    } else if (input$modelType == "Random Forest") {
      # Variable importance plot for random forest
      varImpPlot(modelInfo$model)
    }
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
