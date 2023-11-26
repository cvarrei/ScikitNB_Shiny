# remove.packages("ScikitNB")
# library(devtools)
# install_github("cvarrei/ScikitNB")
library(ScikitNB)

library(shiny)
library(shinythemes)
library(DT)
library(readxl)
library(tidyverse)
library(caret)

# ####### Librairie Bernoulli ########
library(tm)
library(Matrix)
library(parallel)
library(pROC)
library(corrplot)
library(wordcloud)
library(ggplot2)

######## Librairie Categorial #########
library(discretization)

data(text)
data(mixed)

model_layout <- fluidRow(
  column(6,
         uiOutput("selectedTargetFeatureDisplay"),
         uiOutput("uniqueValuesOfTargetFeature"),
         radioButtons("isText", "Is it text?", choices = c("Yes", "No")),
         uiOutput("modelSelectionDropdown"),
         uiOutput("parameters_list"),
         actionButton("createModel", "Instantiate the Model !"),
         sliderInput("testProportion", "Test Dataset Proportion (Max: 0.5):",
                     min = 0.1, max = 0.5, value = 0.3),
         actionButton("split", "Split the dataset!"),
         actionButton("preprocess", "Start the Preprocessing")
  ),
  column(6,
         textOutput("instantiateModelStatus"), # Message du status
         textOutput("splitStatus"),
         textOutput("preprocessStatus"),
         plotOutput("nbCompplot"),
         plotOutput("screeplot")
  ) 
)

training_layout <- fluidRow(
  column(6,h1("Performance on the Training Dataset"),
         actionButton("fit", "Fit the model"),
         verbatimTextOutput("modelSummary")
  ),
  column(6,
         plotOutput("train_confusionMatrixPlot")
  )
)

prediction_layout <- fluidRow(
    column(6, h1("Score on the Test Dataset"),
           actionButton("prediction", "Start the Prediction"),
           verbatimTextOutput("modelScores")
    ),
    column(6,
           plotOutput("test_confusionMatrixPlot"),
           plotOutput("rocCurvePlot")
    )
)


# Define UI for the app
ui <- navbarPage("ScikitNB",
                 theme = shinytheme("spacelab"),
                 tabPanel("Package Presentation",
                          fluidRow(
                            column(12,
                                   # Title
                                   h1("Welcome to the ScikitNB App"),
                                   # Description
                                   p("This application is based on the package 'ScikitNB', a scikit-learn inspired "),
                                   h3("Description:"),
                                   p("Implements Gaussian Naive Bayes Classifier, Bernoulli Naive Bayes Classifier, and Categorical Naive Bayes Classifier in R, providing a comprehensive solution for classification problems. This package is capable of handling numerical, categorical, mixed (numerical and categorical), and text data types, 
                                      making it versatile for a wide range of applications. The implementation 
                                      utilizes the R6 class system."),
                                   h3("Author:"),
                                   p("Bourhani Dounya [Master 2 SISE - Lyon 2]"), 
                                   p("Steiner Fiona [Master 2 SISE - Lyon 2]"),
                                   p("Varangot-Reille Clovis [Master 2 SISE - Lyon 2]"),
                                   h3("Maintainer:"),
                                   p("The package maintainer are"),
                                   p("Bourhani Dounya <dounya.bourhani@univ-lyon2.fr>"), 
                                   p("Steiner Fiona <fiona.steiner@univ-lyon2.fr>"),
                                   p("Varangot-Reille Clovis <c.varangot@univ-lyon2.fr>")
                            )
                          )
                 ),
                 tabPanel("Import Data",
                          fluidRow(
                            column(12,
                                   # Instructions
                                   h3("Data Import Instructions"),
                                   p("Select a CSV or Excel (.xlsx) file to import data. The file should have a specific format. RShiny limit size is 5Mo. "),
                                   p("If you want to modelize a bigger file, use the package directly within RStudio."),
                                   # File input
                                   fileInput("file1", "Choose CSV or Excel File", accept = c(".csv", ".xlsx"))
                            ),
                            column(12,
                                   DT::dataTableOutput("dataTable")
                            ),
                            column(6,
                                   uiOutput("uiTargetFeature")
                            ),
                            column(6,
                                   uiOutput("uiCategoricalColumns")
                            )
                          )
                 ),
                 tabPanel("Parameter Model",
                          model_layout
                 ),
                 tabPanel("Training Performance",
                          training_layout
                 ),
                 tabPanel("Test Scores",
                          prediction_layout
                 )
                 
)


# Define server logic
server <- function(input, output, session) {
  
  # Initialize a reactive value to store the modified dataframe
  modifiedDataFrame <- reactiveVal(NULL)
  
  # Reactive expression to hold the data frame
  dataFrame <- reactive({
    inFile <- input$file1
    if (is.null(inFile)) {
      print("No file uploaded")
      return(NULL)
    }
    
    fileExtension <- tools::file_ext(inFile$datapath)
    if (fileExtension == "csv") {
      read.csv(inFile$datapath)
    } else if (fileExtension == "xlsx") {
      readxl::read_excel(inFile$datapath)
    }
  })
  
  # Render DataTable
  output$dataTable <- renderDT({
    df <- dataFrame()
    if (is.null(df)) return(NULL)
    datatable(df)
  })
  
  # Dynamically create the target feature dropdown
  output$uiTargetFeature <- renderUI({
    df <- dataFrame()
    if (is.null(df)) return(NULL)
    selectInput("targetFeature", "Select Target Feature", choices = colnames(df))
  })
  
  # Dynamically create the categorical columns dropdown
  output$uiCategoricalColumns <- renderUI({
    df <- dataFrame()
    if (is.null(df)) return(NULL)
    selectInput("categoricalColumns", "Select Categorical Columns of integers", choices = colnames(df), multiple = TRUE)
  })
  
  # Reactive expression for target feature column
  targetFeatureColumn <- reactive({
    df <- modifiedDataFrame()
    if (is.null(df)) return(NULL)
    targetFeature <- input$targetFeature
    if (!is.null(targetFeature) && targetFeature %in% colnames(df)) {
      return(df[[targetFeature]])
    } else {
      return(NULL)
    }
  })
  
  observe({
    df <- dataFrame()
    if (is.null(df)) {
      print("Failed to read file into dataframe")
    } else {
      print(paste("Data frame columns:", colnames(df)))
      df <- df %>%
        mutate_if(is.character, as.factor)
      modifiedDataFrame(df) # Initially set the modifiedDataFrame as the original dataframe
    }
  })
  
  observe({
    df <- modifiedDataFrame()
    if (is.null(df)) {
      print("Modified data frame is NULL before factor conversion")
      return()
    }
    
    catCols <- input$categoricalColumns
    if (!is.null(catCols) && !is.null(df)) {
      
      df[catCols] <- lapply(df[catCols], factor)
      modifiedDataFrame(df) # Update the reactive value
      print(paste("Updated data frame with factors:", catCols))
    }
  })
  
  # Display the selected target feature on the third page
  output$selectedTargetFeatureDisplay <- renderUI({
    targetFeature <- input$targetFeature
    if (!is.null(targetFeature)) {
      h3(paste("Selected Target Feature:", targetFeature))
    }
  })
  
  output$uniqueValuesOfTargetFeature <- renderUI({
    df <- modifiedDataFrame()
    if (is.null(df)) return(NULL)
    targetFeature <- input$targetFeature
    if (!is.null(targetFeature) && targetFeature %in% colnames(df)) {
      uniqueValues <- unique(df[[targetFeature]])
      h4("Unique Values of the Target Feature:")
      verbatimTextOutput("uniqueValuesOutput")
    }
  })
  
  output$uniqueValuesOfTargetFeature <- renderUI({
    df <- modifiedDataFrame()
    targetFeature <- input$targetFeature
    if (is.null(df) || is.null(targetFeature)) return(NULL)
    
    # Ensure targetFeature is a valid column name
    if (targetFeature %in% colnames(df)) {
      uniqueValues <- unique(df[[targetFeature]])
      
      # Check if there are exactly two unique values
      if (length(uniqueValues) == 2) {
        selectInput("positiveClass", "Select Positive Class", choices = uniqueValues)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  
  output$uniqueValuesOfTargetFeature <- renderUI({
    df <- modifiedDataFrame()
    targetFeature <- input$targetFeature
    if (is.null(df) || is.null(targetFeature)) return(NULL)
    
    # Ensure targetFeature is a valid column name
    if (targetFeature %in% colnames(df)) {
      uniqueValues <- unique(df[[targetFeature]])
      
      # Check if there are exactly two unique values
      if (length(uniqueValues) == 2) {
        selectInput("positiveClass", "Select Positive Class", choices = uniqueValues)
      } else {
        return(NULL)
      }
    } else {
      return(NULL)
    }
  })
  
  # Reactive expression for data frame excluding targetFeature
  nonTargetFeatureData <- reactive({
    df <- modifiedDataFrame()
    targetFeature <- input$targetFeature
    if (is.null(df) || is.null(targetFeature)) {
      print("Modified data frame or target feature is NULL")
      return(NULL)
    }
    
    # Exclude the target feature and convert character columns to factors
    dfExcludingTarget <- df[, !(colnames(df) %in% targetFeature), drop = FALSE]
    print(paste("Columns in dfExcludingTarget:", colnames(dfExcludingTarget)))
    
    return(dfExcludingTarget)
  })
  
  output$modelSelectionDropdown <- renderUI({
    dfExcludingTarget <- nonTargetFeatureData()
    isText <- input$isText
    
    if (isText == "Yes") {
      selectInput("modelType", "Select Model", choices = "BernoulliNB")
    } else {
      if (is.null(dfExcludingTarget)) {
        print("dfExcludingTarget is NULL")
        return(NULL)
      }
      
      hasNumeric <- any(sapply(dfExcludingTarget, is.numeric))
      hasFactor <- any(sapply(dfExcludingTarget, is.factor))
      print(paste("hasNumeric:", hasNumeric, "hasFactor:", hasFactor))
      
      modelChoices <- NULL
      if (hasNumeric && hasFactor) {
        modelChoices <- c("GaussianNB", "CategoricalNB")
      } else if (hasNumeric) {
        modelChoices <- "GaussianNB"
      } else if (hasFactor) {
        modelChoices <- "CategoricalNB"
      }
      
      if (!is.null(modelChoices) && length(modelChoices) > 0) {
        selectInput("modelType", "Select Model", choices = modelChoices)
      } else {
        print("No valid model choices")
        return(NULL)
      }
    }
  })
  
  output$parameters_list <- renderUI({
    modelType_chosen <- input$modelType
    dfExcludingTarget <- nonTargetFeatureData()
    
    
    if (!is.null(modelType_chosen)) {
      if (modelType_chosen == "GaussianNB") {
        list(
          radioButtons("isautomatic", "Automatic detection of the elbow method: ", choices = c("Yes", "No")),
          numericInput("ncores", "Select the number of processor cores to use for calculations", value=1),
          numericInput("ncompon", "Select the number of components to keep: ", value = 2),
          p("If you have chosen the automatic detection, the number of components indicated below will not be used."),
          selectInput("type", "Select the prediction type", choices=c("comblin", "proba"))
        )
      } else if (modelType_chosen == "CategoricalNB") {
        list(
          numericInput("laplace", "Laplace smoothing", value = 1)
        )
        
      } else if (modelType_chosen == "BernoulliNB") {
        list(
          numericInput("ncores", "Number of processor cores to use for calculations", value=1),
          selectInput("textColumn", "The column containing the text data", choices = colnames(dfExcludingTarget)),
          
          # Parameters for my constructor
          numericInput("alpha", "Alpha (Laplace smoothing parameter)", value=1.0),
          selectInput("fitPrior", "Fit_prior", choices = c(TRUE,FALSE)),
          textInput("classPrior", "Class_prior; e.g., 0.2, 0.5, 0.3", value=0),
          p("Let the value at 0 if you want class_prior=NULL"),
          
          #Parameters for the bag-of-words
          selectInput("toLower", "Corpus in lower case", choices = c(TRUE,FALSE)),
          selectInput("rmPunctuation", "Remove punctuation", choices = c(TRUE,FALSE)),
          selectInput("rmNumbers", "Remove numbers", choices = c(TRUE,FALSE)),
          selectInput("rmStopwords", "Remove stopwords", choices = c("english","french")),
          selectInput("stripWhitespaces", "Strip the white spaces", choices = c(TRUE,FALSE))
          
        )
        
      } else {
        h4("Please select a model type")
      }
    } else {
      h4("Awaiting model type selection...")
    }
  })
  
  model <- reactiveVal(NULL)
  trainingData <- reactiveVal()
  testData <- reactiveVal()
  X_train <- reactiveVal()
  X_train_sc <- reactiveVal()
  y_train <- reactiveVal()
  X_test <- reactiveVal()
  X_test_sc <- reactiveVal()
  y_test <- reactiveVal()
  
  
  # instantiate the model
  observeEvent(input$createModel, {
    modelType_chosen <- input$modelType
    
    if (modelType_chosen == "GaussianNB") {
      # Create the GaussianNB object
      model(GaussianNB$new(n_cores = input$ncores))
      
    } else if (modelType_chosen == "CategoricalNB") {
      # Create the CategoricalNB object
      model(CategoricalNB$new())
      
    } else if (modelType_chosen == "BernoulliNB") {
      
      # process the input$class_prior variable
      classPrior <- input$classPrior
      if (classPrior == "0"){
        classPrior <- NULL
      } else {
        classPrior <- as.numeric(strsplit(input$classPrior, ",")[[1]])
      }
      
      print(input$ncores)
      # Create the Bernoulli object
      model(BernoulliNB$new(alpha=input$alpha, fit_prior=input$fitPrior, 
                            class_prior=classPrior, n_cores=input$ncores))
    }
    
    # Update the message
    output$instantiateModelStatus <- renderText({
      "The model has been correctly instanciated !"
    })
  })
  
  # split the dataset
  observeEvent(input$split, {
    df <- modifiedDataFrame()
    req(df)
    modelType_chosen <- input$modelType
    
    # Get the test proportion
    test_size <- input$testProportion
    
    # Split the data
    set.seed(1)
    if (modelType_chosen == "GaussianNB") {
      
      splitIndex <- createDataPartition(df[[input$targetFeature]], p = 1 - test_size, list = FALSE)
      training <- df[splitIndex, ]
      testing <- df[-splitIndex, ]
      
      trainingData(training)
      testData(testing)
      
      # Update reactive values
      X_train(training[, !(colnames(training) %in% input$targetFeature), drop = FALSE])
      y_train(as.factor(training[[input$targetFeature]]))
      X_test(testing[, !(colnames(testing) %in% input$targetFeature), drop = FALSE])
      y_test(as.factor(testing[[input$targetFeature]]))
      
    } else if(modelType_chosen == "CategoricalNB"){
      
      indices_test <- sample(1:nrow(df), size = round(test_size * nrow(df)))
      training <- df[-indices_test, ]
      testing <- df[indices_test, ]
      
      trainingData(training)
      testData(testing)
      
      X_train(training[, -which(names(training) == input$targetFeature)])
      y_train(as.factor(training[,input$targetFeature]))
      X_test(testing[, -which(names(testing) == input$targetFeature)])
      y_test(as.factor(testing[,input$targetFeature]))
      
    } else if(modelType_chosen == "BernoulliNB") {
      
      sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(1-test_size,test_size))
      training <- df[sample, ]
      testing <- df[!sample, ]
      
      trainingData(training)
      testData(testing)
      
      # Update reactive values
      X_train(training[[input$textColumn]])
      X_test(testing[[input$textColumn]])
      y_train(as.character(training[[input$targetFeature]]))
      y_test(as.character(testing[[input$targetFeature]]))
    }
    
    # Update the message
    output$splitStatus <- renderText({
      "You have correctly splitted your data !"
    })
  })
  
  # start the preprocessing
  observeEvent(input$preprocess, {
    
    output$preprocessStatus <- renderText({
      "Preprocessing... "
    })
    
    modelType_chosen <- input$modelType
    
    currentModel <- model()
    if(modelType_chosen == "GaussianNB") {
      
      X_train_sc(currentModel$preprocessing_train(X_train(), n_compon=input$ncomp))
      X_test_sc(currentModel$preprocessing_test(X_test()))
      
      hasNumeric2 <- any(sapply(X_train(), is.numeric))
      hasFactor2 <- any(sapply(X_train(), is.factor))

      if (hasNumeric2 && hasFactor2) {
        afdm <- AFDM$new()
        # Plot of the Number of Components 
        output$nbCompplot <- renderPlot({
          if (input$isautomatic == "Yes"){
            transformed_data <- afdm$fit_transform(X_train())
          } else {
            transformed_data <- afdm$fit_transform(X_train(), n_compon = input$ncompon)
          }
          afdm$plot_diagonal_auto()
        })
        
        output$screeplot <- renderPlot({
          if (input$isautomatic == "Yes"){
            transformed_data <- afdm$fit_transform(X_train())
          } else {
            transformed_data <- afdm$fit_transform(X_train(), n_compon = input$ncompon)
          }
          afdm$plot_eigen()
        })
      } 
      
    } else if(modelType_chosen == "CategoricalNB"){
      
      X_train_sc(currentModel$preprocessing_train(X_train(), y_train())[[1]])
      X_test_sc(currentModel$preprocessing_test(X_test()))
      
    } else if(modelType_chosen == "BernoulliNB"){
      
      preprocess_X = currentModel$preprocess(X_train(), X_test(), to_lower=input$toLower,
                                             rm_punctuation=input$rmPunctuation, rm_numbers=input$rmNumbers,
                                             rm_stopwords=stopwords(input$rmStopwords),
                                             strip_whitespace=input$stripWhitespaces)
      X_train_sc(preprocess_X[[1]])
      X_test_sc(preprocess_X[[2]])
    }
    
    # Update the message
    output$preprocessStatus <- renderText({
      "Congrats ! Your data is now preprocessed. You can fit and predict in the
      'Model Performance' tab."
    })
  })
  
  ############# TAB Train Performances ####################
  
  # Fit and display of our train performances
  observeEvent(input$fit, {
    # Get the model object
    currentModel <- model() # Assuming 'model' is a reactiveVal storing the model
    req(currentModel)
    modelType_chosen <- input$modelType # Model chosen earlier
    
    currentModel$fit(X_train_sc(), y_train())
    
    # Output for model summary
    output$modelSummary <- renderPrint({ 
      result <- capture.output(currentModel$summary(), type="message")
      result
    })
    
    # For the confusion matrix plot
    output$train_confusionMatrixPlot <- renderPlot({
      req(currentModel) # Ensure the model is fitted
      # Generate the confusion matrix plot
      if (modelType_chosen == "GaussianNB") {
        currentModel$plot_confusionmatrix(X_train_sc(), y_train(), type=input$type)
      } else if(modelType_chosen == "CategoricalNB"){
        currentModel$plot_confusionmatrix(X_train_sc(), y_train())
      } else if (modelType_chosen == "BernoulliNB"){
        currentModel$plot_confusionmatrix(X_train_sc(), y_train())
      } 
    })
  })
    
    ############# TAB Test Scores ####################
    
    observeEvent(input$prediction, {
      # Get the model object
      currentModel <- model() # Assuming 'model' is a reactiveVal storing the model
      req(currentModel)
      modelType_chosen <- input$modelType # Model chosen earlier
      
      # currentModel$fit(X_train_sc(), y_train())
      
      # Output for model summary
      output$modelScores <- renderPrint({ 
        result <- capture.output(currentModel$score(X_test_sc(), y_test()), type="message")
        result
      })
      
      # For the confusion matrix plot
      output$test_confusionMatrixPlot <- renderPlot({
        req(currentModel) # Ensure the model is fitted
        # Generate the confusion matrix plot
        if (modelType_chosen == "GaussianNB") {
          currentModel$plot_confusionmatrix(X_test_sc(), y_test(), type=input$type)
        } else if(modelType_chosen == "CategoricalNB"){
          currentModel$plot_confusionmatrix(X_test_sc(), y_test())
        } else if (modelType_chosen == "BernoulliNB"){
          currentModel$plot_confusionmatrix(X_test_sc(), y_test())
        } 
      })
    
    # For the ROC curve plot
    output$rocCurvePlot <- renderPlot({
      print("rocCurve")
      # currentModel <- model()
      req(currentModel) # Ensure the model is fitted
      
      # Generate the ROC curve plot
      if (modelType_chosen == "GaussianNB"){
        currentModel$plot_roccurve(X_test_sc(), y_test(), input$positiveClass)
      } else if(modelType_chosen == "CategoricalNB"){
        currentModel$plot_roccurve(X_test_sc(), y_test(), input$positiveClass)
      } else if (modelType_chosen == "BernoulliNB"){
        currentModel$plot_roccurve(X_test_sc(), y_test(), input$positiveClass)
      }
    })
    
  })
}


# Run the application
shinyApp(ui = ui, server = server)
