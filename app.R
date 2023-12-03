library(shiny)
library(shinydashboard)
library(rpart)

# Assume 'your_data' is your training dataset
# Assume 'your_response_variable' is the response variable in your dataset

# Train your decision tree model

combined_mod <- readRDS("combined.rds")


ui <- dashboardPage(
  dashboardHeader(title = "Heart Disease Prediction"),
  dashboardSidebar(
    # Body content
    textOutput("prediction")
  ),
  dashboardBody(
    
    # Sidebar content
    helpText("Input how old you currently are:"),
    numericInput("Age", "Age:", value = 40),
    helpText("Input whether you are male or female"),
    selectInput("Sex", "Sex:", c("Male", "Female"), selected = "Male"),
    helpText("Select your current chest pain:\n"),
    #helpText("Typical Angina - chest pain or discomfort caused by reduced blood flow to the heart\n"),
    #helpText("Atypical Angina - may include sensations like burning, fullness, or sharpness\n"),
    #helpText("Non-Anginal Pain - unrelated to heart issues\n"),
    #helpText("Asymptomatic - absence of chest pain or discomfort\n"),
    selectInput("ChestPainType", "Chest Pain Type:",
                c("Typical Angina - chest pain or discomfort caused by reduced blood flow to the heart", "Atypical Angina - may include sensations like burning, fullness, or sharpness",
                  "Non-Anginal Pain - pain unrelated to heart issues", "Asymptomatic - absence of chest pain or discomfort"),
                selected = "Typical Angina"),
    helpText("Use a blood pressure cuff to get your blood pressure cuff or go to a local pharmacy"),
    numericInput("RestingBP", "Resting Blood Pressure:", value = 120),
    helpText("Use a blood test kit to get your cholesterol levels and Blood Sugar"),
    numericInput("Cholesterol", "Cholesterol:", value = 200),
    sliderInput("FastingBS", "Fasting Blood Sugar:", min = 0, max = 1, step = 0.01, value = 0.5),
    helpText("Use a free app on your phone"),
    selectInput("RestingECG", "Resting EKG:",
                c("Normal", "ST-T wave abnormality", "Left ventricular hypertrophy"),
                selected = "Normal"),
    numericInput("MaxHR", "Maximum Heart Rate:", value = 150),
    selectInput("ExerciseAngina", "Exercise-Induced Angina:",
                c("No", "Yes"),
                selected = "No"),
    numericInput("Oldpeak", "Oldpeak:", value = 0),
    selectInput("ST_Slope", "ST Segment Slope:",
                c("Upsloping", "Flat", "Downsloping"),
                selected = "Upsloping"),
    actionButton("predictButton", "Predict")
  )
)


# Define server logic
server <- function(input, output) {
  
  
  predict_button <- reactiveVal(FALSE)
  
  # Make predictions based on user input
  
  MAX_AGE <- 77
  MIN_AGE <- 28
  
  MAX_BP <- 200
  MIN_BP <- 80
  
  MAX_OP <- 6.2
  MIN_OP <- -2.6
  
  
  
  
  output$prediction <- renderText({
    req(input$predictButton)
    
    input_var <- as.numeric(input$input_var)
    prediction <- predict(combined_mod, data.frame(c(input$Age, input$Sex, input$ChestPainType,
                                                              input$RestingBP, input$Cholesterol, input$FastingBS,
                                                              input$RestingECG, input$MaxHR, input$ExerciseAngina,
                                                              input$Oldpeak, input$ST_Slope)))
    
    prediction <- predict(combined_mod, data.frame(c(input$Age, input$Sex, input$ChestPainType,
                                                     input$RestingBP, input$Cholesterol, input$FastingBS,
                                                     input$RestingECG, input$MaxHR, input$ExerciseAngina,
                                                     input$Oldpeak, input$ST_Slope)))
    
    paste("Predicted Class: ", prediction)
  })
  
  # Update the reactive value when the button is pressed
  observeEvent(input$predictButton, {
    predict_button(TRUE)
  })
}

# Run the Shiny app
shinyApp(ui, server)
