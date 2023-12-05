library(shiny)
library(shinydashboard)
library(rpart)
#library(mice)


# Train your decision tree model

combined_mod <- readRDS("combined.rds")
ann_mod <- readRDS("ann_model.rds")
dt_mod <- readRDS("dt_model.rds")
laplace_mod <-readRDS("laplace_model.rds")
lr_mod <- readRDS("lr_model.rds")
rf_mod <- readRDS("rf_model.rds")
rbf_mod <- readRDS("rbf_model.rds")
van_mod <-readRDS("vanilla_model.rds")
#KNN stuff
train_data_labels <- read.csv("train_data_labels.csv")
train_data_predictors <- read.csv("train_data_predictors.csv")


ui <- dashboardPage(
  dashboardHeader(title = "Heart Disease Prediction"),
  dashboardSidebar(
    # Body content
    h3(textOutput("prediction"), align = "center"),
    h5(textOutput("subtext"), align = "center")
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
    helpText("Use a free app on your phone to get your ECG"),
    selectInput("RestingECG", "Resting EKG:",
                c("Normal", "ST-T wave abnormality", "Left ventricular hypertrophy"),
                selected = "Normal"),
    helpText("Enter your heart rate after 1 minute of an intense exercise"),
    numericInput("MaxHR", "Maximum Heart Rate:", value = 150),
    helpText("Enter Yes if you have pain after that exercise, otherwise put No"),
    selectInput("ExerciseAngina", "Exercise-Induced Angina:",
                c("No", "Yes"),
                selected = "No"),
    helpText("Get the peak and the slope from your ECG"),
    numericInput("Oldpeak", "Oldpeak:", value = 0),
    selectInput("ST_Slope", "ST Segment Slope:",
                c("Upsloping", "Flat", "Downsloping"),
                selected = "Upsloping"),
    actionButton("predictButton", "Predict")
  )
)


# Define server logic
server <- function(input, output) {
  
  
  # Reactive values
  predict_button <- reactiveVal(FALSE)
  prediction_result <- reactiveVal(NULL)
  sub <- reactiveVal(NULL)
  
  # Make predictions based on user input
  
  MAX_AGE <- 77
  MIN_AGE <- 28
  
  MAX_BP <- 200
  MIN_BP <- 80
  
  MAX_OP <- 6.2
  MIN_OP <- -2.6
  
  MAX_COL <- 603
  MIN_COL <- 0
  
  MAX_HR <- 202
  MIN_HR <- 60
  
  
  observeEvent(input$predictButton, {
    # Set the predict_button to TRUE when the button is pressed
    predict_button(TRUE)
    
    # Perform the prediction based on user input
    norm_age <- (input$Age - MIN_AGE) / (MAX_AGE - MIN_AGE)
    norm_age <- ifelse( norm_age > 1, 1, norm_age)
    norm_age <- ifelse( norm_age < 0, 0, norm_age)
    
    norm_bp <- (input$RestingBP - MIN_BP) / (MAX_BP - MIN_BP)
    norm_bp <- ifelse( norm_bp > 1, 1, norm_bp)
    norm_bp <- ifelse( norm_bp < 0, 0, norm_bp)
    
    norm_op <- (input$Oldpeak - MIN_OP) / (MAX_OP - MIN_OP)
    norm_op <- ifelse( norm_op > 1, 1, norm_op)
    norm_op <- ifelse( norm_op < 0, 0, norm_op)
    
    norm_col <- (input$Cholesterol - MIN_COL) / (MAX_COL - MIN_COL)
    norm_col <- ifelse( norm_col > 1, 1, norm_col)
    norm_col <- ifelse( norm_col < 0, 0, norm_col)
    
    norm_hr <- (input$MaxHR - MAX_HR) / (MAX_HR - MIN_HR)
    norm_hr <- ifelse( norm_hr > 1, 1, norm_hr)
    norm_hr <- ifelse( norm_hr < 0, 0, norm_hr)
    
    chest_pain <- ifelse(substr(input$ChestPainType,1,3) == "Typ", 1,
                         ifelse(substr(input$ChestPainType,1,3) == "Atp", 2,
                                ifelse(substr(input$ChestPainType,1,3) == "Non", 3, 0) 
                          )
                  )
    
    rest_ecg <- ifelse(substr(input$RestingECG,1,3) == "Nor", 1, 
                       ifelse(substr(input$RestingECG,1,3) == "ST-", 2, 3)
    )
    
    slop <- ifelse(substr(input$ST_Slope,1,3) == "Ups", 1, 
                   ifelse(substr(input$ST_Slope,1,3) == "Fla", 2, 3)
            )
    
    query <-  data.frame(
      age = norm_age,
      sex_m = ifelse(input$Sex == "Male", 1, 0),
      chest_pain_type_ata = ifelse(chest_pain == 2, 1, 0),
      chest_pain_type_nap = ifelse(chest_pain == 3, 1, 0),
      chest_pain_type_ta = ifelse(chest_pain == 1, 1, 0),
      resting_bp = norm_bp,
      cholesterol = norm_col,
      fasting_bs = as.numeric(input$FastingBS),
      resting_ecg_normal = ifelse(rest_ecg == 1, 1, 0),
      resting_ecgst = ifelse(rest_ecg == 2, 1, 0),
      max_hr = norm_hr,
      exercise_angina_y = ifelse(substr(input$ChestPainType,1,1) == "Y", 1, 0 ),
      oldpeak = norm_op,
      st_slope_flat = ifelse(slop == 2, 1, 0),
      st_slope_up = ifelse(slop == 1, 1, 0)
    )
    
    #query <- mice(data.frame(query), m = 1, meth = c("pmm", "pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm","pmm"))
    
    
    #print(query)
    
    lr_predict_n <- predict(lr_mod, query, type = "response")
    tree_predict_n <- 0.5#predict(dt_mod, query)
    knn_predict_n <- 0.5#knn(train_data_predictors, query, train_data_labels, k = 21, prob = 0.4)
    ann_predict_n <- predict(ann_mod, query, type = "response")
    rf_predict_n <- 0.5 #predict(rf_mod, query, type = "response") 
    svm_laplace_predict_n <- 0.5#predict(laplace_mod, query)
    svm_vanilla_predict_n <- 0.5 #predict(van_mod, query)
    svm_rbf_predict_n <- 0.5 #predict(rbf_mod, query)
    
    combinedd <- data.frame(
      
      lr_predict_n,
      tree_predict_n,
      knn_predict_n,
      ann_predict_n,
      svm_rbf_predict_n,
      svm_laplace_predict_n,
      svm_vanilla_predict_n
    )
    
    #print(combinedd)
    
    
    result <- ifelse (lr_predict_n >= 0.5, "You are at risk of stroke", "You are not at risk of a stroke")
    
    
    sub <- ifelse (lr_predict_n >= 0.5,
                   "Ask your health care professional about how you can reduce your risk. Stroke is largely preventable, treatable and beatable. Visit https://www.bcbsil.com/bcchp/getting-care/health-and-wellness/stroke for more information ", 
                   "Make sure you are continue limiting the risks of stroke by keeping a healthy weight, having frequent exercise, and stopping bad habits like smoking")
    #combined_predict <- predict(combined_model, combinedd)
    # Set the prediction result
    prediction_result((result))
    sub(sub)
    
    #paste(output$predictiion)
    
    # Set the predict_button back to FALSE
    predict_button(FALSE)
  })
  
  # Render the prediction result
  output$prediction <- renderText({
    prediction_result()
  })
  
  output$subtext <- renderText({
    sub()
  })
}

# Run the Shiny app
shinyApp(ui, server)
