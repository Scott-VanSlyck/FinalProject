# Load necessary libraries
library(plumber)
library(tidyverse)
library(caret)
library(randomForest)

# Read in data and fit the best model (assumed Random Forest)
DBH <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")
DBH <- DBH %>% select(Diabetes_binary, HighBP, BMI, Smoker, PhysActivity, Veggies, Sex, Age, Income)
DBH$Diabetes_binary <- factor(DBH$Diabetes_binary, levels = c("No_Diabetes", "Diabetes"))

set.seed(270)
trainIndex <- createDataPartition(DBH$Diabetes_binary, p = 0.7, list = FALSE)
trainData <- DBH[trainIndex, ]
testData <- DBH[-trainIndex, ]

# Control for cross evalution with 3-folds

train_control <- trainControl(method = "cv", number = 3, classProbs = TRUE, summaryFunction = mnLogLoss)

# Re-set seed
set.seed(270)

# Fit the best model
log_model1 <- train(Diabetes_binary ~ ., data = trainData, method = "glm", family = "binomial", 
                    trControl = train_control, metric = "logLoss")

# Define plumber API
#* @apiTitle Diabetes Prediction API

#* Return prediction for diabetes status
#* @param HighBP:int High blood pressure (0: No, 1: Yes)
#* @param BMI:double Body mass index
#* @param Smoker:int Smoker status (0: No, 1: Yes)
#* @param PhysActivity:int Physical activity (0: No, 1: Yes)
#* @param Veggies:int Vegetables consumption (0: No, 1: Yes)
#* @param Sex:int Sex (0: Female, 1: Male)
#* @param Age:int Age category (1: 18-24, ..., 13: 80+)
#* @param Income:int Income level (1: <10k, ..., 8: 75k+)
#* @get /pred
function(HighBP = 0, BMI = 28.38, Smoker = 0, PhysActivity = 1, Veggies = 1, Sex = 0, Age = 9, Income = 8) {
  new_data <- data.frame(
    HighBP = as.integer(HighBP),
    BMI = as.numeric(BMI),
    Smoker = as.integer(Smoker),
    PhysActivity = as.integer(PhysActivity),
    Veggies = as.integer(Veggies),
    Sex = as.integer(Sex),
    Age = as.integer(Age),
    Income = as.integer(Income)
  )
  prediction <- predict(rf_model, new_data, type = "prob")
  return(prediction)
}

#* Return API information
#* @get /info
function() {
  list(
    name = "Scott Van Slyck",
    github_url = "https://Scott-VanSlyck.github.io/FinalProject"
  )
}