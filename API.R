# Load necessary libraries

packages <- c("tidyverse", "plumber", "caret")

install_if_missing <- function(package) {
  if (!require(package, character.only = TRUE)) {
    install.packages(package, dependencies = TRUE)
    library(package, character.only = TRUE)
  }
}

lapply(packages, install_if_missing)

# Read in data
DBH <- read_csv("diabetes_binary_health_indicators_BRFSS2015.csv")

# Selecting the variables I want
DBH <- DBH[, c(1,2,5,6,9,11,19,20,22)]

# Changing the possible factor variables into factors
DBH$HighBP <- factor(DBH$HighBP, levels = c(0, 1), labels = c("Not_High", "High"))
DBH$Smoker <- factor(DBH$Smoker, levels = c(0, 1), labels = c("No", "Yes"))
DBH$PhysActivity <- factor(DBH$PhysActivity, levels = c(0, 1), labels = c("No", "Yes"))
DBH$Veggies <- factor(DBH$Veggies, levels = c(0, 1), labels = c("No", "Yes"))
DBH$Sex <- factor(DBH$Sex, levels = c(0, 1), labels = c("Female", "Male"))
DBH$Age <- factor(DBH$Age, 
                  levels = 1:13, 
                  labels = c("18_24", "25_29", "30_34", "35_39", "40_44", "45_49", 
                             "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_plus"))
DBH$Income <- factor(DBH$Income, 
                     levels = 1:8, 
                     labels = c("Less_than_10k", "10k_to_15k", "15k_to_20k", 
                                "20k_to_25k", "25k_to_35k", "35k_to_50k", 
                                "50k_to_75k", "75k_or_more"))
DBH$Diabetes_binary <- factor(DBH$Diabetes_binary, levels = c(0, 1), labels = c("No_Diabetes", "Diabetes"))

# Control for cross-validation with 3 folds
train_control <- trainControl(method = "cv", number = 3, classProbs = TRUE, summaryFunction = mnLogLoss)

# Setset seed
set.seed(270)

# Fit the best model
log_model1 <- train(Diabetes_binary ~ ., data = DBH, method = "glm", family = "binomial", 
                    trControl = train_control, metric = "logLoss")

# Define plumber API
#* @apiTitle Diabetes Prediction API

#* Return prediction for diabetes status
#* @param HighBP:int High blood pressure (0: No, 1: Yes)
#* @param BMI:double Body mass index (Min: 12.00, Max: 98.00)
#* @param Smoker:int Smoker status (0: No, 1: Yes)
#* @param PhysActivity:int Physical activity (0: No, 1: Yes)
#* @param Veggies:int Vegetables consumption (0: No, 1: Yes)
#* @param Sex:int Sex (0: Female, 1: Male)
#* @param Age:int Age category (1: 18-24, ..., 13: 80+)
#* @param Income:int Income level (1: <10k, ..., 8: 75k+)
#* @get /pred
function(HighBP = 0, BMI = 28.38, Smoker = 0, PhysActivity = 1, Veggies = 1, Sex = 0, Age = 9, Income = 8) {
  new_data <- data.frame(
    HighBP = factor(as.integer(HighBP), levels = c(0, 1), labels = c("Not_High", "High")),
    BMI = as.numeric(BMI),
    Smoker = factor(as.integer(Smoker), levels = c(0, 1), labels = c("No", "Yes")),
    PhysActivity = factor(as.integer(PhysActivity), levels = c(0, 1), labels = c("No", "Yes")),
    Veggies = factor(as.integer(Veggies), levels = c(0, 1), labels = c("No", "Yes")),
    Sex = factor(as.integer(Sex), levels = c(0, 1), labels = c("Female", "Male")),
    Age = factor(as.integer(Age), levels = 1:13, labels = c("18_24", "25_29", "30_34", "35_39", "40_44", "45_49", 
                                                            "50_54", "55_59", "60_64", "65_69", "70_74", "75_79", "80_plus")),
    Income = factor(as.integer(Income), levels = 1:8, labels = c("Less_than_10k", "10k_to_15k", "15k_to_20k", 
                                                                 "20k_to_25k", "25k_to_35k", "35k_to_50k", 
                                                                 "50k_to_75k", "75k_or_more"))
  )
  prediction <- predict(log_model1, new_data, type = "prob")
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

# Example function calls for the API
# Example 1: curl "http://localhost:8000/pred?HighBP=1&BMI=30.0&Smoker=1&PhysActivity=0&Veggies=0&Sex=0&Age=10&Income=4"
# Example 2: curl "http://localhost:8000/pred?HighBP=0&BMI=25.0&Smoker=0&PhysActivity=1&Veggies=1&Sex=1&Age=6&Income=8"
# Example 3: curl "http://localhost:8000/pred?HighBP=1&BMI=28.0&Smoker=1&PhysActivity=0&Veggies=0&Sex=1&Age=9&Income=2"