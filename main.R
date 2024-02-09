# installing the libraries
if(!require(tidyverse   )) install.packages("tidyverse"  , repos = "http://cran.us.r-project.org")
if(!require(caret       )) install.packages("caret"      , repos = "http://cran.us.r-project.org")
if(!require(corrplot    )) install.packages("corrplot"   , repos = "http://cran.us.r-project.org")
if(!require(GGally      )) install.packages("GGally"     , repos = "http://cran.us.r-project.org")
if(!require(ROCR        )) install.packages("ROCR"       , repos = "http://cran.us.r-project.org")
if(!require(klaR        )) install.packages("klaR"       , repos = "http://cran.us.r-project.org")
if(!require(xgboost     )) install.packages("xgboost"    , repos = "http://cran.us.r-project.org")
if(!require(pROC        )) install.packages("pROC"       , repos = "http://cran.us.r-project.org")
if(!require(smotefamily )) install.packages("smotefamily", repos = "http://cran.us.r-project.org")

library(corrplot)
library(tidyverse)
library(caret)
library(GGally)
library(ROCR)
library(pROC)
library(smotefamily)
#library(UBL)
################################################################################
############### Defining functions to be used in script#########################
################################################################################

# function for one_hot encoding of categorical feature (type in our case)
one_hot_encode <- function(df_data, categorical_col){
  # Ensure the specified column is categorical
  if (!is.factor(df_data[[categorical_col]])) {
    stop("Specified column is not categorical.")
  }
  
  # Get unique categories in the categorical column
  categories <- levels(df_data[[categorical_col]])
  
  # Create binary columns for each category
  for (category in categories) {
    binary_col <- ifelse(df_data[[categorical_col]] == category, 1, 0)
    col_name <- paste(categorical_col, category, sep = "_")
    df_data[[col_name]] <- binary_col
  }
  
  # Return the encoded dataframe
  df_data
}

################################################################################
train_model <- function(df_training, target,
                        features, method, control){
  # declare the formula (y ~ x1 + x2 + etc.)
  formula <- as.formula(
    paste(
      target, "~", paste(features, collapse = " + ")))
  
  # train the model
  model <- train(formula,
                 data = df_training,
                 method = method,
                 trControl = control)
  model
}

################################################################################
get_confusion_matrix <- function(model, df_test, target) {
  # make confusion matrix
  cm <- confusionMatrix(predict(model, df_test),
                        as.factor(df_test[[target]]))
  cm
}

get_f1_score <- function(model, df_test, target) {
  # make confusion matrix
  my_cm <- get_confusion_matrix(model=model, 
                                df_test=df_test, 
                                target=target)
  # extract F1 score from it
  my_cm[["byClass"]][["F1"]]
}

################################################################################
get_ROC_performance <- function(model, df_test, target) {
  # get probabilities
  predicted_probs <- predict(model, newdata = df_test, type = "prob")[, "No Failure"]
  pred <- prediction(predicted_probs, as.factor(df_test[[target]]))
  # get tpr and fpr to plot ROC
  perf <- performance(pred, "tpr", "fpr")
  perf
}

################################################################################
get_tpr_at_x_fpr <- function(model, df_test, target, x=0.1) {

  perf <- get_ROC_performance(model=model, df_test=df_test, target=target)
  
  # Extract TPR and FPR values
  fpr <- perf@x.values[[1]]
  tpr <- perf@y.values[[1]]
  
  # Interpolate to find TPR at the specified FPR value
  approx_tpr <- approx(fpr, tpr, xout = x)$y
  approx_tpr
}

################################################################################
get_precision_recall_performance <- function(model, df_test, target) {
  # get probabilities
  predicted_probs <- predict(model, newdata = df_test, type = "prob")[, "Failure"]
  pred <- prediction(predicted_probs, as.factor(df_test[[target]]))
  
  # get precision and recall
  perf <- performance(pred, "prec", "rec")
  perf
}

################################################################################
plot_ROC_curves <- function(trained_models, model_names, df_test, target) {
  
  # Create an empty plot
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "False Positive Rate", ylab = "True Positive Rate", 
       main = "ROC curves comparison")
  
  grid(col = "grey", lty = "dotted")
  # Plot ROC curves for each model
  for (i in seq_along(trained_models)) {
    perf <- get_ROC_performance(trained_models[[i]], df_test, target)
    auc <- round(as.numeric(perf@y.values[[1]]), 3)  # Extract AUC value
    
    # Extract TPR and FPR values
    fpr <- perf@x.values[[1]]
    tpr <- perf@y.values[[1]]
    
    # Plot ROC curve
    lines(fpr, tpr, col = i, lwd = 2)
  }
  
  # Add legend
  legend("bottomright", legend = model_names, 
         col = seq_along(trained_models), lwd = 2, cex = 0.8)
}

################################################################################
plot_precision_recall_curves <- function(trained_models, model_names, df_test, target) {
  # Create an empty plot
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "Recall", ylab = "Precision", 
       main = "Precision-Recall curves comparison")
  
  grid(col = "grey", lty = "dotted")
  
  # Plot precision-recall curves for each model
  for (i in seq_along(trained_models)) {
    perf <- get_precision_recall_performance(trained_models[[i]], df_test, target)
    
    # Extract precision and recall values
    recall <- perf@x.values[[1]]
    precision <- perf@y.values[[1]]
    
    # Plot precision-recall curve
    lines(recall, precision, col = i, lwd = 2)
  }
  
  # Add legend
  legend("bottomright", legend = model_names, 
         col = seq_along(trained_models), lwd = 2, cex = 0.8)
}

################################################################################
# Function to perform SMOTE on failure column
smote_for_binary_target <- function(train_data, target_col, features_cols) {
  # Extract features and target variable
  train_features <- train_data[, features_cols]
  train_target <- train_data[[target_col]]
  
  # Perform SMOTE
  df_smote <- SMOTE(train_features, target = train_target)$data
  
  # rename target column to original name
  df_smote <- df_smote %>% 
    rename(!!target_col := class)
  
  df_smote
}

################################################################################
# Function to perform ADASYN on failure column
adasyn_for_multi_target <- function(train_data, target_col, features_cols) {
  # Extract features and target variable
  train_features <- train_data[, features_cols]
  train_target <- train_data[[target_col]]
  
  # Perform ADASYN
  df_adas <- ADAS(train_features, target = train_target)$data
  
  # rename target column to original name
  df_adas <- df_adas %>% 
    rename(!!target_col := class)
  
  # bring back the binary failure column
  df_adas$failure <- ifelse(df_adas[[target_col]] == "No Failure", 
                            "No Failure", "Failure")
  
  df_adas
}

################################################################################
# Function to perform ANS on failure column
ans_for_multi_target <- function(train_data, target_col, features_cols) {
  # Extract features and target variable
  train_features <- train_data[, features_cols]
  train_target <- train_data[[target_col]]
  
  # Perform ANS
  df_ans <- ANS(train_features, target = train_target)$data
  
  # rename target column to original name
  df_ans <- df_ans %>% 
    rename(!!target_col := class)
  
  # bring back the binary failure column
  df_ans$failure <- ifelse(df_ans[[target_col]] == "No Failure",
                           "No Failure", "Failure")
  
  df_ans
}
################################################################################
# Function to perform SLS on failure column
SMOTE_for_multi_target <- function(train_data, target_col, features_cols) {
  
  # Extract features and target variable
  train_features <- train_data[, features_cols]
  train_target <- train_data[[target_col]]
  
  # Perform SLS
  df_SMOTE <- SMOTE(train_features, target = train_target)$data
  
  # rename target column to original name
  df_SMOTE <- df_SMOTE %>% 
    rename(!!target_col := class)
  
  # bring back the binary failure column
  df_SMOTE$failure <- ifelse(df_SMOTE[[target_col]] == "No Failure",
                           "No Failure", "Failure")
  
  df_SMOTE
}

################################################################################
################################# EDA ##########################################
################################################################################
# load data set
df_data <- read.csv("./data/predictive_maintenance.csv")

# show head of data
head(df_data)

# rename columns to more convenient names
df_data <- df_data %>% 
  rename("product_id" = Product.ID,
         "type" = Type,
         "air_t" = Air.temperature..K.,
         "process_t" = Process.temperature..K.,
         "rpm" = Rotational.speed..rpm.,
         "torque" = Torque..Nm.,
         "wear" = Tool.wear..min.,
         "failure_numeric" = Target,
         "failure_type" = Failure.Type)

# crate the failure label column form 0 and 1 ==> no failure and failure
df_data <- df_data %>%
  mutate(failure = ifelse(failure_numeric==1, "Failure", "No Failure"))

# show the head of data set after modifications
head(df_data)

# find number of failures and non failures in the data
table(df_data$failure)

# select columns for pair plot
df_pair <- df_data %>% 
  select(-any_of(c("UDI", "failure", "failure_numeric",
                   "product_id", "failure_type")))

# create the pair plot
pair_plot <- ggpairs(df_pair,
                     mapping=aes(color=df_data$failure),
                     lower = list(continuous = wrap("points", alpha = 0.2), 
                                  combo = "box_no_facet"),
                     diag = list(continuous = wrap("densityDiag", alpha = 0.2))) +
  labs(title ="Feaure pair-wise relations grid plot") + 
  theme(axis.text.x = element_text(angle = 90,))

# show pair plot
pair_plot

# remove df_pair from environment as it's no longer needed
rm(df_pair)

# see number of different types of failure
df_data %>% 
  filter(failure_numeric==1) %>%
  group_by(failure_type) %>% 
  summarize(n_failure = n()) %>% 
  mutate(failure_type=reorder(failure_type,n_failure)) %>%
  ggplot(aes(x=failure_type, y=n_failure)) + 
  geom_col(color = "steelblue", fill="blue3") + 
  labs(x = "Failure type",
       y = "Number of failures",
       title ="Failure type vs Number of failures") + 
  coord_flip()
  
# check if there are rows with recorded failure types but failure_numeric = 0
nrow(df_data %>% 
       filter(failure != "No Failure" & failure_numeric == 0))


# reassign correct failure_numeric to the problematic rows
df_data <- df_data %>% 
  mutate(failure_numeric = ifelse(failure_type=="No Failure", 0, 1))

# one_hot encoding the categorical type column 
df_data$type <- as.factor(df_data$type)
df_data <- one_hot_encode(df_data = df_data, categorical_col = "type")

# re-arrange columns to have the target columns at the end 
all_cols <- names(df_data)
target_cols <- c("failure", "failure_type", "failure_numeric")
other_cols <- setdiff(all_cols, target_cols)
df_data <- df_data[, c(other_cols, target_cols)]

# see the re-arranged dataframe
head(df_data)

# select columns for correlations plot
df_corr <- df_data %>% 
  select(-any_of(c("UDI", "type", "failure",
                   "product_id", "failure_type")))

# plot the correlations using spearman method
ggcorr(df_corr, label = TRUE, label_round=3, 
       method=c("pairwise", "spearman"),
       low = "brown2", 
       mid = "lightgrey", 
       high = "steelblue")

# remove df_corr from environment as it's no longer needed
rm(df_corr)

################################################################################
##############################  Modelling ######################################
################################################################################

# Split the data set into training and test sets
test_indices <- createDataPartition(df_data$failure, times=1, p=0.2, list=FALSE)
test_set     <- df_data[test_indices, ]
train_set    <- df_data[-test_indices,]

# 5 fold cross validation
ctrl <- trainControl(method = "cv", number = 5) 

# methods and respective names to train models
methods      <- c("rpart", "rf", "lda2","pda2", "nb", "xgbTree",  "LogitBoost")
mehtod_names <- c("Decision tree", "Random forest", "LDA", "Penalized DA",
                  "Naive Bayes", "XGBoost", "Boosted logistic regression")

# features to use for training and target to predict
my_features <- c("air_t", "process_t", "rpm", "torque", 
                 "wear", "type_H", "type_L", "type_M")

my_target   <- "failure"

# train the models in lapply
trained_models <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set, 
              target=my_target,
              features=my_features, 
              method=method,
              control=ctrl)
  model
})

# plot ROC curves
plot_ROC_curves(trained_models=trained_models,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_target) 

# plot precision-recall curves
plot_precision_recall_curves(trained_models=trained_models,
                             model_names=mehtod_names, 
                             df_test=test_set, 
                             target=my_target) 


# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target="failure")$table

get_confusion_matrix(trained_models[[which(methods=="rf")]],
                     df_test=test_set,
                     target="failure")$table

# get our metric
get_tpr_at_x_fpr(trained_models[[which(methods=="xgbTree")]],
                 df_test=test_set, 
                 target="failure", x=0.1)

get_tpr_at_x_fpr(trained_models[[which(methods=="rf")]],
                 df_test=test_set,
                 target="failure", x=0.1)

# do SMOTE based on binary target column (failure)
# Do SMOTE on training set to avoid data leakage. Also test set needs to be realistic
train_set_smote <- smote_for_binary_target(train_data = train_set,
                                           target_col = my_target,
                                           features_cols = my_features)

# show the resulting distribution of target column after SMOTE
table(train_set_smote$failure)

# train the models based on new training set from SMOTE
trained_models_SMOTE <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set_smote, 
                       target=my_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

# plot the new ROC curves after SMOTE
plot_ROC_curves(trained_models=trained_models_SMOTE,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_target) 


# plot precision-recall curves
plot_precision_recall_curves(trained_models=trained_models,
                             model_names=mehtod_names, 
                             df_test=test_set, 
                             target=my_target) 

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_SMOTE[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target="failure")$table

get_confusion_matrix(trained_models_SMOTE[[which(methods=="rf")]],
                     df_test=test_set,
                     target="failure")$table

# get our metric
get_tpr_at_x_fpr(trained_models_SMOTE[[which(methods=="xgbTree")]],
             df_test=test_set,
             target="failure", x=0.1)

get_tpr_at_x_fpr(trained_models_SMOTE[[which(methods=="rf")]],
             df_test=test_set,
             target="failure", x=0.1)

# our metric didn't improve after SMOTE on binary target. Let's do ADASYN on 
# failure types to take into account variation in each subclass of failures


train_set_adasyn <- adasyn_for_multi_target(train_data = train_set, 
                                            target_col = "failure_type", 
                                            features_cols = my_features)

train_set_adasyn %>% 
  filter(failure=="Failure") %>%
  group_by(failure_type) %>% 
  summarize(n_failure = n()) %>% 
  mutate(failure_type=reorder(failure_type,n_failure)) %>%
  ggplot(aes(x=failure_type, y=n_failure)) + 
  geom_col(color = "steelblue", fill="blue3") + 
  labs(x = "Failure type",
       y = "Number of failures",
       title ="Failure type vs Number of failures after ADASYN") + 
  coord_flip()

# train the models based on new training set from ADASYN
trained_models_ADASYN <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set_adasyn, 
                       target=my_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

# plot the new ROC curves after ADASYN
plot_ROC_curves(trained_models=trained_models_ADASYN,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_target) 

