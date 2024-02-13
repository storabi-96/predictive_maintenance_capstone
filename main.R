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
library(UBL)
#library(MASS)
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
                        df_test[[target]])
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
  pred <- prediction(predicted_probs, df_test[[target]])
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
  pred <- prediction(predicted_probs, df_test[[target]])
  
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
plot_ROC_curves_cross_comparison <- function(trained_models_1, 
                                             trained_models_2,
                                             set_1_description,
                                             set_2_description,
                                             method_names,
                                             model_names, 
                                             df_test, target,
                                             df_test_2 = data.frame()) {
  
  # Create an empty plot
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "False Positive Rate", ylab = "True Positive Rate", 
       main = "ROC curves comparison")
  
  grid(col = "grey", lty = "dotted")
  legend_names <- character()
  
  if (nrow(df_test_2)==0){df_test_2 <- df_test}
  
  # Plot ROC curves for each model
  for (i in seq_along(method_names)) {
    

    
    perf_2 <- get_ROC_performance(trained_models_2[[i]], 
                                  df_test_2, 
                                  target)
    
    method_name <- trained_models_2[[i]][["method"]]
    perf_1 <- get_ROC_performance(trained_models_1[[which(methods==method_name)]], 
                                  df_test, 
                                  target)
    
    # Extract TPR and FPR values
    fpr_1 <- perf_1@x.values[[1]]
    tpr_1 <- perf_1@y.values[[1]]
    
    # Extract TPR and FPR values
    fpr_2 <- perf_2@x.values[[1]]
    tpr_2 <- perf_2@y.values[[1]]
    
    # Plot ROC curve
    lines(fpr_1, tpr_1, col = i, lwd = 2, lty=1)
    lines(fpr_2, tpr_2, col = i, lwd = 2, lty=2)
    
    legend_names <- c(legend_names, paste(set_1_description, model_names[i]))
    legend_names <- c(legend_names, paste(set_2_description, model_names[i]))
    
  }
  
  # Add legend
  legend("bottomright", legend = legend_names, 
         col = rep(seq_along(model_names), each = 2), 
         lwd = 2, lty = c(1, 2), cex = 0.8)
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
smote_for_binary_target <- function(train_data, target_col, positive_class,
                                    apprx_subclass_pop, features_cols) {
  # Extract features and target variable
  train_features <- train_data[, features_cols]
  train_target <- train_data[[target_col]]
  
  # find dupsize to pass into SMOTE function
  dupsize <- apprx_subclass_pop/table(train_data[[target_col]])[[positive_class]]
  
  # Perform SMOTE
  df_smote <- SMOTE(train_features, target = train_target, 
                    dup_size = dupsize)$data
  
  # rename target column to original name
  df_smote <- df_smote %>% 
    rename(!!target_col := class)
  
  df_smote
}

################################################################################
# Function to perform SMOTE on failure type, over sample based on type frequency
smote_for_failure_type <- function(train_data, target_col, positive_class, 
                                   subclass_type_col, features_cols,
                                   apprx_subclass_pop) {
  
  # Extract features and target variables
  train_features <- train_data[, features_cols]
  train_failure_type <- train_data[[subclass_type_col]]
  
  # Calculate the frequency of each Failure type
  failure_type_freq <- table(train_failure_type)
  
  # most common item in the failure type column (No Failure)
  majority_class <- names(which.max(failure_type_freq))
  
  # Find minority Failure types, exclude the most common one (No Failure)
  failure_types <- names(failure_type_freq[failure_type_freq != majority_class])
  
  # Perform SMOTE for each minority Failure type
  df_smote <- list()
  i <- 1
  for (failure_type in failure_types) {
    # Subset the data for the current Failure type
    subset_data <- train_data[train_failure_type == failure_type |
                                train_failure_type == majority_class, ]
    
    # find the correct dupsize based on the desired subclass population
    dupsize <- apprx_subclass_pop/failure_type_freq[[failure_type]]
    
    # Perform SMOTE on the subset
    smote_subset <- SMOTE(subset_data[, features_cols], 
                          target = subset_data[[target_col]],
                          dup_size = dupsize)
    
    # take the dataset with over-sampled minority and add a failure type column
    df_smote_subclss <- smote_subset$data
    df_smote_subclss[[subclass_type_col]] <- ifelse(
      df_smote_subclss$class == positive_class, 
      failure_type, majority_class)
    
    # Rename the class column to match the original target column name
    df_smote_subclss <- df_smote_subclss %>% 
      rename(!!target_col := class)
    
    # Combine SMOTE results for each Failure type
    df_smote[[i]] <- df_smote_subclss
    i <- i+1
  }
  
  # Combine SMOTE results for all minority Failure types
  df_smote <- do.call(rbind, df_smote)
  
  # remove duplicated rows due to majority class
  df_smote <- distinct(df_smote)
  
  # make both target columns factors again
  df_smote[[target_col]] <- as.factor(df_smote[[target_col]])
  df_smote[[subclass_type_col]] <- as.factor(df_smote[[subclass_type_col]])
  
  # return the final dataframe
  df_smote
}

################################################################################
# Function to perform DBSMOTE on failure type, over sample based on type frequency
DBSMOTE_for_failure_type <- function(train_data, target_col, positive_class, 
                                     subclass_type_col, features_cols,
                                     apprx_subclass_pop) {
  
  # Extract features and target variables
  train_features <- train_data[, features_cols]
  train_failure_type <- train_data[[subclass_type_col]]
  
  # Calculate the frequency of each Failure type
  failure_type_freq <- table(train_failure_type)
  
  # most common item in the failure type column (No Failure)
  majority_class <- names(which.max(failure_type_freq))
  
  # Find minority Failure types, exclude the most common one (No Failure)
  failure_types <- names(failure_type_freq[failure_type_freq != majority_class])
  
  # Perform SMOTE for each minority Failure type
  df_smote <- list()
  i <- 1
  for (failure_type in failure_types) {
    # Subset the data for the current Failure type
    subset_data <- train_data[train_failure_type == failure_type |
                                train_failure_type == majority_class, ]
    
    # find the correct dupsize based on the desired subclass population
    dupsize <- apprx_subclass_pop/failure_type_freq[[failure_type]]
    
    # Perform SMOTE on the subset
    smote_subset <- DBSMOTE(subset_data[, features_cols], 
                            target = subset_data[[target_col]],
                            dupSize = dupsize)
                            #K = 5,
                            #method="type2")
    
    # take the dataset with over-sampled minority and add a failure type column
    df_smote_subclss <- smote_subset$data
    df_smote_subclss[[subclass_type_col]] <- ifelse(
      df_smote_subclss$class == positive_class, 
      failure_type, majority_class)
    
    # Rename the class column to match the original target column name
    df_smote_subclss <- df_smote_subclss %>% 
      rename(!!target_col := class)
    
    # Combine SMOTE results for each Failure type
    df_smote[[i]] <- df_smote_subclss
    i <- i+1
  }
  
  # Combine SMOTE results for all minority Failure types
  df_smote <- do.call(rbind, df_smote)
  
  # remove duplicated rows due to majority class
  df_smote <- distinct(df_smote)
  
  # make both target columns factors again
  df_smote[[target_col]] <- as.factor(df_smote[[target_col]])
  df_smote[[subclass_type_col]] <- as.factor(df_smote[[subclass_type_col]])
  
  # return the final dataframe
  df_smote
}
################################################################################
# Function to perform ENN on failure type, in order to reduce noise in the data
apply_ENN <- function(df, target, features, 
                      k = 5, dist = "Euclidean") {
  
  df <- df[c(features, target)]
  
  # Prepare the formula
  formula <- as.formula(paste(target, "~", 
                              paste(features, collapse = " + ")))
  
  # Apply ENN
  res_enn <- ENNClassif(formula, df, k = k, dist = dist, Cl = "all")
  
  # Remove instances identified by ENN
  data_filtered <- df[-res_enn[[2]], ]
  
  data_filtered
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
  dplyr::select(-any_of(c("UDI", "failure", "failure_numeric",
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

# see what failure types are mentioned in the data set
unique(df_data$failure_type)

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

# 2 issues
# 1: "No Failure" in the plot
# 2: "Random Failures" not in the plot but in the data set (?)

########################## some data cleaning ##################################
################################################################################

# check if there are rows with recorded failure types but failure_numeric = 0
nrow(df_data %>% 
       filter(failure != "No Failure" & failure_numeric == 0))

# reassign correct failure_numeric to the problematic rows
df_data <- df_data %>% 
  mutate(failure_numeric = ifelse(failure_type=="No Failure", 0, 1))

# Make sure failure type is No Failure for rows that have failure_numeric = 0
# this would eliminate "random failures"
df_data <- df_data %>% 
  mutate(failure_type = ifelse(failure=="No Failure", 
                               "No Failure", 
                               failure_type))

# convert categorical features into factors instead of strings
df_data$type <- as.factor(df_data$type)
df_data$failure <- as.factor(df_data$failure)
df_data$failure_type <- as.factor(df_data$failure_type)

################################################################################
# one_hot encoding the categorical type column 
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
  dplyr::select(-any_of(c("UDI", "type", "failure",
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

################################################################################

# let's do a multi-class classification to see which failure instances we are 
# failing to detect
my_target   <- "failure_type"
my_binary_target   <- "failure"

# train the models in lapply
trained_models_multiclass <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set, 
                       target=my_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

get_confusion_matrix(trained_models_multiclass[[which(methods=="rf")]],
                     df_test=test_set,
                     target="failure_type")$table

get_confusion_matrix(trained_models_multiclass[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target="failure_type")$table

plot_ROC_curves(trained_models=trained_models_multiclass,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_binary_target) 


plot_ROC_curves_cross_comparison(trained_models_1 = trained_models, 
                                 trained_models_2 = trained_models_multiclass,
                                 set_1_description = "binary",
                                 set_2_description = "multiclass",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, target=my_binary_target)


get_tpr_at_x_fpr(trained_models_multiclass[[which(methods=="xgbTree")]],
                 df_test=test_set, 
                 target="failure", x=0.1)

get_tpr_at_x_fpr(trained_models_multiclass[[which(methods=="rf")]],
                 df_test=test_set, 
                 target="failure", x=0.1)

get_tpr_at_x_fpr(trained_models_multiclass[[which(methods=="LogitBoost")]],
                 df_test=test_set, 
                 target="failure", x=0.1)

# biggest problem is in tool wear failure. it has less samples in our data. 
# Let's do oversampling. Also seems like the models don't confuse different 
# types of failures

################################################################################
# SMOTE builds "bridges" between different minority points. This can be pretty 
# problematic if we have multiple minority sub-classes. SMOTE based on each 
# class would be a better option. 

my_multiclass_target <- "failure_type"
my_binary_target <- "failure"
positive_class <- "Failure"
n_subclass_synth <- 500

train_set_subclass_smote <- smote_for_failure_type(
  train_data=train_set, 
  target_col=my_binary_target, 
  positive_class=positive_class,
  subclass_type_col=my_multiclass_target, 
  features_cols=my_features,
  apprx_subclass_pop=n_subclass_synth)

# see number of different types of failure
train_set_subclass_smote %>% 
  filter(failure_type != "No Failure") %>%
  group_by(failure_type) %>% 
  summarize(n_failure = n()) %>% 
  mutate(failure_type=reorder(failure_type,n_failure)) %>%
  ggplot(aes(x=failure_type, y=n_failure)) + 
  geom_col(color = "steelblue", fill="blue3") + 
  labs(x = "Failure type",
       y = "Number of failures",
       title ="Failure type vs Number of failures") + 
  coord_flip()

# show the resulting distribution of target column after SMOTE
table(train_set$failure)
table(train_set_subclass_smote$failure)

# train the models based on new training set from SMOTE
trained_models_subclass_SMOTE <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set_subclass_smote, 
                       target=my_multiclass_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

# plot the new ROC curves after SMOTE
plot_ROC_curves(trained_models=trained_models_subclass_SMOTE,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_binary_target) 

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_subclass_SMOTE[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

get_confusion_matrix(trained_models_subclass_SMOTE[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

# get our metric
get_tpr_at_x_fpr(trained_models_subclass_SMOTE[[which(methods=="xgbTree")]],
                 df_test=test_set,
                 target=my_binary_target, x=0.1)

get_tpr_at_x_fpr(trained_models_subclass_SMOTE[[which(methods=="rf")]],
                 df_test=test_set,
                 target=my_binary_target, x=0.1)

plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_multiclass, 
                                 trained_models_2 = trained_models_subclass_SMOTE,
                                 set_1_description = "multiclass",
                                 set_2_description = "multiclass SMOTE",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, target=my_binary_target)

################################################################################
# NEXT: do density based smote (DBSMOTE) which doesn't let bridges 
# between main minority cluster and a random minority point.
my_multiclass_target <- "failure_type"
my_binary_target <- "failure"
positive_class <- "Failure"
n_subclass_synth <- 500

train_set_subclass_DBSMOTE <- DBSMOTE_for_failure_type(
  train_data=train_set, 
  target_col=my_binary_target, 
  positive_class = positive_class,
  subclass_type_col=my_multiclass_target, 
  features_cols=my_features,
  apprx_subclass_pop=n_subclass_synth)

# see number of different types of failure
train_set_subclass_DBSMOTE %>% 
  filter(failure_type != "No Failure") %>%
  group_by(failure_type) %>% 
  summarize(n_failure = n()) %>% 
  mutate(failure_type=reorder(failure_type,n_failure)) %>%
  ggplot(aes(x=failure_type, y=n_failure)) + 
  geom_col(color = "steelblue", fill="blue3") + 
  labs(x = "Failure type",
       y = "Number of failures",
       title ="Failure type vs Number of failures") + 
  coord_flip()

# show the resulting distribution of target column after SMOTE
table(train_set$failure)
table(train_set_subclass_DBSMOTE$failure)

# train the models based on new training set from SMOTE
trained_models_subclass_DBSMOTE <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set_subclass_DBSMOTE, 
                       target=my_multiclass_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

# plot the new ROC curves after DBSMOTE
plot_ROC_curves(trained_models=trained_models_subclass_DBSMOTE,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_binary_target) 

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_subclass_DBSMOTE[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

get_confusion_matrix(trained_models_subclass_DBSMOTE[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

# get our metric
get_tpr_at_x_fpr(trained_models_subclass_DBSMOTE[[which(methods=="xgbTree")]],
                 df_test=test_set,
                 target=my_binary_target, x=0.1)

get_tpr_at_x_fpr(trained_models_subclass_DBSMOTE[[which(methods=="rf")]],
                 df_test=test_set,
                 target=my_binary_target, x=0.1)

plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_multiclass, 
                                 trained_models_2 = trained_models_subclass_DBSMOTE,
                                 set_1_description = "multiclass",
                                 set_2_description = "multiclass DBSMOTE",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, target=my_binary_target)

################################################################################
# Applying ENN on DBSMOTE database before training to reduce noise

my_multiclass_target <- "failure_type"
my_binary_target <- "failure"
positive_class <- "Failure"

# apply ENN
train_set_subclass_DBSMOTE_ENN <- apply_ENN(df=train_set_subclass_DBSMOTE,
                                            target=my_multiclass_target,
                                            features=my_features)

# show the resulting distribution of target column after SMOTE
table(train_set_subclass_DBSMOTE$failure_type)
table(train_set_subclass_DBSMOTE_ENN$failure_type)

# see number of different types of failure
train_set_subclass_DBSMOTE_ENN %>% 
  filter(failure_type != "No Failure") %>%
  group_by(failure_type) %>% 
  summarize(n_failure = n()) %>% 
  mutate(failure_type=reorder(failure_type,n_failure)) %>%
  ggplot(aes(x=failure_type, y=n_failure)) + 
  geom_col(color = "steelblue", fill="blue3") + 
  labs(x = "Failure type",
       y = "Number of failures",
       title ="Failure type vs Number of failures") + 
  coord_flip()

# train models
trained_models_subclass_DBSMOTE_ENN <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set_subclass_DBSMOTE_ENN, 
                       target=my_multiclass_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

# plot the new ROC curves after DBSMOTE_ENN
plot_ROC_curves(trained_models=trained_models_subclass_DBSMOTE_ENN,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_binary_target) 

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_subclass_DBSMOTE_ENN[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

get_confusion_matrix(trained_models_subclass_DBSMOTE_ENN[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

# get our metric
get_tpr_at_x_fpr(trained_models_subclass_DBSMOTE_ENN[[which(methods=="xgbTree")]],
                 df_test=test_set,
                 target=my_binary_target, x=0.1)

get_tpr_at_x_fpr(trained_models_subclass_DBSMOTE_ENN[[which(methods=="rf")]],
                 df_test=test_set,
                 target=my_binary_target, x=0.1)


# compare with original
plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_multiclass, 
                                 trained_models_2 = trained_models_subclass_DBSMOTE_ENN,
                                 set_1_description = "multiclass",
                                 set_2_description = "multiclass DBSMOTE+ENN",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, target=my_binary_target)

################################################################################
################################################################################
# extract features and target
train_set_pre_LDA <- train_set[c(my_features,my_multiclass_target)]  

# apply LDA
lda_result <- MASS::lda(failure_type ~ ., data = train_set_pre_LDA)

# access LDA results
lda_result$scaling  # Linear discriminant coefficients


train_set_LDA <- data.frame(predict(lda_result, train_set_pre_LDA)$x)
train_set_LDA[[my_binary_target]] <- train_set[[my_binary_target]]
train_set_LDA[[my_multiclass_target]] <- train_set[[my_multiclass_target]]


# transform the test set with the same coefficient
test_set_LDA <- data.frame(predict(lda_result, test_set)$x)
test_set_LDA[[my_binary_target]] <- test_set[[my_binary_target]]
test_set_LDA[[my_multiclass_target]] <- test_set[[my_multiclass_target]]


#test_set_LDA %>% ggplot(aes(LD1, LD2, color=failure)) + geom_point(alpha=0.5)

# redefine methods
new_methods      <- c("rf", "xgbTree",  "LogitBoost")
new_mehtod_names <- c("Random forest","XGBoost", "Boosted logistic regression")

# features to use for training
my_features_LDA <- c("LD1", "LD2", "LD3", "LD4")



trained_models_subclass_LDA <- lapply(new_methods, function(method){
  print(method)
  model <- train_model(df_training=train_set_LDA, 
                       target=my_multiclass_target,
                       features=my_features_LDA, 
                       method=method,
                       control=ctrl)
  model
})

# plot the new ROC curves 
plot_ROC_curves(trained_models=trained_models_subclass_LDA,
                model_names=new_methods, 
                df_test=test_set_LDA, 
                target=my_binary_target) 

# show confusion matrix for XGboost and random forest
trained_models_subclass_LDA[[1]][["method"]]
get_confusion_matrix(trained_models_subclass_LDA[[1]],
                     df_test=test_set_LDA,
                     target=my_multiclass_target)$table

trained_models_subclass_LDA[[2]][["method"]]
get_confusion_matrix(trained_models_subclass_LDA[[2]],
                     df_test=test_set_LDA,
                     target=my_multiclass_target)$table

trained_models_subclass_LDA[[3]][["method"]]
get_confusion_matrix(trained_models_subclass_LDA[[3]],
                     df_test=test_set_LDA,
                     target=my_multiclass_target)$table

# get our metric
trained_models_subclass_LDA[[1]][["method"]]
get_tpr_at_x_fpr(trained_models_subclass_LDA[[1]],
                 df_test=test_set_LDA,
                 target=my_binary_target, x=0.1)

trained_models_subclass_LDA[[2]][["method"]]
get_tpr_at_x_fpr(trained_models_subclass_LDA[[2]],
                 df_test=test_set_LDA,
                 target=my_binary_target, x=0.1)


plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_multiclass, 
                                 trained_models_2 = trained_models_subclass_LDA,
                                 set_1_description = "multiclass",
                                 set_2_description = "multiclass LDA space",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, df_test_2 = test_set_LDA,
                                 target=my_binary_target)

################################################################################
################################################################################
train_set <- train_set %>%
  mutate(strain=wear*torque, delta_T= process_t-air_t, power=rpm*torque)

test_set <- test_set %>%
  mutate(strain=wear*torque, delta_T= process_t-air_t, power=rpm*torque)


# methods and respective names to train models
methods      <- c("rpart", "rf", "lda2","pda2", "nb", "xgbTree",  "LogitBoost")
mehtod_names <- c("Decision tree", "Random forest", "LDA", "Penalized DA",
                  "Naive Bayes", "XGBoost", "Boosted logistic regression")

# features to use for training and target to predict
my_features <- c("delta_T", "power", "strain", "rpm", "torque", 
                 "wear", "type_H", "type_L", "type_M")

my_target   <- "failure_type"



# train the models in lapply
trained_models_subclass_physical_features <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set, 
                       target=my_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

# plot ROC curves
plot_ROC_curves(trained_models=trained_models_subclass_physical_features,
                model_names=mehtod_names, 
                df_test=test_set, 
                target="failure") 

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_subclass_physical_features[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target="failure_type")$table

get_confusion_matrix(trained_models_subclass_physical_features[[which(methods=="rf")]],
                     df_test=test_set,
                     target="failure_type")$table

# get our metric
get_tpr_at_x_fpr(trained_models_subclass_physical_features[[which(methods=="xgbTree")]],
                 df_test=test_set, 
                 target="failure", x=0.1)

get_tpr_at_x_fpr(trained_models_subclass_physical_features[[which(methods=="rf")]],
                 df_test=test_set,
                 target="failure", x=0.1)

# ensemble next!












