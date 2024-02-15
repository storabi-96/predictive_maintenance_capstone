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
library(MASS)
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
plot_ROC_curves <- function(trained_models, model_names, df_test, target, title) {
  
  # Create an empty plot
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "False Positive Rate", ylab = "True Positive Rate", 
       main = title)
  
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

  
  # create a dictionary using setNames()
  method_model_dictionary <- setNames(model_names, method_names)
  
  # Create an empty plot
  plot(0, 0, type = "n", xlim = c(0, 1), ylim = c(0, 1), 
       xlab = "False Positive Rate", ylab = "True Positive Rate", 
       main = "ROC curves comparison")
  
  grid(col = "grey", lty = "dotted")
  legend_names <- character()
  
  # use same test set if another one is not given
  if (nrow(df_test_2)==0){df_test_2 <- df_test}
  
  color_index <- 1
  # plot ROC curves for the methods which are in "method_names" argument
  for (i in seq(1, length(trained_models_1))) {
    
    method_name_1 <- trained_models_1[[i]][["method"]]
    
    for (ii in seq(1, length(trained_models_2))) {
      
      method_name_2 <- trained_models_2[[ii]][["method"]]
      
      if (method_name_1==method_name_2 & method_name_2 %in% method_names){
        
        perf_1 <- get_ROC_performance(trained_models_1[[i]], 
                                      df_test, 
                                      target)
        
        perf_2 <- get_ROC_performance(trained_models_2[[ii]], 
                                      df_test_2, 
                                      target)
        
        # extract TPR and FPR values
        fpr_1 <- perf_1@x.values[[1]]
        tpr_1 <- perf_1@y.values[[1]]
        fpr_2 <- perf_2@x.values[[1]]
        tpr_2 <- perf_2@y.values[[1]]
        
        # plot ROC curve
        lines(fpr_1, tpr_1, col = color_index, lwd = 2, lty=1)
        lines(fpr_2, tpr_2, col = color_index, lwd = 2, lty=2)
        
        # make the right legends
        legend_names <- c(legend_names, 
                          paste(set_1_description, 
                            method_model_dictionary[[method_name_1]]))
        
        legend_names <- c(legend_names, 
                          paste(set_2_description, 
                            method_model_dictionary[[method_name_1]]))
        
        color_index <- color_index + 1
        }
      }
    }

  # Add legend
  legend("bottomright", legend = legend_names, 
         col = rep(seq_along(model_names), each = 2), 
         lwd = 2, lty = c(1, 2), cex = 0.8)
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
  #failure_types <- names(failure_type_freq[failure_type_freq != majority_class])
  failure_types <- setdiff(names(failure_type_freq), c(majority_class))
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
  #failure_types <- names(failure_type_freq[failure_type_freq != majority_class])
  failure_types <- setdiff(names(failure_type_freq), c(majority_class))
  
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
# converting multiclass classification to binary classification
conver_to_binary_confusion_matrix <- function(multiclass_confusion_matrix){
  
  # Extract true negatives (TN)
  TN <- multiclass_confusion_matrix["No Failure", "No Failure"]
  
  # Calculate false positives (FP)
  FP <- sum(multiclass_confusion_matrix[, "No Failure"][-2])
  
  # Calculate false negatives (FN)
  FN <-  sum(multiclass_confusion_matrix["No Failure", -2 ])
  
  # Calculate true positives (TP)
  TP <- sum(diag(multiclass_confusion_matrix)) - TN
  
  # Construct the binary confusion matrix
  binary_confusion_matrix <- matrix(c(TP, FP, FN, TN), nrow = 2, byrow = TRUE,
                                    dimnames = list(c("Predicted Failure", 
                                                      "Predicted No Failure"),
                                                    c("Failure", "No Failure")))
  binary_confusion_matrix
}
################################################################################
# calculates f1 score after converting multiclass classification to binary
get_f1_score<- function(binary_confusion_matrix){
  
  precision <- binary_confusion_matrix[1,1]/(binary_confusion_matrix[1,1] + binary_confusion_matrix[1,2])
  recall <- binary_confusion_matrix[1,1]/(binary_confusion_matrix[1,1] + binary_confusion_matrix[2,1])
  F1 <- 2 * ( (precision*recall) / (precision+recall) )
  F1
}
################################################################################
# function hard voting ensemble 
predict_ensemble <- function(models, ensemble_methods, new_data) {
  
  predictions <- sapply(ensemble_methods, function(method) {
    predict(models[[which(methods==method)]], new_data)
  })
  
  # Combine predictions using majority voting
  majority_vote <- apply(predictions, 1, function(row) {
    names(which.max(table(row)))
  })
  majority_vote
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
  labs(title ="Feature pair-wise relations grid plot") + 
  theme(axis.text.x = element_text(angle = 90,))

# show pair plot (attention: plot takes a bit of time to complete)
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
################################################################################
########################## some data cleaning ##################################
################################################################################

# reassign correct failure_numeric to the problematic rows
df_data <- df_data %>% 
  mutate(failure_numeric = ifelse(failure_type=="No Failure", 0, 1),
         failure = ifelse(failure_type=="No Failure", "No Failure", failure))

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
set.seed(1)

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
trained_models_binary <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set, 
              target=my_target,
              features=my_features, 
              method=method,
              control=ctrl)
  model
})

# plot ROC curves
plot_ROC_curves(trained_models=trained_models_binary,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_target) 

###
# get our metric
tpr_at_10fpr <- sapply(methods, function(method){
  
  tprs <- get_tpr_at_x_fpr(trained_models_binary[[which(methods==method)]],
                           df_test=test_set, 
                           target=my_target, x=0.1)
  round(tprs, 4)
  
})

# create the table
tpr_table <- matrix(unname(tpr_at_10fpr), nrow = 1, byrow = TRUE)
rownames(tpr_table) <- c("binary classification")
colnames(tpr_table) <- names(tpr_at_10fpr)
tpr_table

###
# get f1-scores
f1_scores <- sapply(methods, function(method){
  
  f1 <- get_f1_score(
    get_confusion_matrix(
      trained_models_binary[[which(methods==method)]],
      df_test=test_set,
      target="failure")$table)
  round(f1, 4)
  
})

# create the table
f1_table <- matrix(unname(f1_scores), nrow = 1, byrow = TRUE)
rownames(f1_table) <- c("binary classification")
colnames(f1_table) <- names(f1_scores)
f1_table

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_binary[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_target)$table

get_confusion_matrix(trained_models_binary[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_target)$table
################################################################################

# let's do a multi-class classification to see which failure instances we are 
# failing to detect
my_multiclass_target   <- "failure_type"
my_binary_target   <- "failure"

# train the models in lapply
trained_models_multiclass <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set, 
                       target=my_multiclass_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

plot_ROC_curves(trained_models=trained_models_multiclass,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_binary_target) 

##
# get new tpr scores at 10% fpr
new_tpr_row <- unname(sapply(methods, function(method){
    tpr <- get_tpr_at_x_fpr(trained_models_multiclass[[which(methods==method)]],
                       df_test=test_set, 
                       target=my_binary_target, x=0.1)
    round(tpr, 4)}))

# add them to the table
tpr_table <- rbind(tpr_table, new_tpr_row)    
rownames(tpr_table)[nrow(tpr_table)] <- "multiclass classification"
tpr_table

##
# get new f1-scores
new_f1_row <- unname(
  sapply(methods, function(method){
  f1 <- get_f1_score(
    conver_to_binary_confusion_matrix(
    get_confusion_matrix(
      trained_models_multiclass[[which(methods==method)]],
      df_test=test_set,
      target=my_multiclass_target)$table))
  round(f1, 4)}))

# add them to the table
f1_table <- rbind(f1_table, new_f1_row)    
rownames(f1_table)[nrow(f1_table)] <- "multiclass classification"
f1_table

plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_binary, 
                                 trained_models_2 = trained_models_multiclass,
                                 set_1_description = "binary",
                                 set_2_description = "multiclass",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, target=my_binary_target)

get_confusion_matrix(trained_models_multiclass[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

get_confusion_matrix(trained_models_multiclass[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

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

# show the resulting distribution of target column after SMOTE
table(train_set$failure_type)
table(train_set_subclass_smote$failure_type)

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

##
# get new tpr scores at 10% fpr
new_tpr_row <- unname(sapply(methods, function(method){
  tpr <- get_tpr_at_x_fpr(trained_models_subclass_SMOTE[[which(methods==method)]],
                          df_test=test_set, 
                          target=my_binary_target, x=0.1)
  round(tpr, 4)}))

# add them to the table
tpr_table <- rbind(tpr_table, new_tpr_row)    
rownames(tpr_table)[nrow(tpr_table)] <- "multiclass + SMOTE"
tpr_table

##
# get new f1-scores
new_f1_row <- unname(
  sapply(methods, function(method){
    f1 <- get_f1_score(
      conver_to_binary_confusion_matrix(
        get_confusion_matrix(
          trained_models_subclass_SMOTE[[which(methods==method)]],
          df_test=test_set,
          target=my_multiclass_target)$table))
    round(f1, 4)}))

# add them to the table
f1_table <- rbind(f1_table, new_f1_row)    
rownames(f1_table)[nrow(f1_table)] <- "multiclass + SMOTE"
f1_table


plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_multiclass, 
                                 trained_models_2 = trained_models_subclass_SMOTE,
                                 set_1_description = "multiclass",
                                 set_2_description = "multiclass SMOTE",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, target=my_binary_target)


# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_subclass_SMOTE[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

get_confusion_matrix(trained_models_subclass_SMOTE[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table


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


# show the resulting distribution of target column after SMOTE
table(train_set$failure_type)
table(train_set_subclass_DBSMOTE$failure_type)

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

##
# get new tpr scores at 10% fpr
new_tpr_row <- unname(sapply(methods, function(method){
  tpr <- get_tpr_at_x_fpr(trained_models_subclass_DBSMOTE[[which(methods==method)]],
                          df_test=test_set, 
                          target=my_binary_target, x=0.1)
  round(tpr, 4)}))

# add them to the table
tpr_table <- rbind(tpr_table, new_tpr_row)    
rownames(tpr_table)[nrow(tpr_table)] <- "multiclass + DBSMOTE"
tpr_table

##
# get new f1-scores
new_f1_row <- unname(
  sapply(methods, function(method){
    f1 <- get_f1_score(
      conver_to_binary_confusion_matrix(
        get_confusion_matrix(
          trained_models_subclass_DBSMOTE[[which(methods==method)]],
          df_test=test_set,
          target=my_multiclass_target)$table))
    round(f1, 4)}))

# add them to the table
f1_table <- rbind(f1_table, new_f1_row)    
rownames(f1_table)[nrow(f1_table)] <- "multiclass + DBSMOTE"
f1_table

plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_multiclass, 
                                 trained_models_2 = trained_models_subclass_DBSMOTE,
                                 set_1_description = "multiclass",
                                 set_2_description = "multiclass DBSMOTE",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, target=my_binary_target)

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_subclass_DBSMOTE[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

get_confusion_matrix(trained_models_subclass_DBSMOTE[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

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

##
# get new tpr scores at 10% fpr
new_tpr_row <- unname(sapply(methods, function(method){
  tpr <- get_tpr_at_x_fpr(trained_models_subclass_DBSMOTE_ENN[[which(methods==method)]],
                          df_test=test_set, 
                          target=my_binary_target, x=0.1)
  round(tpr, 4)}))

# add them to the table
tpr_table <- rbind(tpr_table, new_tpr_row)    
rownames(tpr_table)[nrow(tpr_table)] <- "multiclass + DBSMOTE/ENN"
tpr_table

##
# get new f1-scores
new_f1_row <- unname(
  sapply(methods, function(method){
    f1 <- get_f1_score(
      conver_to_binary_confusion_matrix(
        get_confusion_matrix(
          trained_models_subclass_DBSMOTE_ENN[[which(methods==method)]],
          df_test=test_set,
          target=my_multiclass_target)$table))
    round(f1, 4)}))

# add them to the table
f1_table <- rbind(f1_table, new_f1_row)    
rownames(f1_table)[nrow(f1_table)] <- "multiclass + DBSMOTE/ENN"
f1_table

# compare with original
plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_multiclass, 
                                 trained_models_2 = trained_models_subclass_DBSMOTE_ENN,
                                 set_1_description = "multiclass",
                                 set_2_description = "multiclass DBSMOTE+ENN",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set, target=my_binary_target)

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_subclass_DBSMOTE_ENN[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

get_confusion_matrix(trained_models_subclass_DBSMOTE_ENN[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

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


test_set_LDA %>% ggplot(aes(LD1, LD2, color=failure)) + geom_point(alpha=0.5)

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

##
# get new tpr scores at 10% fpr
new_tpr_row <- unname(sapply(seq_along(new_methods), function(i){

  tpr <- get_tpr_at_x_fpr(trained_models_subclass_LDA[[i]],
                          df_test=test_set_LDA, 
                          target=my_binary_target, x=0.1)
  round(tpr, 4)}))

# NA for the missing methods
new_tpr_row_modified <- rep(NA, length(methods))
new_tpr_row_modified[methods %in% new_methods] <- new_tpr_row

# add them to the table
tpr_table <- rbind(tpr_table, new_tpr_row_modified)    
rownames(tpr_table)[nrow(tpr_table)] <- "multiclass in LD space"
tpr_table

##
# get new f1-scores
new_f1_row <- unname(
  sapply(seq_along(new_methods), function(i){
    f1 <- get_f1_score(
      conver_to_binary_confusion_matrix(
        get_confusion_matrix(
          trained_models_subclass_DBSMOTE_ENN[[i]],
          df_test=test_set,
          target=my_multiclass_target)$table))
    round(f1, 4)}
    )
  )

# NA for the missing methods
new_f1_row_modified <- rep(NA, length(methods))
new_f1_row_modified[methods %in% new_methods] <- new_f1_row

# add them to the table
f1_table <- rbind(f1_table, new_f1_row_modified)    
rownames(f1_table)[nrow(f1_table)] <- "multiclass in LD space"
f1_table

# show confusion matrix for XGboost and random forest
trained_models_subclass_LDA[[1]][["method"]]
get_confusion_matrix(trained_models_subclass_LDA[[1]],
                     df_test=test_set_LDA,
                     target=my_multiclass_target)$table

trained_models_subclass_LDA[[2]][["method"]]
get_confusion_matrix(trained_models_subclass_LDA[[2]],
                     df_test=test_set_LDA,
                     target=my_multiclass_target)$table

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
# define new features which make physical sense
train_set <- train_set %>%
  mutate(strain=wear*torque, # strain on the machine
         delta_T= process_t-air_t, # temperature difference
         power=rpm*torque) # power 

# define the same features for the test set
test_set <- test_set %>%
  mutate(strain=wear*torque, 
         delta_T= process_t-air_t, 
         power=rpm*torque)


# methods and respective names to train models
methods      <- c("rpart", "rf", "lda2","pda2", "nb", "xgbTree",  "LogitBoost")
mehtod_names <- c("Decision tree", "Random forest", "LDA", "Penalized DA",
                  "Naive Bayes", "XGBoost", "Boosted logistic regression")

# features to use for training and target to predict
my_features <- c("delta_T", "power", "strain", "rpm", "torque", 
                 "wear", "type_H", "type_L", "type_M")

my_multiclass_target   <- "failure_type"

# train the models in lapply
trained_models_subclass_physical_features <- lapply(methods, function(method){
  print(method)
  model <- train_model(df_training=train_set, 
                       target=my_multiclass_target,
                       features=my_features, 
                       method=method,
                       control=ctrl)
  model
})

# plot ROC curves
plot_ROC_curves(trained_models=trained_models_subclass_physical_features,
                model_names=mehtod_names, 
                df_test=test_set, 
                target=my_binary_target) 

##
# get new tpr scores at 10% fpr
new_tpr_row <- unname(sapply(methods, function(method){
  tpr <- get_tpr_at_x_fpr(trained_models_subclass_physical_features[[which(methods==method)]],
                          df_test=test_set, 
                          target=my_binary_target, x=0.1)
  round(tpr, 4)}))

# add them to the table
tpr_table <- rbind(tpr_table, new_tpr_row)    
rownames(tpr_table)[nrow(tpr_table)] <- "multiclass with feature engineering"
tpr_table

##
# get new f1-scores
new_f1_row <- unname(
  sapply(methods, function(method){
    f1 <- get_f1_score(
      conver_to_binary_confusion_matrix(
        get_confusion_matrix(
          trained_models_subclass_physical_features[[which(methods==method)]],
          df_test=test_set,
          target=my_multiclass_target)$table))
    round(f1, 4)}))

# add them to the table
f1_table <- rbind(f1_table, new_f1_row)    
rownames(f1_table)[nrow(f1_table)] <- "multiclass with feature engineering"
f1_table


# compare the ROC curves of new approach to the ones of original multiclass models
plot_ROC_curves_cross_comparison(trained_models_1 = trained_models_multiclass, 
                                 trained_models_2 = trained_models_subclass_physical_features,
                                 set_1_description = "multiclass",
                                 set_2_description = "multiclass physical features",
                                 method_names = c("rf","xgbTree"),
                                 model_names = c("Random forest", "XGBoost"), 
                                 df_test=test_set,
                                 target=my_binary_target)

# show confusion matrix for XGboost and random forest
get_confusion_matrix(trained_models_subclass_physical_features[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

get_confusion_matrix(trained_models_subclass_physical_features[[which(methods=="rf")]],
                     df_test=test_set,
                     target=my_multiclass_target)$table

################################################################################
################################################################################
# Ensemble 

# models to be used in ensemble
ensemble_methods <- c("rf","xgbTree", "LogitBoost")

# predict based on voting of models in ensemble
ensemble_predictions <- predict_ensemble(
  models=trained_models_subclass_physical_features, 
  ensemble_methods=ensemble_methods,
  new_data=test_set)

# make sure ensemble output as same factor levels
ensemble_predictions <- factor(ensemble_predictions, 
                               levels = c(levels(test_set$failure_type)))

# make confusion table for multiclass
ensemble_prediction_table <- table(ensemble_predictions, test_set$failure_type)

# convert multiclass to binary confusion matrix
binary_confusion_matrix <- conver_to_binary_confusion_matrix(ensemble_prediction_table)
binary_confusion_matrix

# calculate f1-score
get_f1_score(binary_confusion_matrix)


# plot F1-scores
max_F1s <- apply(f1_table, 1, max, na.rm = TRUE)
max_F1s <- unname(max_F1s)
max_F1s <- c(max_F1s, get_f1_score(binary_confusion_matrix))

plot(0, 0, type = "n", xlim = c(1, 8), ylim = c(0, 1), 
     xlab = "Iteration", ylab = "F1-score", 
     main = "Evolution of F1-scores for the best performing model in each iteration")
grid(col = "grey", lty = "dotted")
lines(max_F1s, col = "blue3", lwd = 2)


