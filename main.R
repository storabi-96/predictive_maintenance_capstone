# installing the libraries
if(!require(tidyverse)) install.packages("tidyverse" , repos = "http://cran.us.r-project.org")
if(!require(caret    )) install.packages("caret"     , repos = "http://cran.us.r-project.org")
if(!require(corrplot )) install.packages("corrplot"  , repos = "http://cran.us.r-project.org")
if(!require(GGally   )) install.packages("GGally"    , repos = "http://cran.us.r-project.org")
if(!require(ROCR     )) install.packages("ROCR"      , repos = "http://cran.us.r-project.org")
if(!require(klaR     )) install.packages("klaR"      , repos = "http://cran.us.r-project.org")
if(!require(xgboost  )) install.packages("xgboost"   , repos = "http://cran.us.r-project.org")
if(!require(pROC     )) install.packages("pROC"      , repos = "http://cran.us.r-project.org")

library(corrplot)
library(tidyverse)
library(caret)
library(GGally)
library(ROCR)
library(pROC)

################################################################################
############### Defining functions to be used in script#########################
################################################################################

get_ROC_performance <- function(model, df_test, target) {
  # get probabilities
  predicted_probs <- predict(model, newdata = df_test, type = "prob")[, "Failure"]
  pred <- prediction(predicted_probs, as.factor(df_test[[target]]))
  # get ROC
  perf <- performance(pred, "tpr", "fpr")
  return(perf)
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
    tpr <- perf@x.values[[1]]
    fpr <- perf@y.values[[1]]
    
    # Plot ROC curve
    lines(fpr, tpr, col = i, lwd = 2)
  }
  
  # Add legend
  legend("bottomright", legend = model_names, 
         col = seq_along(trained_models), lwd = 2, cex = 0.8)
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

# find number of failures and non failures in the data
table(df_data$failure)
# crate the failure label column form 0 and 1 ==> no failure and failure
df_data <- df_data %>%
  mutate(failure = ifelse(failure_numeric==1, "Failure", "No Failure"))

# show the head of data set after modifications
head(df_data)

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


# select columns for correlations plot
df_corr <- df_data %>% 
  select(-any_of(c("UDI", "type", "failure",
                   "product_id", "failure_type")))

# plot the correlations using spearman method
ggcorr(df_corr, label = TRUE, label_round=3, 
       method=c("pairwise", "spearman"),
       low = "brown2", mid = "lightgrey", high = "steelblue")

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
methods      <- c("rpart", "rf", "lda2",  "qda", "nb", "xgbTree",  "LogitBoost")
mehtod_names <- c("Decision tree", "Random forest", "LDA", "QDA", "Naive Bayes", 
                  "XGBoost", "Boosted logistic regression")

# features to use for training and target to predict
my_features <- c("air_t", "process_t", "rpm", "torque", "wear", "type")
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


get_confusion_matrix(trained_models[[which(methods=="xgbTree")]],
                     df_test=test_set,
                     target="failure")

get_confusion_matrix(trained_models[[which(methods=="rf")]],
                     df_test=test_set,
                     target="failure")
