# installing the libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret))     install.packages("caret"    , repos = "http://cran.us.r-project.org")
if(!require(ggExtra))   install.packages("ggExtra"  , repos = "http://cran.us.r-project.org")
if(!require(corrplot))  install.packages("corrplot" , repos = "http://cran.us.r-project.org")
if(!require(GGally))    install.packages("GGally"   , repos = "http://cran.us.r-project.org")
if(!require(pROC))      install.packages("pROC"     , repos = "http://cran.us.r-project.org")
if(!require(kernlab))   install.packages("kernlab"     , repos = "http://cran.us.r-project.org")


library(corrplot)
library(tidyverse)
library(caret)
library(ggExtra)
library(GGally)
library(pROC)

################################################################################
################################################################################
################################################################################

create_ROC_curve <- function(actual, predicted_probabilities) {
  # Calculate true positive rate (TPR) and false positive rate (FPR) for various thresholds
  thresholds <- seq(0, 1, by = 0.01)
  TPR <- numeric(length(thresholds))
  FPR <- numeric(length(thresholds))
  for (i in seq_along(thresholds)) {
    threshold <- thresholds[i]
    predicted <- ifelse(predicted_probabilities > threshold, 1, 0)
    confusion <- table(predicted, actual)
    if (sum(dim(confusion)) == 4) { # Both classes present in confusion matrix
      TPR[i] <- confusion[2, 2] / (confusion[2, 2] + confusion[2, 1])
      FPR[i] <- confusion[1, 2] / (confusion[1, 2] + confusion[1, 1])
    } else { # One class missing in confusion matrix
      TPR[i] <- NA
      FPR[i] <- NA
    }
  }
  
  # Remove NA values
  TPR <- TPR[!is.na(TPR)]
  FPR <- FPR[!is.na(FPR)]
  
  # Plot ROC curve
  plot(FPR, TPR, type = "l", lwd = 2, col = "blue", xlab = "False Positive Rate", ylab = "True Positive Rate",
       main = "Receiver Operating Characteristic (ROC) Curve")
  abline(0, 1, col = "red") # Add diagonal line for reference
  text(0.5, 0.5, paste("AUC =", round(trapz(TPR, FPR), 3)), adj = c(0.5, 0.5), col = "red")
}

################################################################################
################################################################################
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
n_failure <- sum(df_data %>% 
                   filter(failure_numeric==1) %>% 
                   pull(failure_numeric))
n_no_failure <- nrow(df_data) - n_failure

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
  theme(axis.text.x = element_text(angle = 90,))

# show pair plot
pair_plot

# remove df_pair from environment as it's no longer needed
rm(df_pair)

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

# Split the data set into training and test sets
test_indices <- createDataPartition(df_data$failure_numeric, 
                                    times=1, p=0.2, list=FALSE)

test_set  <- df_data[test_indices,]
train_set <- df_data[-test_indices,]

# train a decision tree, predict and get f1 score
ctrl <- trainControl(method = "cv", number = 5) # 5 fold cross validation
fit_dt <- train(failure ~ air_t + process_t + rpm + torque + wear + type,
                data = train_set,
                method = "rpart",
                trControl = ctrl)

cm_dt <- confusionMatrix(predict(fit_dt, test_set), 
                         as.factor(test_set$failure))

f1_dt <- cm_dt[["byClass"]][["F1"]]

# train SVM classifier with radial basis kernel, predict and get f1score
ctrl <- trainControl(method = "cv", number = 5) # 5 fold cross validation
fit_svm <- train(failure ~ air_t + process_t + rpm + torque + wear + type, 
                 data = train_set, 
                 method = "svmRadial",
                 trControl = ctrl)

cm_svm <- confusionMatrix(predict(fit_svm, test_set), 
                          as.factor(test_set$failure))

f1_svm <- cm_svm[["byClass"]][["F1"]]

# train a random forest, predict and get f1 score
ctrl <- trainControl(method = "cv", number = 5) # 5 fold cross validation
fit_rf <- train(failure ~ air_t + process_t + rpm + torque + wear + type,
                data = train_set,
                method = "rf",
                trControl = ctrl)

cm_rf <- confusionMatrix(predict(fit_rf, test_set), 
                         as.factor(test_set$failure))

f1_rf <- cm_rf[["byClass"]][["F1"]]


pred_probs <- predict(fit_rf, test_set, type = "prob")

