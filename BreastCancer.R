library(tidyverse)
library(dplyr)
library(corrplot)
library(pROC)
library(MASS)
library(caret)
library(MLmetrics)
library(rpart) # use for decision tree
library(rpart.plot) # use for decision tree
library(randomForest) # use for random forest
library(varImp)
library(gbm) # use for gradient boosting
library(kableExtra)
library(car)


url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/breast-cancer-wisconsin.data"
data <- read.csv(file = url, header = FALSE,
                 col.names = c("ID","clump_thickness", "uniformity_size", "uniformity_shape", "marginal_adhesion", "single_epithelial_cell_size", "bare_nuclei", "bland_chromatin", "normal_nucleoli","mitoses", "diagnosis"))
str(data)

# Remove observations that contain missing values
data <- data[data$bare_nuclei != "?",]

# Remove the patient's unique identifier 
data <- select(data, -1) 

# Change the dependent variable into a binary variable where 2 means "benign" and 4 means "malignant"
data <- data %>% mutate(diagnosis = ifelse(diagnosis == 2, 0, 1),
                        diagnosis = as.factor(diagnosis),
                        bare_nuclei = as.integer(as.character(bare_nuclei)))
summary(data)
ggplot(data, aes(x = diagnosis, color = "red")) +
  geom_bar(fill = "#fb6a4a") +
  ggtitle("Distribution of diagnosis for the entire dataset") +
  theme_minimal() +
  theme(legend.position = "none")

sum(data$diagnosis == 1)/nrow(data)
sum(train$diagnosis == 1)/nrow(train)
sum(test$diagnosis == 1)/nrow(test)

correlation <- cor(data[,-10])
corrplot(correlation, type = "lower", col = c("pink", "lightblue"), addCoef.col = "black", tl.col = "black")


# split the data into train and test data
set.seed(3011) 
train_index <- sample(nrow(data), size = round(0.75 * nrow(data)), replace = FALSE)
train <- data[train_index,]
test <- data[-train_index,]

###########################################
#            Logistic Regression          #
###########################################

# Logistic Regression on the train data
lm <- glm(formula = diagnosis ~ ., data = train, family = binomial())
summary(lm)
vif(lm)

lm2 <- glm(formula = diagnosis ~ ., data = train %>% select(-c(uniformity_size, single_epithelial_cell_size, mitoses)), family = binomial())
summary(lm2)

lm3 <- glm(formula = diagnosis ~ ., data = train %>% select(-c(uniformity_size, single_epithelial_cell_size, bare_nuclei, mitoses)), family = binomial())
summary(lm3)

lm4 <- glm(formula = diagnosis ~ ., data = train %>% select(-c(uniformity_size, mitoses)), family = binomial())

stepAIC(lm, direction = "backward")
stepAIC(lm)
vif(lm)
AIC(lm, lm2, lm3, lm4)
plot(lm3)
par(mfrow = c(2,2))
plot(lm3)

BIC(lm, lm2, lm3, lm4)


# Apply the lm model to the train data
pred_train <- predict(lm3, train, type = 'response')
hist(pred_train_lm, 
     main = "Distribution of Predicted Values",
     xlab = "Predicted Values", 
     col = "#6baed6")

# Calculate and display the area under the ROC curve on the train data
# A receiver operating characteristic curve, or ROC curve, is a graphical plot that illustrates the diagnostic ability of a binary classifier system as its discrimination threshold is varied

roc(train$diagnosis, pred_train, percent = TRUE, plot = TRUE, print.auc = TRUE)

#plot(pr.curve(pred_train, train$diagnosis, curve = TRUE))
# Calculate classification performance metrics Accuracy and F1
pred_cat <- ifelse(pred_train >= 0.5, 1, 0)
Accuracy(y_true = train$diagnosis, y_pred = pred_cat) #98.35% 9824
F1_Score(y_true = train$diagnosis, y_pred = pred_cat) #98.74 9867
ConfusionMatrix(y_true = train$diagnosis, y_pred = pred_cat)

# Find the threshold that gives the most accurate model
accuracy <- 0
f1 <- 0
threshold <- 0

for(i in seq(0.1, 0.9, by = 0.01)){
  pred_cat_train <- ifelse(pred_train < i, 0, 1)
  a = Accuracy(y_true = train$diagnosis, y_pred = pred_cat_train)
  b = F1_Score(y_true = train$diagnosis, y_pred = pred_cat_train)
  
  if(a > accuracy & b > f1){
    accuracy = a
    f1 = b
    threshold = i
  }
}
accuracy 
f1 
threshold #0.48

# Apply the lm model to the test data
pred_test <- predict(lm3, test, type = 'response')
hist(pred_test)

# calculate and display the Area Under the Curve (AUC) on the train data
roc(test$diagnosis, pred_test, percent = TRUE, plot = TRUE, print.auc = TRUE)  #98.56% 9832

# Calculate performance measures Accuracy and F1
pred_cat_test <- ifelse(pred_test >= 0.48, 1, 0)
Accuracy(y_true = test$diagnosis, y_pred = pred_cat_test) #94.15%
F1_Score(y_true = test$diagnosis, y_pred = pred_cat_test) #95.37%
ConfusionMatrix(y_true = test$diagnosis, y_pred = pred_cat_test)


###########################################
#               Decision Tree             #
###########################################

# Instead of manually checking different combinations of hyper-parameters, 
# let's create a grid of the hyper-parameters in order to  find the right combination that 
# maximized the desired performance measure

set.seed(3011)
tree_parameters <- data.frame(minsplit_para = floor(runif(8, 10, 60)), 
                              maxdepth_para = floor(runif(8, 10, 30)))

# I will also create a table to keep track of the RMSE on the train and test data at each iteration

AUC_tree <- data.frame(AUC_train_tree = numeric(), AUC_test_tree = numeric()) # empty data frame
AUC_tree # this will be used to keep track of the AUC on the train and test data at each iteration

AUC_train_besttree <- 0
AUC_test_besttree <- 0

for(para_comb in 1:nrow(tree_parameters)){
  decision_tree <- rpart(diagnosis ~ .,  data = train,
                      control = rpart.control(minsplit = tree_parameters[para_comb, "minsplit_para"], # Minimum samples for a node split 
                                              maxdepth = tree_parameters[para_comb, "maxdepth_para"])) # Maximum depth of tree
  
  pred_train_tree <- as.data.frame(predict(decision_tree, train, type='prob'))
  AUC_train_tree <- roc(train$diagnosis, pred_train_tree$`1`, percent = TRUE, plot = TRUE)
  
  pred_test_tree <- as.data.frame(predict(decision_tree, test, type='prob'))
  AUC_test_tree <- roc(test$diagnosis, pred_test_tree$`1`, percent = TRUE, plot = TRUE) 
  
  AUC_tree[para_comb, ] <- c(AUC_train_tree$auc, AUC_test_tree$auc)
  AUC_train_besttree = ifelse(AUC_train_besttree > AUC_train_tree$auc, AUC_train_besttree, AUC_train_tree$auc)
  AUC_test_besttree = ifelse(AUC_test_besttree > AUC_test_tree$auc, AUC_test_besttree, AUC_test_tree$auc)
}
cbind(tree_parameters, AUC_tree)

train <- train %>% mutate(diagnosis = as.numeric(as.character(diagnosis)),
                          diagnosis = ifelse(diagnosis == 0, "benign", "malignant"))
# According to the grid search, the best decision tree would have a minsplit of 11 and maxdepth of 10
best_decision_tree <- rpart(diagnosis ~., data = train,
                            control = rpart.control(minsplit = 11,
                                                    maxdepth = 10))
summary(best_decision_tree)
# The top 4 most important independent variables are uniformity_shape, uniformity_size, single_epithelial_cell_size, and bland chromatin

rpart.plot(x = best_decision_tree, box.palette="RdBu", shadow.col="gray", nn=TRUE, yesno = 2)

train$diagnosis <- ifelse(train$diagnosis == "benign", 0, 1) %>% as.factor()
best_decision_tree <- rpart(diagnosis ~., data = train,
                            control = rpart.control(minsplit = 11,
                                                    maxdepth = 10))

###########################################
#               Random Forest             #
###########################################

set.seed(160)
rf_parameters <- data.frame(nodesize = round(runif(10,5,20)),
                            sampsize= round(runif(10,1,400)),
                            mtry = round(runif(10,1,10)),
                            ntree = round(runif(10,1,400)))

AUC_rf <- data.frame(AUC_train_rf = numeric(), AUC_test_rf = numeric()) 
AUC_train_bestrf <- 0
AUC_test_bestrf <- 0

for(paracomb_rf in 1:nrow(rf_parameters)){
  random_forest <- randomForest(diagnosis ~ ., data = train,
                                nodesize = rf_parameters[paracomb_rf, "nodesize"],
                                sampsize = rf_parameters[paracomb_rf, "sampsize"],
                                mtry = rf_parameters[paracomb_rf, "mtry"],
                                ntree = rf_parameters[paracomb_rf, "ntree"])
  
  pred_train_rf <- as.data.frame(predict(random_forest, train, type='prob'))
  AUC_train_rf <- roc(train$diagnosis, pred_train_rf$`1`, percent = TRUE, plot = TRUE)
  
  pred_test_rf <- as.data.frame(predict(random_forest, test, type='prob'))
  AUC_test_rf <- roc(test$diagnosis, pred_test_rf$`1`, percent = TRUE, plot = TRUE) 
  
  AUC_rf[paracomb_rf, ] <- c(AUC_train_rf$auc, AUC_test_rf$auc)
  AUC_train_bestrf = ifelse(AUC_train_bestrf > AUC_train_rf$auc, AUC_train_bestrf, AUC_train_rf$auc)
  AUC_test_bestrf = ifelse(AUC_test_bestrf > AUC_test_rf$auc, AUC_test_bestrf, AUC_test_rf$auc)
}
cbind(rf_parameters, AUC_rf)

# According to the grid search, the best random forest model would have a 
best_random_forest <- randomForest(diagnosis ~ ., data = train,
                                   nodesize = 9,
                                   sampsize = 329,
                                   mtry = 7,
                                   ntree = 210)
# check details about the random forest model
best_random_forest

# check what variables are important
varImp(best_random_forest)
varImpPlot(best_random_forest)
# The top 4 most important independent variables are uniformity_shape, uniformity_size, single_epithelial_cell_size, and bland chromatin


###########################################
#             Gradient Boosting           #
###########################################

train <- train %>% mutate(diagnosis = as.numeric(as.character(diagnosis)))

#opt_n.trees <- gbm.perf(gradient_boosting, method = "cv")

set.seed(3011)
gb_parameters <- data.frame(sample_size = round(runif(10,0.5,1), 2),
                           min_size= round(runif(10,5,20)),
                           num_tree = round(runif(10,20,200)),
                           shrink = round(runif(10,0.1,0.5), 2))

AUC_gb <- data.frame(AUC_train_gb = numeric(), AUC_test_gb = numeric()) 
AUC_train_bestgb <- 0
AUC_test_bestgb <-0

for(paracomb_gb in 1:nrow(gb_parameters)){
  gradient_boosting <- gbm(diagnosis ~ ., data = train, 
                           distribution = "bernoulli",
                           n.trees = gb_parameters[paracomb_gb,'num_tree'],
                           shrinkage = gb_parameters[paracomb_gb,'shrink'], 
                           interaction.depth = 3,
                           bag.fraction = gb_parameters[paracomb_gb,'sample_size'], 
                           n.minobsinnode = gb_parameters[paracomb_gb,'min_size'], 
                           verbose = TRUE)

  pred_train_gb <- predict(gradient_boosting, train, type = "response", n.trees = gb_parameters[paracomb_gb,'num_tree'])
  AUC_train_gb <- roc(train$diagnosis, pred_train_gb, percent = TRUE, plot = TRUE)
  
  pred_test_gb <- predict(gradient_boosting, test, type = "response", n.trees = gb_parameters[paracomb_gb,'num_tree'])
  AUC_test_gb <- roc(test$diagnosis, pred_test_gb, percent = TRUE, plot = TRUE) 
  
  AUC_gb[paracomb_gb, ] <- c(AUC_train_gb$auc, AUC_test_gb$auc)
  AUC_train_bestgb = ifelse(AUC_train_bestgb > AUC_train_gb$auc, AUC_train_bestgb, AUC_train_gb$auc)
  AUC_test_bestgb = ifelse(AUC_test_bestgb > AUC_test_gb$auc, AUC_test_bestgb, AUC_test_gb$auc)
  
}

cbind(gb_parameters, AUC_gb)

best_gradient_boosting <- gbm(diagnosis ~ ., data = train, 
                         distribution = "bernoulli",
                         n.trees = 159,
                         shrinkage = 0.42, 
                         interaction.depth = 3,
                         bag.fraction = 0.55, 
                         n.minobsinnode = 6, 
                         verbose = TRUE)


###########################################
#              Model Selection            #
###########################################

model_name <- rbind("Logistic Regression", "Decision Tree", "Random Forest", "Gradient Boosting Machine")
AUC_values <- data.frame(Model = character(),
                         AUC_train = numeric(), 
                         AUC_test = numeric(),
                         AUC_data = numeric())

pred_lm <- predict(lm3, data, type = 'response')
AUC_lm <- roc(data$diagnosis, pred_lm, percent = TRUE, plot = TRUE, print.auc = TRUE)

pred_tree <- as.data.frame(predict(best_decision_tree, data, type='prob'))
AUC_tree <- roc(data$diagnosis, pred_tree$`1`, percent = TRUE, plot = TRUE) 

pred_rf <- as.data.frame(predict(best_random_forest, data, type='prob'))
AUC_rf <- roc(data$diagnosis, pred_rf$`1`, percent = TRUE, plot = TRUE)

pred_gb <- predict(best_gradient_boosting, data, type = "response", n.trees = 159)
AUC_gb <- roc(data$diagnosis, pred_gb, percent = TRUE, plot = TRUE)

AUC_values[c(1,2,3,4),c(2,3,4)] <- c(rbind(AUC_train_lm$auc, AUC_train_besttree, AUC_train_bestrf, AUC_train_bestgb), rbind(AUC_test_lm$auc, AUC_test_besttree, AUC_test_bestrf, AUC_test_bestgb), rbind(AUC_lm$auc, AUC_tree$auc, AUC_rf$auc, AUC_gb$auc))
AUC_values$Model <- model_name
AUC_values <- AUC_values %>% mutate(AUC_train = round(AUC_train, 2),
                                    AUC_test = round(AUC_test, 2),
                                    AUC_data = round(AUC_data, 2))

train <- train %>% mutate(diagnosis = as.numeric(as.character(diagnosis)))
pred_train_bestgb <- predict(best_gradient_boosting, train, type = "response", n.trees = 159)
pred_test_bestgb <- predict(best_gradient_boosting, test, type = "response", n.trees = 159)

pred_cat_train_gb <- ifelse(pred_train_bestgb < 0.5, 0, 1)
pred_cat_test_gb <- as.factor(ifelse(pred_test_bestgb < 0.5, 0, 1))

Accuracy(y_true = train$diagnosis, y_pred = pred_cat_train_gb)
Accuracy(y_true = test$diagnosis, y_pred = pred_cat_test_gb)
F1_Score(y_true = train$diagnosis, y_pred = pred_cat_train_gb)
F1_Score(y_true = test$diagnosis, y_pred = pred_cat_test_gb)

confusionMatrix(pred_cat_test_gb, test$diagnosis)


roc(test$diagnosis, pred_test_bestgb, percent = TRUE, plot = TRUE)

rel_inf_gb <- as.data.frame(summary.gbm(best_gradient_boosting, plotit = FALSE))
rel_inf_gb %>% 
  arrange(desc(rel.inf)) %>%
  top_n(4) %>%
  ggplot(aes(x = reorder(var, -rel.inf), 
             y = rel.inf,
             fill = rel.inf)) +
  geom_col() +
  coord_flip() +
  xlab('Features') +
  ylab('Relative Influence') +
  ggtitle("Top 4 Predictors of Breast Cancer") +
  theme_minimal()
