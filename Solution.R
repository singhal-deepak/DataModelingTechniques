# IDS 572 - Assignment 2
# Charu Yadav (653762591)
# Deepak Singhal (672190946)



#download old and training dataset file from https://archive.ics.uci.edu/ml/datasets/Phishing+Websites and convert them 
#into csv format

#library
# install.packages("caret")
# install.packages("ggplot2")
# install.packages("rpart")
# install.packages("rpart.plot")
# install.packages("e1071")
# install.packages("randomForest")
library(rpart.plot)
library(rpart)
library(e1071)
library(randomForest)
library(ggplot2)
library(caret)

new = read.csv("C:/Users/n/Documents/UIC/Fall Sem/572/Training_Dataset.csv")
old = read.csv("C:/Users/n/Documents/UIC/Fall Sem/572/csv_result.csv")
# removing id (irrelevant parameter) 
new <- new[-1] 
old <- old[-1] 

colnames(old) <- c("having_IP_Address", "URL_Length","Shortining_Service","having_At_Symbol",
                    "double_slash_redirecting","Prefix_Suffix","having_Sub_Domain","SSLfinal_State",
                    "Domain_registeration_length","Favicon", "port","HTTPS_token","Request_URL",
                    "URL_of_Anchor","Links_in_tags","SFH","Submitting_to_email","Abnormal_URL",
                    "Redirect","on_mouseover","RightClick","popUpWidnow","Iframe","age_of_domain",
                    "DNSRecord","web_traffic","Page_Rank","Google_Index",
                    "Links_pointing_to_page","Statistical_report","Result") 

colnames(new) <- c("having_IP_Address", "URL_Length","Shortining_Service","having_At_Symbol",
                    "double_slash_redirecting","Prefix_Suffix","having_Sub_Domain","SSLfinal_State",
                    "Domain_registeration_length","Favicon", "port","HTTPS_token","Request_URL",
                    "URL_of_Anchor","Links_in_tags","SFH","Submitting_to_email","Abnormal_URL",
                    "Redirect","on_mouseover","RightClick","popUpWidnow","Iframe","age_of_domain",
                    "DNSRecord","web_traffic","Page_Rank","Google_Index",
                    "Links_pointing_to_page","Statistical_report","Result") 

attach(new)
High <- ifelse(Result >= 0, "YES", "NO")
High <- as.factor(High)
new <- data.frame(new, High)
new <- new[-31]

attach(old)
High <- ifelse(Result >= 0, "YES", "NO")
High <- as.factor(High)
old <- data.frame(old, High)
old <- old[-31]

data <- rbind(new, old)

names <- c("having_IP_Address", "URL_Length", "Shortining_Service","having_At_Symbol",
           "double_slash_redirecting","Prefix_Suffix","having_Sub_Domain","SSLfinal_State",
           "Domain_registeration_length","Favicon", "port","HTTPS_token","Request_URL",
           "URL_of_Anchor","Links_in_tags","SFH","Submitting_to_email","Abnormal_URL",
           "Redirect","on_mouseover","RightClick","popUpWidnow","Iframe","age_of_domain",
           "DNSRecord","web_traffic","Page_Rank","Google_Index",
           "Links_pointing_to_page","Statistical_report")

data[,names] <- lapply(data[,names] , factor)

# check whether data is balanced or not
summary(data$High)
# As "no" class (6260) and "yes" class (7251) are almost equal, so there is no need to over sample

attach(data)
# data cleaning
#1) checking 
sum(complete.cases(data))
#output of above - no missing and nonull values
summary(data)
# output of above - no outliers (there are only classification independent variables)

set.seed(2)
indx <- sample(2, nrow(data), replace = T, prob = c(0.7, 0.3))
train <- data[indx == 1, ]
test <- data[indx == 2, ]

summary(train$High)
summary(test$High)

# Decision tree 
tree_model <- rpart(High ~ ., data = train)
rpart.plot(tree_model)
tree_pred_prob <- predict(tree_model, test, type = "prob")
tree_pred_class <- predict(tree_model, test, type = "class")
mean(test$High == tree_pred_class)
# accuracy on test data = 89.29 %
tree_pred_class_train <- predict(tree_model, train, type = "class")
mean(train$High == tree_pred_class_train)
# accuracy on test data = 89.64 %

# Naive Bayes
naive_e1071 <- naiveBayes(High ~ ., data = train)
predict(naive_e1071, newdata = test, type = "raw")
preded <- predict(naive_e1071, newdata = test)
table(preded, test$High)
mean(test$High == preded)
# accuracy on test data = 82.36 %
prededd <- predict(naive_e1071, newdata = train)
mean(train$High == prededd)
# accuracy on train data = 82.93 %

# Random Forest Model
data <- rbind(train, test)
rf <- randomForest(High ~ ., data = data, mtry = sqrt(ncol(data)-1), ntree = 300, proximity = T, importance = T)
rf$predicted
confusionMatrix(rf$predicted, data$High, positive = "YES")
#accuracy of complete dataset = 96.87%

# WE ARE USING 3 FOLD - CROSS VALIDATION TECHNIQUE TO FIND OUT WHICH MODEL IS THE BEST (DECISION TREE OR NAIVE BAYES 
# OR RANDOM FOREST) 
# In this scenario, RECALL is most crucial parameter because anyone who has been victim of phishing attack, should get notified at 
# any cost, in this scenario, FN is more critical. We will choose the model with the highest recall.

# 3 fold : cross validation for Decision tree
k = 3
nmethod = 1
folds = cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
models.recall = matrix(-1,k,nmethod,dimnames=list(paste0("Fold", 1:k), c("rf")))
for(i in 1:k)
{
  testIndexes = which(folds==i, arr.ind=TRUE)
  testData = data[testIndexes, ]
  trainData = data[-testIndexes, ]
  print(summary(trainData$High))
  print(summary(testData$High))
  tree_model <- rpart(High ~ ., data = trainData)
  
  tree_pred_class <- predict(tree_model, testData, type = "class")
  table = table(tree_pred_class,testData$High)
  print(table)
  table = as.vector(table)
  names(table) <- c("TN","FP","FN","TP")
  recall = table["TP"]/(table["TP"]+table["FN"])
  print(recall)
  models.recall[i] = recall 
}
mean(models.recall)
# recall is 90.27 % (even in kfold data split, its balanced)

# 3 fold : cross validation for naive bayes
k = 3
nmethod = 1
folds = cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
models.recall = matrix(-1,k,nmethod,dimnames=list(paste0("Fold", 1:k), c("rf")))
for(i in 1:k)
{
  testIndexes = which(folds==i, arr.ind=TRUE)
  testData = data[testIndexes, ]
  trainData = data[-testIndexes, ]
  print(summary(trainData$High))
  print(summary(testData$High))
  naive_e1071 <- naiveBayes(High ~ ., data = trainData)
  preded <- predict(naive_e1071, newdata = testData)
  table = table(preded,testData$High)
  print(table)
  table = as.vector(table)
  names(table) <- c("TN","FP","FN","TP")
  recall = table["TP"]/(table["TP"]+table["FN"])
  print(recall)
  models.recall[i] = recall 
}
mean(models.recall)
# Recall is 83.64 % (even in kfold data split, its balanced)

# 3 fold : cross validation for Random forest
k = 3
nmethod = 1
folds = cut(seq(1,nrow(data)),breaks=k,labels=FALSE)
models.recall = matrix(-1,k,nmethod,dimnames=list(paste0("Fold", 1:k), c("rf")))
for(i in 1:k)
{
  testIndexes = which(folds==i, arr.ind=TRUE)
  testData = data[testIndexes, ]
  trainData = data[-testIndexes, ]
  print(summary(trainData$High))
  print(summary(testData$High))
  rf = randomForest(High ~ ., data = trainData, ntree = 100, mtry = sqrt(ncol(trainData) - 1)) 
  
  predicted = predict(rf, newdata = testData, type = "class")
  table = table(predicted,testData$High)
  table = as.vector(table)
  names(table) <- c("TN","FP","FN","TP")
  recall = table["TP"]/(table["TP"]+table["FN"])
  models.recall[i] = recall
}
mean(models.recall)
# recall = 96.77 % (even in kfold data split, its balanced)

# So, recall is maximum for Random forest (96.77) than decision tree (90.27) and naive bayes (83.64), hence random forest 
# is the best model



