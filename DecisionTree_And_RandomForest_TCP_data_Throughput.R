########## 
library(C50)
setwd("D:/Sprint/1015");
d1 <- read.table("D:/Sprint/1015/TCP Performace Data for LM/Throughput.csv", header = TRUE, sep = ",")
head(d1)
## Let's delete the first 5 columns (this is for demonstration only) and name the new frame as d2
del1 = c(-1,-2,-3,-4,-5)
d2 = d1[,del1] 

## the following will copy throughut (the last column) into the vector output_numeric
N = dim(d2)[2]
output_numeric = d2[,N]

## Convert output_numeric into a Category using three levels: Bad, Good, Excellent
library(arules)
output_class =  factor(discretize(output_numeric, method = "cluster", categories= 3),labels= c("B", "G","E"))

# threshold = median(d2[,N])
# rows = which(d2[,N] < threshold);
# output_class = rep("G",length(d2[,N]));
# output_class[rows] = "B";
print(output_class[1:10])
d2 = data.frame(d2[,-N], output_class) 

#############################################################
#########  Cross Validation
set.seed(33) ## This is needed to be able to reproduce the same numbers
train_rows = sample(1:nrow(d2), nrow(d2)/4) 
# train_rows now contains the row numbers that we just selected randomly
train_dataSet    = d2[ train_rows,]  ## thus will copy from d2 all the rows we selected for training
validate_dataSet = d2[-train_rows,]  ## this will delete from d2 all the rows we selected for training

###### (A) Start with a Decison Tree Algorithm to have a reference
##### 1) TRAIN:
m3 <- C5.0(train_dataSet[,-N], train_dataSet[,N])
summary(m3)
###############################################################
##### 2) VALIDATE:
predicted_values = predict(m3, validate_dataSet[,-N], type = "class")
mean_error = mean(predicted_values != validate_dataSet[,N]);
mean_error_percentage = 100*mean_error;
cat("Mean Error = ",mean_error_percentage,"%\n")


###### (B) RANDOM FOREST
#install.packages("randomForest")
library(randomForest)
##### 1) TRAIN:
m4 <- randomForest(output_class ~ .,train_dataSet, ntree = 10)
importance(m4)
#getTree(m4,1,labelVar = TRUE)

##### 2) VALIDATE:
predicted_values2 = predict(m4, validate_dataSet[,-N], type = "class")
mean_error2 = mean(predicted_values2 != validate_dataSet[,N]);
mean_error_percentage2 = 100*mean_error2;
cat("Mean Error = ",mean_error_percentage2,"%\n")

## To get a comparable mean error as that of C5.0 algorithm try increasing ntree to, say, 200. 
## It should not take long to compute on d2 data set.



########## 
library(C50)
setwd("D:/Sprint/1015");
d1 <- read.table("D:/Sprint/1015/TCP Performace Data for LM/Throughput.csv", header = TRUE, sep = ",")
head(d1)
## Let's delete the first 5 columns (this is for demonstration only) and name the new frame as d2
del1 = c(-1,-2,-3,-4,-5)
d2 = d1[,del1] 

## the following will copy throughut (the last column) into the vector output_numeric
N = dim(d2)[2]
output_numeric = d2[,N]

## Convert output_numeric into a Category using three levels: Bad, Good, Excellent
library(arules)
output_class =  factor(discretize(output_numeric, method = "cluster", categories= 3),labels= c("B", "G","E"))
print(output_class[1:10])
d2 = data.frame(d2[,-N], output_class)
## Model Building
library(caret)
library(corrplot)
library(kknn)
library(randomForest)
library(kernlab)
inTrain <- createDataPartition(d2$output_class, p = 2/3, list = F)
train <- d2[inTrain,]
test <- d2[-inTrain,]

t.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
#randomForest

#For the random forest, only the mtry hyperparameter is available for tuning. 
#Mtry values of 1 through 13 will be passed into the train function's tuneGrid argument. 
#Mtry is the number of variables randomly sampled as candidates at each split.

# rf.grid <- expand.grid(mtry = 1:13)
# rf.train <- train(output_class ~ ., data = train, method = "rf",
#                   trControl = t.ctrl, tuneGrid = rf.grid,
#                   preProcess = c("center", "scale"))
# plot(rf.train)

#Fit Random Forest Model
rf = randomForest(output_class ~ .,  
                  ntree = 20,
                  data = train)
plot(rf)
print(rf)
# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")

# Variable Importance
varImpPlot(rf,  
           sort = T,
           n.var=10,
           main="Top 10 - Variable Importance")


#Variable Importance
var.imp = data.frame(importance(rf,  
                                type=2))
# make row names as columns
var.imp$Variables = row.names(var.imp)  
print(var.imp[order(var.imp$MeanDecreaseGini,decreasing = T),])

# Predicting response variable
predict.output_class = predict(rf , test)

# Create Confusion Matrix
print(  
  confusionMatrix(data = predict.output_class,  
                  reference = test$output_class))

d3=d2[,c(-1,-2,-3)]

inTrain <- createDataPartition(d3$output_class, p = 2/3, list = F)
train <- d2[inTrain,]
test <- d2[-inTrain,]
rf = randomForest(output_class ~ .,  
                  ntree = 20,
                  data = train)
print(  
  confusionMatrix(data = predict.output_class,  
                  reference = test$output_class))


# As with the data, the Caret package's train function will be used to train the models, 
#all the predictor variables will be used, and preprocessing will be done within the cross-validation loop. The predictor variables will be standardized and the cross-validation method will be repeated cross-validation repeated 5 times using 5 folds.
# k-nearest neighbours
# For k-nearest neighbours, 5 kmax, 2 distance, 3 kernels will be used. 
#In total, that is 13 possible cominations.

t.ctrl <- trainControl(method = "repeatedcv", number = 5, repeats = 5)
kknn.grid <- expand.grid(kmax = c(3, 5, 7, 9, 11), distance = c(1, 2),
                         kernel = c("rectangular", "cos", "gaussian"))
kknn.train <- train(output_class ~ ., data = train.red, method = "kknn",
                    trControl = t.ctrl, tuneGrid = kknn.grid,
                    preProcess = c("center", "scale"))
plot(kknn.train)




