#Prepare a model for glass classification using KNN
#Data Description:
#RI : refractive index
#Na: Sodium (unit measurement: weight percent in corresponding oxide, as are attributes 4-10)
#Mg: Magnesium
#AI: Aluminum
#Si: Silicon
#K:Potassium
#Ca: Calcium
#Ba: Barium
#Fe: Iron
#Type: Type of glass: (class attribute)
#1 -- building_windows_float_processed
#2 --building_windows_non_float_processed
#3 --vehicle_windows_float_processed
#4 --vehicle_windows_non_float_processed (none in this database)
#5 --containers
#6 --tableware
#7 --headlamps

glass_data <- read.csv(file.choose())

table(glass_data$Type)
# below is the proportion of glass availble for diff types
## 1  2  3  5  6  7 
## 70 76 17 13  9 29 

prop.table(table(glass_data$Type))

## 1          2          3          5          6          7 
## 0.32710280 0.35514019 0.07943925 0.06074766 0.04205607 0.13551402 

str(glass_data)

## standarzie the data

standard_glassData <- scale(glass_data[1:9]) ## data without o/p varaible

## now combine standarize data with o/p variable

data_glass <- cbind(standard_glassData,glass_data[10])


## now will split data into train and test 

library(caTools)

## here we wil split data into 70,30 ratio

glass_split <- sample.split(data_glass$Type,SplitRatio = 0.70)

train_data <- subset(data_glass,glass_split==TRUE)
test_data <- subset(data_glass,glass_split==FALSE)

## Now will build our KNN model

library(class)
library(caret)

glas_predict <- knn(train_data[,2:10],test_data[,2:10],train_data$Type,k=4)
glas_predict

table(glas_predict)

table(glas_predict,test_data$Type) ## comparing predicted values with original values

#glas_predict  1  2  3  5  6  7
#           1 21  0  0  0  0  0
#           2  0 23  0  0  0  0
#           3  0  0  5  0  0  0
#           5  0  0  0  2  0  0
#           6  0  0  0  0  2  0
#           7  0  0  0  2  1  9
> 
  ## lets build model of train and test,will check their accuracy
  
train_glass_pred <- knn(train_data,train_data,train_data$Type,k=3)
train_accuracy <- mean(glas_predict == train_data$Type)
train_accuracy ## train data is giving 0.3020134 accuracy

test_glass_pred <- knn(train_data, test_data, train_data$Type, k=3)
test_accuracy <- mean(glas_predict == test_data$Type)
test_accuracy ## test data is giving 0.9538462 accuracy


## Now we will evaluate the performance of the model

install.packages("gmodels")

library(gmodels)

CrossTable(glas_predict,test_data$Type)

## accuracy for train model is too low so lets try some other value for k

glas_predict1 <- knn(train_data[,2:10],test_data[,2:10],train_data$Type,k=8)
glas_predict1



table(glas_predict1)

table(glas_predict1,test_data$Type) ## comparing predicted values with original values

#glas_predict  1  2  3  5  6  7
#           1 21  0  0  0  0  0
#           2  0 23  0  0  0  0
#           3  0  0  5  0  0  0
#           5  0  0  0  2  0  0
#           6  0  0  0  0  2  0
#           7  0  0  0  2  1  9
> 
  ## lets build model of train and test,will check their accuracy
  
train_glass_pred1 <- knn(train_data,train_data,train_data$Type,k=8)
train_accuracy1 <- mean(glas_predict1 == train_data$Type)
train_accuracy1 ## train data is giving 0.295302 accuracy

test_glass_pred1 <- knn(train_data, test_data, train_data$Type, k=8)
test_accuracy1 <- mean(glas_predict1 == test_data$Type)
test_accuracy1 ## test data is giving 0.8923077 accuracy

## lets use for loop to try diff k values and their accuracy

train_accuracy2 <- NULL 
for (i in seq(1,10,1))
{
  train_glass_pred2 <- knn(train_data,train_data,train_data$Type,k=i)
  train_accuracy2 <- c(train_accuracy2,mean(train_glass_pred2==train_data$Type))
}

test_accuracy2 <- NULL
for (i in seq(1,10,1))
{
  test_glass_pred2 <- knn(train_data, test_data, train_data$Type, k=i)
  test_accuracy2 <- c(test_accuracy2,mean(test_glass_pred2==test_data$Type))
}

## now will store these train and test accuracy in new data frame

accuracy_dataframe <- data.frame(list(train_accuracy2=train_accuracy2,test_accuracy2=test_accuracy2,neigh=seq(1,10,1))) 

library(ggplot2) 
ggplot(accuracy_dataframe,aes(x=neigh))+
  geom_line(aes(y=train_accuracy2,colour="Train Accuracy"),lwd=1)+
  geom_line(aes(y=test_accuracy2,colour="Test Accuracy"),lwd=1)+
  scale_fill_manual(" ",breaks=c("train_accuracy2","test_accuracy2"),values = c("train_accuracy2"="blue","test_accuracy"="red"))

## from the graph it has observed that test accuracy is falling down from 5 so ,we got min k value is 5.
