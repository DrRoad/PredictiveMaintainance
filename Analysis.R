setwd("C:/Ambarish/Predictive Maintainance Analysis")

data <- read.table("secom.data")

names(data)


labels <- read.table("secom_labels.data")

label <- labels$V1


head(label)

secom <- cbind(data,label)

head(secom)

#Replace the missing values of NaN with 0.
secom[secom == "NaN"] <- 0

########################################################################


library(caret)

control <- rfeControl(functions=rfFuncs)

results <- rfe(secom[,1:590], as.factor(secom[,591]), sizes=c(100), rfeControl=control)

predictors(results)

cols <-varImp(results)

cols.names <- row.names(cols)

cols$names <- cols.names

#get the 100 names

predictors100 <- cols[1:100,2]

########################################

secom2 <- secom[,predictors100]

secom2$label <- secom$label

index <- createDataPartition(secom2$label,p=0.7,list = FALSE,times = 1)

#Step 2: Split the data into test and training sets
secom2.train <- secom2[index,]

secom2.test <- secom2[-index,]

labels <- as.factor(secom2.train$label)

labels.test <-as.factor(secom2.test$label)

secom2.train <- secom2.train[,1:100]

secom2.test <- secom2.test[,1:100]


#Do a Random Forest
library(randomForest)

RF <- randomForest(secom2.train, labels, xtest=secom2.test)

predictions <- RF$test$predicted

table(predictions,labels.test)


accuracy <- (442)/(442+28)

accuracy

