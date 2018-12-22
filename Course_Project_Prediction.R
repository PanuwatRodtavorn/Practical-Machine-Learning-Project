library(lattice)
library(ggplot2)
library(caret)
library(randomForestExplainer)
set.seed(127)
# import data
training<-read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"),na.strings=c("NA","#DIV/0!",""))
testing<-read.csv(url("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"),na.strings=c("NA","#DIV/0!",""))
#spit train and test set(random sampling)
intrain<-createDataPartition(training$classe,p = 0.7,list = FALSE)
train<-training[intrain,]
test<-training[-intrain,]
##Exception handing##########
#remove NA entire column
removeNA_Col<-function(x){x[,colSums(is.na(x))<nrow(x)]}
train<-removeNA_Col(train)
test<-removeNA_Col(test)
#remove NA row 
complete <- function(x){x[,sapply(x,function(y)!any(is.na(y)))]}
train<-complete(train)
test<-complete(test)
##delete primary key
train<-train[,-c(1:7)]
test<-test[,-c(1:7)]
#########################################
##fit model via Randomforest 
## perform 5 fold CV
rf.control <- trainControl(method = "repeatedcv",number = 5,repeats = 2)
rf.fit<-train(classe~.,data=train,method="rf",trControl = rf.control,verbose=FALSE)
## check variable importance
varImp(rf.fit)
####################################
##predicting test set
rf.pred<-predict(rf.fit,test)
#Accuracy
confusionMatrix(rf.pred,test$classe)
##predict the testing set (quiz)
rf.pred_quiz<-predict(rf.fit,removeNA_Col(testing))


