setwd("~/Rhome/PMLProj")
library(caret);

## Loading and cleaning Data

set.seed(24601)

# load data cleaning NAs
trainingLoad <- read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!",""))
testingLoad <- read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!",""))

# survey data
summary(trainingLoad)
describe(trainingLoad)

# eliminate NA data

NAs <- apply(trainingLoad,2,function(x) {sum(is.na(x))}) 
training <- trainingLoad[,which(NAs == 0)]
testing <- testingLoad[,which(NAs == 0)]

# force useful columns to numeric
twnums <- training
for(i in c(8:ncol(twnums)-1)) {twnums[,i] = as.numeric(as.character(twnums[,i]))}
tstnums <- testing
for(i in c(8:ncol(tstnums)-1)) {tstnums[,i] = as.numeric(as.character(tstnums[,i]))}

# remove non useful features
goodfeats <- colnames(twnums[colSums(is.na(twnums)) == 0])[-(1:7)]
featureset <- twnums[goodfeats]
goodfeats <- colnames(tstnums[colSums(is.na(tstnums)) == 0])[-(1:7)]
featuretest <- tstnums[goodfeats]

## Building data sets for training and cross validation. 
### Using 75% for training and 25% for Cross Validation.
trgroup <- createDataPartition(y = featureset$classe, p=0.75,list=FALSE)
trainset <- featureset[trgroup,]
cvset <- featureset[-trgroup,]

## Training...
trCon = trainControl(method = "cv", number = 5)
modelFit <- train(classe ~ ., trControl=trCon, method="rf", data=trainset)
modelFit


## Calculation the errors using the Cross Validation Set.
cvrun <- predict(modelFit, cvset)
cvsamperr <- sum(cvrun == cvset$classe)/nrow(cvset)


## Generating data for the prediction vector for the Assigment Submission
answers <- predict(modelFit, testing)

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)
