# clear environment 
rm(list=ls())

# load packages 
pks <- c("dplyr","ggplot2","caret","magrittr","randomForest")
lapply(pks,require,character.only=T)

# import data
fileList <- list.files(pattern = "*.csv")
dataList <- lapply(fileList,read.csv,stringsAsFactors=F)

# inspect data list
names(dataList) <- fileList
lapply(dataList,dim)
lapply(dataList,names)

# assign training and test sets to data frames
train <- dataList$train.csv
test <- dataList$test.csv
test$Survived <- NA 
fullData <- rbind(train,test) 


# data struture
fullData %>% str()

# create title variable - proxy for social class & gender
nameList <- strsplit(fullData$Name,split=",")
nameList2 <- unlist(lapply(nameList,function(x) x[2]))
fullData$Title <- unlist(lapply(strsplit(nameList2,split=". "),function(x) x[1]))
fullData %>% str()

## number of missing observations per variable
unlist(lapply(fullData,function(x) sum(is.na(x))))

# randomly assign age from existing age values
hist(fullData$Age)
ageSample <- as.numeric(na.omit(fullData$Age))
sampleNum <- length(fullData[is.na(fullData$Age),]$Age)
fullData[is.na(fullData$Age),]$Age <- sample(ageSample,sampleNum)
hist(fullData$Age)
# does not appear to significantly affect distribution 


# code certain variables as factors
fullData$Survived <- as.factor(fullData$Survived)
fullData$Sex <- as.factor(fullData$Sex)
fullData$Title <- as.factor(fullData$Title)
fullData %>% str()


# impute fare value
fullData %>%
  ggplot(aes(y=Fare,x=factor(Pclass))) +
  geom_boxplot()

fareModel <- randomForest(Fare ~ Age + Title + Pclass,
                        data=fullData[!is.na(fullData$Fare),])
FarePred <- predict(fareModel,fullData[is.na(fullData$Fare),])
fullData[is.na(fullData$Fare),]$Fare <- FarePred

# number of blank observations per variable
# only appropriate for character variabels
unlist(lapply(fullData,function(x) sum(x=="")))


# split datasets from formatting
train <- fullData[!is.na(fullData$Survived),]
test <- fullData[is.na(fullData$Survived),]


# fit a random forest model
rfModel <- randomForest(Survived ~ Pclass + Sex + Age + 
                          SibSp + Parch + Fare + Title,
                        data = train,
                        imporance=T)

# visualize random forest model fit
plot(rfModel,ylim=c(0,0.5))
legend('topright', colnames(rfModel$err.rate), col=1:3, fill=1:3)


# what mattered most?
varImpPlot(rfModel)
impData <- importance(rfModel)
impData2 <- data.frame(Feature=row.names(impData),
                       Importance=impData[,'MeanDecreaseGini'])
row.names(impData2) <- NULL
impData2 %>%
  ggplot(aes(x=reorder(Feature,Importance),y=Importance)) +
  geom_bar(stat="identity") +
  coord_flip()


# predict on the test set
testPrediction <- predict(rfModel,test)
solution <- data.frame(PassengerId = test$PassengerId, 
                       Survived = testPrediction)
row.names(solution) <- NULL


# print solution
write.csv(solution,"TitanicPrediction_rf.csv",row.names = F)
