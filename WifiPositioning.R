#Wifi Positioning
#Start Date: December 3, 2018
#First Sprint: Dec 3 to Dec 5, 2018 - Exploring & visualizing the data
##Cleaning environment and uploading data
rm(list =ls())
setwd("C:/Users/carol/Desktop/Ubiqum/R/Wifi Positioning/task3-2-wifi-caroldorofei")
trainset <- read.csv("trainingData.csv")
validation <- read.csv("validationData.csv")

##Libraries
library(dplyr)
library(ggplot2)
library(reshape2)
library(rlist)
library(prob)
library(caret)
library(lattice)
library(base)
library(kazaam)
library(pbdMPI)
library(doParallel)
library(foreach)
library(iterators)
library(parallel)

##Data Preprocessing

###Column names
names(trainset)

###Check and change data types
str(trainset$FLOOR) #numeric -> change to factor
trainset$FLOOR <- as.factor(trainset$FLOOR)
str(trainset$BUILDINGID) #numeric -> change to factor
trainset$BUILDINGID <- as.factor(trainset$BUILDINGID)

str(validation$FLOOR) #numeric -> change to factor
validation$FLOOR <- as.factor(validation$FLOOR)
str(validation$BUILDINGID) #numeric -> change to factor
validation$BUILDINGID <- as.factor(validation$BUILDINGID)


###Change 1 & 2 in RELATIVEPOSITION to inside & outside
#trainset$RELATIVEPOSITION[trainset$RELATIVEPOSITION %in% "1"] <- "inside"
#trainset$RELATIVEPOSITION[trainset$RELATIVEPOSITION %in% "2"] <- "outside"

##Data Exploration
clipping1 <- trainset %>% filter (TIMESTAMP == 1371713733)
clipping1$BUILDINGID #Building 1, 1
clipping1$FLOOR #Floor 2, 2
clipping1$SPACEID #SpaceID 106, 216
clipping1$RELATIVEPOSITION #Position 2, 2 (outside, outside)
clipping1$USERID
clipping1 <- clipping1 %>% select(-c(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION,USERID,PHONEID,TIMESTAMP))

meltedclipping1 <- melt(clipping1)

meltedclipping1 <- meltedclipping1[meltedclipping1$value != 100,]

ggplot(meltedclipping1, aes(variable,value)) + geom_line(group=1) + theme(axis.text.x = element_text(size=8.5, angle=80))

clipping2 <- trainset %>% filter (TIMESTAMP == 1371713691)
clipping2$BUILDINGID #Building 1
clipping2$FLOOR #Floor 1
clipping2$SPACEID #SpaceID 106
clipping2$RELATIVEPOSITION #Position 2 (outside)

clipping2 <- clipping2 %>% select(-c(LONGITUDE,LATITUDE,FLOOR,BUILDINGID,SPACEID,RELATIVEPOSITION,USERID,PHONEID,TIMESTAMP))

meltedclipping2 <- melt(clipping2)

meltedclipping2 <- meltedclipping2[meltedclipping2$value != 100,]

ggplot(meltedclipping2, aes(variable,value)) + geom_point() + theme(axis.text.x = element_text(size=8.5, angle=80))

##Building 0
building0 <- trainset[trainset$BUILDINGID == 0,]
building1 <- trainset[trainset$BUILDINGID == 1,]
building2 <- trainset[trainset$BUILDINGID == 2,]

summary(building0$FLOOR) # 4 floors
summary(building1$FLOOR) # 4 floors
summary(building2$FLOOR) # 5 floors

building0$SPACEID <- as.factor(building0$SPACEID)
building1$SPACEID <- as.factor(building1$SPACEID)
building2$SPACEID <- as.factor(building2$SPACEID)

str(building0$SPACEID) # 101 to 241 (78 rooms)
str(building1$SPACEID) # 1 to 249 (86 rooms)
str(building2$SPACEID) # 101 to 254 (97 rooms)


trainset$POSITION <- paste(trainset$BUILDINGID,trainset$FLOOR,trainset$SPACEID,trainset$RELATIVEPOSITION) 
trainset$POSITION <- as.factor(trainset$POSITION)


#Count number of times WPA are detected in each building
detectedbuilding0 <- summary(building0)
detectedbuilding0 <- list(detectedbuilding0)
min(building0$WAP001)

##Detected or not detected?

###Finding out what WAPs are never detected
detectedWAPs <- c()

for (i in 1:520){
  
  if (min(trainset[,i]) != 100){
    detectedWAPs <- append(detectedWAPs,i)
  }
  
}
####Total number of WPAs never detected: 55 
520 - length(detectedWAPs)



TotalWAPs <- c(1:520)
Notdetected <- setdiff(TotalWAPs, detectedWAPs)  


###Finding out which WAPs are never detected in Building 0
WAPsB0 <- c()

for (i in 1:520){
  
  if (min(building0[,i]) != 100){
    WAPsB0 <- append(WAPsB0,i)
  }
  
    }
 
####Number of WAPs detected in building 0: 200
length(WAPsB0)

NotdetectedB0 <- setdiff(TotalWAPs,WAPsB0)

###Finding out which WAPs are never detected in Building 1
WAPsB1 <- c()

for (i in 1:520){
  
  if (min(building1[,i]) != 100){
    WAPsB1 <- append(WAPsB1,i)
  }
  
}

####Number of WAPs detected in building 1: 207
length(WAPsB1)

###Finding out which WAPs are never detected in Building 2
WAPsB2 <- c()

for (i in 1:520){
  
  if (min(building2[,i]) != 100){
    WAPsB2 <- append(WAPsB2,i)
  }
  
}

####Number of WAPs detected in building 2: 203
length(WAPsB2)

####Intersecting WAPs detected in each building

#####WAPs detected in B0 & B1: 59
WAPsB0B1 <- intersect(WAPsB0,WAPsB1)
length(WAPsB0B1)

#####WAPs detected in B0 & B2:7 
WAPsB0B2 <- intersect(WAPsB0,WAPsB2)
length(WAPsB0B2)

#####WAPs detected in B1 & B2: 82
WAPsB1B2 <- intersect(WAPsB1,WAPsB2)
length(WAPsB1B2)

#####WAPs detected in B0, B1 & B2: 
WAPsB0B1B2 <- intersect(WAPsB0,WAPsB1,WAPsB2)
length(WAPsB0B1B2)


##Validation Data

###Finding out what WAPs are never detected
detectedValidation <- c()

for (i in 1:520){
  
  if (min(validation[,i]) != 100){
    detectedValidation <- append(detectedValidation,i)
  }
  
}
####Total number of WPAs never detected: 153 
Notdetected.Validation <- setdiff(TotalWAPs, detectedValidation)  
length(Notdetected.Validation)

###Finding out which WAPs are never detected in Building 0
WAPsB0 <- c()

for (i in 1:520){
  
  if (min(building0[,i]) != 100){
    WAPsB0 <- append(WAPsB0,i)
  }
  
}

####Number of WAPs detected in building 0: 200
length(WAPsB0)

NotdetectedB0 <- setdiff(TotalWAPs,WAPsB0)

###Finding out which WAPs are never detected in Building 1
WAPsB1 <- c()

for (i in 1:520){
  
  if (min(building1[,i]) != 100){
    WAPsB1 <- append(WAPsB1,i)
  }
  
}

####Number of WAPs detected in building 1: 207
length(WAPsB1)

###Finding out which WAPs are never detected in Building 2
WAPsB2 <- c()

for (i in 1:520){
  
  if (min(building2[,i]) != 100){
    WAPsB2 <- append(WAPsB2,i)
  }
  
}

####Number of WAPs detected in building 2: 203
length(WAPsB2)



## Comparison trainset vs validation set
length(Notdetected)#55
length(Notdetected.Validation)#153

intersectedWAP <- intersect(detectedWAPs,detectedValidation)


cleantrainset <- trainset[,intersectedWAP]

cleantrainset[cleantrainset == 100] <- -105

variance <- apply(cleantrainset,2,function(x) sum(x != -105))

mean(variance)

varianceCheck <- variance[variance <= 50]

names(varianceCheck)

below50Variance <- cleantrainset[,names(varianceCheck)]

summary(below50Variance)

Variance <- nearZeroVar(cleantrainset, saveMetrics = TRUE, freqCut = 100, uniqueCut = 2)

VarianceFalse <- Variance[Variance$nzv == "FALSE",]
  
View(VarianceFalse)


cleantrainset <- cbind(cleantrainset,trainset[,521:529])


###Removing WAPs which were detected below 50 times ???



###Removing observations in which no WAP was detected

cleantrainset <- cleantrainset[apply(cleantrainset[,1:312],1,mean) != -105,]


###PCA (for longitude/latitude)
prcomp(cleantrainset,scale=FALSE)


###Analysis of values between -30 and 0

weird <- c(-30:0)
weird_values <- cleantrainset[apply(cleantrainset[,1:312],1,function(x) max(x) >= -30),]


###Data sets Building+Floor
cleantrainset$BuildingFloor <- paste(cleantrainset$BUILDINGID,"_",cleantrainset$FLOOR)

cleantrainset$BuildingFloor <- as.factor(cleantrainset$BuildingFloor)

B0F0 <- cleantrainset[cleantrainset$BuildingFloor == "0 _ 0",]

B0F1 <- cleantrainset[cleantrainset$BuildingFloor == "0 _ 1",]
  
B0F2 <- cleantrainset[cleantrainset$BuildingFloor == "0 _ 2",]

B0F3 <- cleantrainset[cleantrainset$BuildingFloor == "0 _ 3",]   



B1F0 <- cleantrainset[cleantrainset$BuildingFloor == "1 _ 0",]  
  
B1F1 <- cleantrainset[cleantrainset$BuildingFloor == "1 _ 1",]

B1F2 <- cleantrainset[cleantrainset$BuildingFloor == "1 _ 2",]

B1F3 <- cleantrainset[cleantrainset$BuildingFloor == "1 _ 3",]

##Back to the training set

cleanbuilding0 <- cleantrainset[cleantrainset$BUILDINGID == 0,]
cleanbuilding1 <- cleantrainset[cleantrainset$BUILDINGID == 1,]
cleanbuilding3 <- cleantrainset[cleantrainset$BUILDINGID == 2,]

ggplot(cleanbuilding0, aes(FLOOR,WAP016)) + geom_jitter()

max(cleantrainset$WAP001[cleanbuilding0])




goodsignal <- data.frame()

#for (eachrow in cleantrainset){
  
  #for(eachcol in 1:312)
  #if (eachcol >= -45 & eachcol != 100){
    #goodsignal <- rbind(goodsignal,eachrow)
  #}
  
#}



#Modelling
##Create vector with WAPs
#WAPs <- grep("WAP",names(cleantrainset), value = TRUE)

##Parallel Processing
cl <- makeCluster(2)
registerDoParallel(cl)


##Create training and test sets 1 (2 samples with 20% data each)
set.seed(123)

inTraining1 <- createDataPartition(cleantrainset$BUILDINGID, times = 2, p = .02)

training1 <- cleantrainset[inTraining1$Resample1,]
test1 <- cleantrainset[inTraining1$Resample2,]

##Check distribution of observations in buildings
ggplot(cleantrainset, aes(LATITUDE,LONGITUDE)) + geom_point()
ggplot(training1, aes(LATITUDE,LONGITUDE)) + geom_point()
ggplot(test1, aes(LATITUDE,LONGITUDE)) + geom_point()


##Create 10-fold cross-validation
fitcontrol <- trainControl(method = "repeatedcv",number = 10,repeats = 10)

#K-NN (312 WAPs, sample 399 rows)
##Train k-nn1 (312 WAPs)
knnFit1 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
                 -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-BuildingFloor,
                 data = training1, method = "knn", preProc = c("center","scale"), 
                 tuneLength = 15, trControl = fitcontrol)

knnFit1

#Apply k-nn1 to test set
testknn1 <- predict(knnFit1, test1)

#postResample k-nn1 to assess the metrics of the predictions
postResample(testknn1, test1$BUILDINGID)
##Accuracy 0.9899, Kappa 0.9842

#Plot confusion matrix k-nn1
confusionMatrix(data = testknn1, test1$BUILDINGID)

#RF1 (312 WAPs, sample 399 rows)
##Train RF1 (312 WAPs)
RFfit1 <- train(BUILDINGID~.-LONGITUDE-LATITUDE-FLOOR-SPACEID
                -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-BuildingFloor,
                data = training1, method = "rf", 
                preProc = c("center","scale"), ntree= 20, 
                tuneLength = 15, trControl = fitcontrol)

RFfit1

#Apply RF1 to test set
testRF1 <- predict(RFfit1, test1)

#postResample RF1 to assess the metrics of the predictions
postResample(testRF1, test1$BUILDINGID)
##Accuracy 0.9949, Kappa 0.9921 

#Plot confusion matrix RF1
confusionMatrix(data = testRF1, test1$BUILDINGID)

#SVM1 (312 WAPs, sample 399 rows)
##Train SVM1 (312 WAPs)
SVMLinearfit1 <- train(BUILDINGID~.-LONGITUDE-LATITUDE-FLOOR-SPACEID
                -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-BuildingFloor,
                data = training1, method = "svmLinear", 
                preProc = c("center","scale"),  
                tuneLength = 15, trControl = fitcontrol)

SVMLinearfit1

#Apply SVM1 to test set
testSVMLinear1 <- predict(SVMLinearfit1, test1)

#postResample SVM1 to assess the metrics of the predictions
postResample(testSVMLinear1, test1$BUILDINGID)
##Accuracy 1, Kappa 1 

#Plot confusion matrix SVM1
confusionMatrix(data = testSVMLinear1, test1$BUILDINGID)




