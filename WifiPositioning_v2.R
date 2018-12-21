#Wifi Positioning
#Start Date: December 3, 2018
#Deadline: Dec 21, 2018 

#Cleaning environment and uploading data ----
rm(list =ls())
setwd("C:/Users/carol/Desktop/Ubiqum/R/Wifi Positioning/task3-2-wifi-caroldorofei")
trainset <- read.csv("trainingData.csv")
validation <- read.csv("validationData.csv")

#Libraries ----
pacman::p_load(dplyr,ggplot2,reshape2,rlist,prob,caret,lattice,kazaam,pbdMPI,plotly)


#Data Preprocessing ----
##Converting floor and building into factors
trainset$FLOOR <- as.factor(trainset$FLOOR)
trainset$BUILDINGID <- as.factor(trainset$BUILDINGID)

validation$FLOOR <- as.factor(validation$FLOOR)
validation$BUILDINGID <- as.factor(validation$BUILDINGID)

##Finding out what WAPs are never detected ----
###TRAINING
detectedWAPs <- c()

for (i in 1:520){
  
  if (min(trainset[,i]) != 100){
    detectedWAPs <- append(detectedWAPs,i)
  }
  
}

##VALIDATION 
detectedValidation <- c()

for (i in 1:520){
  
  if (min(validation[,i]) != 100){
    detectedValidation <- append(detectedValidation,i)
  }
  
}

##Creating data sets with the intersected WAPs ----
intersectedWAP <- intersect(detectedWAPs,detectedValidation)

cleantrainset <- trainset[,intersectedWAP]

cleanvalidation <- validation[,intersectedWAP]

##Changing 100s to -105s ----
cleantrainset[cleantrainset == 100] <- -105

cleanvalidation[cleanvalidation == 100] <- -105

##Add 9 columns after WAPs ----
cleantrainset <- cbind(cleantrainset,trainset[,521:529])

cleanvalidation <- cbind(cleanvalidation,validation[,521:529])

##Removing observations in which no WAP was detected

cleantrainset <- cleantrainset[apply(cleantrainset[,1:312],1,mean) != -105,]

###Removing observation which contain values between -30 and 0

cleantrainset3 <- cleantrainset[apply(cleantrainset[,1:312],1,function(x) max(x) < -30),]

###Check for Waps not detected after removing rows - NONE in cleantraining3
detectedWAPs3 <- c()

for (i in 1:312){
  
  if (mean(cleantrainset3[,i]) == -105){
    detectedWAPs3 <- append(detectedWAPs3,i)
  }
  
}



###Finding observations with only one WAP detected ----
cleantrainset4 <- cleantrainset3[apply(cleantrainset3[,1:312],1,function(x) sum(x != -105)) > 2,]

cleantrainset5 <- cleantrainset3[apply(cleantrainset3[,1:312],1,function(x) sum(x != -105)) > 3,]

###Creating Building+Floor columns ----
cleantrainset$BuildingFloor <- paste0("B",cleantrainset$BUILDINGID,"F",cleantrainset$FLOOR)
cleantrainset$BuildingFloor <- as.factor(cleantrainset$BuildingFloor)

cleantrainset3$BuildingFloor <- paste0("B",cleantrainset3$BUILDINGID,"F",cleantrainset3$FLOOR)
cleantrainset3$BuildingFloor <- as.factor(cleantrainset3$BuildingFloor)

cleanvalidation$BuildingFloor <- paste0("B",cleanvalidation$BUILDINGID,"F",cleanvalidation$FLOOR)
cleanvalidation$BuildingFloor <- as.factor(cleanvalidation$BuildingFloor)


#WEIGHTED DATASETS
clean3WAPs <- cleantrainset3[,1:312]
clean3noWAPs <- cleantrainset3[,313:322]
cleantrainset3.2 <- cbind(as.data.frame(apply(clean3WAPs,2,function(x) 
  ifelse (x >= -70, x-x*0.2,x))),clean3noWAPs)

clean3WAPs_Val <- cleanvalidation[,1:312]
clean3noWAPs_Val <- cleanvalidation[,313:322]
cleanvalidation3.2 <- cbind(as.data.frame(apply(clean3WAPs_Val,2,function(x) 
  ifelse (x >= -70, x-x*0.2,x))),clean3noWAPs_Val)



##Creating datasets for each building ----
cleanbuilding0 <- cleantrainset[cleantrainset$BUILDINGID == 0,]
cleanbuilding1 <- cleantrainset[cleantrainset$BUILDINGID == 1,]
cleanbuilding3 <- cleantrainset[cleantrainset$BUILDINGID == 2,]

#MODELLING - BUILDING ----
##Parallel Processing
#cl <- makeCluster(2)
#registerDoParallel(cl)

##Create training and test sets 1 (sample with 20% data each)
set.seed(123)

inTraining1 <- createDataPartition(cleantrainset$BUILDINGID, times = 1, p = .02)

training1 <- cleantrainset[inTraining1$Resample1,]

##Check distribution of observations in buildings
ggplot(cleantrainset, aes(LATITUDE,LONGITUDE)) + geom_point()
ggplot(training1, aes(LATITUDE,LONGITUDE)) + geom_point()

##Create training and test sets 3 (sample with 20% data)
inTraining3 <- createDataPartition(cleantrainset3$BUILDINGID, times = 1, p = .3)
training3 <- cleantrainset3[inTraining3$Resample1,]
training3.Regr <- training3
training3.Regr$BUILDINGID <- as.numeric(training3.Regr$BUILDINGID)

##Create 10-fold cross-validation
fitcontrol <- trainControl(method = "repeatedcv",number = 2,repeats = 2)

#K-NN BUILDING (312 WAPs, sample 399 rows)
##Train k-nn1 (312 WAPs)
#knnFit1 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
#                 -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-BuildingFloor,
#                 data = training3, method = "knn", preProc = c("center","scale"), 
#                 tuneLength = 15, trControl = fitcontrol)



knnFit1 <- readRDS("knnSampleBuilding.rds")
knnFit1
#Accuracy 0.9927, Kappa 0.9884

#Apply k-nn1 to test set
testknn1 <- predict(knnFit1, cleanvalidation)

#postResample k-nn1 to assess the metrics of the predictions
postResample(testknn1, validation$BUILDINGID)
##training: Accuracy 0.9676, Kappa 0.9491

#Plot confusion matrix k-nn1
confusionMatrix(data = testknn1, validation$BUILDINGID)

#RF1 (312 WAPs, sample 399 rows)
##Train RF1 (312 WAPs)
RFfit1 <- readRDS("RFSampleBuilding.rds")

RFfit1
#Accuracy 0.9922, Kappa 0.9878

#Apply RF1 to test set
testRF1 <- predict(RFfit1, cleanvalidation)

#postResample RF1 to assess the metrics of the predictions
postResample(testRF1, cleanvalidation$BUILDINGID)
##Accuracy 0.9784, Kappa 0.9660 

#Plot confusion matrix RF1
confusionMatrix(data = testRF1, validation$BUILDINGID)


#SVMLinear1 (312 WAPs, sample 399 rows)
##Train SVMLinear1 (312 WAPs)
SVMLinearfit1 <- readRDS("SVMLinearSampleBuilding.rds")
SVMLinearfit1
#Accuracy 0.9934, Kappa 0.9896

#Apply SVMLinear1 to test set
Predicted_BuildingSVMLinear1 <- predict(SVMLinearfit1, cleanvalidation)

#postResample SVMLinear1 to assess the metrics of the predictions
postResample(Predicted_BuildingSVMLinear1, cleanvalidation$BUILDINGID)
##Accuracy 0.9892, Kappa 0.9830 

#Plot confusion matrix SVMLinear1
confusionMatrix(data = Predicted_BuildingSVMLinear1, validation$BUILDINGID)


#SVMLinear2 BUILDING (312 WAPs, 19861 rows)
##Train SVMLinear2 (312 WAPs)
SVMLinearfit2 <- readRDS("SVMLinear2SampleBuilding.rds")
SVMLinearfit2
#Accuracy 0.9999, Kappa 0.9999

#Apply SVMLinear2 to validation set
Predicted_BuildingSVMLinear2 <- predict(SVMLinearfit2, cleanvalidation)

#postResample SVMLinear1 to assess the metrics of the predictions
postResample(Predicted_BuildingSVMLinear2, cleanvalidation$BUILDINGID)
##Accuracy 0.9991, Kappa 0.9986 

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_BuildingSVMLinear2, cleanvalidation$BUILDINGID)


#SVMLinear4 BUILDING (312 WAPs, 19389 - rows removed observations which contain values >= -30)
##Train SVMLinear3 (312 WAPs, 19389 rows)
SVMLinearfit4 <- readRDS("SVMLinear4Building.rds")
SVMLinearfit4
#Accuracy 0.9999, Kappa 0.9999

#Apply SVMLinear4 to validation set
Predicted_BuildingSVMLinear4 <- predict(SVMLinearfit4, cleanvalidation)

#postResample SVMLinear1 to assess the metrics of the predictions
postResample(Predicted_BuildingSVMLinear4, cleanvalidation$BUILDINGID)
##Accuracy 0.9991, Kappa 0.9986 

#Plot confusion matrix SVMLinear4
confusionMatrix(data = Predicted_BuildingSVMLinear4, cleanvalidation$BUILDINGID)

#SVMLinear5 Weighted BUILDING (312 WAPs, 19861 rows)
##Train SVMLinear5 (312 WAPs)
SVMLinearfit5 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
                       -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-BuildingFloor,
                       data = cleantrainset3.2, method = "svmLinear", 
                       tuneLength = 15, trControl = fitcontrol)

SVMLinearfit5

#Apply SVMLinear5 to validation set
Predicted_BuildingSVMLinear5 <- predict(SVMLinearfit5, cleanvalidation3.2)

#postResample SVMLinear1 to assess the metrics of the predictions
postResample(Predicted_BuildingSVMLinear5, cleanvalidation3.2$BUILDINGID)
##Accuracy 0.9991, Kappa 0.9986 

#Plot confusion matrix SVMLinear5
confusionMatrix(data = Predicted_BuildingSVMLinear5, cleanvalidation$BUILDINGID)

validationBUILDING$RealBuilding <- cleanvalidation$BUILDINGID
filter(validationBUILDING, BUILDINGID == 1) %>% filter(validationBUILDING, RealBuilding == 0)

###MODELLING FLOOR ----

###New training set with predicted BUILDING
validationBUILDING <- cleanvalidation

validationBUILDING$BuildingFloor <- paste0("B",Predicted_BuildingSVMLinear4,"F",cleanvalidation$FLOOR)
validationBUILDING$BuildingFloor <- as.factor(validationBUILDING$BuildingFloor)

#K-NN Floor (312 WAPs + Predicted Building+Floor, sample 399 rows)
##Train k-nn1 (312 WAPs)
#knnFloorfit1 <- train(BuildingFloor~. -LONGITUDE-LATITUDE-SPACEID
#                      -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR,
#                      data = training1, method = "knn", preProc = c("center","scale"), 
#                      tuneLength = 15, trControl = fitcontrol)

knnFloorfit1 <- readRDS("knnSampleFloor.rds")
knnFloorfit1

#Apply k-nn1 to test set
knnFloor1 <- predict(knnFloorfit1, validationBUILDING)

#postResample k-nn1 to assess the metrics of the predictions
postResample(knnFloor1, cleanvalidation$BuildingFloor)
##Accuracy 0.5877, Kappa 0.5491

#Plot confusion matrix k-nn1
confusionMatrix(data = knnFloor1, cleanvalidation$BuildingFloor)


#SVMLinear Floor (312 WAPs, sample 399 rows)
##Train SVMLinear Floor (312 WAPs)
#SVMLinearFloor1 <- train(BuildingFloor~. -LONGITUDE-LATITUDE-SPACEID
#                         -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR,
#                         data = training1, method = "svmLinear", 
#                         tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor1 <- readRDS("SVMLinear1SampleFloor.rds")
SVMLinearFloor1

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear1 <- predict(SVMLinearFloor1, validationBUILDING)

#postResample SVMLinear1 Floor to assess the metrics of the predictions
postResample(Predicted_FloorSVMLinear1, cleanvalidation$BuildingFloor)
##Accuracy 0.8181, Kappa 0.7981 

#Plot confusion matrix SVMLinear1
confusionMatrix(data = Predicted_FloorSVMLinear1, cleanvalidation$BuildingFloor)

#SVMLinear Floor (312 WAPs, 19389 rows - cleantrainset3)
##Train SVMLinear Floor (312 WAPs)
#SVMLinearFloor2 <- train(BuildingFloor~. -LONGITUDE-LATITUDE-SPACEID
#                         -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR,
#                         data = cleantrainset3, method = "svmLinear", 
#                         tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor2 <- readRDS("SVMLinear2Floor.rds")
SVMLinearFloor2

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear2 <- predict(SVMLinearFloor2, validationBUILDING)

#postResample SVMLinear1 Floor to assess the metrics of the predictions
postResample(Predicted_FloorSVMLinear2, cleanvalidation$BuildingFloor)
##Accuracy 0.8830, Kappa 0.8691 

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_FloorSVMLinear2, cleanvalidation$BuildingFloor)



##MODELLING FLOOR for individual buildings

###Split training & validation sets per building
B0_trainset3WAPs <- cleantrainset3[cleantrainset3$BUILDINGID == 0,1:312]
B0_trainset3NoWAPs <- cleantrainset3[cleantrainset3$BUILDINGID == 0,313:322]

B1_trainset3WAPs <- cleantrainset3[cleantrainset3$BUILDINGID == 1,1:312]
B1_trainset3NoWAPs <- cleantrainset3[cleantrainset3$BUILDINGID == 1,313:322]

B2_trainset3WAPs <- cleantrainset3[cleantrainset3$BUILDINGID == 2,1:312]
B2_trainset3NoWAPs <- cleantrainset3[cleantrainset3$BUILDINGID == 2,313:322]

B0_validation3WAPs <- cleanvalidation[cleanvalidation$BUILDINGID == 0,1:312]
B0_validation3NoWAPs <- cleanvalidation[cleanvalidation$BUILDINGID == 0,313:322]

B1_validation3WAPs <- cleanvalidation[cleanvalidation$BUILDINGID == 1,1:312]
B1_validation3NoWAPs <- cleanvalidation[cleanvalidation$BUILDINGID == 1,313:322]

B2_validation3WAPs <- cleanvalidation[cleanvalidation$BUILDINGID == 2,1:312]
B2_validation3NoWAPs <- cleanvalidation[cleanvalidation$BUILDINGID == 2,313:322]

##Finding WAPs never detected per building
###TRAINING & VALIDATION
###BUILDING 0
B0_trainset3 <- cbind(B0_trainset3WAPs[,apply(B0_trainset3WAPs,2,mean) != -105],B0_trainset3NoWAPs)
B0_validation3 <- cbind(B0_validation3WAPs[,apply(B0_validation3WAPs,2,mean) != -105],B0_validation3NoWAPs)

intersectedB0 <- intersect(names(B0_trainset3),names(B0_validation3)) 
length(intersectedB0)#149

B0_trainset3 <- B0_trainset3[,intersectedB0]
B0_trainset3$BuildingFloor <- factor(B0_trainset3$BuildingFloor)
B0_validation3 <- B0_validation3[,intersectedB0]
B0_validation3$BuildingFloor <- factor(B0_validation3$BuildingFloor)



B1_trainset3 <- cbind(B1_trainset3WAPs[,apply(B1_trainset3WAPs,2,mean) != -105],B1_trainset3NoWAPs)

B1_validation3 <- cbind(B1_validation3WAPs[,apply(B1_validation3WAPs,2,mean) != -105],B1_validation3NoWAPs)

intersectedB1 <- intersect(names(B1_trainset3),names(B1_validation3)) 
length(intersectedB1)#156

B1_trainset3 <- B1_trainset3[,intersectedB1]
B1_trainset3$BuildingFloor <- factor(B1_trainset3$BuildingFloor)
B1_validation3 <- B1_validation3[,intersectedB1]
B1_validation3$BuildingFloor <- factor(B1_validation3$BuildingFloor)

B2_trainset3 <- cbind(B2_trainset3WAPs[,apply(B2_trainset3WAPs,2,mean) != -105],B2_trainset3NoWAPs)
B2_validation3 <- cbind(B2_validation3WAPs[,apply(B2_validation3WAPs,2,mean) != -105],B2_validation3NoWAPs)

intersectedB2 <- intersect(names(B2_trainset3),names(B2_validation3)) 
length(intersectedB2)#116

B2_trainset3 <- B2_trainset3[,intersectedB2]
B2_trainset3$BuildingFloor <- factor(B2_trainset3$BuildingFloor)
B2_validation3 <- B2_validation3[,intersectedB2]
B2_validation3$BuildingFloor <- factor(B2_validation3$BuildingFloor)


check <- union(intersectedB0,intersectedB1,intersectedB2)
length(check)

B0_validationBUILDING <- validationBUILDING[validationBUILDING$BUILDINGID == "0",]
B0_validationBUILDING$BUILDINGID <- factor(B0_validationBUILDING$BUILDINGID)
B0_validationBUILDING$BuildingFloor <- factor(B0_validationBUILDING$BuildingFloor)

B1_validationBUILDING <- validationBUILDING[validationBUILDING$BUILDINGID == "1",]
B1_validationBUILDING$BUILDINGID <- factor(B1_validationBUILDING$BUILDINGID)

B2_validationBUILDING <- validationBUILDING[validationBUILDING$BUILDINGID == "2",]
B2_validationBUILDING$BUILDINGID <- factor(B2_validationBUILDING$BUILDINGID)


##MODELS

#SVMLinear Floor BUILDING 0
##Train SVMLinear Floor 
#SVMLinearFloor2_B0 <- train(BuildingFloor~. -LONGITUDE-LATITUDE-SPACEID
#                         -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR,
#                         data = B0_trainset3, method = "svmLinear", 
#                         tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor2_B0 <- readRDS("SVMLinear2Floor_B0.rds")
SVMLinearFloor2_B0

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear2_B0 <- predict(SVMLinearFloor2_B0, B0_validationBUILDING)

#postResample SVMLinear1 Floor to assess the metrics of the predictions
postResample(Predicted_FloorSVMLinear2_B0, B0_validation3$BuildingFloor)
##Accuracy 0.9477, Kappa 0.9265 

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_FloorSVMLinear2_B0, B0_validation3$BuildingFloor)

#SVMLinear Floor BUILDING 0
##Train SVMLinear Floor 
B0_trainset3v2 <- B0_trainset3
B0_trainset3v2$BuildingFloor <- NULL
B0_validationBUILDINGv2 <- B0_validationBUILDING
B0_validationBUILDINGv2$BuildingFloor <- NULL

#SVMLinearFloor2_B0v2 <- train(FLOOR~. -LONGITUDE-LATITUDE-SPACEID
#                         -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP,
#                         data = B0_trainset3v2, method = "svmLinear", 
#                         tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor2_B0v2 <- readRDS("SVMLinear2Floor_B0.rds")
SVMLinearFloor2_B0v2

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear2_B0v2 <- predict(SVMLinearFloor2_B0v2, B0_validationBUILDINGv2)

#postResample SVMLinear1 Floor to assess the metrics of the predictions
postResample(Predicted_FloorSVMLinear2_B0v2, B0_validationBUILDINGv2$FLOOR)
##Accuracy 0.9477, Kappa 0.9265 





#SVMLinear Floor BUILDING 1
##Train SVMLinear Floor 
#SVMLinearFloor2_B1 <- train(BuildingFloor~. -LONGITUDE-LATITUDE-SPACEID
#                            -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR,
#                            data = B1_trainset3, method = "svmLinear", 
#                            tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor2_B1 <- readRDS("SVMLinear2Floor_B1.rds")
SVMLinearFloor2_B1

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear2_B1 <- predict(SVMLinearFloor2_B1, B1_validationBUILDING)

#postResample SVMLinear1 Floor to assess the metrics of the predictions
postResample(Predicted_FloorSVMLinear2_B1, B1_validation3$BuildingFloor)
##Accuracy 0.7948, Kappa 0.7056 

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_FloorSVMLinear2_B1, B1_validation3$BuildingFloor)

#RF Floor BUILDING 1
##Train RF Floor 
#RFFloor2_B1 <- train(BuildingFloor~. -LONGITUDE-LATITUDE-SPACEID
#                            -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR,
#                            data = B1_trainset3, method = "rf", ntree = 20, 
#                            tuneLength = 15, trControl = fitcontrol)

RFFloor2_B1 <- readRDS("RF2Floor_B1.rds")
RFFloor2_B1

#Apply RF Floor to test set
Predicted_FloorRF2_B1 <- predict(RFFloor2_B1, B1_validationBUILDING)

#postResample RF2 Floor to assess the metrics of the predictions
postResample(Predicted_FloorRF2_B1, B1_validation3$BuildingFloor)
##Accuracy 0.8078, Kappa 0.7267 

#Plot confusion matrix RF2
confusionMatrix(data = Predicted_FloorRF2_B1, B1_validation3$BuildingFloor)


#Error analysis
checkVal_B1F2 <- B1_validation3
checkVal_B1F2$PredFloor <- Predicted_FloorRF2_B1
checkVal_B1F2 <- filter(checkVal_B1F2, Predicted_FloorRF2_B1 == "B1F2" & BuildingFloor == "B1F1")
checkVal_B1F2$BuildingFloor <- checkVal_B1F2$PredFloor 
checkVal_B1F2$PredFloor <- NULL
checkVal_B1F2$Real_Pred <- "Predicted"
checkVal_B1F2Real <- B1_validation3
checkVal_B1F2Real$Real_Pred <- "Real" 
checkVal_B1F2a <- rbind(checkVal_B1F2Real,checkVal_B1F2)

#The plot below shows all validation points for B1 in blue and wrong predictions in pink 
#Real: B1F1, Predicted B1F2
ggplot(checkVal_B1F2a, aes(checkVal_B1F2a$LATITUDE,checkVal_B1F2a$LONGITUDE, colour = checkVal_B1F2a$Real_Pred)) +
  geom_point()

#Range Longitude -7569 to -7474
#Range Latitude 4864840 to 4864901

checkRangeB1F1 <- B1_trainset3 %>% filter(LONGITUDE >= -7569 & LONGITUDE <= -7474) %>%
  filter(LATITUDE >= 4864840 & LATITUDE <= 4864901) %>% filter(BuildingFloor == "B1F1")

checkRangeB1F1Train <- B1_trainset3 %>% filter(BuildingFloor == "B1F1")

checkRangeB1F1Train$Train_error <- "Training"

checkRangeB1F1Error <- checkVal_B1F2[,1:156]

checkRangeB1F1Error$Train_error <- "Error"

checkTrainError <- rbind(checkRangeB1F1Train,checkRangeB1F1Error)

ggplot(checkRangeB1F1Train,aes(checkRangeB1F1Train$LATITUDE,checkRangeB1F1Train$LONGITUDE)) +
  geom_point()


#The plot below shows the trainset points in B0 floor 1 and the 37/42 errors 
ggplot(checkTrainError,aes(checkTrainError$LATITUDE,checkTrainError$LONGITUDE, colour = checkTrainError$Train_error)) +
  geom_point()


checkVal_B1F3 <- B1_validation3
checkVal_B1F3$PredFloor <- Predicted_FloorRF2_B1
checkVal_B1F3 <- filter(checkVal_B1F3, Predicted_FloorRF2_B1 == "B1F3" & BuildingFloor == "B1F1")
checkVal_B1F3$BuildingFloor <- checkVal_B1F3$PredFloor 
checkVal_B1F3$PredFloor <- NULL
checkVal_B1F3$Real_Pred <- "Predicted"
checkVal_B1F3Real <- B1_validation3
checkVal_B1F3Real$Real_Pred <- "Real" 
checkVal_B1F3a <- rbind(checkVal_B1F3Real,checkVal_B1F3)



ggplot(checkVal_B1F3a, aes(checkVal_B1F3a$LATITUDE,checkVal_B1F3a$LONGITUDE, colour = checkVal_B1F3a$Real_Pred)) +
  geom_point()


ggplot(B1_trainset3, aes(B1_trainset3$LATITUDE,B1_trainset3$LONGITUDE)) +
  geom_point()


#SVMLinear Floor BUILDING 2
##Train SVMLinear Floor 
#SVMLinearFloor2_B2 <- train(BuildingFloor~. -LONGITUDE-LATITUDE-SPACEID
#                            -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR,
#                            data = B2_trainset3, method = "svmLinear", 
#                            tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor2_B2 <- readRDS("SVMLinear2Floor_B2.rds")
SVMLinearFloor2_B2

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear2_B2 <- predict(SVMLinearFloor2_B2, B2_validationBUILDING)

#postResample SVMLinear1 Floor to assess the metrics of the predictions
postResample(Predicted_FloorSVMLinear2_B2, B2_validation3$BuildingFloor)
##Accuracy 0.9328, Kappa 0.9080 

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_FloorSVMLinear2_B2, B2_validation3$BuildingFloor)

#MODELLING LONGITUDE/LATITUDE ----
##Preparing data frames for Regression 
cleantrainset3.Regr <- cleantrainset3
cleantrainset3.Regr$BUILDINGID <- as.numeric(cleantrainset3.Regr$BUILDINGID)

validationBUILDING.Regr <- validationBUILDING
validationBUILDING.Regr$BUILDINGID <- as.numeric(validationBUILDING.Regr$BUILDINGID)

B0_trainset3.Regr <- B0_trainset3
B0_trainset3.Regr$BUILDINGID <- 0

B1_trainset3.Regr <- B1_trainset3
B1_trainset3.Regr$BUILDINGID <- 1

B2_trainset3.Regr <- B2_trainset3
B2_trainset3.Regr$BUILDINGID <- 2

B0_validation3.Regr <- data.frame(B0_validation3)
B0_validation3.Regr$BUILDINGID <- 0

B1_validation3.Regr <- B1_validation3
B1_validation3.Regr$BUILDINGID <- 1

B2_validation3.Regr <- B2_validation3
B2_validation3.Regr$BUILDINGID <- 2

B0_validationBUILDING.Regr <- B0_validationBUILDING
B0_validationBUILDING.Regr$BUILDINGID <- 0

B1_validationBUILDING.Regr <- B1_validationBUILDING
B1_validationBUILDING.Regr$BUILDINGID <- 1

B2_validationBUILDING.Regr <- B2_validationBUILDING
B2_validationBUILDING.Regr$BUILDINGID <- 2


##Based on the variance below, we have decided to predict latitude before longitude
summary(cleantrainset3$LONGITUDE)#varies from -7691 to -7301 (390 "variance") 
summary(cleantrainset3$LATITUDE)#varies from 4864746 to 4865017 (271 "variance")

#LATITUDE ----
#K-NN Latitude (312 WAPs + Predicted Building, )
##Train k-nn1 LATITUDE (312 WAPs)

#knnLatitudefit1 <- train(LATITUDE~. -LONGITUDE-SPACEID
#                         -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                         data = cleantrainset3.Regr, method = "knn", preProc = c("center","scale"), 
#                         tuneLength = 15, trControl = fitcontrol)

knnLatitudefit1 <- readRDS("knnLatitudefit1.rds")
knnLatitudefit1


#Apply k-nn1 to test set
knnLatitude1 <- predict(knnLatitudefit1, validationBUILDING.Regr)

#postResample k-nn1 to assess the metrics of the predictions
postResample(knnLatitude1, validationBUILDING.Regr$LATITUDE)
##RMSE 15.62, Rsquared 0.95, MAE 7.55

#Error analysis
ErroLatitude_Knn <-  knnLatitude1 - validationBUILDING.Regr$LATITUDE
hist(ErroLatitude_Knn)

#RF Latitude (312 WAPs + Predicted Building, )
##Train RF1 LATITUDE (312 WAPs)

#RFLatitudefit1 <- train(LATITUDE~. -LONGITUDE-SPACEID
#                         -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                         data = training3.Regr, method = "rf", ntree = 5,preProc = c("center","scale"), 
#                         tuneLength = 15, trControl = fitcontrol)

RFLatitudefit1 <- readRDS("RFLatitude1.rds")

RFLatitudefit1


#Apply RF1 to test set
RFLatitude1 <- predict(RFLatitudefit1, validationBUILDING.Regr)

#postResample RF1 to assess the metrics of the predictions
postResample(RFLatitude1, cleanvalidation$LATITUDE)
##RMSE 11.566, Rsquared 0.97, MAE 7.67 

#Error analysis
ErroLatitude_RF <-  RFLatitude1 - cleanvalidation$LATITUDE
hist(ErroLatitude_RF)


#SVMLinear Latitude (312 WAPs + Predicted Building, )
##Train SVMLinear1 LATITUDE (312 WAPs)

#SVMLinearLatitudefit1 <- train(LATITUDE~. -LONGITUDE-SPACEID
#                        -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                        data = training3.Regr, method = "svmLinear", preProc = c("center","scale"), 
#                        tuneLength = 15, trControl = fitcontrol)

SVMLinearLatitudefit1 <- readRDS("SVMLinearLatitude1.rds")
SVMLinearLatitudefit1

#Apply RF1 to test set
SVMLinearLatitude1 <- predict(SVMLinearLatitudefit1, validationBUILDING.Regr)

#postResample RF1 to assess the metrics of the predictions
postResample(SVMLinearLatitude1, validationBUILDING.Regr$LATITUDE)
##RMSE 16, Rsquared 0.94, MAE 11.82 

#Error analysis
ErroLatitude_SVMLinear1 <-  SVMLinearLatitude1 - validationBUILDING.Regr$LATITUDE
hist(ErroLatitude_SVMLinear1)


#RF Latitude per BUILDING
##Train RF1 LATITUDE B0
#RFLatitudefit1_B0 <- train(LATITUDE~. -LONGITUDE-SPACEID
#                        -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                        data = B0_trainset3.Regr, method = "rf", ntree = 5,preProc = c("center","scale"), 
#                        tuneLength = 15, trControl = fitcontrol)

RFLatitudefit1_B0 <- readRDS("RFLatitudefit1_B0.rds")
RFLatitudefit1_B0

#Apply RF1 to test set

RFLatitude1_B0 <- predict(RFLatitudefit1_B0, B0_validationBUILDING.Regr)


#postResample RF1 to assess the metrics of the predictions
postResample(RFLatitude1_B0, B0_validation3.Regr$LATITUDE)
##RMSE 6.277, Rsquared 0.96, MAE 4.41 

#Error analysis
ErroLatitude_RF_B0 <- RFLatitude1_B0 - B0_validation3.Regr$LATITUDE
hist(ErroLatitude_RF_B0)


##Train RF1 LATITUDE B1
#RFLatitudefit1_B1 <- train(LATITUDE~. -LONGITUDE-SPACEID
#                           -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                           data = B1_trainset3.Regr, method = "rf", ntree = 5,preProc = c("center","scale"), 
#                           tuneLength = 15, trControl = fitcontrol)

RFLatitudefit1_B1 <- readRDS("RFLatitudefit1_B1.rds")
RFLatitudefit1_B1

#Apply RF1 to test set

RFLatitude1_B1 <- predict(RFLatitudefit1_B1, B1_validationBUILDING.Regr)


#postResample RF1 to assess the metrics of the predictions
postResample(RFLatitude1_B1, B1_validation3.Regr$LATITUDE)
##RMSE 11.99, Rsquared 0.88, MAE 8.58 

#Error analysis
ErrorLatitude_RF_B1 <- RFLatitude1_B1 - B1_validation3.Regr$LATITUDE
hist(ErrorLatitude_RF_B1)


##Train RF1 LATITUDE B2
#RFLatitudefit1_B2 <- train(LATITUDE~. -LONGITUDE-SPACEID
#                           -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                           data = B2_trainset3.Regr, method = "rf", ntree = 5,preProc = c("center","scale"), 
#                           tuneLength = 15, trControl = fitcontrol)

RFLatitudefit1_B2 <- readRDS("RFLatitudefit1_B2.rds")
RFLatitudefit1_B2

#Apply RF1 to test set

RFLatitude1_B2 <- predict(RFLatitudefit1_B2, B2_validationBUILDING.Regr)


#postResample RF1 to assess the metrics of the predictions
postResample(RFLatitude1_B2, B2_validation3.Regr$LATITUDE)
##RMSE 11.78, Rsquared 0.83, MAE 8.02

#Error analysis
ErrorLatitude_RF_B2 <- RFLatitude1_B2 - B2_validation3.Regr$LATITUDE
hist(ErrorLatitude_RF_B2)




#LONGITUDE ----
#K-NN Longitude (312 WAPs + Predicted Building, )
##Train k-nn1 LONGITUDE (312 WAPs)

#knnLongitudefit1 <- train(LONGITUDE~. -LATITUDE-SPACEID
#                          -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                          data = training3.Regr, method = "knn", preProc = c("center","scale"), 
#                          tuneLength = 15, trControl = fitcontrol)

knnLongitudefit1 <- readRDS("knnLongitudefit1.rds")
knnLongitudefit1

#Apply k-nn1 to test set
knnLongitude1 <- predict(knnLongitudefit1, validationBUILDING.Regr)

#postResample k-nn1 to assess the metrics of the predictions
postResample(knnLongitude1, validationBUILDING.Regr$LONGITUDE)
##RMSE 18.91, Rsquared 0.97, MAE 8.28 

#Error analysis
ErrorLongitude_knn <- knnLongitude1 - validationBUILDING.Regr$LONGITUDE
hist(ErrorLongitude_knn)


#RF Longitude (312 WAPs + Predicted Building, )
##Train RF1 LONGITUDE (312 WAPs)

#RFLongitudefit1 <- train(LONGITUDE~. -LATITUDE-SPACEID
#                        -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                        data = training3.Regr, method = "rf", ntree = 5, preProc = c("center","scale"), 
#                        tuneLength = 15, trControl = fitcontrol)

RFLongitudefit1 <- readRDS("RFLongitudefit1.rds")
RFLongitudefit1

#RFLongitudefit1 <- readRDS("RFLongitudefit1.rds")

#Apply RF1 to test set
RFLongitude1 <- predict(RFLongitudefit1, validationBUILDING.Regr)

#postResample RF1 to assess the metrics of the predictions
postResample(RFLongitude1, cleanvalidation$LONGITUDE)
##RMSE 12.55, Rsquared 0.989, MAE 7.88 

#Error analysis
ErrorLongitude_RF <- RFLongitude1 - validationBUILDING.Regr$LONGITUDE
hist(ErrorLongitude_RF)



##Train SVMLinear1 LONGITUDE (312 WAPs)

#SVMLinearLongitudefit1 <- train(LONGITUDE~. -LATITUDE-SPACEID
#                         -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                         data = training3.Regr, method = "svmLinear", preProc = c("center","scale"), 
#                         tuneLength = 15, trControl = fitcontrol)

SVMLinearLongitudefit1 <- readRDS("SVMLinearLongitudefit1.rds")
SVMLinearLongitudefit1

#SVMLinearLongitudefit1 <- readRDS("SVMLinearLongitudefit1.rds")

#Apply SVMLinear1 to test set
SVMLinearLongitude1 <- predict(SVMLinearLongitudefit1, validationBUILDING.Regr)

#postResample RF1 to assess the metrics of the predictions
postResample(SVMLinearLongitude1, validationBUILDING.Regr$LONGITUDE)
##RMSE 17.64, Rsquared 0.97, MAE 13.30 

#Error analysis
ErrorLongitude_SVM <- SVMLinearLongitude1 - validationBUILDING.Regr$LONGITUDE
hist(ErrorLongitude_SVM)



#RF Longitude per BUILDING
##Train RF1 LONGITUDE B0
#RFLongitudefit1_B0 <- train(LONGITUDE~. -LATITUDE-SPACEID
#                           -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                           data = B0_trainset3.Regr, method = "rf", ntree = 5,preProc = c("center","scale"), 
#                           tuneLength = 15, trControl = fitcontrol)

RFLongitudefit1_B0 <- readRDS("RFLongitudefit1_B0.rds")
RFLongitudefit1_B0

#Apply RF1 to test set

RFLongitude1_B0 <- predict(RFLongitudefit1_B0, B0_validationBUILDING.Regr)


#postResample RF1 to assess the metrics of the predictions
postResample(RFLongitude1_B0, B0_validation3.Regr$LONGITUDE)
##RMSE 8.27, Rsquared 0.90, MAE 5.32 

#Error analysis
ErrorLongitude_RF_B0 <- RFLongitude1_B0 - B0_validation3.Regr$LONGITUDE
hist(ErrorLongitude_RF_B0)



##Train RF1 LONGITUDE B1
#RFLongitudefit1_B1 <- train(LONGITUDE~. -LATITUDE-SPACEID
#                           -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                           data = B1_trainset3.Regr, method = "rf", ntree = 5,preProc = c("center","scale"), 
#                           tuneLength = 15)


RFLongitudefit1_B1 <- readRDS("RFLongitudefit1_B1.rds")
RFLongitudefit1_B1

#Apply RF1 to test set

RFLongitude1_B1 <- predict(RFLongitudefit1_B1, B1_validationBUILDING.Regr)


#postResample RF1 to assess the metrics of the predictions
postResample(RFLongitude1_B1, B1_validation3.Regr$LONGITUDE)
##RMSE 10.59, Rsquared 0.95, MAE 7.76 

#Error analysis
ErrorLongitude_RF_B1 <- RFLongitude1_B1 - B1_validation3.Regr$LONGITUDE
hist(ErrorLongitude_RF_B1)


##Train RF1 LONGITUDE B2
#RFLongitudefit1_B2 <- train(LONGITUDE~. -LATITUDE-SPACEID
#                           -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP-FLOOR-BuildingFloor,
#                           data = B2_trainset3.Regr, method = "rf", ntree = 5,preProc = c("center","scale"), 
#                           tuneLength = 15,trControl = fitcontrol)

RFLongitudefit1_B2 <- readRDS("RFLongitudefit1_B2.rds")
RFLongitudefit1_B2

#Apply RF1 to test set

RFLongitude1_B2 <- predict(RFLongitudefit1_B2, B2_validationBUILDING.Regr)

#postResample RF1 to assess the metrics of the predictions
postResample(RFLongitude1_B2, B2_validationBUILDING.Regr$LONGITUDE)
##RMSE 12.60, Rsquared 0.84, MAE 8.37

#ERROR ANALYSIS ----
##Create validation set with predicted LAT & LONG
validationLatLong_B0 <- B0_validation3.Regr
validationLatLong_B0$Real_Predicted <- "Real"

validationLatLong_B0Pred <-  B0_validation3.Regr
validationLatLong_B0Pred$LATITUDE <-  RFLatitude1_B0
validationLatLong_B0Pred$LONGITUDE <- RFLongitude1_B0
validationLatLong_B0Pred$Real_Predicted <- "Predicted"

validationLatLong_B0Compl <- rbind(validationLatLong_B0,validationLatLong_B0Pred)

B0_validation_predictions <- B0_validation3.Regr
B0_validation_predictions$Pred_Latitude <- RFLatitude1_B0
B0_validation_predictions$Pred_Longitude <- RFLongitude1_B0

B0_validation_predictions$Error_Latitude <- B0_validation_predictions$Pred_Latitude-B0_validation_predictions$LATITUDE
B0_validation_predictions$Error_Longitude <- B0_validation_predictions$Pred_Longitude-B0_validation_predictions$LONGITUDE

summary(B0_validation_predictions$Error_Latitude)
hist(B0_validation_predictions$Error_Latitude)

B0_higherror10 <- filter(B0_validation_predictions, Error_Latitude <= -10 | Error_Latitude >= 10)
summary(B0_higherror10)
#F0 7, F1 22, F2 15, F3 11

B0_higherror20 <- filter(B0_validation_predictions, Error_Latitude <= -20 | Error_Latitude >= 20)
summary(B0_higherror20)
#F0 0, F1 2, F2 0, F3 3

ggplot(validationLatLong_B0Compl,aes(validationLatLong_B0Compl$LONGITUDE,validationLatLong_B0Compl$LATITUDE, colour = Real_Predicted)) +
  geom_point()

ggplot(validationLatLong_B0) +
  geom_point(aes(validationLatLong_B0$LONGITUDE,validationLatLong_B0$LATITUDE))


#Check unique points (LAT/LONG) in trainset and validation

LatLong <- trainset %>% select(LATITUDE,LONGITUDE)

uniqueLatLong <- unique(LatLong)
#692 unique combinations

uniqueLatLong$Train_Val <- "Train"

ggplot(uniqueLatLong,aes(uniqueLatLong$LATITUDE,uniqueLatLong$LONGITUDE))+
  geom_point()

LatLong_Val <- validation %>% select(LATITUDE,LONGITUDE)

uniqueLatLong_Val <- unique(LatLong_Val)
#1068 unique combinations
uniqueLatLong_Val$Train_Val <- "Validation"

ggplot(uniqueLatLong_Val,aes(uniqueLatLong_Val$LATITUDE,uniqueLatLong_Val$LONGITUDE))+
  geom_point()

uniqueTrain_Val <- rbind(uniqueLatLong,uniqueLatLong_Val) 

plot_ly(x=uniqueTrain_Val$LATITUDE,y=uniqueTrain_Val$LONGITUDE,z=uniqueTrain_Val$Train_Val,
        color=uniqueTrain_Val$Train_Val,
        type = "scatter3d",mode="markers")

ggplot(uniqueTrain_Val,aes(uniqueTrain_Val$LATITUDE,uniqueTrain_Val$LONGITUDE,
                           colour=uniqueTrain_Val$Train_Val))+
  geom_point()

plot_ly(x=validationLatLong_B0Compl$LATITUDE,y=validationLatLong_B0Compl$LONGITUDE,
        z=validationLatLong_B0Compl$FLOOR,
        color=validationLatLong_B0Compl$Real_Predicted,
        type = "scatter3d",mode="markers")




  
