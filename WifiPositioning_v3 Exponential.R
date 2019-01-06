#Wifi Positioning
#Start Date: December 3, 2018
#Deadline: Dec 21, 2018 

#Cleaning environment and uploading data ----
rm(list =ls())
setwd("C:/Users/carol/Desktop/Ubiqum/R/Wifi Positioning/task3-2-wifi-caroldorofei")
trainset <- read.csv("trainingData.csv")
validation <- read.csv("validationData.csv")

#Libraries ----
pacman::p_load(dplyr,ggplot2,reshape2,rlist,prob,caret,
               lattice,kazaam,pbdMPI,plotly,EnvStats,
               gridExtra,grid,formattable)


#Data Preprocessing ----
##Converting floor and building into factors
trainset$FLOOR <- as.factor(trainset$FLOOR)
trainset$BUILDINGID <- as.factor(trainset$BUILDINGID)

validation$FLOOR <- as.factor(validation$FLOOR)
validation$BUILDINGID <- as.factor(validation$BUILDINGID)

##FEATURE SELECTION - Removing WAPs never detected ----
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

##Creating data sets with the intersected WAPs
intersectedWAP <- intersect(detectedWAPs,detectedValidation)

cleantrainset <- trainset[,intersectedWAP] #19937 obs of 312 variables
cleanvalidation <- validation[,intersectedWAP] #1111 obs of 312 variables

##Changing 100s to -105s ----
cleantrainset[cleantrainset == 100] <- -105
cleanvalidation[cleanvalidation == 100] <- -105

##Add 9 columns after WAPs
cleantrainset <- cbind(cleantrainset,trainset[,521:529])
#19937 obs of 321 variables
cleanvalidation <- cbind(cleanvalidation,validation[,521:529])
#1111 obs of 321 variables

##Removing observations (ROWS) in which no WAP was detected ----

rows_noWAPdetected <- cleantrainset[apply(cleantrainset[,1:312],1,mean) == -105,]
#76 obs
cleantrainset1 <- cleantrainset[apply(cleantrainset[,1:312],1,mean) != -105,]
#19861 obs of 321 variables

###Duplicated rows ----
duplicaterows <- cleantrainset1[duplicated(cleantrainset1),]
#637 rows
cleantrainset2 <- unique(cleantrainset1)
#19224 obs of 321 variables

###Removing observation which contain values between -30 and 0 ----
weirdvalues <- cleantrainset2[apply(cleantrainset2[,1:312],1,function(x) max(x) > -30),]
#459 obs
cleantrainset3 <- cleantrainset2[apply(cleantrainset2[,1:312],1,function(x) max(x) < -30),]
#18752

weirdB0 <- weirdvalues %>% filter(BUILDINGID == 0)
nrow(weirdB0)#1 row
weirdB1 <- weirdvalues %>% filter(BUILDINGID == 1)
nrow(weirdB1)#8 rows
weirdB2 <- weirdvalues %>% filter(BUILDINGID == 2)
nrow(weirdB2)#450 rows
weird6B2 <- weirdB2 %>% filter(USERID == 6)
nrow(weird6B2)#397 rows
weird6 <- weirdvalues %>% filter(USERID == 6)
nrow(weird6)#397 rows

clean3User6 <- cleantrainset3 %>% filter(USERID == 6)
nrow(clean3User6)#580 rows
clean3User6_B0 <- clean3User6 %>% filter(BUILDINGID == 0) 
nrow(clean3User6_B0)#0


###Check for Waps not detected after removing rows - NONE in cleantraining2 & 3 ----
detectedWAPs2 <- c()

for (i in 1:312){
  
  if (mean(cleantrainset2[,i]) == -105){
    detectedWAPs2 <- append(detectedWAPs2,i)
  }
  
}


detectedWAPs3 <- c()

for (i in 1:312){
  
  if (mean(cleantrainset3[,i]) == -105){
    detectedWAPs3 <- append(detectedWAPs3,i)
  }
  
}

#Checking EXPONENTIAL CURVE ----
#Part 1
checklog <- c(31:105)
checklog2 <- checklog^10

checklog3 <- as.data.frame(cbind(checklog,checklog2))
checklog4 <- c(31:105)
checklog3 <- as.data.frame(cbind(checklog3,checklog4))
plot(checklog3$checklog4,checklog)
plot(checklog3$checklog,checklog3$checklog2)

#Part 2
checklog <- c(31:105)
checklog2 <- 1/checklog^6

checklog3 <- as.data.frame(cbind(checklog,checklog2))
checklog4 <- c(31:105)
checklog3 <- as.data.frame(cbind(checklog3,checklog4))
plot(checklog3$checklog4,checklog)
plot(checklog3$checklog,checklog3$checklog2)

checklog <- c(31:105)
checklog2 <- 10^checklog

checklog3 <- as.data.frame(cbind(checklog,checklog2))
checklog4 <- c(31:105)
checklog3 <- as.data.frame(cbind(checklog3,checklog4))
plot(checklog3$checklog4,checklog)
plot(checklog3$checklog,checklog3$checklog2)

logtest <- cleantrainset2
logtest$log <- 10^logtest$WAP001

#Exponential curve ----
##CLEANTRAINSET2 -(10^x)
clean2WAPs <- cleantrainset2[,1:312]
clean2noWAPs <- cleantrainset2[,313:321]
clean2WAPs_Val <- cleanvalidation[,1:312]
clean2noWAPs_Val <- cleanvalidation[,313:321]


cleantrainset2_Exp <- cbind(as.data.frame(apply(clean2WAPs,2,function(x) 10^x)),clean2noWAPs)

##CLEANTRAINSET3 10^x
clean3WAPs <- cleantrainset3[,1:312]
clean3noWAPs <- cleantrainset3[,313:321]
clean3WAPs_Val <- cleanvalidation[,1:312]
clean3noWAPs_Val <- cleanvalidation[,313:321]

cleantrainset3_Exp <- cbind(as.data.frame(apply(clean3WAPs,2,function(x) 10^x)),clean3noWAPs)
##CLEANVALIDATION -(10^x)
cleanvalidation_Exp <- cbind(as.data.frame(apply(clean3WAPs_Val,2,function(x) 10^x)),clean3noWAPs_Val)

#Exponential inverted
##CLEANTRAINSET2 1/x^6
cleantrainset2_Exp2 <- cbind(as.data.frame(apply(clean2WAPs,2,function(x) 1/x^6)),clean2noWAPs)
##CLEANTRAINSET3 1/x^6
cleantrainset3_Exp2 <- cbind(as.data.frame(apply(clean3WAPs,2,function(x) 1/x^6)),clean3noWAPs)
##CLEANVALIDATION 1/x^6
cleanvalidation_Exp2 <- cbind(as.data.frame(apply(clean3WAPs_Val,2,function(x) 1/x^6)),clean3noWAPs_Val)


###Creating Building+Floor columns ----
cleantrainset2$BuildingFloor <- paste0("B",cleantrainset2$BUILDINGID,"F",cleantrainset2$FLOOR)
cleantrainset2$BuildingFloor <- as.factor(cleantrainset2$BuildingFloor)
cleantrainset2_Exp$BuildingFloor <- paste0("B",cleantrainset2_Exp$BUILDINGID,"F",cleantrainset2_Exp$FLOOR)
cleantrainset2_Exp$BuildingFloor <- as.factor(cleantrainset2_Exp$BuildingFloor)
cleantrainset2_Exp2$BuildingFloor <- paste0("B",cleantrainset2_Exp2$BUILDINGID,"F",cleantrainset2_Exp2$FLOOR)
cleantrainset2_Exp2$BuildingFloor <- as.factor(cleantrainset2_Exp2$BuildingFloor)
cleantrainset3$BuildingFloor <- paste0("B",cleantrainset3$BUILDINGID,"F",cleantrainset3$FLOOR)
cleantrainset3$BuildingFloor <- as.factor(cleantrainset3$BuildingFloor)
cleantrainset3_Exp$BuildingFloor <- paste0("B",cleantrainset3_Exp$BUILDINGID,"F",cleantrainset3_Exp$FLOOR)
cleantrainset3_Exp$BuildingFloor <- as.factor(cleantrainset3_Exp$BuildingFloor)
cleantrainset3_Exp2$BuildingFloor <- paste0("B",cleantrainset3_Exp2$BUILDINGID,"F",cleantrainset3_Exp2$FLOOR)
cleantrainset3_Exp2$BuildingFloor <- as.factor(cleantrainset3_Exp2$BuildingFloor)
cleanvalidation$BuildingFloor <- paste0("B",cleanvalidation$BUILDINGID,"F",cleanvalidation$FLOOR)
cleanvalidation$BuildingFloor <- as.factor(cleanvalidation$BuildingFloor)
cleanvalidation_Exp$BuildingFloor <- paste0("B",cleanvalidation_Exp$BUILDINGID,"F",cleanvalidation_Exp$FLOOR)
cleanvalidation_Exp$BuildingFloor <- as.factor(cleanvalidation_Exp$BuildingFloor)
cleanvalidation_Exp2$BuildingFloor <- paste0("B",cleanvalidation_Exp2$BUILDINGID,"F",cleanvalidation_Exp2$FLOOR)
cleanvalidation_Exp2$BuildingFloor <- as.factor(cleanvalidation_Exp2$BuildingFloor)



#GGPLOT CONFUSION MATRIX----
ggplotConfusionMatrix <- function(m){
  mytitle <- paste("Accuracy", round(as.numeric(m$overall[1]),3),
                   "Kappa", round(as.numeric(m$overall[2]),2))
  p <- ggplot(data = as.data.frame(m$table) ,
              aes(x = Reference, y = Prediction)) +
    geom_tile(aes(fill = log(Freq)), colour = "white") +
    scale_fill_gradient(low="white",high = "steelblue",na.value = "white") +
    geom_text(aes(x = Reference, y = Prediction, label = Freq)) +
    ggtitle(mytitle) +
    theme(legend.position = "none") 
  return(p)
}

#MODELLING - BUILDING ----

##Create 10-fold cross-validation ----
fitcontrol <- trainControl(method = "repeatedcv",number = 2,repeats = 2)

#SVMLinear BUILDING 
##Data partition ----
set.seed(123)

inTraining2<- createDataPartition(cleantrainset2$BUILDINGID, times = 1, p = .1)

training2 <- cleantrainset[inTraining2$Resample1,]

inTraining3<- createDataPartition(cleantrainset3$BUILDINGID, times = 1, p = .1)

training3 <- cleantrainset[inTraining3$Resample1,]


##MODEL 1 BUILDING - Train SVMLinear CLEANTRAINSET2 ----
#SVMLinearfit2 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
#                       -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP,
#                      data = cleantrainset2, method = "svmLinear", 
#                       tuneLength = 15, trControl = fitcontrol)

SVMLinearfit2BUILDING <- readRDS("SVMLinearfit2BUILDING.rds")

Predicted_BuildingSVMLinear2 <- predict(SVMLinearfit2BUILDING, cleanvalidation)

postResample(Predicted_BuildingSVMLinear2, cleanvalidation$BUILDINGID)
##Accuracy 1, Kappa 1 

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_BuildingSVMLinear2, cleanvalidation$BUILDINGID)

##MODEL 2 BUILDING - Train SVMLinear CLEANTRAINSET3----
#SVMLinearfit3 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
#                      -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP,
#                      data = cleantrainset3, method = "svmLinear", 
#                      tuneLength = 15, trControl = fitcontrol)

SVMLinearfit3BUILDING <- readRDS("SVMLinearfit3BUILDING.rds")

#Apply SVMLinear2 to validation set
Predicted_BuildingSVMLinear3 <- predict(SVMLinearfit3BUILDING, cleanvalidation)

#postResample SVMLinear1 to assess the metrics of the predictions
Building_SVMLinear <- postResample(Predicted_BuildingSVMLinear3, cleanvalidation$BUILDINGID)
##Accuracy 1, Kappa 1 

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_BuildingSVMLinear3, cleanvalidation$BUILDINGID)

##MODEL 3 BUILDING - Train k-nn CLEANTRAINSET2 ----
#KNNfit2 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
#                       -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP,
#                       data = cleantrainset2, method = "knn", 
#                       tuneLength = 15, trControl = fitcontrol)

KNNfit2BUILDING <- readRDS("KNNfit2BUILDING.rds")

Predicted_BuildingKNN2 <- predict(KNNfit2BUILDING, cleanvalidation)

postResample(Predicted_BuildingKNN2, cleanvalidation$BUILDINGID)
##Accuracy 0.9981, Kappa 0.9972 

confusionMatrix(data = Predicted_BuildingKNN2, cleanvalidation$BUILDINGID)

##MODEL 4 BUILDING - Train k-nn SAMPLE CLEANTRAINSET3 ----
#KNNfit3 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
#                 -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP,
#                 data = training3, method = "knn", 
#                 tuneLength = 15, trControl = fitcontrol)

KNNfit3BUILDING <- readRDS("KNNfit3BUILDING.rds")

Predicted_BuildingKNN3 <- predict(KNNfit3BUILDING, cleanvalidation)

Building_KNN_Sample <- postResample(Predicted_BuildingKNN3, cleanvalidation$BUILDINGID)
##Accuracy 0.9784, Kappa 0.9660 

confusionMatrix(data = Predicted_BuildingKNN3, cleanvalidation$BUILDINGID)

##MODEL 5 BUILDING - Train RF CLEANTRAINSET3 ----
RFfit3 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
                 -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP,
                 data = cleantrainset3, method = "rf",ntree=5, 
                 tuneLength = 15, trControl = fitcontrol)
RFfit3
RFfit2BUILDING <- readRDS("RFfit2BUILDING.rds")


Predicted_BuildingRF3 <- predict(RFfit3, cleanvalidation)

Building_RF <- postResample(Predicted_BuildingRF3, cleanvalidation$BUILDINGID)
##Accuracy 0.9955, Kappa 0.9929 

cfm1 <- confusionMatrix(data = Predicted_BuildingRF3, cleanvalidation$BUILDINGID)

ggplotConfusionMatrix(cfm1)

##MODEL 6 BUILDING - Train RF SAMPLE CLEANTRAINSET3 ----
#RFfit3 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
#                 -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP,
#                 data = training3, method = "rf",ntree=5, 
#                 tuneLength = 15, trControl = fitcontrol)

RFfit3BUILDING <- readRDS("RFfit3BUILDING.rds")

Predicted_BuildingRF3 <- predict(RFfit3BUILDING, cleanvalidation)

Building_RF_Sample <- postResample(Predicted_BuildingRF3, cleanvalidation$BUILDINGID)
##Accuracy 0.98, Kappa 0.97 

confusionMatrix(data = Predicted_BuildingRF3, cleanvalidation$BUILDINGID)

##MODEL 7 BUILDING - Train SVMLinear SAMPLE CLEANTRAINSET3 ----
SVMLinearfit3 <- train(BUILDINGID~. -LONGITUDE-LATITUDE-FLOOR-SPACEID
                       -RELATIVEPOSITION-USERID-PHONEID-TIMESTAMP,
                      data = training3, method = "svmLinear", 
                       tuneLength = 15, trControl = fitcontrol)

SVMLinearfit3

Predicted_BuildingSVMLinear3SAMPLE <- predict(SVMLinearfit3, cleanvalidation)

Building_SVMLinear_Sample <- postResample(Predicted_BuildingSVMLinear3SAMPLE, cleanvalidation$BUILDINGID)
##Accuracy 0.99, Kappa 0.99 

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_BuildingSVMLinear2, cleanvalidation$BUILDINGID)



#Metrics BUILDING sample ----
Metrics_Building_Sample <- rbind(Building_SVMLinear_Sample,Building_RF_Sample,Building_KNN_Sample)
Metrics_Building_Sample <- format(Metrics_Building_Sample, digits = 4, format = "f")
grid.newpage()
grid.table(Metrics_Building_Sample,theme=ttheme_default())

#Metrics BUILDING ----
Metrics_Building <- rbind(Building_SVMLinear,Building_RF)
Metrics_Building <- format(Metrics_Building, digits = 3, format = "f")
grid.newpage()
grid.table(Metrics_Building,theme=ttheme_default())
###MODELLING FLOOR ----
###New VALIDATION set with predicted BUILDING ----
validationBUILDING <- cleanvalidation
validationBUILDING$RealBuilding <- validationBUILDING$BUILDINGID
validationBUILDING$BUILDINGID <- Predicted_BuildingSVMLinear2

validationBUILDING_Exp <- cleanvalidation_Exp
validationBUILDING_Exp$RealBuilding <- validationBUILDING_Exp$BUILDINGID
validationBUILDING_Exp$BUILDINGID <- Predicted_BuildingSVMLinear2

validationBUILDING_Exp2 <- cleanvalidation_Exp2
validationBUILDING_Exp2$RealBuilding <- validationBUILDING_Exp2$BUILDINGID
validationBUILDING_Exp2$BUILDINGID <- Predicted_BuildingSVMLinear2

##MODELLING FLOOR for individual buildings

###Split training & validation sets per building----
B0_WAPstrain2 <- cleantrainset2[cleantrainset2$BUILDINGID == 0,313:322]
B1_WAPstrain2 <- cleantrainset2[cleantrainset2$BUILDINGID == 1,313:322]
B2_WAPstrain2 <- cleantrainset2[cleantrainset2$BUILDINGID == 2,313:322]

B0_train2 <- cleantrainset2[cleantrainset2$BUILDINGID == 0,1:312]
B1_train2 <- cleantrainset2[cleantrainset2$BUILDINGID == 1,1:312]
B2_train2 <- cleantrainset2[cleantrainset2$BUILDINGID == 2,1:312]

B0_train2Exp <- cleantrainset2_Exp[cleantrainset2_Exp$BUILDINGID == 0,1:312]
B1_train2Exp <- cleantrainset2_Exp[cleantrainset2_Exp$BUILDINGID == 1,1:312]
B2_train2Exp <- cleantrainset2_Exp[cleantrainset2_Exp$BUILDINGID == 2,1:312]

#B0_train2Exp2 <- cleantrainset2_Exp2[cleantrainset2_Exp2$BUILDINGID == 0,1:312]
#B1_train2Exp2 <- cleantrainset2_Exp2[cleantrainset2_Exp2$BUILDINGID == 1,1:312]
#B2_train2Exp2 <- cleantrainset2_Exp2[cleantrainset2_Exp2$BUILDINGID == 2,1:312]

B0_WAPstrain3 <- cleantrainset3[cleantrainset3$BUILDINGID == 0,313:322]
B1_WAPstrain3 <- cleantrainset3[cleantrainset3$BUILDINGID == 1,313:322]
B2_WAPstrain3 <- cleantrainset3[cleantrainset3$BUILDINGID == 2,313:322]

B0_train3 <- cleantrainset3[cleantrainset3$BUILDINGID == 0,1:312]
B1_train3 <- cleantrainset3[cleantrainset3$BUILDINGID == 1,1:312]
B2_train3 <- cleantrainset3[cleantrainset3$BUILDINGID == 2,1:312]

B0_train3Exp <- cleantrainset3_Exp[cleantrainset3_Exp$BUILDINGID == 0,1:312]
B1_train3Exp <- cleantrainset3_Exp[cleantrainset3_Exp$BUILDINGID == 1,1:312]
B2_train3Exp <- cleantrainset3_Exp[cleantrainset3_Exp$BUILDINGID == 2,1:312]

#B0_train3Exp2 <- cleantrainset3_Exp2[cleantrainset3_Exp2$BUILDINGID == 0,1:312]
#B1_train3Exp2 <- cleantrainset3_Exp2[cleantrainset3_Exp2$BUILDINGID == 1,1:312]
#B2_train3Exp2 <- cleantrainset3_Exp2[cleantrainset3_Exp2$BUILDINGID == 2,1:312]

B0_WAPsValidation <- validationBUILDING[validationBUILDING$BUILDINGID == 0,313:323]
B1_WAPsValidation <- validationBUILDING[validationBUILDING$BUILDINGID == 1,313:323]
B2_WAPsValidation <- validationBUILDING[validationBUILDING$BUILDINGID == 2,313:323]


B0_validation <- validationBUILDING[validationBUILDING$BUILDINGID == 0,1:312]
B1_validation <- validationBUILDING[validationBUILDING$BUILDINGID == 1,1:312]
B2_validation <- validationBUILDING[validationBUILDING$BUILDINGID == 2,1:312]

B0_validation_Exp <- validationBUILDING_Exp[validationBUILDING_Exp$BUILDINGID == 0,1:312]
B1_validation_Exp <- validationBUILDING_Exp[validationBUILDING_Exp$BUILDINGID == 1,1:312]
B2_validation_Exp <- validationBUILDING_Exp[validationBUILDING_Exp$BUILDINGID == 2,1:312]

#B0_validation_Exp2 <- validationBUILDING_Exp2[validationBUILDING_Exp2$BUILDINGID == 0,1:312]
#B1_validation_Exp2 <- validationBUILDING_Exp2[validationBUILDING_Exp2$BUILDINGID == 1,1:312]
#B2_validation_Exp2 <- validationBUILDING_Exp2[validationBUILDING_Exp2$BUILDINGID == 2,1:312]

##Removing WAPs never detected per building ----
###VALIDATION
B0_validation <- B0_validation[,apply(B0_validation,2,mean) != -105]
B0_validation <- as.data.frame(cbind(B0_validation,B0_WAPsValidation))
B1_validation <- B1_validation[,apply(B1_validation,2,mean) != -105]
B1_validation <- as.data.frame(cbind(B1_validation,B1_WAPsValidation))
B2_validation <- B2_validation[,apply(B2_validation,2,mean) != -105]
B2_validation <- as.data.frame(cbind(B2_validation,B2_WAPsValidation))

B0_validation_Exp <- B0_validation_Exp[,apply(B0_validation_Exp,2,mean) != 10^(-105)]
B0_validation_Exp <- as.data.frame(cbind(B0_validation_Exp,B0_WAPsValidation))
B1_validation_Exp <- B1_validation_Exp[,apply(B1_validation_Exp,2,mean) != 10^(-105)]
B1_validation_Exp <- as.data.frame(cbind(B1_validation_Exp,B1_WAPsValidation))
B2_validation_Exp <- B2_validation_Exp[,apply(B2_validation_Exp,2,mean) != 10^(-105)]
B2_validation_Exp <- as.data.frame(cbind(B2_validation_Exp,B2_WAPsValidation))

#B0_validation_Exp2 <- B0_validation_Exp2[,apply(B0_validation_Exp2,2,mean) != 10^(-105)]
#B0_validation_Exp2 <- as.data.frame(cbind(B0_validation_Exp2,B0_WAPsValidation))
#B1_validation_Exp2 <- B1_validation_Exp2[,apply(B1_validation_Exp2,2,mean) != 10^(-105)]
#B1_validation_Exp2 <- as.data.frame(cbind(B1_validation_Exp2,B1_WAPsValidation))
#B2_validation_Exp2 <- B2_validation_Exp2[,apply(B2_validation_Exp2,2,mean) != 10^(-105)]
#B2_validation_Exp2 <- as.data.frame(cbind(B2_validation_Exp2,B2_WAPsValidation))

B0_train2 <- B0_train2[,apply(B0_train2,2,mean) != -105]
B0_train2 <- as.data.frame(cbind(B0_train2,B0_WAPstrain2))
B0_train2_int <- intersect(names(B0_train2),names(B0_validation))
B0_train2 <- B0_train2[,B0_train2_int]
B0_validation <- B0_validation[,B0_train2_int]

B1_train2 <- B1_train2[,apply(B1_train2,2,mean) != -105]
B1_train2 <- as.data.frame(cbind(B1_train2,B1_WAPstrain2))
B1_train2_int <- intersect(names(B1_train2),names(B1_validation))
B1_train2 <- B1_train2[,B1_train2_int]
B1_validation <- B1_validation[,B1_train2_int]

B2_train2 <- B2_train2[,apply(B2_train2,2,mean) != -105]
B2_train2 <- as.data.frame(cbind(B2_train2,B2_WAPstrain2))
B2_train2_int <- intersect(names(B2_train2),names(B2_validation))
B2_train2 <- B2_train2[,B2_train2_int]
B2_validation <- B2_validation[,B2_train2_int]

B0_train2Exp <- B0_train2Exp[,apply(B0_train2Exp,2,mean) != 10^(-105)]
B0_train2Exp <- as.data.frame(cbind(B0_train2Exp,B0_WAPstrain2))
B0_train2Exp_int <- intersect(names(B0_train2Exp),names(B0_validation_Exp))
B0_train2Exp <- B0_train2Exp[,B0_train2Exp_int]
B0_validation_Exp <- B0_validation_Exp[,B0_train2Exp_int]
                             
B1_train2Exp <- B1_train2Exp[,apply(B1_train2Exp,2,mean) != 10^(-105)]
B1_train2Exp <- as.data.frame(cbind(B1_train2Exp,B1_WAPstrain2))
B1_train2Exp_int <- intersect(names(B1_train2Exp),names(B1_validation_Exp))
B1_train2Exp <- B1_train2Exp[,B1_train2Exp_int]
B1_validation_Exp <- B1_validation_Exp[,B1_train2Exp_int]

B2_train2Exp <- B2_train2Exp[,apply(B2_train2Exp,2,mean) != 10^(-105)]
B2_train2Exp <- as.data.frame(cbind(B2_train2Exp,B2_WAPstrain2))
B2_train2Exp_int <- intersect(names(B2_train2Exp),names(B2_validation_Exp))
B2_train2Exp <- B2_train2Exp[,B2_train2Exp_int]
B2_validation_Exp <- B2_validation_Exp[,B2_train2Exp_int]

#B0_train2Exp2 <- B0_train2Exp2[,apply(B0_train2Exp2,2,mean) != -105]
#B0_train2Exp2 <- as.data.frame(cbind(B0_train2Exp2,B0_WAPstrain2))
#B0_train2Exp2_int <- intersect(names(B0_train2Exp2),names(B0_validation_Exp2))
#B0_train2Exp2 <- B0_train2Exp2[,B0_train2Exp2_int]

#B1_train2Exp2 <- B1_train2Exp2[,apply(B1_train2Exp2,2,mean) != -105]
#B1_train2Exp2 <- as.data.frame(cbind(B1_train2Exp2,B1_WAPstrain2))
#B1_train2Exp2_int <- intersect(names(B1_train2Exp2),names(B1_validation_Exp2))
#B1_train2Exp2 <- B1_train2Exp2[,B1_train2Exp2_int]

#B2_train2Exp2 <- B2_train2Exp2[,apply(B2_train2Exp2,2,mean) != -105]
#B2_train2Exp2 <- as.data.frame(cbind(B2_train2Exp2,B2_WAPstrain2))
#B2_train2Exp2_int <- intersect(names(B2_train2Exp2),names(B2_validation_Exp2))
#B2_train2Exp2 <- B2_train2Exp2[,B2_train2Exp2_int]

B0_train3 <- B0_train3[,apply(B0_train3,2,mean) != -105]
B0_train3 <- as.data.frame(cbind(B0_train3,B0_WAPstrain3))
B0_train3_int <- intersect(names(B0_train3),names(B0_validation))
B0_train3 <- B0_train3[,B0_train3_int]

B1_train3 <- B1_train3[,apply(B1_train3,2,mean) != -105]
B1_train3 <- as.data.frame(cbind(B1_train3,B1_WAPstrain3))
B1_train3_int <- intersect(names(B1_train3),names(B1_validation))
B1_train3 <- B1_train3[,B1_train3_int]

B2_train3 <- B2_train3[,apply(B2_train3,2,mean) != -105]
B2_train3 <- as.data.frame(cbind(B2_train3,B2_WAPstrain3))
B2_train3_int <- intersect(names(B2_train3),names(B2_validation))
B2_train3 <- B2_train3[,B2_train3_int]

B0_train3Exp <- B0_train3Exp[,apply(B0_train3Exp,2,mean) != 10^(-105)]
B0_train3Exp <- as.data.frame(cbind(B0_train3Exp,B0_WAPstrain3))
B0_train3Exp_int <- intersect(names(B0_train3Exp),names(B0_validation_Exp))
B0_train3Exp <- B0_train3Exp[,B0_train3Exp_int]

B1_train3Exp <- B1_train3Exp[,apply(B1_train3Exp,2,mean) != 10^(-105)]
B1_train3Exp <- as.data.frame(cbind(B1_train3Exp,B1_WAPstrain3))
B1_train3Exp_int <- intersect(names(B1_train3Exp),names(B1_validation_Exp))
B1_train3Exp <- B1_train3Exp[,B1_train3Exp_int]

B2_train3Exp <- B2_train3Exp[,apply(B2_train3Exp,2,mean) != 10^(-105)]
B2_train3Exp <- as.data.frame(cbind(B2_train3Exp,B2_WAPstrain3))
B2_train3Exp_int <- intersect(names(B2_train3Exp),names(B2_validation_Exp))
B2_train3Exp <- B2_train3Exp[,B2_train3Exp_int]

#B0_train3Exp2 <- B0_train3Exp2[,apply(B0_train3Exp2,2,mean) != -105]
#B0_train3Exp2 <- as.data.frame(cbind(B0_train3Exp2,B0_WAPstrain3))
#B0_train3Exp2_int <- intersect(names(B0_train3Exp2),names(B0_validation_Exp2))
#B0_train3Exp2 <- B0_train3Exp2[,B0_train3Exp2_int]

#B1_train3Exp2 <- B1_train3Exp2[,apply(B1_train3Exp2,2,mean) != -105]
#B1_train3Exp2 <- as.data.frame(cbind(B1_train3Exp2,B1_WAPstrain3))
#B1_train3Exp2_int <- intersect(names(B1_train3Exp2),names(B1_validation_Exp2))
#B1_train3Exp2 <- B1_train3Exp2[,B1_train3Exp2_int]

#B2_train3Exp2 <- B2_train3Exp2[,apply(B2_train3Exp2,2,mean) != -105]
#B2_train3Exp2 <- as.data.frame(cbind(B2_train3Exp2,B2_WAPstrain3))
#B2_train3Exp2_int <- intersect(names(B2_train3Exp2),names(B2_validation_Exp2))
#B2_train3Exp2 <- B2_train3Exp2[,B2_train3Exp2_int]

B0_train2$BUILDINGID <- factor(B0_train2$BUILDINGID)
B0_train2$FLOOR <- factor(B0_train2$FLOOR)
B0_train3$BUILDINGID <- factor(B0_train3$BUILDINGID)
B0_train3$FLOOR <- factor(B0_train3$FLOOR)
B0_validation$BUILDINGID <- factor(B0_validation$BUILDINGID)
B0_validation$FLOOR <- factor(B0_validation$FLOOR)

B1_train2$BUILDINGID <- factor(B1_train2$BUILDINGID)
B1_train2$FLOOR <- factor(B1_train2$FLOOR)
B1_train3$BUILDINGID <- factor(B1_train3$BUILDINGID)
B1_train3$FLOOR <- factor(B1_train3$FLOOR)
B1_validation$BUILDINGID <- factor(B1_validation$BUILDINGID)
B1_validation$FLOOR <- factor(B1_validation$FLOOR)

B2_train2$BUILDINGID <- factor(B2_train2$BUILDINGID)
B2_train2$FLOOR <- factor(B2_train2$FLOOR)
B2_train3$BUILDINGID <- factor(B2_train3$BUILDINGID)
B2_train3$FLOOR <- factor(B2_train3$FLOOR)
B2_validation$BUILDINGID <- factor(B2_validation$BUILDINGID)
B2_validation$FLOOR <- factor(B2_validation$FLOOR)

B0_train2Exp$BUILDINGID <- factor(B0_train2Exp$BUILDINGID)
B0_train2Exp$FLOOR <- factor(B0_train2Exp$FLOOR)
B0_train3Exp$BUILDINGID <- factor(B0_train3Exp$BUILDINGID)
B0_train3Exp$FLOOR <- factor(B0_train3Exp$FLOOR)
B0_validation_Exp$BUILDINGID <- factor(B0_validation_Exp$BUILDINGID)
B0_validation_Exp$FLOOR <- factor(B0_validation_Exp$FLOOR)

B1_train2Exp$BUILDINGID <- factor(B1_train2Exp$BUILDINGID)
B1_train2Exp$FLOOR <- factor(B1_train2Exp$FLOOR)
B1_train3Exp$BUILDINGID <- factor(B1_train3Exp$BUILDINGID)
B1_train3Exp$FLOOR <- factor(B1_train3Exp$FLOOR)
B1_validation_Exp$BUILDINGID <- factor(B1_validation_Exp$BUILDINGID)
B1_validation_Exp$FLOOR <- factor(B1_validation_Exp$FLOOR)

B2_train2Exp$BUILDINGID <- factor(B2_train2Exp$BUILDINGID)
B2_train2Exp$FLOOR <- factor(B2_train2Exp$FLOOR)
B2_train3Exp$BUILDINGID <- factor(B2_train3Exp$BUILDINGID)
B2_train3Exp$FLOOR <- factor(B2_train3Exp$FLOOR)
B2_validation_Exp$BUILDINGID <- factor(B2_validation_Exp$BUILDINGID)
B2_validation_Exp$FLOOR <- factor(B2_validation_Exp$FLOOR)

##MODELS ----
#MODEL 1 B0 FLOOR Floor_B0_SVM2----
##Train SVMLinear Floor 
#SVMLinearFloor2_B0boxcox <- train(y=B0_train2$FLOOR,x=B0_train2[,c(1:139)],
#                     data = B0_train2, method = "svmLinear",
#                     tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor2_B0 <- readRDS("SVMLinearFloor2_B0.rds")
SVMLinearFloor2_B0

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear2_B0 <- predict(SVMLinearFloor2_B0, B0_validation[,1:139])

#postResample SVMLinear1 Floor to assess the metrics of the predictions
Floor_B0_SVM <- postResample(Predicted_FloorSVMLinear2_B0, B0_validation$FLOOR)
##Accuracy 0.9310, Kappa 0.9026 

#Plot confusion matrix SVMLinear2
cf1 <- confusionMatrix(data = Predicted_FloorSVMLinear2_B0, B0_validation$FLOOR)
ggplotConfusionMatrix(cf1)

#MODEL 2 B0 FLOOR Floor_B0_SVM2Exp ----
#SVMLinearFloor2Exp_B0 <- train(y=B0_train2Exp$FLOOR,x=B0_train2Exp[,c(1:139)],
#                            data = B0_train2Exp, method = "svmLinear",
#                            tuneLength = 15, trControl = fitcontrol)

#SVMLinearFloor2Exp_B0

#Apply SVMLinear1 Floor to test set
#Predicted_FloorSVMLinear2Exp_B0 <- predict(SVMLinearFloor2Exp_B0, B0_validation[,1:139])

#postResample SVMLinear1 Floor to assess the metrics of the predictions
#Floor_B0_SVM2Exp <- postResample(Predicted_FloorSVMLinear2Exp_B0, B0_validation$FLOOR)
##Accuracy 0.388, Kappa 0 

#Plot confusion matrix SVMLinear2
#confusionMatrix(data = Predicted_FloorSVMLinear2Exp_B0, B0_validation$FLOOR)

#MODEL 3 B0 FLOOR Floor_B0_RF2 ----
#RFFloor2_B0 <- train(y=B0_train3$FLOOR,x=B0_train3[,c(1:139)],
#                     data = B0_train2, method = "rf",ntree=5,
#                     tuneLength = 15)

RFFloor2_B0 <- readRDS("RFFloor2_B0.rds")

Predicted_RFFloor2_B0 <- predict(RFFloor2_B0, B0_validation)

Floor_B0_RF <- postResample(Predicted_RFFloor2_B0, B0_validation$FLOOR)
##Accuracy 0.93, Kappa 0.91 

#Plot confusion matrix SVMLinear2
cfm3 <- confusionMatrix(data = Predicted_RFFloor2_B0, B0_validation$FLOOR)
ggplotConfusionMatrix(cfm3)

#Metrics B0 Floor
Metrics_Floor_B0 <- rbind(Floor_B0_SVM,Floor_B0_RF)
Metrics_Floor_B0 <- format(Metrics_Floor_B0, digits = 3, format = "f")
grid.newpage()
grid.table(Metrics_Floor_B0,theme=ttheme_default())

#MODEL 4 B0 FLOOR Floor_B0_KNN2 SAMPLE----
inTrainingB0<- createDataPartition(B0_train3$BUILDINGID, times = 1, p = .1)
trainingB0 <- B0_train3[inTrainingB0$Resample1,]

KNNFloor2_B0 <- train(y=B0_train3$FLOOR,x=B0_train3[,c(1:139)],
                     data = trainingB0, method = "knn",
                     preProcess = c("center","scale"),
                     tuneLength = 15)

KNNFloor2_B0

Predicted_KNNFloor2_B0 <- predict(KNNFloor2_B0, B0_validation)

Floor_B0_RF2 <- postResample(Predicted_RFFloor2_B0, B0_validation$FLOOR)
##Accuracy 0., Kappa 0. 

#Plot confusion matrix 
cm2 <- confusionMatrix(data = Predicted_KNNFloor2_B0, B0_validation$FLOOR)

#MODEL 5 B0 FLOOR Floor_B0_SVM3----
##Train SVMLinear Floor 
SVMLinearFloor3_B0 <- train(y=B0_train3$FLOOR,x=B0_train3[,c(1:139)],
                     data = B0_train3, method = "svmLinear",
                     tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor3_B0 <- readRDS("SVMLinearFloor2_B0.rds")
SVMLinearFloor3_B0

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear3_B0 <- predict(SVMLinearFloor3_B0, B0_validation[,1:139])

#postResample SVMLinear1 Floor to assess the metrics of the predictions
Floor_B0_SVM3 <- postResample(Predicted_FloorSVMLinear3_B0, B0_validation$FLOOR)
##Accuracy 0.9310, Kappa 0.9026 

#Plot confusion matrix SVMLinear2
cfm2 <- confusionMatrix(data = Predicted_FloorSVMLinear2_B0, B0_validation$FLOOR)

ggplotConfusionMatrix(cfm2)

#MODEL 6 B0 FLOOR Floor_B0_RF3 ----
#RFFloor2_B0 <- train(y=B0_train3$FLOOR,x=B0_train3[,c(1:139)],
#                     data = B0_train2, method = "rf",ntree=5,
#                     tuneLength = 15)

RFFloor2_B0 <- readRDS("RFFloor2_B0.rds")

Predicted_RFFloor2_B0 <- predict(RFFloor2_B0, B0_validation)

Floor_B0_RF2 <- postResample(Predicted_RFFloor2_B0, B0_validation$FLOOR)
##Accuracy 0.93, Kappa 0.91 

#Plot confusion matrix SVMLinear2
cm2 <- confusionMatrix(data = Predicted_RFFloor2_B0, B0_validation$FLOOR)

#Metrics B0 Floor
Metrics_Floor_B0 <- rbind(Floor_B0_SVM2,Floor_B0_RF2)
Metrics_Floor_B0 <- format(Metrics_Floor_B0, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Floor_B0,theme=ttheme_default())

#Metrics B0 Floor ----
Metrics_Floor_B0 <- rbind(Floor_B0_SVM2,Floor_B0_RF2)
Metrics_Floor_B0 <- format(Metrics_Floor_B0, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Floor_B0,theme=ttheme_default())

#MODEL 1 B1 FLOOR Floor_B0_SVM2 ----
SVMLinearFloor2_B1 <- train(y=B1_train2$FLOOR,x=B1_train2[,c(1:139)],
                            data = B1_train2, method = "svmLinear",
                            tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor2_B1

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear2_B1 <- predict(SVMLinearFloor2_B1, B1_validation[,1:139])

#postResample SVMLinear1 Floor to assess the metrics of the predictions
Floor_B1_SVM <- postResample(Predicted_FloorSVMLinear2_B1, B1_validation$FLOOR)
##Accuracy 0.7883, Kappa 0.6888 

#Plot confusion matrix SVMLinear2
cfm4 <- confusionMatrix(data = Predicted_FloorSVMLinear2_B1, B1_validation$FLOOR)
ggplotConfusionMatrix(cfm4)

#MODEL 2 B1 FLOOR Floor_B0_SVM2Exp----
SVMLinearFloor3_B1 <- train(y=B1_train3$FLOOR,x=B1_train3[,c(1:139)],
                               data = B1_train3, method = "svmLinear",
                               tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor3_B1

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear3_B1 <- predict(SVMLinearFloor3_B1, B1_validation[,1:139])

#postResample SVMLinear1 Floor to assess the metrics of the predictions
Floor_B0_SVM3 <- postResample(Predicted_FloorSVMLinear3_B1, B1_validation$FLOOR)
##Accuracy , Kappa  

#Plot confusion matrix SVMLinear2
confusionMatrix(data = Predicted_FloorSVMLinear2Exp_B1, B1_validation$FLOOR)

#MODEL 3 B1 FLOOR Floor_B1_RF2 ----

RFFloor2_B1 <- train(y=B1_train2$FLOOR,x=B1_train2[,c(1:139)],
                     data = B1_train2, method = "rf",ntree=5,
                     tuneLength = 15, trControl = fitcontrol)

RFFloor2_B1

#Apply to test set
Predicted_RFFloor2_B1 <- predict(RFFloor2_B1, B1_validation)

#postResample to assess the metrics of the predictions
Floor_B1_RF <- postResample(Predicted_RFFloor2_B1, B1_validation$FLOOR)
##Accuracy 0.7752, Kappa 0.6787 

#Plot confusion matrix SVMLinear2
cfm5 <- confusionMatrix(data = Predicted_RFFloor2_B1, B1_validation$FLOOR)
ggplotConfusionMatrix(cfm5)

#Metrics B1 Floor ----
Metrics_Floor_B1 <- rbind(Floor_B1_SVM,Floor_B1_RF)
Metrics_Floor_B1 <- format(Metrics_Floor_B1, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Floor_B1,theme=ttheme_default())
#MODEL 1 B2 FLOOR Floor_B2_SVM2 ----
SVMLinearFloor2_B2 <- train(y=B2_train2$FLOOR,x=B2_train2[,c(1:106)],
                            data = B2_train2, method = "svmLinear",
                            tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor2_B2

Predicted_FloorSVMLinear2_B2 <- predict(SVMLinearFloor2_B2, B2_validation[,1:106])

Floor_B2_SVM <- postResample(Predicted_FloorSVMLinear2_B2, B2_validation$FLOOR)
##Accuracy 0.9216, Kappa 0.8929 

#Plot confusion matrix SVMLinear2
cfm7 <- confusionMatrix(data = Predicted_FloorSVMLinear2_B2, B2_validation$FLOOR)
ggplotConfusionMatrix(cfm7)



#MODEL 2 B2 FLOOR Floor_B2_SVM3 ----
SVMLinearFloor3_B2 <- train(y=B2_train3$FLOOR,x=B2_train3[,1:106],
                               data = B2_train3, method = "svmLinear",
                               tuneLength = 15, trControl = fitcontrol)

SVMLinearFloor3_B2

#Apply SVMLinear1 Floor to test set
Predicted_FloorSVMLinear3_B2 <- predict(SVMLinearFloor3_B2, B2_validation[,1:106])

#postResample SVMLinear1 Floor to assess the metrics of the predictions
postResample(Predicted_FloorSVMLinear3_B2, B2_validation$FLOOR)
##Accuracy 0.9253, Kappa 0.89  

#Plot confusion matrix SVMLinear2
cfm8 <- confusionMatrix(data = Predicted_FloorSVMLinear3_B2, B2_validation$FLOOR)
ggplotConfusionMatrix(cfm8)


#MODEL 3 B2 FLOOR Floor_B2_RF2 ----

RFFloor2_B2 <- train(y=B2_train2$FLOOR,x=B2_train2[,c(1:106)],
                     data = B2_train2, method = "rf",ntree=5,
                     tuneLength = 15, trControl = fitcontrol)

RFFloor2_B2

#Apply to test set
Predicted_RFFloor2_B2 <- predict(RFFloor2_B2, B2_validation[,c(1:106)])

#postResample to assess the metrics of the predictions
Floor_B2_RF <- postResample(Predicted_RFFloor2_B2, B2_validation$FLOOR)
##Accuracy 0.8731, Kappa 0.8271

#Plot confusion matrix SVMLinear2
cfm6 <- confusionMatrix(data = Predicted_RFFloor2_B2, B2_validation$FLOOR)

ggplotConfusionMatrix(cfm6)



#Metrics B2 Floor ----
Metrics_Floor_B2 <- rbind(Floor_B2_SVM,Floor_B2_RF)
Metrics_Floor_B2 <- format(Metrics_Floor_B2, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Floor_B2,theme=ttheme_default())
#Error analysis ----
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


#MODELLING LONGITUDE/LATITUDE ----
##Preparing data frames for Regression 
cleantrainset3.Regr <- cleantrainset3
cleantrainset3.Regr$BUILDINGID <- as.numeric(cleantrainset3.Regr$BUILDINGID)

validationBUILDING.Regr <- validationBUILDING
validationBUILDING.Regr$BUILDINGID <- as.numeric(validationBUILDING.Regr$BUILDINGID)



##Based on the variance below, we have decided to predict latitude before longitude
summary(cleantrainset3$LONGITUDE)#varies from -7691 to -7301 (390 "variance") 
summary(cleantrainset3$LATITUDE)#varies from 4864746 to 4865017 (271 "variance")

#LATITUDE ----
#MODEL1 B0 Latitude Lat_B0_RF2 ----
#RFlatitude2_B0 <- train(y=B0_train2$LATITUDE,x=B0_train2[,c(1:139)],
#                            data = B0_train2, method = "rf",ntree=5,
#                            tuneLength = 15, trControl = fitcontrol)

RFlatitude2_B0 <- readRDS("RFlatitude2_B0.rds")

Predicted_RFlatitude2_B0 <- predict(RFlatitude2_B0, B0_validation[,1:139])

Lat_B0_RF <- postResample(Predicted_RFlatitude2_B0, B0_validation$LATITUDE)
##RMSE 6.91, Rsquared 0.95, MAE 4.81  

#Error analysis
ErroLatitude_RF2_B0 <-  Predicted_RFlatitude2_B0 - B0_validation$LATITUDE
hist(ErroLatitude_RF2_B0)

#MODEL2 B0 Latitude Lat_B0_RF2Exp ----
#RFlatitude2Exp_B0 <- train(y=B0_train2Exp$LATITUDE,x=B0_train2Exp[,c(1:139)],
#                        data = B0_train2Exp, method = "rf",ntree=5,
#                        preProcess = c("BoxCox","center","scale"),
#                        tuneLength = 15, trControl = fitcontrol)

RFlatitude2Exp_B0 <- readRDS("RFlatitude2Exp_B0.rds")

Predicted_RFlatitude2Exp_B0 <- predict(RFlatitude2Exp_B0, B0_validation_Exp[,1:139])

Lat_B0_RF2Exp <- postResample(Predicted_RFlatitude2Exp_B0, B0_validation_Exp$LATITUDE)
##RMSE 7.35, Rsquared 0.95, MAE 4.82    

#Error analysis
ErroLatitude_RF2Exp_B0 <-  Predicted_RFlatitude2Exp_B0 - B0_validation_Exp$LATITUDE
hist(ErroLatitude_RF2Exp_B0)

#MODEL3 B0 Latitude Lat_B0_RF3 ----
#RFlatitude3_B0 <- train(y=B0_train3$LATITUDE,x=B0_train3[,c(1:139)],
#                        data = B0_train3, method = "rf",ntree=5,
#                        tuneLength = 15, trControl = fitcontrol)

#RFlatitude3_B0

#Predicted_RFlatitude3_B0 <- predict(RFlatitude3_B0, B0_validation[,1:139])

#Lat_B0_RF3 <- postResample(Predicted_RFlatitude3_B0, B0_validation$LATITUDE)
#model3 ##RMSE 6.44, Rsquared 0.95, MAE 4.56  


#Error analysis
#ErroLatitude_RF3_B0 <-  Predicted_RFlatitude3_B0 - B0_validation$LATITUDE
#hist(ErroLatitude_RF3_B0)

#MODEL4 B0 Latitude Lat_B0_RF3Exp ----
RFlatitude3Exp_B0 <- train(y=B0_train3Exp$LATITUDE,x=B0_train3Exp[,c(1:139)],
                           data = B0_train3Exp, method = "rf",ntree=5,
                           preProcess = c("BoxCox","center","scale"),
                           tuneLength = 15, trControl = fitcontrol)

RFlatitude3Exp_B0

Predicted_RFlatitude3Exp_B0 <- predict(RFlatitude3Exp_B0, B0_validation_Exp[,1:139])

Lat_B0_RF3Exp <- postResample(Predicted_RFlatitude3Exp_B0, B0_validation_Exp$LATITUDE)
#model4 RF3Exp ##RMSE 6.9, Rsquared 0.95, MAE 4.65     
#model1 RF2    ##RMSE 6.99, Rsquared 0.95, MAE 4.71  


#Error analysis
ErroLatitude_RF3Exp_B0 <-  Predicted_RFlatitude3Exp_B0 - B0_validation_Exp$LATITUDE
hist(ErroLatitude_RF3Exp_B0)

#MODEL5 B0 Latitude Lat_B0_SVM3 ----
#SVMlatitude3_B0 <- train(y=B0_train3$LATITUDE,x=B0_train3[,c(1:139)],
#                        data = B0_train3, method = "svmLinear",
#                        preProcess = c("center","scale"),
#                        tuneLength = 15, trControl = fitcontrol)

SVMlatitude3_B0 <- readRDS("SVMlatitude3_B0.rds")

Predicted_SVMlatitude3_B0 <- predict(SVMlatitude3_B0, B0_validation[,1:139])

Lat_B0_SVM <- postResample(Predicted_SVMlatitude3_B0, B0_validation$LATITUDE)
#model5 ##RMSE 14.91, Rsquared 0.81, MAE 10.29   

#Error analysis
ErroLatitude_SVM3_B0 <-  Predicted_SVMlatitude3_B0 - B0_validation$LATITUDE
hist(ErroLatitude_SVM3_B0)

#MODEL6 B0 Latitude Lat_B0_SVM3Exp ----
SVMlatitude3Exp_B0 <- train(y=B0_train3Exp$LATITUDE,x=B0_train3Exp[,c(1:139)],
                            data = B0_train3Exp, method = "svmLinear",
                            preprocess=c("center","scale"),
                            tuneLength = 15, trControl = fitcontrol)

SVMlatitude3Exp_B0

Predicted_SVMlatitude3Exp_B0 <- predict(SVMlatitude3Exp_B0, B0_validation[,1:139])

Lat_B0_SVM3Exp <- postResample(Predicted_SVMlatitude3Exp_B0, B0_validation$LATITUDE)
#model6 ##RMSE , Rsquared 0., MAE    

#Error analysis
ErroLatitude_SVM3_B0 <-  Predicted_SVMlatitude3_B0 - B0_validation$LATITUDE
hist(ErroLatitude_SVM3_B0)
#MODEL7 B0 Latitude Lat_B0_KNN3 ----
#KNNlatitude3_B0 <- train(y=B0_train3$LATITUDE,x=B0_train3[,c(1:139)],
#                         data = B0_train3, method = "knn",
#                         preprocess=c("center","scale"),
#                         tuneLength = 15, trControl = fitcontrol)

KNNlatitude3_B0 <- readRDS("KNNlatitude3_B0.rds")

KNNlatitude3_B0

Predicted_KNNlatitude3_B0 <- predict(KNNlatitude3_B0, B0_validation[,1:139])

Lat_B0_KNN <- postResample(Predicted_KNNlatitude3_B0, B0_validation$LATITUDE)
#model6 ##RMSE 4.89 , Rsquared 0.97, MAE 3.19    

#Error analysis
ErroLatitude_KNN3_B0 <-  Predicted_KNNlatitude3_B0 - B0_validation$LATITUDE
hist(ErroLatitude_KNN3_B0)

#Metrics B0 Latitute ----
Metrics_Lat_B0 <- rbind(Lat_B0_KNN,Lat_B0_RF,Lat_B0_SVM)
Metrics_Lat_B0 <- format(Metrics_Lat_B0, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Lat_B0,theme=ttheme_default())

#MODEL1 B1 Latitude Lat_B1_RF2 ----
#RFlatitude2_B1 <- train(y=B1_train2$LATITUDE,x=B1_train2[,c(1:146)],
#                        data = B1_train2, method = "rf",ntree=5,
#                        tuneLength = 15, trControl = fitcontrol)

RFlatitude2_B1 <- readRDS("RFlatitude2_B1.rds")

Predicted_RFlatitude2_B1 <- predict(RFlatitude2_B1, B1_validation[,1:146])

Lat_B1_RF <- postResample(Predicted_RFlatitude2_B1, B1_validation$LATITUDE)
##RMSE 11.43, Rsquared 0.89, MAE 7.94   

#Error analysis
ErroLatitude_RF2_B1 <-  Predicted_RFlatitude2_B1 - B1_validation$LATITUDE
hist(ErroLatitude_RF2_B1)
#MODEL2 B1 Latitude Lat_B1_RF2Exp ----
RFlatitude2Exp_B1 <- train(y=B1_train2Exp$LATITUDE,x=B1_train2Exp[,c(1:139)],
                           data = B1_train2Exp, method = "rf",ntree=5,
                           preProcess = c("BoxCox","center","scale"),
                           tuneLength = 15, trControl = fitcontrol)

RFlatitude2Exp_B1

Predicted_RFlatitude2Exp_B1 <- predict(RFlatitude2Exp_B1, B1_validation_Exp[,1:139])

Lat_B1_RF2Exp <- postResample(Predicted_RFlatitude2Exp_B1, B1_validation_Exp$LATITUDE)
##RMSE 12.12, Rsquared 0.88, MAE 8.27    

#Error analysis
ErroLatitude_RF2Exp_B1 <-  Predicted_RFlatitude2Exp_B1 - B1_validation_Exp$LATITUDE
hist(ErroLatitude_RF2Exp_B1)

#MODEL3 B1 Latitude Lat_B1_RF3 ----
RFlatitude3_B1 <- train(y=B1_train3$LATITUDE,x=B1_train3[,c(1:139)],
                        data = B1_train3, method = "rf",ntree=5,
                        tuneLength = 15, trControl = fitcontrol)

RFlatitude3_B1

Predicted_RFlatitude3_B1 <- predict(RFlatitude3_B1, B1_validation[,1:139])

Lat_B1_RF3 <- postResample(Predicted_RFlatitude3_B1, B1_validation$LATITUDE)
##RMSE 11.6, Rsquared 0.88, MAE 8.05

#Error analysis
ErroLatitude_RF3_B1 <-  Predicted_RFlatitude3_B1 - B1_validation$LATITUDE
hist(ErroLatitude_RF3_B1)

#MODEL4 B1 Latitude Lat_B1_SVM3 ----
SVMlatitude3_B1 <- train(y=B1_train3$LATITUDE,x=B1_train3[,c(1:146)],
                         data = B1_train3, method = "svmLinear",
                         preprocess = c("center","scale"),
                         tuneLength = 15, trControl = fitcontrol)

SVMlatitude3_B1 <- readRDS("SVMlatitude3_B1.rds")

Predicted_SVMlatitude3_B1 <- predict(SVMlatitude3_B1, B1_validation[,1:146])

Lat_B1_SVM <- postResample(Predicted_SVMlatitude3_B1, B1_validation$LATITUDE)
#model4 ##RMSE 16.18, Rsquared 0.80, MAE 11.92   

#Error analysis
ErroLatitude_SVM3_B1 <-  Predicted_SVMlatitude3_B1 - B1_validation$LATITUDE
hist(ErroLatitude_SVM3_B1)
#MODEL5 B1 Latitude Lat_B1_KNN3 ----
#KNNlatitude3_B1 <- train(y=B1_train3$LATITUDE,x=B1_train3[,c(1:146)],
#                         data = B1_train3, method = "knn",
#                         preprocess=c("center","scale"),
#                         tuneLength = 15, trControl = fitcontrol)

KNNlatitude3_B1 <- readRDS("KNNlatitude3_B1.rds")

KNNlatitude3_B1

Predicted_KNNlatitude3_B1 <- predict(KNNlatitude3_B1, B1_validation[,1:146])

Lat_B1_KNN <- postResample(Predicted_KNNlatitude3_B1, B1_validation$LATITUDE)
##RMSE 11.2, Rsquared 0.899, MAE 6.68     

#Error analysis
ErroLatitude_KNN3_B1 <-  Predicted_KNNlatitude3_B1 - B1_validation$LATITUDE
hist(ErroLatitude_KNN3_B1)
#Metrics B1 Latitute ----
Metrics_Lat_B1 <- rbind(Lat_B1_KNN,Lat_B1_RF,Lat_B1_SVM)
Metrics_Lat_B1 <- format(Metrics_Lat_B1, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Lat_B1,theme=ttheme_default())

#MODEL1 B2 Latitude Lat_B2_RF2 ----
#RFlatitude2_B2 <- train(y=B2_train2$LATITUDE,x=B2_train2[,c(1:106)],
#                        data = B2_train2, method = "rf",ntree=5,
#                        tuneLength = 15, trControl = fitcontrol)

RFlatitude2_B2 <- readRDS("RFlatitude2_B2.rds") 

Predicted_RFlatitude2_B2 <- predict(RFlatitude2_B2, B2_validation[,1:106])

Lat_B2_RF <- postResample(Predicted_RFlatitude2_B2, B2_validation$LATITUDE)
##RMSE 10.82, Rsquared 0.85, MAE 7.08    

#Error analysis
ErroLatitude_RF2_B2 <-  Predicted_RFlatitude2_B2 - B2_validation$LATITUDE
hist(ErroLatitude_RF2_B2)

#MODEL2 B2 Latitude Lat_B2_RF2Exp ----
RFlatitude2Exp_B2 <- train(y=B2_train2Exp$LATITUDE,x=B2_train2Exp[,c(1:106)],
                           data = B2_train2Exp, method = "rf",ntree=5,
                           preProcess = c("BoxCox","center","scale"),
                           tuneLength = 15, trControl = fitcontrol)

RFlatitude2Exp_B2

Predicted_RFlatitude2Exp_B2 <- predict(RFlatitude2Exp_B2, B2_validation_Exp[,1:106])

Lat_B2_RF2Exp <- postResample(Predicted_RFlatitude2Exp_B2, B2_validation_Exp$LATITUDE)
##RMSE 10.39, Rsquared 0.87, MAE 7.24     

#Error analysis
ErroLatitude_RF2Exp_B2 <-  Predicted_RFlatitude2Exp_B2 - B2_validation_Exp$LATITUDE
hist(ErroLatitude_RF2Exp_B2)

#MODEL3 B2 Latitude Lat_B2_RF3 ----
RFlatitude3_B2 <- train(y=B2_train3$LATITUDE,x=B2_train3[,c(1:106)],
                        data = B2_train3, method = "rf",ntree=5,
                        tuneLength = 15, trControl = fitcontrol)

RFlatitude3_B2

Predicted_RFlatitude3_B2 <- predict(RFlatitude3_B2, B2_validation[,1:106])

Lat_B2_RF3 <- postResample(Predicted_RFlatitude3_B2, B2_validation$LATITUDE)
##RMSE 10.12, Rsquared 0.87, MAE 7.11 
#Error analysis
ErroLatitude_RF3_B2 <-  Predicted_RFlatitude3_B2 - B2_validation$LATITUDE
hist(ErroLatitude_RF3_B2)

#MODEL4 B2 Latitude Lat_B2_SVM3 ----
#SVMlatitude3_B2 <- train(y=B2_train3$LATITUDE,x=B2_train3[,c(1:106)],
#                         data = B2_train3, method = "svmLinear",
#                         preprocess = c("center","scale"),
#                         tuneLength = 15, trControl = fitcontrol)

SVMlatitude3_B2 <- readRDS("SVMlatitude3_B2.rds")

Predicted_SVMlatitude3_B2 <- predict(SVMlatitude3_B2, B2_validation[,1:106])

Lat_B2_SVM <- postResample(Predicted_SVMlatitude3_B2, B2_validation$LATITUDE)
##RMSE 15.53, Rsquared 0.74, MAE 11.53    

#Error analysis
ErroLatitude_SVM3_B2 <-  Predicted_SVMlatitude3_B2 - B2_validation$LATITUDE
hist(ErroLatitude_SVM3_B2)

#MODEL5 B2 Latitude Lat_B2_RF3Exp ----
RFlatitude3Exp_B2 <- train(y=B2_train3Exp$LATITUDE,x=B2_train3Exp[,c(1:106)],
                           data = B2_train3Exp, method = "rf",ntree=5,
                           preProcess = c("BoxCox","center","scale"),
                           tuneLength = 15, trControl = fitcontrol)

RFlatitude3Exp_B2

Predicted_RFlatitude3Exp_B2 <- predict(RFlatitude3Exp_B2, B2_validation_Exp[,1:106])

Lat_B2_RF3Exp <- postResample(Predicted_RFlatitude3Exp_B2, B2_validation_Exp$LATITUDE)
##RMSE 11.34, Rsquared 0.84, MAE 7.69     

#Error analysis
ErroLatitude_RF3Exp_B2 <-  Predicted_RFlatitude3Exp_B2 - B2_validation_Exp$LATITUDE
hist(ErroLatitude_RF3Exp_B2)

#MODEL6 B2 Latitude Lat_B2_KNN3 ----
#KNNlatitude3_B2 <- train(y=B2_train3$LATITUDE,x=B2_train3[,c(1:106)],
#                         data = B2_train3, method = "knn",
#                         preprocess=c("center","scale"),
#                         tuneLength = 15, trControl = fitcontrol)


KNNlatitude3_B2 <- readRDS("KNNlatitude3_B2.rds")

KNNlatitude3_B2

Predicted_KNNlatitude3_B2 <- predict(KNNlatitude3_B2, B2_validation[,1:106])

Lat_B2_KNN <- postResample(Predicted_KNNlatitude3_B2, B2_validation$LATITUDE)
##RMSE 9.5, Rsquared 0.89, MAE 5.92     

#Error analysis
ErroLatitude_KNN3_B1 <-  Predicted_KNNlatitude3_B1 - B1_validation$LATITUDE
hist(ErroLatitude_KNN3_B1)
#Metrics B2 Latitute ----
Metrics_Lat_B2 <- rbind(Lat_B2_KNN,Lat_B2_RF,Lat_B2_SVM)
Metrics_Lat_B2 <- format(Metrics_Lat_B2, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Lat_B2,theme=ttheme_default())

#LONGITUDE
#MODEL1 B0 Longitude Long_B0_RF2 ----
RFlongitude2_B0 <- train(y=B0_train2$LONGITUDE,x=B0_train2[,c(1:139)],
                        data = B0_train2, method = "rf",ntree=5,
                        tuneLength = 15, trControl = fitcontrol)

RFlongitude2_B0

Predicted_RFlongitude2_B0 <- predict(RFlongitude2_B0, B0_validation[,1:139])

Long_B0_RF2 <- postResample(Predicted_RFlongitude2_B0, B0_validation$LONGITUDE)
##RMSE 7.48, Rsquared 0.92, MAE 5.05

#Error analysis
ErroLongitude_RF2_B0 <-  Predicted_RFlongitude2_B0 - B0_validation$LONGITUDE
hist(ErroLongitude_RF2_B0)

#MODEL2 B0 Longitude Long_B0_KNN3 ----
KNNlongitude3_B0 <- train(y=B0_train3$LONGITUDE,x=B0_train3[,c(1:139)],
                         data = B0_train3, method = "knn",
                         preprocess=c("center","scale"),
                         tuneLength = 15, trControl = fitcontrol)


KNNlongitude3_B0 <- readRDS("KNNlongitude3_B0.rds")

KNNlongitude3_B0

Predicted_KNNlongitude3_B0 <- predict(KNNlongitude3_B0, B0_validation[,1:139])

Long_B0_KNN <- postResample(Predicted_KNNlongitude3_B0, B0_validation$LONGITUDE)
##RMSE 5.9, Rsquared 0.95, MAE 3.66     

#Error analysis
ErroLongitude_KNN3_B0 <-  Predicted_KNNlongitude3_B0 - B0_validation$LONGITUDE
hist(ErroLongitude_KNN3_B0)
#MODEL3 B0 Longitude Long_B0_SVM3 ----
#SVMlongitude3_B0 <- train(y=B0_train3$LONGITUDE,x=B0_train3[,c(1:139)],
#                          data = B0_train3, method = "svmLinear",
#                          preProcess = c("center","scale"),
#                          tuneLength = 15, trControl = fitcontrol)

SVMlongitude3_B0 <- readRDS("SVMlongitude3_B0.rds")

SVMlongitude3_B0

Predicted_SVMlongitude3_B0 <- predict(SVMlongitude3_B0, B0_validation[,1:139])

Long_B0_SVM <- postResample(Predicted_SVMlongitude3_B0, B0_validation$LONGITUDE)
##RMSE 11.48, Rsquared 0.81, MAE 8.38     

#Error analysis
ErroLongitude_SVM3_B0 <-  Predicted_SVMlongitude3_B0 - B0_validation$LONGITUDE
hist(ErroLongitude_SVM3_B0)

#Metrics B0 Longitute ----
Metrics_Long_B0 <- rbind(Long_B0_KNN,Long_B0_RF,Long_B0_SVM)
Metrics_Long_B0 <- format(Metrics_Long_B0, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Long_B0,theme=ttheme_default())

#MODEL1 B1 Longitude Long_B1_RF2 ----
#RFlongitude2_B1 <- train(y=B1_train2$LONGITUDE,x=B1_train2[,c(1:146)],
#                        data = B1_train2, method = "rf",ntree=5,
#                        tuneLength = 15, trControl = fitcontrol)

RFlongitude2_B1 <- readRDS("RFlongitude2_B1.rds")

Predicted_RFlongitude2_B1 <- predict(RFlongitude2_B1, B1_validation[,1:146])

Long_B1_RF <- postResample(Predicted_RFlongitude2_B1, B1_validation$LONGITUDE)
##RMSE 11.29, Rsquared 0.94, MAE 7.70   

#Error analysis
ErroLongitude_RF2_B1 <-  Predicted_RFlongitude2_B1 - B1_validation$LONGITUDE
hist(ErroLongitude_RF2_B1)
#MODEL2 B1 Longitude Long_B1_KNN3 ----
#KNNlongitude3_B1 <- train(y=B1_train3$LONGITUDE,x=B1_train3[,c(1:146)],
#                          data = B1_train3, method = "knn",
#                          preprocess=c("center","scale"),
#                          tuneLength = 15, trControl = fitcontrol)

KNNlongitude3_B1 <- readRDS("KNNlongitude3_B1.rds")

KNNlongitude3_B1

Predicted_KNNlongitude3_B1 <- predict(KNNlongitude3_B1, B1_validation[,1:146])

Long_B1_KNN <- postResample(Predicted_KNNlongitude3_B1, B1_validation$LONGITUDE)
##RMSE 9.9, Rsquared 0.95, MAE 6.42     

#Error analysis
ErroLongitude_KNN3_B1 <-  Predicted_KNNlongitude3_B1 - B1_validation$LONGITUDE
hist(ErroLongitude_KNN3_B1)

#MODEL3 B1 Longitude Long_B0_SVM3 ----
#SVMlongitude3_B1 <- train(y=B1_train3$LONGITUDE,x=B1_train3[,c(1:146)],
#                          data = B1_train3, method = "svmLinear",
#                          preProcess = c("center","scale"),
#                          tuneLength = 15, trControl = fitcontrol)

SVMlongitude3_B1 <- readRDS("SVMlongitude3_B1.rds")

SVMlongitude3_B1

Predicted_SVMlongitude3_B1 <- predict(SVMlongitude3_B1, B1_validation[,1:146])

Long_B1_SVM <- postResample(Predicted_SVMlongitude3_B1, B1_validation$LONGITUDE)
##RMSE 21.53, Rsquared 0.82, MAE 15.85     

#Error analysis
ErroLongitude_SVM3_B1 <-  Predicted_SVMlongitude3_B1 - B1_validation$LONGITUDE
hist(ErroLongitude_SVM3_B1)

#Metrics B1 Longitute ----
Metrics_Long_B1 <- rbind(Long_B1_KNN,Long_B1_RF,Long_B1_SVM)
Metrics_Long_B1 <- format(Metrics_Long_B1, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Long_B1,theme=ttheme_default())

#MODEL1 B2 Longitude Long_B2_RF2 ----
#RFlongitude2_B2 <- train(y=B2_train2$LONGITUDE,x=B2_train2[,c(1:106)],
#                        data = B2_train2, method = "rf",ntree=5,
#                        tuneLength = 15, trControl = fitcontrol)

RFlongitude2_B2 <- readRDS("RFlongitude2_B2.rds")

Predicted_RFlongitude2_B2 <- predict(RFlongitude2_B2, B2_validation[,1:106])

Long_B2_RF <- postResample(Predicted_RFlongitude2_B2, B2_validation$LONGITUDE)
##RMSE 12.06, Rsquared 0.85, MAE 8.52   

#Error analysis
ErroLatitude_RF2_B2 <-  Predicted_RFlatitude2_B2 - B2_validation$LATITUDE
hist(ErroLatitude_RF2_B2)

#MODEL2 B2 Longitude Long_B2_KNN3 ----
#KNNlongitude3_B2 <- train(y=B2_train3$LONGITUDE,x=B2_train3[,c(1:106)],
#                          data = B2_train3, method = "knn",
#                          preprocess=c("center","scale"),
#                          tuneLength = 15, trControl = fitcontrol)

KNNlongitude3_B2 <- readRDS("KNNlongitude3_B2.rds")

KNNlongitude3_B2

Predicted_KNNlongitude3_B2 <- predict(KNNlongitude3_B2, B2_validation[,1:106])

Long_B2_KNN <- postResample(Predicted_KNNlongitude3_B2, B2_validation$LONGITUDE)
##RMSE 11.6, Rsquared 0.87, MAE 7.19     

#Error analysis
ErroLongitude_KNN3_B2 <-  Predicted_KNNlongitude3_B2 - B2_validation$LONGITUDE
hist(ErroLongitude_KNN3_B2)


#MODEL3 B2 Longitude Long_B2_SVM3 ----
#SVMlongitude3_B2 <- train(y=B2_train3$LONGITUDE,x=B2_train3[,c(1:106)],
#                          data = B2_train3, method = "svmLinear",
#                          preProcess = c("center","scale"),
#                          tuneLength = 15, trControl = fitcontrol)

SVMlongitude3_B2 <- readRDS("SVMlongitude3_B2.rds")

SVMlongitude3_B2

Predicted_SVMlongitude3_B2 <- predict(SVMlongitude3_B2, B2_validation[,1:106])

Long_B2_SVM <- postResample(Predicted_SVMlongitude3_B2, B2_validation$LONGITUDE)
##RMSE 16.54, Rsquared 0.73, MAE 12.79     

#Error analysis
ErroLongitude_SVM3_B2 <-  Predicted_SVMlongitude3_B2 - B2_validation$LONGITUDE
hist(ErroLongitude_SVM3_B2)

#Metrics B2 Longitute ----
Metrics_Long_B2 <- rbind(Long_B2_KNN,Long_B2_RF,Long_B2_SVM)
Metrics_Long_B2 <- format(Metrics_Long_B2, digits = 2, format = "f")
grid.newpage()
grid.table(Metrics_Long_B2,theme=ttheme_default())

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



test <- filter(validationBUILDING, BUILDINGID == 1) 

test1 <- filter(test, RealBuilding == 0)
View(test1)  

View(test1[,apply(test1,1,function(x) x != -105)])
#Potential problematic WAPs -> 27(-74), 28(-74), 57(-94) & 58(-92)