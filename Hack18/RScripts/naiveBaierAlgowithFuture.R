library("klaR")
library("caret")
options(warn=-1)
library("data.table")
library("jsonlite")
#initializing global variables
rm(list = setdiff(ls(), lsf.str()))
model=""
testingdata=""
AIR_data <- fread("C:/Hack18/Data/NovandDec17.csv")
initializeClassifier=function(){
#loading data from CSV
#AIR_data <- read.csv("C:/Hack18/Data/NovandDec17.csv") #old slower way
#for too large data we can split -b100m bigfile.csv
#AIR_data <- fread("C:/Hack18/Data/NovandDec17.csv")
AIR_data<- as.data.frame(unclass(AIR_data))
air=AIR_data[, !(names(AIR_data) %in% c("FL_DATE"))]   #omitting flight date


#setting up factors for decisions
air$DISTANCE=ifelse(as.numeric(air$DISTANCE) < 1118,"SHORT","LONG")
air$DISTANCE=as.factor(air$DISTANCE)
air$isDELAY=ifelse(is.na(as.numeric(air$ARR_DELAY_NEW)+as.numeric(air$DEP_DELAY_NEW)+as.numeric(air$CARRIER_DELAY)+as.numeric(air$WEATHER_DELAY)+as.numeric(air$NAS_DELAY)+as.numeric(air$SECURITY_DELAY)+as.numeric(air$LATE_AIRCRAFT_DELAY)),"NO","YES")
air$isDELAY=as.factor(air$isDELAY)

air=air[,c(-6,-7,-8,-9,-11,-12,-13,-14,-15)]#removing unwanted predictors

# Splitting the data into training and testing sets 
# using 90% as training data and 10 % as test data
trainIndex=createDataPartition(air$isDELAY,p=0.9,list=FALSE)
trainingdata=air[trainIndex,]
testingdata<<-trainingdata[0,]

#Applying the Naive Bayes model to the training data
model<<-NaiveBayes(trainingdata[,-7],trainingdata$isDELAY) #passing train data exculding the isDelay column values

#save classifier to file to use later via WebSerivce
#saveRDS(trainingdata[0,], "C:/Hack18/ObjectsDump/test_data_structure.rds")
#saveRDS(model, "C:/Hack18/ObjectsDump/NaiveBayesmodel.rds")
}

myAdd=function(x,y){
    sum=x+y
    return(sum)
}

resetTestingObject=function(){
testingdata<<-testingdata[0,]
}


populateTestingDataObject=function(MONTH,DAY_OF_WEEK,UNIQUE_CARRIER,ORIGIN,DEST,DISTANCE,isDELAY){
#put multiple times and remeber index of each request
#setting delay to true just mock value.
DISTANCE=ifelse(as.numeric(DISTANCE) < 1118,"SHORT","LONG")
if(length(isDELAY)==0){
	isDELAY="YES"
}
testingdata <<-rbind(testingdata,data.frame("MONTH"=MONTH,"DAY_OF_WEEK"=DAY_OF_WEEK,"UNIQUE_CARRIER"=UNIQUE_CARRIER,"ORIGIN"=ORIGIN,"DEST"=DEST,"DISTANCE"=DISTANCE,"isDELAY"=isDELAY))
}

findisDelayForCarriers=function(){
#Applying the model used for training data to predict test data
predictor=predict(model,testingdata[,-7])
predictions=cbind(Predicted=as.character(predictor$class),Actual=as.character(testingdata$isDELAY))

return(predictions[,-1])
}

findaveragedelayforcarriers=function(){
AIR_data$FL_DATE=as.Date(AIR_data$FL_DATE,format = "%m/%d/%Y")
#currentDate=Sys.time()
currentDate="2017-12-10"   #static date for demo
listofcarrierswithdelay=list()
for(carrier in unique(AIR_data$UNIQUE_CARRIER)) {
carrierspecific=subset(AIR_data, ORIGIN == "DFW" & DEST=="ORD" & UNIQUE_CARRIER==carrier & (FL_DATE < as.Date(currentDate) & as.Date(currentDate) - FL_DATE <=15 ) )
totalDelayForCarrier=as.numeric(carrierspecific$ARR_DELAY_NEW)+as.numeric(carrierspecific$DEP_DELAY_NEW)+as.numeric(carrierspecific$CARRIER_DELAY)+as.numeric(carrierspecific$WEATHER_DELAY)+as.numeric(carrierspecific$NAS_DELAY)+as.numeric(carrierspecific$SECURITY_DELAY)+as.numeric(carrierspecific$LATE_AIRCRAFT_DELAY)
averageDelayForCarrier=sum(na.omit(totalDelayForCarrier))/length(totalDelayForCarrier)  # Average delay based on last 15 days trend
  listofcarrierswithdelay[[carrier]]=ifelse(is.na(sum(na.omit(totalDelayForCarrier))/length(totalDelayForCarrier)),0,sum(na.omit(totalDelayForCarrier))/length(totalDelayForCarrier))
}
 return (toJSON(listofcarrierswithdelay, pretty = TRUE, auto_unbox = TRUE))
}
