library("klaR")
library("caret")
options(warn=-1)
library("data.table")
library("jsonlite")
library(lubridate)
#initializing global variables
rm(list = setdiff(ls(), lsf.str()))
model=""
testingdata=""
dayofweeklist=list("Monday"=1,"Tuesday"=2,"Wednesday"=3,"Thursday"=4,"Friday"=5,"Saturday"=6,"Sunday"=7)
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
totalDelay=gettotaldelay(air)

air$isDELAY=as.factor( ifelse(totalDelay>0,"YES","NO") )
air$isARR_DELAY=as.factor(  ifelse((ifelse(is.na(as.numeric(as.character(air$ARR_DELAY_NEW))),0,as.numeric(as.character(air$ARR_DELAY_NEW))))>0,"YES","NO")  )
air$isDEP_DELAY=as.factor( ifelse((ifelse(is.na(as.numeric(as.character(air$DEP_DELAY_NEW))),0,as.numeric(as.character(air$DEP_DELAY_NEW))))>0,"YES","NO") )
air$isCARRIER_DELAY=as.factor( ifelse((ifelse(is.na(as.numeric(as.character(air$CARRIER_DELAY))),0,as.numeric(as.character(air$CARRIER_DELAY))))>0,"YES","NO") )
air$isWEATHER_DELAY=as.factor( ifelse((ifelse(is.na(as.numeric(as.character(air$WEATHER_DELAY))),0,as.numeric(as.character(air$WEATHER_DELAY))))>0,"YES","NO") )
air$isNAS_DELAY=as.factor( ifelse((ifelse(is.na(as.numeric(as.character(air$NAS_DELAY))),0,as.numeric(as.character(air$NAS_DELAY))))>0,"YES","NO") )
air$isSECURITY_DELAY=as.factor( ifelse((ifelse(is.na(as.numeric(as.character(air$SECURITY_DELAY))),0,as.numeric(as.character(air$SECURITY_DELAY))))>0,"YES","NO") )
air$isLATE_AIRCRAFT_DELAY=as.factor( ifelse((ifelse(is.na(as.numeric(as.character(air$LATE_AIRCRAFT_DELAY))),0,as.numeric(as.character(air$LATE_AIRCRAFT_DELAY))))>0,"YES","NO") )

air=air[,c(-6,-7,-8,-9,-11,-12,-13,-14,-15)]#removing unwanted predictors

# Splitting the data into training and testing sets 
# using 90% as training data and 10 % as test data
trainIndex=createDataPartition(air$isDELAY,p=0.9,list=FALSE)
trainingdata=air[trainIndex,]
#testingdata<<-air[-trainIndex,]
testingdata<<-trainingdata[0,]

#Applying the Naive Bayes model to the training data
modelTD<<-NaiveBayes(trainingdata[,-c(7:14)],trainingdata$isDELAY) #passing train data exculding the isDelay column values
modelAD<<-NaiveBayes(trainingdata[,-c(7:14)],trainingdata$isARR_DELAY) 
modelDD<<-NaiveBayes(trainingdata[,-c(7:14)],trainingdata$isDEP_DELAY) 
modelCD<<-NaiveBayes(trainingdata[,-c(7:14)],trainingdata$isCARRIER_DELAY) 
modelWD<<-NaiveBayes(trainingdata[,-c(7:14)],trainingdata$isWEATHER_DELAY) 
modelND<<-NaiveBayes(trainingdata[,-c(7:14)],trainingdata$isNAS_DELAY) 
modelSD<<-NaiveBayes(trainingdata[,-c(7:14)],trainingdata$isSECURITY_DELAY) 
modelLD<<-NaiveBayes(trainingdata[,-c(7:14)],trainingdata$isLATE_AIRCRAFT_DELAY) 

#save classifier to file to use later via WebSerivce
#saveRDS(trainingdata[0,], "C:/Hack18/ObjectsDump/test_data_structure.rds")
#saveRDS(model, "C:/Hack18/ObjectsDump/NaiveBayesmodel.rds")
}

myAdd=function(x,y,st){
    sum=x+y
    return(sum)
}

gettotaldelay=function(air){
totalDelay=(ifelse(is.na(as.numeric(as.character(air$ARR_DELAY_NEW))),0,as.numeric(as.character(air$ARR_DELAY_NEW))))+
(ifelse(is.na(as.numeric(as.character(air$DEP_DELAY_NEW))),0,as.numeric(as.character(air$DEP_DELAY_NEW))))+
(ifelse(is.na(as.numeric(as.character(air$CARRIER_DELAY))),0,as.numeric(as.character(air$CARRIER_DELAY))))+
(ifelse(is.na(as.numeric(as.character(air$WEATHER_DELAY))),0,as.numeric(as.character(air$WEATHER_DELAY))))+
(ifelse(is.na(as.numeric(as.character(air$NAS_DELAY))),0,as.numeric(as.character(air$NAS_DELAY))))+
(ifelse(is.na(as.numeric(as.character(air$SECURITY_DELAY))),0,as.numeric(as.character(air$SECURITY_DELAY))))+
(ifelse(is.na(as.numeric(as.character(air$LATE_AIRCRAFT_DELAY))),0,as.numeric(as.character(air$LATE_AIRCRAFT_DELAY))))

return(totalDelay)
}


resetTestingObject=function(){
testingdata<<-testingdata[0,]
}

delaypredictionforairlines=function(airline,traveldate,origin,dest,distance){
#loop traveldate + 5 days 
#extract month ,dayofweek based on date
resetTestingObject()
daycounter=1
currenttraveldate=as.Date(traveldate,format="%d/%m/%Y")
while (daycounter<=5)
{
daystart=as.character(dayofweeklist[weekdays(as.Date(currenttraveldate,format="%d/%m/%Y"))])
#cat(month(as.Date(currenttraveldate, format="%d/%m/%Y"))," || ",daystart," || ",airline," || ",origin," || ",dest," || ",distance," || ","\n")
populateTestingDataObject(month(as.Date(currenttraveldate, format="%d/%m/%Y")) ,daystart,airline,origin,dest,distance,"")


currenttraveldate=currenttraveldate+1
daycounter=daycounter+1
}
findisDelayForAirlines(traveldate)
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
predictorTD=predict(modelTD,testingdata[,-c(7:14)])
predictorAD=predict(modelAD,testingdata[,-c(7:14)])
predictorDD=predict(modelDD,testingdata[,-c(7:14)])
predictorCD=predict(modelCD,testingdata[,-c(7:14)])
predictorWD=predict(modelWD,testingdata[,-c(7:14)])
predictorND=predict(modelND,testingdata[,-c(7:14)])
predictorSD=predict(modelSD,testingdata[,-c(7:14)])
predictorLD=predict(modelLD,testingdata[,-c(7:14)])
predictionsTD=cbind(Predicted=as.character(predictorTD$class),Actual=as.character(testingdata$isDELAY))
predictionsAD=cbind(Predicted=as.character(predictorAD$class),Actual=as.character(testingdata$isARR_DELAY))
predictionsDD=cbind(Predicted=as.character(predictorDD$class),Actual=as.character(testingdata$isDEP_DELAY))
predictionsCD=cbind(Predicted=as.character(predictorCD$class),Actual=as.character(testingdata$isCARRIER_DELAY))
predictionsWD=cbind(Predicted=as.character(predictorWD$class),Actual=as.character(testingdata$isWEATHER_DELAY))
predictionsND=cbind(Predicted=as.character(predictorND$class),Actual=as.character(testingdata$isNAS_DELAY))
predictionsSD=cbind(Predicted=as.character(predictorSD$class),Actual=as.character(testingdata$isSECURITY_DELAY))
predictionsLD=cbind(Predicted=as.character(predictorLD$class),Actual=as.character(testingdata$isLATE_AIRCRAFT_DELAY))
count=1
delayPredictionJson=list()
for(i in unique(testingdata$UNIQUE_CARRIER)) { 
row=list()
row[["isDelay"]]=predictionsTD[,1][count]
row[["isARR_DELAY"]]=predictionsAD[,1][count]
row[["isDEP_DELAY"]]=predictionsDD[,1][count]
row[["isCARRIER_DELAY"]]=predictionsCD[,1][count]
row[["isWEATHER_DELAY"]]=predictionsWD[,1][count]
row[["isNAS_DELAY"]]=predictionsND[,1][count]
row[["isSECURITY_DELAY"]]=predictionsSD[,1][count]
row[["isLATE_AIRCRAFT_DELAY"]]=predictionsLD[,1][count]
delayPredictionJson[[i]]<-row  
count=count+1
 }
return(toJSON(delayPredictionJson, pretty = TRUE, auto_unbox = TRUE))
#Err=1-sum(predictor$class == testingdata$isDELAY)/length(testingdata$isDELAY)
}

findisDelayForAirlines=function(traveldate){
#Applying the model used for training data to predict test data
predictorTD=predict(modelTD,testingdata[,-c(7:14)])
predictorAD=predict(modelAD,testingdata[,-c(7:14)])
predictorDD=predict(modelDD,testingdata[,-c(7:14)])
predictorCD=predict(modelCD,testingdata[,-c(7:14)])
predictorWD=predict(modelWD,testingdata[,-c(7:14)])
predictorND=predict(modelND,testingdata[,-c(7:14)])
predictorSD=predict(modelSD,testingdata[,-c(7:14)])
predictorLD=predict(modelLD,testingdata[,-c(7:14)])
predictionsTD=cbind(Predicted=as.character(predictorTD$class),Actual=as.character(testingdata$isDELAY))
predictionsAD=cbind(Predicted=as.character(predictorAD$class),Actual=as.character(testingdata$isARR_DELAY))
predictionsDD=cbind(Predicted=as.character(predictorDD$class),Actual=as.character(testingdata$isDEP_DELAY))
predictionsCD=cbind(Predicted=as.character(predictorCD$class),Actual=as.character(testingdata$isCARRIER_DELAY))
predictionsWD=cbind(Predicted=as.character(predictorWD$class),Actual=as.character(testingdata$isWEATHER_DELAY))
predictionsND=cbind(Predicted=as.character(predictorND$class),Actual=as.character(testingdata$isNAS_DELAY))
predictionsSD=cbind(Predicted=as.character(predictorSD$class),Actual=as.character(testingdata$isSECURITY_DELAY))
predictionsLD=cbind(Predicted=as.character(predictorLD$class),Actual=as.character(testingdata$isLATE_AIRCRAFT_DELAY))
daycounter=1
currenttraveldate=as.Date(traveldate,format="%d/%m/%Y")
delayPredictionJson=list()

while (daycounter<=5)
{
row=list()
row[["Date"]]=currenttraveldate
row[["isDelay"]]=predictionsTD[,1][daycounter]
row[["isARR_DELAY"]]=predictionsAD[,1][daycounter]
row[["isDEP_DELAY"]]=predictionsDD[,1][daycounter]
row[["isCARRIER_DELAY"]]=predictionsCD[,1][daycounter]
row[["isWEATHER_DELAY"]]=predictionsWD[,1][daycounter]
row[["isNAS_DELAY"]]=predictionsND[,1][daycounter]
row[["isSECURITY_DELAY"]]=predictionsSD[,1][daycounter]
row[["isLATE_AIRCRAFT_DELAY"]]=predictionsLD[,1][daycounter]
delayPredictionJson[[daycounter]]=row
currenttraveldate=currenttraveldate+1
daycounter=daycounter+1
}
return(toJSON(delayPredictionJson, pretty = TRUE, auto_unbox = TRUE))
#Err=1-sum(predictor$class == testingdata$isDELAY)/length(testingdata$isDELAY)
}


findaveragedelayforcarriers=function(departuredate){
AIR_data$FL_DATE=as.Date(AIR_data$FL_DATE,format = "%m/%d/%Y")
#currentDate=Sys.time()
currentDate=departuredate   #static date for demo
listofcarrierswithdelay=list()
for(carrier in unique(AIR_data$UNIQUE_CARRIER)) {
carrierspecific=subset(AIR_data, ORIGIN == "DFW" & DEST=="ORD" & UNIQUE_CARRIER==carrier & (FL_DATE < as.Date(currentDate) & as.Date(currentDate) - FL_DATE <=8 ) )
totalDelayForCarrier=gettotaldelay(carrierspecific)
averageDelayForCarrier=sum(na.omit(totalDelayForCarrier))/length(totalDelayForCarrier)  # Average delay based on last 15 days trend
  listofcarrierswithdelay[[carrier]]=ifelse(is.na(sum(na.omit(totalDelayForCarrier))/length(totalDelayForCarrier)),0,sum(na.omit(totalDelayForCarrier))/length(totalDelayForCarrier))
}
 return (toJSON(listofcarrierswithdelay, pretty = TRUE, auto_unbox = TRUE))
}


getpasttrendjson=function(departuredate){
AIR_data$FL_DATE=as.Date(AIR_data$FL_DATE,format = "%m/%d/%Y")
currentDate=departuredate
#past trend report generator
pasttrendjson=list()
counter=1
for(carrier in unique(AIR_data$UNIQUE_CARRIER)) {
carrierspecific=subset(AIR_data, ORIGIN == "DFW" & DEST=="ORD" & UNIQUE_CARRIER==carrier & (FL_DATE < as.Date(currentDate) & as.Date(currentDate) - FL_DATE <=8 ) )
carrierspecific[is.na(carrierspecific)] <- 0
for(uniquedate in as.character(unique(carrierspecific$FL_DATE))){
datespecific=subset(carrierspecific,FL_DATE==uniquedate)

row=list()
row[["Date"]]=uniquedate
row[["Carrier"]]=carrier
row[["avgARR_DELAY"]]=ifelse(is.na(sum(as.numeric(as.character(datespecific$ARR_DELAY_NEW)))/length(datespecific$ARR_DELAY_NEW)),0,sum(as.numeric(as.character(datespecific$ARR_DELAY_NEW)))/length(datespecific$ARR_DELAY_NEW))

row[["avgDEP_DELAY"]]=ifelse(is.na(sum(as.numeric(as.character(datespecific$DEP_DELAY_NEW)))/length(datespecific$DEP_DELAY_NEW)),0,sum(as.numeric(as.character(datespecific$DEP_DELAY_NEW)))/length(datespecific$DEP_DELAY_NEW))
row[["avgCARRIER_DELAY"]]=ifelse(is.na(sum(as.numeric(as.character(datespecific$CARRIER_DELAY)))/length(datespecific$CARRIER_DELAY)),0,sum(as.numeric(as.character(datespecific$CARRIER_DELAY)))/length(datespecific$CARRIER_DELAY))
row[["avgWEATHER_DELAY"]]=ifelse(is.na(sum(as.numeric(as.character(datespecific$WEATHER_DELAY)))/length(datespecific$WEATHER_DELAY)),0,sum(as.numeric(as.character(datespecific$WEATHER_DELAY)))/length(datespecific$WEATHER_DELAY))
row[["avgNAS_DELAY"]]=ifelse(is.na(sum(as.numeric(as.character(datespecific$NAS_DELAY)))/length(datespecific$NAS_DELAY)),0,sum(as.numeric(as.character(datespecific$NAS_DELAY)))/length(datespecific$NAS_DELAY))
row[["avgSECURITY_DELAY"]]=ifelse(is.na(sum(as.numeric(as.character(datespecific$SECURITY_DELAY)))/length(datespecific$SECURITY_DELAY)),0,sum(as.numeric(as.character(datespecific$SECURITY_DELAY)))/length(datespecific$SECURITY_DELAY))
row[["avgLATE_AIRCRAFT_DELAY"]]=ifelse(is.na(sum(as.numeric(as.character(datespecific$LATE_AIRCRAFT_DELAY)))/length(datespecific$LATE_AIRCRAFT_DELAY)),0,sum(as.numeric(as.character(datespecific$LATE_AIRCRAFT_DELAY)))/length(datespecific$LATE_AIRCRAFT_DELAY))
pasttrendjson[[counter]]=row
counter=counter+1

}

}
return(toJSON(pasttrendjson, pretty = TRUE, auto_unbox = TRUE))
}
