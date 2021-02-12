library("klaR")
library("caret")
options(warn=-1)
model= readRDS("C:/Hack18/ObjectsDump/NaiveBayesmodel.rds")
testingdata=readRDS("C:/Hack18/ObjectsDump/test_data_structure.rds")

myAdd=function(x,y){
    sum=x+y
    return(sum)
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

findDelayForCarriers=function(){
#Applying the model used for training data to predict test data
predictor=predict(model,testingdata[,-7])
predictions=cbind(Predicted=as.character(predictor$class),Actual=as.character(testingdata$isDELAY))

return(predictions[,-1])
}
