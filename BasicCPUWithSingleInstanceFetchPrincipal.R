
# Fetch data from mysql
library(RMySQL)
library(forecast)
library(TSclust)

mydb = dbConnect(MySQL(), user='admin', password='reskingond', host='genie2.ckrmuolit65p.us-east-1.rds.amazonaws.com' ,dbname='devfactory_genie');


fetchQuery="select * from metrics_data m where  m.resource_id='i-476d7dd9' and metric_id=1 LIMIT 0 ,10000"
# InstanceData = fetch(dbSendQuery(mydb,fetchQuery ));
InstanceData = dbGetQuery(mydb,fetchQuery );
InstanceData$timestamp = as.POSIXlt(InstanceData$timestamp) 
OrderedInstanceData =  InstanceData[order(InstanceData$timestamp),];

myts = ts(OrderedInstanceData$value,frequency=2016)
distMatrix  = dist(OrderedInstanceData$value, method="DTW")
hc <- hclust(distMatrix, method="average")
plot(hc, labels=observedLabels, main="")
plot(myts);
mystl = stl(myts, s.window="periodic");
#plot(mystl)
plot(mystl$time.series)
seasonplot(myts)

fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
# double exponential - models level and trend
fit <- HoltWinters(myts, gamma=FALSE)
# triple exponential - models level, trend, and seasonal components
fit <- HoltWinters(myts)
#accuracy(fit)

plot(forecast(fit, 2016))
#plot(ForeCastedInstanceData$mean)
ForeCastedInstanceData = forecast(fit, 2016);
TOffTime = ForeCastedInstanceData$mean

cols <- 1:2016

TOffTime[cols][TOffTime[cols] > 5] = 100
#4 Get Time Map

TimeList <- as.data.frame(matrix(nrow=2016,ncol=1))
class(TimeList[1,1])
TimeList[,1] = as.POSIXct(TimeList[,1])
  TimeList[1,1] = as.POSIXct("2010-12-07 08:00:00")
  
for (i in 1:2016) {
    TimeList[i,1] = as.POSIXlt(Sys.time()) + 300 * i 
}

FinalRecomemndations = data.frame(TimeList[,1], TOffTime)
FinalRecomemndations[,1] = as.POSIXct(FinalRecomemndations[,1])

ScheduleRecommendation <- as.data.frame(matrix(nrow=1,ncol=9))
ScheduleRecommendation[,2] = as.POSIXct(ScheduleRecommendation[,2])
ScheduleRecommendation[,5] = as.POSIXct(ScheduleRecommendation[,5])
starttimeflag = FALSE;
J=1;
NumberOfDataPoints=0;

for (i in 1:2016){
  #print ("------loop");
  #print(i)
  #print(FinalRecomemndations[i,2])
  if (FinalRecomemndations[i,2]== 100){
    #print ("------loop 100 step out");
      if (((FinalRecomemndations[i-1,2])!= 100)& (i-1>0)) {
        J=J+1;
        ScheduleRecommendation[J-1,9] = NumberOfDataPoints;
      }
      
      #print(J)
      #print(starttime)
      #print(endtime)
      starttimeflag = FALSE;
      NumberOfDataPoints=0;
      next;
  }
  
  if ((FinalRecomemndations[i,2]< 100) & (!starttimeflag)){
   # print ("------loop not 100 ");
    starttime = FinalRecomemndations[i,1]
   # print(starttime)
   
    ScheduleRecommendation[J,1] =J;
    ScheduleRecommendation[J,2] = starttime;
    ScheduleRecommendation[J,3] = weekdays(starttime)
    ScheduleRecommendation[J,4] = strftime(starttime, format="%H:%M")
    
    starttimeflag=TRUE;
    #print(starttimeflag);
    
  }
  else 
  {
   #print ("------loop else endtime plays ");
    endtime = FinalRecomemndations[i,1];
    #print(endtime)
    ScheduleRecommendation[J,5] = endtime;
    ScheduleRecommendation[J,6] = weekdays(endtime)
    ScheduleRecommendation[J,7] = strftime(endtime, format="%H:%M")
    NumberOfDataPoints= NumberOfDataPoints+1;
    
    
  }
    
}

# Split into subset to get the final recomemndations



dbDisconnect(mydb)

# Get Off time Greater than 6 Hours

ScheduleRecommendationSet = ScheduleRecommendation[ScheduleRecommendation$V9>100,];
print("On Off Recommendations Instance Id ---i-476d7dd9 ")
ScheduleRecommendationSet;





