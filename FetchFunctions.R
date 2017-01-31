GetOnOffRecommendations <- function(OrderedInstanceData) {
  
  
  ScheduleRecommendationSet <- as.data.frame(matrix(nrow=1,ncol=9))
  myts = ts(OrderedInstanceData$value,frequency=2016)
  
  #ModelName =  paste(OrderedInstanceData$resource_id[1], "ts.rdata",sep="-");
  #save(myts, file = ModelName);
  
  #plot(myts);
  #OrderedInstanceData$resource_id;
  result <- tryCatch({
    
   
    
    mystl = stl(myts, s.window="periodic");
    #print("Over 2 Periods....");
    plot(mystl)
    #seasonplot(myts)
    
    #fit <- HoltWinters(myts, beta=FALSE, gamma=FALSE)
    # double exponential - models level and trend
    #fit <- HoltWinters(myts, gamma=FALSE)
    # triple exponential - models level, trend, and seasonal components
    fit <- HoltWinters(myts)
    #accuracy(fit)
    
    #plot(forecast(fit, 2016))
    #plot(ForeCastedInstanceData$mean)
    ForeCastedInstanceData = forecast(fit, 2016);
    TOffTime = ForeCastedInstanceData$mean
    
    cols <- 1:2016
    
    TOffTime[cols][TOffTime[cols] > 4] = 100
    #4 Get Time Map
    
    TimeList <- as.data.frame(matrix(nrow=2016,ncol=1))
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
        if (i==1) next;
        #print ("------loop 100 step out");
        if (((FinalRecomemndations[i-1,2])!= 100)& (i-1>0)) {
          J=J+1;
          ScheduleRecommendation[J-1,9] = NumberOfDataPoints;
          #print(ScheduleRecommendation[J-1,]);
        }
        
        #print(J)
        #print(starttime)
        #print(endtime)
        starttimeflag = FALSE;
        NumberOfDataPoints=0;
        next;
      }
      
      if ((FinalRecomemndations[i,2]< 100) & (!starttimeflag)){

        starttime = FinalRecomemndations[i,1]
        ScheduleRecommendation[J,1] =OrderedInstanceData$resource_id[1];
        ScheduleRecommendation[J,2] = starttime;
        ScheduleRecommendation[J,3] = paste(weekdays(starttime),strftime(starttime, format="%H:%M"),sep="-")
        ScheduleRecommendation[J,4] = strftime(starttime, format="%H:%M")
        starttimeflag=TRUE;
        #print ("-exit-----loop not 100 ");
        #print(starttimeflag);
        
      }
      else 
      {
        #print ("------ START loop else endtime plays ");
        endtime = FinalRecomemndations[i,1];
        #print(endtime)
        ScheduleRecommendation[J,5] = endtime;
        #ScheduleRecommendation[J,6] = weekdays(endtime)
        ScheduleRecommendation[J,6] = paste(weekdays(endtime),strftime(endtime, format="%H:%M"),sep="-")
        ScheduleRecommendation[J,7] = strftime(endtime, format="%H:%M")
        NumberOfDataPoints= NumberOfDataPoints+1;
       
        #print ("------END  -- loop else endtime plays ");
      }
      
    }
    # Get Off time Greater than 6 Hours
    ScheduleRecommendationSet = ScheduleRecommendation[ScheduleRecommendation$V9>100,];
    
    print("successful exit...");
    #print(ScheduleRecommendationSet);
    #ScheduleRecommendationSet;
    print("successful exit...ScheduleRecommendationSet");
    
    return (ScheduleRecommendationSet);
    
    for (o in 1:length(ScheduleRecommendationSet[,1])){
      ScheduleStringData=''
      print (o)
      if (!is.na(ScheduleRecommendationSet[v,7])){
        #print("MF");
        #print(ScheduleRecommendationSet[o,3])
        ScheduleStringData= paste(ScheduleStringData, ScheduleRecommendationSet[o,3],sep=" ");
        ScheduleStringData= paste(ScheduleStringData , ScheduleRecommendationSet[o,4],sep=" ");
        ScheduleStringData= paste(ScheduleStringData ,ScheduleRecommendationSet[o,6] ,sep=" TO ");
        ScheduleStringData= paste(ScheduleStringData , ScheduleRecommendationSet[o,7],sep=" ");
        ScheduleRecommendationSet[o,8]=ScheduleStringData
        #print(ScheduleStringData)
      }
    }
    return (ScheduleRecommendationSet);
    
    
  }, warning = function(war) {
    print("warning exit...");
    return(NULL);
    
  }, error = function(err) {
    print("error ocurred....");
    #print(err);
    return(NULL);
    
  }, finally = {
    
    
  }) # END tryCatch
  
  
 
}
