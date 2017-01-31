
# Fetch data from mysql
library(RMySQL)
library(forecast)
library(jsonlite)
# Function Includes
source("C:/OnOffSchedules/FetchFunctions.R")


GetValue = function (Data,name){
  for (i in 1:length(Data[,1]) )
  {
    if (Data[i,1]==name)
      return (Data[i,2]);
    
  }
  return ("");
}


# Database Connection
mydb = dbConnect(MySQL(), user='admin', password='reskingond', host='genie2.ckrmuolit65p.us-east-1.rds.amazonaws.com' ,dbname='devfactory_genie');


InstancesQuery="select distinct m.resource_id from metrics_data  m LIMIT 0 ,100"
Instances = dbGetQuery(mydb,InstancesQuery );
for (i in 1:length(Instances[,1])) {
  fetchQuery = paste(" select * from metrics_data m where  m.resource_id='",Instances[i,1],sep="");
  #fetchQuery = paste(" select m.resource_id,m.timestamp,m.value, i.account from metrics_data m inner join instances i on i.platform_instance_id = m.resource_id where  m.resource_id='",Instances[i,1],sep="");
  fetchQuery = paste(fetchQuery,"' and metric_id=1 LIMIT 0 ,5000",sep="")
  #fetchQuery="select * from metrics_data m where  m.resource_id='" + Instances[i,1] + "' and metric_id=1 LIMIT 0 ,4000"
  print(fetchQuery[1]);
  InstanceData = dbGetQuery(mydb,fetchQuery );
  length(InstanceData$resource_id)
  if (length(InstanceData$resource_id)>2000){
      InstanceData$timestamp = as.POSIXlt(InstanceData$timestamp) 
      OrderedInstanceData =  InstanceData[order(InstanceData$timestamp),];
      OnOffSet = GetOnOffRecommendations(OrderedInstanceData);
   
      if ((length(OnOffSet)==0) | (length(OnOffSet[,1])==0)) 
          next;
      
      # Write DataSet to a file OR Database.
      AccountQuery = paste("select distinct account,tags,instance_type from devfactory_genie.instances where platform_instance_id='",Instances[i,1],sep="");
      AccountQuery = paste(AccountQuery,"' LIMIT 0 ,1",sep="");
      AccountData = dbGetQuery(mydb,AccountQuery );
      AccountData$tags
      Data = fromJSON(AccountData$tags)
      name = GetValue(Data,"Name");
      EnvironmentType =GetValue(Data,"EnvironmentType"); 
      Owner = GetValue(Data,"Owner");
      InstanceType = AccountData$instance_type
      df = data.frame(name,EnvironmentType,Owner,InstanceType)
      dfJson = toJSON(df)
      #dfJson= gsub("[", " ", dfJson[1])
      #dfJson= gsub("]", "", dfJson[1])
      print(OnOffSet);
      # Instance Grouping
     
      ScheduleString="";
      for (jj in 1:length(OnOffSet[,1])) {
            if (  (!is.na(OnOffSet[jj,3])) &   (!is.na(OnOffSet[jj,6]))
              ){
                
                ScheduleString = paste(OnOffSet[jj,3],"Off" , sep=" ");
                ScheduleString = paste(ScheduleString, OnOffSet[jj,6], sep=" ");
                ScheduleString = paste(ScheduleString, "On", sep=" ");
                             
            }
        
      }
      StoreRecommendationQuery = paste("INSERT INTO `devfactory_genie`.`on_off_recommendation`(`account`,`instance_id`,`schedule`,`tags`,) VALUES (","'",sep="");
      StoreRecommendationQuery = paste(StoreRecommendationQuery,AccountData$account,sep="");
      StoreRecommendationQuery = paste(StoreRecommendationQuery,"','",sep="");
      StoreRecommendationQuery = paste(StoreRecommendationQuery,Instances[i,1],sep="");
      StoreRecommendationQuery = paste(StoreRecommendationQuery,"','",sep="");
      StoreRecommendationQuery = paste(StoreRecommendationQuery,ScheduleString,sep="");
      StoreRecommendationQuery = paste(StoreRecommendationQuery,"','",sep="");
      StoreRecommendationQuery = paste(StoreRecommendationQuery,dfJson,sep="");
      StoreRecommendationQuery = paste(StoreRecommendationQuery,"')",sep="");
      print(StoreRecommendationQuery);
      #dbGetQuery(mydb,StoreRecommendationQuery );
      
      
     
  }
}

dbDisconnect(mydb);

