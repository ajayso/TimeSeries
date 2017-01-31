library(jsonlite)
library(RMySQL)



mydb = dbConnect(MySQL(), user='admin', password='reskingond', host='genie2.ckrmuolit65p.us-east-1.rds.amazonaws.com' ,dbname='devfactory_genie');

AccountQuery = "select distinct account,tags from devfactory_genie.instances where platform_instance_id='i-1cc836b4' LIMIT 0 ,1";
AccountData = dbGetQuery(mydb,AccountQuery );

Data = fromJSON(AccountData$tags)

GetValue = function (Data,name){
  for (i in 1:length(Data[,1]) )
    {
    if (Data[i,1]==name)
      return (Data[i,2]);
      
  }
}

name = GetValue(Data,"Name");
EnvironmentType =GetValue(Data,"EnvironmentType"); 
Owner = GetValue(Data,"Owner");
df = data.frame(name,EnvironmentType,Owner)
dfJson = toJSON(df)

"value"), row.names = c(NA, -5L), class = "data.frame")

