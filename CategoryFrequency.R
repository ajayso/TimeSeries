library(jsonlite)
library(RMySQL)
library(tm)
library(SnowballC)
library(wordcloud)



mydb = dbConnect(MySQL(), user='admin', password='reskingond', host='genie2.ckrmuolit65p.us-east-1.rds.amazonaws.com' ,dbname='devfactory_genie');
AccountQuery = "select distinct account,  tags from devfactory_genie.instances  LIMIT 0 ,70000";
AccountData = dbGetQuery(mydb,AccountQuery );

onetext ="";
for (i in 1:length(AccountData[,1]) ){
  #print(i)
  if(is.na(AccountData[i,2])) next;
  cc= fromJSON(AccountData[i,2]);
  if(length(cc)==0) next;
  for (kk in 1: length(cc)){
    
    if (!is.null(cc[[kk]]$key)){
      onetext = paste(onetext, cc[[kk]]$key,sep=" ")
    }
  }
  
  # onetext = paste(onetext,AccountData[i,2], sep=" " )
  #write(AccountData[i,2],file="myfile3.json",append=TRUE)
  
}
review_corpus = Corpus(VectorSource(onetext))
review_corpus = tm_map(review_corpus, removeWords,c("appdata"))
review_dtm <- DocumentTermMatrix(review_corpus)
review_dtm
terms = findFreqTerms(review_dtm, 5)
toJSON(terms)