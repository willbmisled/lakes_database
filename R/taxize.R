library(taxize)
library(tidyverse)


tax<-read.csv('output/nla_phyto_taxonomy.csv',stringsAsFactors = FALSE)
  tax<-mutate(tax,genus=ifelse(genus=="Sphaerodinium/Glenodinium/Peridiniopsis Complex","Sphaerodinium",genus))
  tax<-mutate(tax,genus=ifelse(genus=="Staurodesmus/Arthrodesmus/Octacanthium,\"Staurodesmus\"","Staurodesmus",genus))
  
tax<-arrange(tax,genus)
  
itis<-classification(tax$genus,db='itis')


itis1<-c()
  for(i in c(1:length(tsn))){
    if(is.null(dim(itis[[i]]))==TRUE) {
      i=i+1
    } else {
    x<-spread(itis[[i]][-3],rank,name)
    y<-spread(itis[[i]][-1],rank,id)
      names(x)<-paste(names(x),'itis',sep="_")
      names(y)<-paste(names(y),'tsn',sep="_")
  itis1<-bind_rows(itis1,cbind(x,y))
 } }


#ncbi

ncbi<-classification(tax$genus,db='ncbi')

save(itis,ncbi,file='output/temp_taxonomy.rda')
