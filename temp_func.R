
#function to reformat the raw NLA data based on a modified data structure file
formatData<-function(x='nla2012_waterchem_wide.csv',y='waterchem.csv'){
#read the raw data; trycatch will look for the data in two directories
a<-tryCatch(read.csv(paste('data/raw_data/',x,sep=''),sep=',',na.strings = c(""," ","NA","N/A")),
            warning = function(e) read.csv(paste('../data/raw_data/',x,sep=''),sep=',',na.strings = c(""," ","NA","N/A")))

#get the modified data structure
struc<-tryCatch(read.csv(paste('data/workfiles/',y,sep=''),sep=',',na.strings = c(""," ","NA","N/A")),
            warning = function(e) read.csv(paste('../data/workfiles/',y,sep=''),sep=',',na.strings = c(""," ","NA","N/A")))

#choose columns to keep, gather, or delete
keeps<-filter(struc,parameter=='column_name')%>%select(DATA_COLUMN)%>% unlist(use.names = FALSE)
gathers<-filter(struc,parameter!='column_name',column_name!='delete')%>%select(DATA_COLUMN)%>%distinct()%>% unlist(use.names = FALSE)
deletes<-filter(struc,parameter=='delete')%>%select(DATA_COLUMN)%>% unlist(use.names = FALSE)

#gather 
wc<-gather(a,DATA_COLUMN,value,one_of(gathers))

#deconstruct the COLUMN_NAME (parameter)
  #df "struc" has the new row and column names
    #delete unwanted columns
a<-left_join(wc,struc)%>%select(-one_of(deletes))

#create new df based on descontructed column_name
v<-distinct(a,column_name)%>% unlist(use.names = FALSE)
ch<-select(wc,one_of(keeps)) %>% distinct()

for(i in c(1:length(v))){
  w<-filter(a,column_name==v[i])%>%
    select(UID,parameter,value)
  names(w)[3]<-v[i]
  ch<-left_join(ch,w)
}

#output
return(list(ch,struc))
}


q<-formatData('nla2012_waterchem_wide.csv','waterchem.csv')
waterchem<-q[[1]]
struc_waterchem<-q[[2]]



q<-formatData('nla2012_chla_wide.csv','chla.csv')
chla_new<-q[[1]]
struc_chla<-q[[2]]

