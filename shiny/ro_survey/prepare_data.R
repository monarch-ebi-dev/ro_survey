library(data.table)
library(stringr)
library(plyr)
library(DT)

datadir<-"/Volumes/EBI/ro_survey_data/"
ontologypath<-"/Volumes/EBI/tmp/ro/ontologies/"
rodirpath<-"/Volumes/EBI/tmp/ro/"
ws = "/ws/ro_survey/shiny/rmd/rosurvey_obo.RData"
p_excluded = c("ro")

get_csv<-function(dir,identifier) {
  temp = paste(dir,list.files(dir,pattern=paste("rosurvey_",identifier,"_.*.csv",sep="")),sep="")
  temp = temp[file.size(temp) > 1]
  
  l <- lapply(temp, fread, sep=",",stringsAsFactors = FALSE,header=TRUE)
  l <- mapply(cbind,l,csv=temp,SIMPLIFY = FALSE)
  df <- rbindlist( l ,use.names = TRUE)
  
  if(identifier!="corpus") {
    if("o" %in% names(df)) {
      df[,fn:=o]
      df[,o:=gsub(".owl","",gsub(ontologypath,"",fn))]
      df[,corpus:=sapply(strsplit(df$o,"_"), `[`, 1)]
      df[,ftype:=sapply(strsplit(df$o,"_"), `[`, 2)]
      df[,o:=str_replace_all(pattern=paste(corpus,"_",sep=""),replacement = "",o)]
      df[,o:=str_replace_all(pattern=paste(ftype,"_",sep=""),replacement = "",o)]
    }
  } else {
    df[,o:=tolower(o)]
  }
  return(df)
}

successfull_collection<-function(dir,identifier) {
  temp = paste(dir,list.files(dir,pattern=paste("rosurvey_",identifier,"_.*.csv",sep="")),sep="")
  f<-gsub(".owl","",gsub(dir,"",temp))
  f<-gsub(paste("rosurvey_",identifier,"_",sep=""),"",f)
  f<-gsub(".*_","",f)
  f<-gsub(".csv","",f)
  return(unique(f))
}

df_domainrangedata<-get_csv(datadir,"domainrangedata")
df_axiomdata<-get_csv(datadir,"axiomdata")
df_allaxiomdata<-get_csv(datadir,"allaxiomdata")
df_expressiondata<-get_csv(datadir,"expressiondata")
df_property<-get_csv(datadir,"propertydata")
df_label<-get_csv(datadir,"labels")
df_corpus<-get_csv(datadir,"corpus")
df_merge<-get_csv(datadir,"merge")


success_o<-successfull_collection(datadir,"axiomdata")
# Prepare success data
df_merge[,merged_success:=merged_success==1]
df_merge[,remove_success:=remove_success==1]

df_corpus<-merge(df_corpus,df_merge[,.(filepath,merged_success,remove_success)],by=c("filepath"),all.x = TRUE)
df_corpus$merged_success<-ifelse(is.na(df_corpus$merged_success),FALSE,df_corpus$merged_success)
df_corpus$remove_success<-ifelse(is.na(df_corpus$remove_success),FALSE,df_corpus$remove_success)
df_corpus$collectdata_success <- ifelse((df_corpus$o %in% success_o),TRUE,FALSE)
df_corpus$excluded <- ifelse((df_corpus$o %in% p_excluded),TRUE,FALSE)
ct_download_sucess<-count(df_corpus[,.(corpus,download_success,merged_success,collectdata_success,excluded)])

valid_ontologies<-(df_corpus[df_corpus$collectdata_success&!df_corpus$excluded,]$o)

df_domainrangedata<-df_domainrangedata[corpus=="obo"&(o %in% valid_ontologies),]
df_allaxiomdata<-df_allaxiomdata[corpus=="obo"&(o %in% valid_ontologies),]
df_axiomdata<-df_axiomdata[corpus=="obo"&(o %in% valid_ontologies),]
df_expressiondata<-df_expressiondata[corpus=="obo"&(o %in% valid_ontologies),]
df_property<-df_property[corpus=="obo"&(o %in% valid_ontologies),]

df_property[,p_label:=NULL]
df_label[,o:=gsub(rodirpath,"",filename)]
df_ro<-df_label[o=="ro.owl"&type=="ObjectProperty"&label!="topObjectProperty",.(entity,label)]



# Prepare property data
df_p<-unique(df_property[,.(o,ftype,corpus,p)])
df_p<-merge(df_p,df_ro,by.x = "p",by.y = "entity",all.x = TRUE)
df_p$corpussize=length(valid_ontologies)



# Propare domain range asserted data
df_domain_range_asserted<-unique(df_property[domain!=""|range!="",.(ftype,corpus,p,domain,range,o)])

# Prepare axiom data
df_ax<-unique(df_axiomdata[axiomtype!="Declaration",.(o,ftype,corpus,axiomtype,axiomid,p,otherp)])
df_ax<-merge(df_ax,df_ro,by.x = "p",by.y = "entity",all.x = TRUE)

# Prepare expressiondata
df_exp<-unique(df_expressiondata[,.(o,ftype,corpus,expressiontype,axiomid,p)])
df_expo<-unique(df_expressiondata[,.(o,ftype,corpus,expressiontype,p)])

# Prepare domain and range analysis
df_dom<-unique(df_domainrangedata[,.(o,ftype,corpus,subject,p,expressiontype)])
df_dom<-df_dom[df_dom$subject!="http://www.w3.org/2002/07/owl#Thing",]

df_ran<-unique(df_domainrangedata[,.(o,ftype,corpus,object,p,expressiontype)])
df_ran<-df_ran[df_ran$object!="http://www.w3.org/2002/07/owl#Thing",]

df_allaxiomdata<-unique(df_allaxiomdata[,.(o,axiomtype,axiomid,ftype,corpus)])
ct_allaxiomdata<-plyr::count(df_allaxiomdata[,.(o,axiomtype,ftype,corpus)])


df_domainrangedata<-NULL
df_axiomdata<-NULL
df_expressiondata<-NULL
df_property<-NULL
df_allaxiomdata<-NULL
save.image(file=ws)
