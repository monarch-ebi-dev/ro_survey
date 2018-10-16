library(data.table)
library(stringr)
library(plyr)
library(DT)

datadir<-"/Volumes/EBI/ro_survey_data/"
ontologypath<-"/Volumes/EBI/tmp/ro/ontologies/"
rodirpath<-"/Volumes/EBI/tmp/ro/"
ws = "/ws/ro_survey/shiny/ro_survey/rosurvey.RData"

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

pc<-function(x,y) {
  return(round(100*(x/y),2))
}

df_domainrangedata<-get_csv(datadir,"domainrangedata")
df_axiomdata<-get_csv(datadir,"axiomdata")
df_expressiondata<-get_csv(datadir,"expressiondata")
df_property<-get_csv(datadir,"propertydata")
df_property[,p_label:=NULL]
df_label<-get_csv(datadir,"labels")
df_corpus<-get_csv(datadir,"corpus")
df_merge<-get_csv(datadir,"merge")

df_label[,o:=gsub(rodirpath,"",filename)]
df_ro<-df_label[o=="ro.owl"&type=="ObjectProperty"&label!="topObjectProperty",.(entity,label)]

df_merge[,merged_success:=merged_success==1]
df_merge[,remove_success:=remove_success==1]
df_corpus<-merge(df_corpus,df_merge[,.(filepath,merged_success,remove_success)],by=c("filepath"),all.x = TRUE)
ct_download_sucess<-count(df_corpus[,.(corpus,download_success,merged_success)])

# Prepare property data
df_p<-unique(df_property[,.(o,ftype,corpus,p)])
df_p<-merge(df_p,df_ro,by.x = "p",by.y = "entity",all.x = TRUE)
nrow(df_p)
df_p<-merge(df_p,ct_download_sucess[ct_download_sucess$merged_success,c("corpus","merged_success","freq")],by="corpus",all.x = TRUE)
nrow(df_p)
df_p[,corpussize:=freq]
df_p[,freq:=NULL]
ct_p<-count(df_p[,.(ftype,corpus,p,label,corpussize)])
ct_p$pc<-pc(ct_p$freq,ct_p$corpussize)
#ct_p<-merge(ct_p,df_ro,by.x = "p",by.y = "entity",all = TRUE) //every relation is used at least once, else this would be necessary.
ct_p<-ct_p[order(-ct_p$freq),]

ct_o<-count(df_p[,.(ftype,corpus,o)])
ct_o<-ct_o[order(-ct_o$freq),]
df_domain_range_asserted<-unique(df_property[domain!=""|range!="",.(ftype,corpus,p,domain,range)])
df_po<-unique(df_p[,.(ftype,corpus,corpussize,o)])
ct_ro<-count(df_po[,.(ftype,corpus,corpussize)])
ct_ro$pc<-pc(ct_ro$freq,ct_ro$corpussize)

# Prepare axiom data
df_ax<-unique(df_axiomdata[axiomtype!="Declaration",.(o,ftype,corpus,axiomtype,axiomid,p)])
df_ax<-merge(df_ax,df_ro,by.x = "p",by.y = "entity",all.x = TRUE)
ct_axt<-count(df_ax[,.(ftype,corpus,p,axiomtype,label)])
ct_axt<-ct_axt[order(-ct_axt$freq),]
df_axo<-unique(df_axiomdata[axiomtype!="Declaration",.(o,ftype,corpus,axiomtype,p)])
ct_axto<-count(df_axo[,.(ftype,corpus,p,axiomtype)])
ct_axto<-merge(ct_axto,df_ro,by.x = "p",by.y = "entity",all.x = TRUE)
ct_axto<-ct_axto[order(-ct_axto$freq),]

ct_ax<-count(df_ax[,.(ftype,corpus,p,label)])
names(ct_ax)<-c("ftype","corpus","p","label","axioms")
ct_p<-merge(ct_p,ct_ax,by=c("ftype","corpus","p","label"),all = TRUE)

# Prepare expressiondata
df_exp<-unique(df_expressiondata[,.(o,ftype,corpus,expressiontype,axiomid,p)])
ct_exp<-count(df_exp[,.(ftype,corpus,p,expressiontype)])
ct_exp<-ct_exp[order(-ct_exp$freq),]
df_expo<-unique(df_expressiondata[,.(o,ftype,corpus,expressiontype,p)])
ct_expo<-count(df_expo[,.(ftype,corpus,p,expressiontype)])
ct_expo<-ct_expo[order(-ct_expo$freq),]

# Prepare domain and range analysis
df_dom<-unique(df_domainrangedata[,.(o,ftype,corpus,subject,p,expressiontype)])
ct_dom<-count(df_dom[,.(ftype,corpus,p,subject,expressiontype)])
ct_dom<-ct_dom[order(-ct_dom$freq),]
df_ran<-unique(df_domainrangedata[,.(o,ftype,corpus,object,p,expressiontype)])
ct_ran<-count(df_ran[,.(ftype,corpus,p,object,expressiontype)])
ct_ran<-ct_ran[order(-ct_ran$freq),]
df_domainrangedata<-NULL
df_axiomdata<-NULL
df_expressiondata<-NULL
df_property<-NULL
save.image(file=ws)
