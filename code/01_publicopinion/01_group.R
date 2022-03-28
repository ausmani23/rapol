#########################################################
#########################################################

#clear workspace
rm(list=ls())

#load packages
require(stringr)
require(plyr)
require(dplyr)
require(zoo)
require(data.table)
require(tidyr)
require(rprojroot)

#set dirs
rootdir<-find_root(
  criterion=has_file('_rapol.Rproj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#helper functions
setwd(pcodedir); dir()
source('readpoll_functions.R')
source('getinfo2.R')

#helper function for reading
readme.csv<-function(x) {
  df<-read.csv(x,stringsAsFactors=F)
  df$X<-NULL
  return(df)
}

############################################################# 
############################################################# 

#load all the df's
setwd(datadir)
abcdfs<-readme.csv('abcdfs.csv')
anesdfs<-readme.csv('anesdf.csv')
cbsdfs<-readme.csv('cbsdfs.csv')
gallupdfs<-readme.csv('gallupdfs.csv')
gssdfs<-readme.csv('gssdf.csv')
nbclatdfs<-readme.csv('nbclatdfs.csv')
roperdfs<-readme.csv('roperdfs.csv')
timedfs<-readme.csv('timedfs.csv')
allpolls<-list(
  abcdfs,
  anesdfs,
  cbsdfs,
  gallupdfs,
  gssdfs,
  nbclatdfs,
  roperdfs,
  timedfs
)
names(allpolls)<-c(
  "ABC",
  "ANES",
  "CBS",
  "Gallup",
  "GSS",
  "NBCLAT",
  "Roper",
  "Time"
)

############################################################# 
############################################################# 

#ERROR-CHECKING

############################################################# 
############################################################# 

#checking whether all vars have _n, _np, and _n variants
# questions<-allcodes$name 
# allnames<-sapply(allpolls,names) %>% 
#   unlist %>% 
#   unname %>% 
#   unique
# tmp<-lapply(seq_along(questions),function(k) {
#   #k<-1
#   pro<-questions[k]
#   con<-getnegative(pro)
#   neutral<-getneutral(pro)
#   tmpchecker<-c(pro,con,neutral)
#   present<-sapply(tmpchecker,function(x) x%in%allnames) %>%
#     sum
#   return(present)
# }); names(tmp)<-questions

# if(sum(tmp<3)>0) 
#   stop("Not all variants present.")

#check NA occurrence of all key variables
blackNAs<-lapply(allpolls,function(poll) {
  #poll<-abcdfs
  tapply(poll$black,poll$id,function(x) {
    100*sum(is.na(x))/length(x)
  })
}) %>% unlist %>% sort
blackNAs

femaleNAs<-lapply(allpolls,function(poll) {
  #poll<-abcdfs
  tapply(poll$female,poll$id,function(x) {
    100*sum(is.na(x))/length(x)
  })
}) %>% unlist %>% sort
femaleNAs

edNAs<-lapply(allpolls,function(poll) {
  #poll<-abcdfs
  tapply(poll$ed_f,poll$id,function(x) {
    100*sum(is.na(x))/length(x)
  })
}) %>% unlist %>% sort
edNAs

ageNAs<-lapply(allpolls,function(poll) {
  #poll<-abcdfs
  tapply(poll$age,poll$id,function(x) {
    100*sum(is.na(x))/length(x)
  })
}) %>% unlist %>% sort
ageNAs

############################################################# 
############################################################# 

#does each polldf have the same set of IV's?
ivs<-lapply(allpolls,function(poll) {
  #poll<-abcdfs
  allnames<-names(poll)
  tmpregex<-"\\_(n|p|np|a|na|m|nm)(t)?$"
  tmpselector<-!str_detect(allnames,tmpregex)
  allivs<-allnames[tmpselector]
  return(allivs)
}); names(ivs)<-names(allpolls)

#take a look at those ivs which are not in all df's
tmp<-sort(table(ivs %>% 
                  unlist),
          decreasing=T)
probs<-tmp[tmp<8] %>% 
  names #these vars don't appear in all df's

#which dfs do they not appear in?
ivs.missing<-lapply(probs,function(prob) {
  #prob<-"female"
  present<-sapply(allpolls,function(df) {
    prob%in%names(df)
  })
  missing<-names(allpolls)[!present]
  return(missing)
})
names(ivs.missing)<-probs
#story is that we don't have state info for GSS and NBCLAT
#(and also for a subset of the data in other polls)

############################################################# 
############################################################# 

#now, check the distribution and type of each IV
#for each, I'll have different concerns
#"birthyear","black","age","female","ed_f"

iv<-"age"
lapply(allpolls,function(poll) {
  #poll<-abcdfs  
  tapply(poll[[iv]],poll$id,function(x) {
    !(class(x)=="integer" | class(x)=="numeric")
  }) %>% sum
}) #always integer or numeric

lapply(allpolls,function(poll) {
  poll[[iv]][poll[[iv]]<18 & !is.na(poll[[iv]])] %>% table
}) #just in the ANEs

iv<-"birthyear"
lapply(allpolls,function(poll) {
  #poll<-abcdfs  
  tapply(poll[[iv]],poll$id,function(x) {
    !(class(x)=="integer" | class(x)=="numeric")
  }) %>% sum
}) #always integer or numeric

lapply(allpolls,function(poll) {
  range(poll[[iv]],na.rm=T)
}) #believable range

iv<-"black"
lapply(allpolls,function(poll) {
  #poll<-abcdfs  
  tapply(poll[[iv]],poll$id,unique) #in some cases NA's
}) #no zero's anywhere

black.proportion<-lapply(allpolls,function(poll) {
  #poll<-abcdfs  
  tapply(poll[[iv]],poll$id,function(x) {
    mean(x,na.rm=T)  
  }) 
}) %>% 
  unlist %>% 
  sort(decreasing=T)
#we have an all-black sample in 1969, 
#and then a bunch of oversamples of blacks
#ANES and GSS also seem to oversample.. 

iv<-"ed_f"
lapply(allpolls,function(poll) {
  #poll<-abcdfs  
  tapply(poll[[iv]],poll$id,function(x) {
    0%in%unique(x)
  }) %>% sum
}) #no zero's anywhere

iv<-"female"
lapply(allpolls,function(poll) {
  #poll<-abcdfs  
  tapply(poll[[iv]],poll$id,table,useNA="ifany") #in some cases NA's
}) #no zero's anywhere

lapply(allpolls,function(poll) {
  #poll<-abcdfs  
  missing.polls<-tapply(poll[[iv]],poll$id,
                        function(x) sum(!is.na(x))==0) #in some cases NA's
  which(missing.polls)
}) #no zero's anywhere

############################################################# 
############################################################# 

#save all the loaded polls to RData file, 
#which next file will load
setwd(filesdir)
save(
  allpolls,
  file="01po_grouped.RDS"
)

