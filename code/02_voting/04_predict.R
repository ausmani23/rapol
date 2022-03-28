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

#########################################################
#########################################################

#PREDICT DIRECTION OF PUNITIVE VOTING
#we have a handful of hand-coded punitive votes
#we want to use this to determine punitive votes
#in all law and order votes ever conducted in the house

#this is a prediction problem
#we will use DW NOMINATE probability information
#as well as information about the party of president at the time
#to guess which direction is punitive and which direction is not

#########################################################
#########################################################

setwd(filesdir); dir()
mvotesdf<-fread(
  '02_voting_fulldf.csv'
)

#code cast codes
mvotesdf$vote<-NA #voted no
tmp<-mvotesdf$cast_code%in%c(
  1,2,3
)
mvotesdf$vote[tmp]<-1 #voted yes
tmp<-mvotesdf$cast_code%in%c(
  4,5,6, #negative
  7,8,9 #present or absetntion
)
mvotesdf$vote[tmp]<-0

tmp<-mvotesdf$punitive=="YES"
mvotesdf$punitive[tmp]<-1
tmp<-mvotesdf$punitive=="NO"
mvotesdf$punitive[tmp]<-0
mvotesdf$punitive<-as.numeric(mvotesdf$punitive)

#########################################################
#########################################################

#METHOD 1

#let's use data we have to build prediction
mvotesdf

#for each vote we coded, 
#we want the nominate scores
#in each dimension
#of those who voted yes
#and of those who voted otherwise
avgsdf<-mvotesdf[
  !is.na(punitive) & #bills we've hand-coded 
    !is.na(vote)
  ,
  .(
    punitive=unique(punitive),
    nominate_dim1_avg=mean(nominate_dim1),
    nominate_dim2_avg=mean(nominate_dim2)
  )
  ,
  by=c("congress_rollnumber","vote")
  ]
avgsdf<-avgsdf[order(avgsdf$congress_rollnumber)]

#votes of 1 on a punitive bill should be punitive
#votes of 0 on not punitive bill are also punitive
avgsdf$class<-0
tmp<-(avgsdf$punitive==1 & 
        avgsdf$vote==1) | 
  (avgsdf$punitive==0 & 
     avgsdf$vote==0)
avgsdf$class[tmp]<-1

avgsdf

#we want to reshape this, 
#so that each row is a vote
avgsdf<-gather(
  avgsdf,
  var,
  val,
  nominate_dim1_avg:nominate_dim2_avg
)
avgsdf$var<-paste0(
  avgsdf$var,
  "_",
  avgsdf$vote
)
avgsdf$class<-avgsdf$vote<-NULL
avgsdf<-spread(
  avgsdf,
  var,
  val
)

#########################################################
#########################################################

#using information about the avg nominate scores
#in each dimension, of those voting punitive
#and those voting not punitive

#we wil see how we do predicting others
#let's do this N times, and see which model is best
#by looking at the average accuracy across splits

set.seed(23)
tmpdf<-lapply(1:10,function(i) {
  #i<-1
  ind = sample(2, nrow(avgsdf), replace=TRUE, prob=c(0.7,0.3))
  trainData = avgsdf[ind==1,]
  testData = avgsdf[ind==2,]
  trainData$congress_rollnumber<-NULL
  
  #train a simple logistic regression
  m.logit<-glm(
    data=trainData,
    formula=punitive ~ .,
    family='binomial'
  )
  predictions<-predict(
    m.logit,
    newdata=testData,
    type='response'
  )
  tab_logit_20<-table(testData$punitive,as.numeric(predictions>0.2))
  tab_logit_50<-table(testData$punitive,as.numeric(predictions>0.5))
  tab_logit_80<-table(testData$punitive,as.numeric(predictions>0.8))
  
  #train a simple decision tree
  require(rpart)
  require(rpart.plot)
  m.rpart<-rpart(
    data=trainData,
    punitive ~ .,
    method = 'class'
  )
  #rpart.plot(m.rpart, extra = 106)
  predict_punitive<-predict(m.rpart,testData,type = 'class')
  tab_rpart<-table(testData$punitive,predict_punitive)
  
  #estimate accuracy of the various measures
  tab_list<-list(
    logit_20=tab_logit_20,
    logit_50=tab_logit_50,
    logit_80=tab_logit_80,
    tab_rpart=tab_rpart
  )
  sapply(tab_list,function(tab) {
    sum(diag(tab)) / sum(tab)
  }) %>% t %>% as.data.frame
  
}) %>% rbind.fill

lapply(tmpdf,mean)
lapply(tmpdf,mean)
#on average, logit 50 seems pretty good
#so we proceed w/ the simplest approach 

#given the lack of data, 
#our pref model will be a logit model fit to all the data
regdf<-avgsdf
regdf$congress_rollnumber<-NULL
m.logit<-glm(
  data=regdf,
  formula=punitive ~ .,
  family='binomial'
)
summary(m.logit)

#########################################################
#########################################################

#classify everything using a simple logit
classdf<-mvotesdf[
  is.na(punitive) & #bills we haven't hand coded
    !is.na(vote)
  ,
  .(
    #na.rm necessary b/c some members are missing scores
    nominate_dim1_avg=mean(nominate_dim1,na.rm=T),
    nominate_dim2_avg=mean(nominate_dim2,na.rm=T)
  )
  ,
  by=c("congress_rollnumber","vote")
  ]
classdf<-gather(
  classdf,
  var,
  val,
  nominate_dim1_avg:nominate_dim2_avg
)
classdf$var<-paste0(
  classdf$var,
  "_",
  classdf$vote
)
classdf$vote<-NULL
classdf<-spread(
  classdf,
  var,
  val
)

#classify!
predict_punitive_classdf<-predict(m.logit,newdata=classdf,type='response')
classdf$punitive<-as.numeric(predict_punitive_classdf>0.5)
table(classdf$punitive,useNA='a') 

#merge this back in
mvotesdf<-merge(
  mvotesdf,
  classdf[,c("congress_rollnumber","punitive")],
  by='congress_rollnumber',
  all=T
)

#we now need to indicate whether our punitive vote
#is hand-coded, or a guess based on dw-nominate scores
mvotesdf$punitive<-mvotesdf$handcoded<-NA
tmp<-!is.na(mvotesdf$punitive.x)
mvotesdf$punitive[tmp]<-mvotesdf$punitive.x[tmp]
mvotesdf$handcoded[tmp]<-T
tmp<-!is.na(mvotesdf$punitive.y)
mvotesdf$punitive[tmp]<-mvotesdf$punitive.y[tmp]
mvotesdf$handcoded[tmp]<-F
table(mvotesdf$punitive,useNA='a')
table(mvotesdf$handcoded)
mvotesdf$punitive.x<-mvotesdf$punitive.y<-NULL

#########################################################
#########################################################

#BASIC VARS OF INTEREST

#get the party
mvotesdf$party<-"Other"
tmp<-mvotesdf$party_code==100
mvotesdf$party[tmp]<-"Democrats"
table(mvotesdf$party_code)
tmp<-mvotesdf$party_code==200
mvotesdf$party[tmp]<-"Republicans"

#get the year
mvotesdf$year<-year(mvotesdf$date)

#get groups
mvotesdf$group<-NA
tmp<-mvotesdf$black==0 & 
  mvotesdf$party=="Republicans"
mvotesdf$group[tmp]<-"Republicans"
tmp<-mvotesdf$black==0 & 
  mvotesdf$party=="Democrats"
mvotesdf$group[tmp]<-"Democrats"
tmp<-mvotesdf$black==1
mvotesdf$group[tmp]<-"CBC"
table(mvotesdf$group)

#classify votes
mvotesdf$punitive_vote<-0
tmp<-(mvotesdf$punitive==1 &
  mvotesdf$vote==1) |
  (mvotesdf$punitive==0 & 
     mvotesdf$vote==0)
mvotesdf$punitive_vote[tmp]<-1

#########################################################
#########################################################

#save out
setwd(filesdir); dir()
write.csv(
  mvotesdf,
  "02_voting_fulldf_classified.csv",
  row.names=F
)
