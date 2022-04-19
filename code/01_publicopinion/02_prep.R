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

#LOAD

#load all the polls
setwd(filesdir); dir()
load("01po_grouped.RDS")

#put in a single df and order
fulldf<-rbind.fill(allpolls)
fulldf<-fulldf[order(fulldf$id),]

##########################################################
##########################################################

#PREP IV'S

###respondent id
#each row is a respondent,
fulldf$respid<-1:nrow(fulldf)

###gender 
###(1=male, 2=female)
femaleinfo<-!is.na(fulldf$female)
fulldf$gender<-NA
fulldf$gender[fulldf$female==0 & femaleinfo]<-1
fulldf$gender[fulldf$female==1 & femaleinfo]<-2
tableNA(fulldf$gender)

###race 
###(1=white,2=black,3=other)
raceinfo<-!is.na(fulldf$white) & 
  !is.na(fulldf$black) 
names(fulldf)[1:20]
#if no race info, make NA
fulldf$race<-NA
fulldf$race[raceinfo]<-3 #wherever raceinfo, make other as default
fulldf$race[fulldf$white & raceinfo]<-1 #white
fulldf$race[fulldf$black & raceinfo]<-2 #black
tableNA(fulldf$race)
#add race2 (pace Sheridan and Gelman (2015))
fulldf$race2<-fulldf$race
fulldf$race2[fulldf$race==3]<-1 #non-black

###education 
###(1=HS drop, 2=HS grad, 3=some college, 4=college grad)
fulldf$ed<-fulldf$ed_f #no changes necessary
#age (1=<=29, 2=30-44, 3=45-65, 4=65+)
fulldf$age.old<-fulldf$age
ageinfo<-!is.na(fulldf$age)
fulldf$age<-NA
fulldf$age[fulldf$age.old<=29 & ageinfo]<-1 
fulldf$age[fulldf$age.old>=30 & fulldf$age.old<=44 & ageinfo]<-2 
fulldf$age[fulldf$age.old>=45 & fulldf$age.old<=64 & ageinfo]<-3
fulldf$age[fulldf$age.old>=65 & ageinfo]<-4
tableNA(fulldf$age)

###cohort 
quantile(
  fulldf$birthyear,
  c(0,0.25,0.5,0.75,1),
  na.rm=T
)
#roughly by quantile/meaningful dates
#(1=<=1925, 2=1926-1945, 3=1945-1964, 4=1965+)
cohortinfo<-!is.na(fulldf$birthyear)
fulldf$cohort<-NA
fulldf$cohort[fulldf$birthyear<=1925 & 
                cohortinfo]<-1 
fulldf$cohort[fulldf$birthyear>=1926 & 
                fulldf$birthyear<=1944 & 
                cohortinfo]<-2 
fulldf$cohort[fulldf$birthyear>=1945 & 
                fulldf$birthyear<=1964 & 
                cohortinfo]<-3
fulldf$cohort[fulldf$birthyear>=1965 & 
                cohortinfo]<-4
tableNA(fulldf$cohort) #not exactly as well balanced, but this is cost of getting mass inc gen.

###poll-level
###year
tableNA(fulldf$year)
###poll
tableNA(fulldf$id)
fulldf$pollid<-fulldf$id

###state-level
###numeric code in order of respective statea2
tableNA(fulldf$state_alpha2) #only 35 people in Alaska!
a2levels<-
  names(table(fulldf$state_alpha2)) %>% 
  sort
fulldf$state_alpha2<-factor(
  fulldf$state_alpha2,
  levels=a2levels
)
tableNA(fulldf$state_alpha2)
# geta2.mrp<-function(a2) {
#   which(a2levels==a2)
# }
# geta2.mrp("MI") #quick function to retrieve numeric coding

###region 
###(1=NE, 2=MW, 3=South, 4=West)
tableNA(fulldf$region.num)
tableNA(fulldf$region)
fulldf$region<-fulldf$region.num
#where state_alpha2, present
fulldf$regionDC<-fulldf$region
fulldf$regionDC[fulldf$state_alpha2=="DC"]<-5 #extra region
fulldf$regionDC[is.na(fulldf$state_alpha2)]<-NA

###division (1-9)
tableNA(fulldf$division)
fulldf$division<-fulldf$division.num
#this only useful where state_alpha2
fulldf$divisionDC<-fulldf$division
fulldf$divisionDC[fulldf$state_alpha2=="DC"]<-10 #extra division
fulldf$divisionDC[is.na(fulldf$state_alpha2)]<-NA

vars<-c(
  "age",
  "gender",
  "year",
  "region",
  "division",
  "state_alpha2"
)
lapply(vars,function(x) class(fulldf[[x]]))

###dem-based interactions
tmpdf<-expand.grid(
  v1=c(
    "race",
    "race2"
  ),
  v2=c(
    "age",
    "gender",
    "year",
    "region",
    "division",
    "state_alpha2",
    "ed"
  ),
  stringsAsFactors=F
)
tmpseq.i<-1:nrow(tmpdf)
for(i in tmpseq.i) {
  print(i)
  #i<-1
  thisrow<-tmpdf[i,]
  newname<-paste0(
    thisrow$v1,
    "X",
    thisrow$v2
  )
  #make these all distinct categories
  fulldf[[newname]]<-paste0(
    fulldf[[thisrow$v1]],
    "_",
    fulldf[[thisrow$v2]]
  )
  #should be NA if any of the constituent variables are NA
  tmp<-is.na(fulldf[[thisrow$v1]]) | is.na(fulldf[[thisrow$v2]])
  fulldf[[newname]][tmp]<-NA
}

##########################################################
##########################################################

#PREP DV'S/RESHAPE DATA

#flip the dataset
#goal is respondent ID-question dataset
#which I will classify by dimension

#we will need data.table
#since this is going to be a very large dataset
fulldf<-as.data.table(fulldf)

#ignore all respondent id attributes for now
#melt using the question vars and resp-id
idvars<-c("respid") #id
tmpregex<-"\\_(n|p|np|a|na|m|nm)(t)?$"
qvars<-names(fulldf)[str_detect(names(fulldf),tmpregex)]
variable.name<-"qgroup"
value.name<-"response"
tmpdf<-melt(
  fulldf,
  id.vars=idvars,
  measure.vars=qvars
)

#trim this dataframe
#only need responses
#anything that is NA
tmpdf<-tmpdf[!is.na(value)]
#also don't need 't' vars,
#these can be generated from other info
tmpdf<-tmpdf[!str_detect(variable,"\\_(pt|mt|at)$")]

#get question/type
tmpdf$question<-str_replace(
  tmpdf$variable,
  "^(.*)\\_.*$",
  "\\1"
)
tmpdf$type<-str_replace(
  tmpdf$variable,
  "^.*\\_(.*)$",
  "\\1"
)
tmpdf$variable<-NULL #not needed

#get dimension
tmpdf$dimension<-NA
tmprows<-tmpdf$type%in%c("p","np")
tmpdf$dimension[tmprows]<-"punitive"
tmprows<-tmpdf$type%in%c("a","na")
tmpdf$dimension[tmprows]<-"anxiety"
tmprows<-tmpdf$type%in%c("m","nm")
tmpdf$dimension[tmprows]<-"mistrust"

#order
tmplevels<-c("p","np","a","na","m","nm","n")
tmpdf$type<-factor(
  tmpdf$type,
  levels=tmplevels
)
tmpdf<-tmpdf[order(question,type),]

#hacky way to get dimensions
require(zoo) 
sum(is.na(tmpdf$dimension))
tmpdf$dimension<-na.locf(tmpdf$dimension)
sum(is.na(tmpdf$dimension))
#all assigned correctly
tmp<-tmpdf[,unique(dimension),.(question)]
table(tmp$question)

#now, remove dim info in response
#we have dim info in a grouping var
head(tmpdf)
tmpdf$type %>% unique
tmpdf$type2<-NA
tmprows<-tmpdf$type%in%c("p","m","a")
tmpdf$type2[tmprows]<-"aff"
tmprows<-tmpdf$type%in%c("np","nm","na")
tmpdf$type2[tmprows]<-"neg"
tmprows<-tmpdf$type%in%c("n")
tmpdf$type2[tmprows]<-"neut"
tmpdf$type<-NULL
head(tmpdf)

#now, spread to get 
#respondent.id - question df
finaldf<-spread(
  tmpdf,
  type2,
  value
)

##########################################################
##########################################################

#MERGE
#get respondent info from fulldf
rootvars<-varsdf$oldname
intvars<-names(fulldf)[str_detect(names(fulldf),"X")]
keepvars<-c(rootvars,intvars) %>%
  unique
tmp<-keepvars%in%names(fulldf)
if(sum(!tmp)>0)
  stop()
finaldf<-merge(
  finaldf,
  fulldf[,keepvars,with=F],
  by="respid",
  all.X=T
)


##########################################################
##########################################################

#ADD QUESTION INFO
tmpvars<-c("question","alt","deathpen")
tmpdf<-questionsdf[,tmpvars]
tmpdf$alt<-as.numeric(tmpdf$alt)
tmpdf$deathpen<-as.numeric(tmpdf$deathpen)
finaldf<-merge(
  finaldf,
  tmpdf,
  by="question",
  all.x=T
)
finaldf

#add question interactions
#with question
finaldf$raceXquestion<-paste0(finaldf$race,"_",finaldf$question)
finaldf$race2Xquestion<-paste0(finaldf$race2,"_",finaldf$question)
#q's w/ alterntive
finaldf$raceXalt<-paste0(finaldf$race,"_",finaldf$alt)
finaldf$race2Xalt<-paste0(finaldf$race2,"_",finaldf$alt)
#q's w/ deathpen
finaldf$raceXdeathpen<-paste0(finaldf$race,"_",finaldf$deathpen)
finaldf$race2Xdeathpen<-paste0(finaldf$race2,"_",finaldf$alt)

#and some extra interactions
#for models w/ xrace interactions
#add a couple more interactions here
finaldf$raceXdivisionXyear<-paste0(
  finaldf$race,
  "_",
  finaldf$division,
  "_",
  finaldf$year
)
finaldf$raceXedXyear<-paste0(
  finaldf$race,
  "_",
  finaldf$ed,
  "_",
  finaldf$year
)

##########################################################
##########################################################

#FINALIZE/SAVE OUT

#how many respondents are in here twice or more
tmptable<-table(finaldf$respid)
100*sum(tmptable==1)/length(tmptable)
100*sum(tmptable==2)/length(tmptable)
100*sum(tmptable==3)/length(tmptable)
100*sum(tmptable==4)/length(tmptable) 
100*sum(tmptable>5)/length(tmptable) 

#how many respondents from original don't give any info?
tmp<-!fulldf$respid%in%finaldf$respid
missing.respondents<-fulldf$respid[tmp] %>%
  unique
length(missing.respondents)

#take a look
tmp<-missing.respondents[1:10]
fulldf[fulldf$respid%in%tmp,]

#from how many different polls?
tmp<-fulldf$respid%in%missing.respondents
polls.wmissing<-fulldf$pollid[tmp]
table(polls.wmissing) %>% sort(decreasing=T)

#these are people who didn't answer/weren't asked
#nothing we can do about them, move on
length(unique(finaldf$respid)) 
#our finaldf is about 275,115 respondents
nrow(finaldf)
#if we ignore repeaters, 485,096 obs

##########################################################
##########################################################

# OUTPUT A DESCRIPTIVE TABLE

require(xtable)
finaldf<-data.table(finaldf)
tabdf <- finaldf[
  ,
  .(
    N=.N,
    white=sum(race==1,na.rm=T),
    black=sum(race==2,na.rm=T),
    other=sum(race==3,na.rm=T),
    range=paste0(min(year),"-",max(year))
  )
  ,
  by=c('question')
]
names(tabdf)<-c('Question ID','Respondents','White','Black','Other','Period')

#how many respondents
nrow(finaldf)
length(unique(finaldf$respid))
length(unique(finaldf$pollid))

tabdf_latex<- xtable(
  tabdf,
  caption='Information about Questions in the Public Opinion Sample',
  type='latex'
)
setwd(outputdir); dir()
print(
  tabdf_latex,
  file='tab_po_questions_EDIT.tex'
)

#also output the full text of these questions
setwd(datadir); dir()
qinfo<-readLines('questions_fortex.txt')
setwd(outputdir); dir()
write(qinfo,'list_po_questions_EDIT.tex')

##########################################################
##########################################################

#save out
setwd(filesdir)
# rm("fulldf") #confusing me
# save.image(file="prepped.RData")
write.csv(
  finaldf,
  '01po_dataframe.csv',
  row.names=F
)
