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

#READ IN DATA

#read in a catalog of votes
setwd(filesdir); dir()
allvotesdf<-fread(
  '02_voting_votesdf.csv'
)
nrow(unique(allvotesdf))==nrow(allvotesdf)

#MEMBER-VOTES
#read in member-vote database
setwd(datadir); dir()
mvotesdf<-fread(
  'Hall_votes.csv'
)
nrow(unique(mvotesdf))==nrow(mvotesdf)
mvotesdf$congress_rollnumber<-paste0(
  mvotesdf$congress,"-",mvotesdf$rollnumber
)

#MEMBER INFO
#read in data
setwd(filesdir); dir()
membersdf<-fread(
  '02_voting_membersdf.csv'
)
nrow(unique(membersdf))==nrow(membersdf)

#########################################################
#########################################################

#PUT IT ALL TOGETHER

#restrict to punitive votes and those in the house
tmp<-(allvotesdf$punitivevote==TRUE & 
  allvotesdf$chamber=="house")
allvotesdf<-allvotesdf[tmp,]
allvotesdf$congress_rollnumber<-paste0(
  allvotesdf$congress,"-",allvotesdf$rollnumber
)

#we only need those votes in mvotes
#which are coded in punvotes as punvotes
tmp<-allvotesdf$congress_rollnumber%in%mvotesdf$congress_rollnumber
if(sum(!tmp)!=0) stop() 
#997 votes; all of them are in
tmp<-mvotesdf$congress_rollnumber%in%allvotesdf$congress_rollnumber
mvotesdf<-mvotesdf[tmp,]
nrow(unique(mvotesdf))==nrow(mvotesdf)

#merge the info we have into mvotes
mergevars<-c(
  "congress_rollnumber",
  "date",
  "punitive", #identifies whether this was hand-coded
  "yea_count",
  "nay_count",
  "nominate_mid_1",
  "nominate_mid_2",
  "nominate_log_likelihood"
)
mvotesdf<-merge(
  mvotesdf,
  allvotesdf[,mergevars,with=F],
  by="congress_rollnumber"
)
nrow(unique(mvotesdf))==nrow(mvotesdf)

#merge member info into mvotes
tmporder<-order(
  membersdf$icpsr,
  membersdf$congress
)
membersdf<-membersdf[tmporder,]
membersdf
mergevars<-c(
  "congress",
  "black",
  "bioname",
  "icpsr",
  "state_abbrev",
  "party_code",
  "nominate_dim1",
  "nominate_dim2"
)
mvotesdf<-merge(
  mvotesdf,
  unique(membersdf[,mergevars,with=F]), #this has some dups
  by=c("congress","icpsr")
)
nrow(unique(mvotesdf))==nrow(mvotesdf)

#########################################################
#########################################################

#classify WoT votes
tmp <- allvotesdf$date>ymd('2001-09-11') & 
  (
    (str_detect(allvotesdf$vote_desc,'terror') & 
       !is.na(allvotesdf$vote_desc))
  ) 
#there are only 8 of these, no way they make a difference

#########################################################
#########################################################

#OUTPUT

#write out
setwd(filesdir); dir()
write.csv(
  mvotesdf,
  "02_voting_fulldf.csv",
  row.names=F
)

#########################################################
#########################################################

# #output a random sample for credibility inspection
# #stratifeid by decade
# #urls only start before 1973, though
# #and are absent for some votes
# allvotesdf$decade<-floor(year(allvotesdf$date)/10) * 10
# 
# #take five from each decade
# set.seed(23)
# decades<-unique(allvotesdf$decade)
# sampledf<-lapply(decades,function(thisdecade) {
#   #thisdecade<-decades[1]
#   tmp<-allvotesdf$decade==thisdecade &
#     is.na(allvotesdf$punitive) #one that we haven't coded
#   sample_n(allvotesdf[tmp,],ifelse(sum(tmp)>10,10,sum(tmp)))
# }) %>% rbind.fill
# 
# #keep only the cols w/ info
# sampledf<-sampledf[,sapply(sampledf,function(x) sum(!is.na(x)))>0]
# setwd(outputdir)
# write.csv(
#   sampledf,
#   'mvotesdf_sample.csv'
# )
