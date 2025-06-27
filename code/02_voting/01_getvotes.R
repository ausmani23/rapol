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

#LOAD HAND-CODED VOTES
#source: our own hand-coding of punitive votes

setwd(datadir); dir()
votesdf<-read.csv(
  'housevotes_handcoded_revised.csv',
  stringsAsFactors=F
)

votesdf$housevote<-tolower(votesdf$housevote)

votesdf$billid<-paste(
  votesdf$session,
  votesdf$year,
  votesdf$housevote,
  sep="_"
)

#these are dates w/ dem pre
keydates<-c(
  #dem
  "11/22/1963",
  "01/20/1969",
  #dem
  "01/21/1977",
  "01/20/1981",
  #dem
  "01/21/1993",
  "01/20/2001",
  #dem
  "01/21/2009",
  "01/20/2017"
)
datesdf<-matrix(
  keydates,
  ncol=2,
  byrow=T
) %>% as.data.frame
names(datesdf)<-c(
  "stdate",
  "enddate"
)
datesdf$stdate<-base::as.Date(
  datesdf$stdate,
  format="%m/%d/%Y"
)
datesdf$enddate<-base::as.Date(
  datesdf$enddate,
  format="%m/%d/%Y"
)
#classify
#get all dates
demprez.dates<-lapply(1:nrow(datesdf),function(i) {
  thisrow<-datesdf[i,]
  thisrow$stdate:thisrow$enddate
}) %>% unlist

#add this var
votesdf$demprez<-0
votesdf$date<-base::as.Date(
  votesdf$date,
  format="%m/%d/%Y"
)
tmp<-votesdf$date%in%demprez.dates
votesdf$demprez[tmp]<-1

#add date
votesdf$date<-lubridate::ymd(votesdf$date)

#congress will denote which session of congress
#this was called session in votesdf
votesdf$congress<-votesdf$session
votesdf$session<-NULL

#COMMENTED OUT FOR REPLICATION
#(no need to upload all these .csv's to dataverse)
# #we need bill number
# #this we can get from the topline 
# #of the congress votes data that we downloaded
# tmpdir<-file.path(
#   datadir,
#   "votes"
# ); setwd(tmpdir)
# tmpseq.i<-seq_along(dir())
# metadf<-lapply(tmpseq.i,function(i) {
#   x<-dir()[i]
#   tmp<-readLines(
#     x,n=1
#   )
#   bill_name<-str_extract(tmp,"(H\\.\\sR\\.|H\\.Res\\.|H\\.R\\.|H\\sR|HR|S\\.)\\s[0-9]+") %>%
#     tolower
#   data.frame(
#     filename=dir()[i],
#     bill_name,
#     stringsAsFactors=F
#   )
# }) %>% rbind.fill
# setwd(datadir); fwrite(metadf,'02_handcodedmeta.csv')
setwd(datadir); metadf<-fread('02_handcodedmeta.csv')

#extract what you need to merge votesdf
metadf$billid<-str_extract(
  metadf$filename,
  "[0-9]{2,3}\\-[0-9]{4}\\_h[0-9]{2,3}"
) %>% str_replace("\\-","_")

#we want bill number
#and bill prefix
metadf$bill_number<-str_extract(
  metadf$bill_name,"[0-9]+"
)
metadf$bill_type<-str_extract(
  metadf$bill_name,"[a-z\\s\\.]+"
) %>% str_replace_all("\\s","") %>%
  str_replace_all("\\.","")

#put this back into votesdf
votesdf<-merge(
  votesdf,
  metadf, 
  by='billid',
  all.x=T
)

#########################################################
#########################################################

#load all votes from JSON file
#these are from https://voteview.com
setwd(datadir)
allvotesdf<-fread(
  "HSall_rollcalls_withissues.csv",
  stringsAsFactors=F
)

#give each row a rowid
#this will be useful when we have to discover
#information about bills in pundf for manaul match
allvotesdf$rowid<-1:nrow(allvotesdf)

#renames,lowercasing
allvotesdf$chamber<-tolower(allvotesdf$chamber)

#fix bill number/prefix (no bill numbers for congress 99 and 100)
allvotesdf$bill_prefix<-str_extract(allvotesdf$bill_number,"[A-z]+")
allvotesdf$bill_prefix<-tolower(allvotesdf$bill_prefix)
allvotesdf$bill_number<-str_extract(allvotesdf$bill_number,"[0-9]+")

#clean up bill prefix and chamber in allvotesdf, call it bill_type
#there are many different prefixes for the same thing
allvotesdf$bill_type<-NA
tmptab<-table(allvotesdf$bill_prefix) %>% sort(decreasing=T)

#house bills
tmptype<-c("hr","h","hhr")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-"hr"

#senate bills
tmptype<-c("s")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-"s"

#house resolutions
tmptype<-c("hres","hre")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-"hres"

#senate resolutions
tmptype<-c("sres","sre","sr")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-"sres"

#house concurrent resolutions
tmptype<-c("hconres","hcr","hconr","hcon","hcre","hcres")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-"hcon"

#senate concurrent resolutions
tmptype<-c("sconres","scr","scre","sconr","scres","scon")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-"scon"

#house joint resolutions
tmptype<-c("hjr","hjres","hjre","hj","hrj")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-"hjres"

#senate joint resolutions
tmptype<-c("sjres","sjre","sjr")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-"sjres"

#unknown codes
tmptype<-c("pn","ht","treatydoc")
tmp<-allvotesdf$bill_prefix%in%tmptype & 
  !is.na(allvotesdf$bill_prefix)
allvotesdf$bill_type[tmp]<-NA

#take a look at bill_type
tmptab<-table(allvotesdf$bill_type,useNA = 'a')
tmptab/sum(tmptab) #about 30% are NA's.. 

#########################################################
#########################################################

#MERGE votesdf W/ allvotesdf
#all votes we hand-coded in votesdf
#should be in allvotesdf

#we loop through every vote in allvotesdf
#and find its row in votesdf
#we do this automatically for most, 
#but some we have to match manually

allvotesdf$bill_id<-paste0(
  allvotesdf$congress,"-",
  allvotesdf$bill_type,"-",
  allvotesdf$bill_number
)
votesdf$bill_id<-paste0(
  votesdf$congress,"-",
  votesdf$bill_type,"-",
  votesdf$bill_number
)
votesdf$congress_rollnumber<-paste0(
  votesdf$congress,"-",
  str_replace(votesdf$housevote,"h","")
)

#we will use rollnumber where clerk_rollnumber doesn't exist
allvotesdf$congress[!is.na(allvotesdf$rollnumber)] %>% unique
allvotesdf$date[!is.na(allvotesdf$clerk_rollnumber)] %>% range
allvotesdf$congress_rollnumber<-paste0(
  allvotesdf$congress,"-",
  allvotesdf$rollnumber
)
tmp<-!is.na(allvotesdf$clerk_rollnumber)
allvotesdf$congress_rollnumber[tmp]<-paste0(
  allvotesdf$congress[tmp],"-",
  allvotesdf$clerk_rollnumber[tmp]
)

tmpseq.i<-1:nrow(votesdf)
votesdf$rowid<-sapply(tmpseq.i,function(i) {
  #i<-42
  print(i)
  
  thisrow<-votesdf[i,]
  
  #try to match by congress and rollnumber and date
  tmp<-allvotesdf$congress_rollnumber==thisrow$congress_rollnumber &
    allvotesdf$chamber=="house"
  allvotesdf[tmp,]
  thisrow
  
  y<-NA
  
  #some manual matches here
  if(thisrow$billid=='101_1989_h336') y<-78076
  if(thisrow$billid=='91_1970_h367') y<-58873
  
  #auto matches if no manual match
  if(is.na(y)) {
    if(sum(tmp)==1) {
      #dates should match
      if(allvotesdf$date[tmp]==thisrow$date) {
        y<-allvotesdf$rowid[tmp]
      } else {
        #this is wrong, and we have to match by billid
        tmp2<-allvotesdf$bill_id==thisrow$bill_id & 
          allvotesdf$date==thisrow$date
        if(sum(tmp2)==1) {
          y<-allvotesdf$rowid[tmp2]
        } else {
          stop(print(i))
        }
      }
    } else if(sum(tmp)>1) {
      
      tmp3<-tmp & allvotesdf$bill_id==thisrow$bill_id
      if(sum(tmp3)==1) {
        y<-allvotesdf$rowid[tmp3]
      } else {
        stop(print(i))
      }
      
    } else {
      stop(print(i))
    }
  }
  y
})

#remove superfluous vars from votesdf
votesdf$bill_id<-
  votesdf$bill_number<-
  votesdf$congress_rollnumber<-
  votesdf$bill_type<-
  votesdf$date<-
  votesdf$congress<-NULL

#merge!
fulldf<-merge(
  votesdf,
  allvotesdf,
  by='rowid',
  all=T
)

#inspection reveals this worked well

#########################################################
#########################################################

#GENERATE FULL SAMPLE

#we have a sample of hand-coded major law and order votes
#we now use these hand-coded law and order votes
#to infer the larger set of punitive votes that we didn't hand code
#and, later, what constitutes a 'punitive' vote (i.e., yes or no)

#these are our hand-coded bills
tmp<-!is.na(fulldf$billtitle)
sum(tmp)==nrow(votesdf) #everything matched

#these are the different issue-coding variables
names(fulldf)
tmpvars<-c(
  "issue_codes",
  "peltzman_codes",
  "clausen_codes",
  "crs_policy_area"
)

table(fulldf$crs_policy_area[tmp]) %>% sort #mostly crime and law enforcement
table(fulldf$issue_codes[tmp]) %>% sort #judiciary and narcotics
table(fulldf$peltzman_codes[tmp]) %>% sort #domestic social policy/regulation
table(fulldf$clausen_codes[tmp]) %>% sort #civil liberties

#peltzman and clausen seem too general
#instead, we will look at crime/law enforcement
#and complement this w/ judiciary and narcotics

fulldf$crs_policy_area %>% unique
fulldf$issue_codes %>% unique
fulldf$clausen_codes %>% unique

#this is all relevant bills in the sample
tmp<-!is.na(fulldf$crs_policy_area) & 
  fulldf$crs_policy_area=="Crime and Law Enforcement" 
tmp<-tmp | (
  !is.na(fulldf$issue_codes) & 
    fulldf$issue_codes%in%c("Judiciary","Narcotics")
)
tmp<-tmp | (
  !is.na(fulldf$punitive)
)
sum(tmp) 
fulldf$punitivevote<-tmp

#########################################################
#########################################################

#limit the data to the postwar period
tmp<-year(fulldf$date)>=1945
fulldf<-fulldf[tmp,]

#########################################################
#########################################################

#can we identify a woT sample
tmp<-year(fulldf$date)>2001 & 
  fulldf$punitivevote
sum(fulldf$punitivevote)

#########################################################
#########################################################

#save out
setwd(filesdir); dir()
write.csv(
  fulldf,
  "02_voting_votesdf.csv",
  row.names=F
)
