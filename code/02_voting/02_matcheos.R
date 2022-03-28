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

#HELPER FUNCTION

matchme<-function(vector,string) {
  
  #vector<-ssdf$firstname
  #string<-thisfirstname
  
  ###make both of these lower
  vector<-tolower(vector)
  string<-tolower(string)
  
  ###remove apostrophe's et al
  ###special characters
  vector<-str_replace_all(
    vector,
    "(\\s|\\.|\\-|')",
    ""
  )
  string<-str_replace_all(
    string,
    "(\\s|\\.|\\-|')",
    ""
  )
  
  ####
  match<-str_detect(vector,fixed(string))
  if(sum(match)==0) {
    match<-str_detect(fixed(string),vector)
    if(sum(match)==0)
      match<-rep(F,length(vector))
  }
  return(match)
}

#test
string<-"Hello"; vector<-c("Apple","World","Hello!")
matchme(vector,string)
string<-"Hello!"; vector<-c("Apple","World","Hello")
matchme(vector,string)
string<-"Yahoo!"; vector<-c("Apple","World","Hello")
matchme(vector,string)

#########################################################
#########################################################

#read in data on all members
setwd(datadir); dir()
membersdf<-fread(
  'HSall_members.csv'
)

#names are stored w/ 1, 2 or 3 commas
tmp<-str_count(membersdf$bioname,",")
membersdf[tmp==3,]
membersdf[tmp==2,]
membersdf[tmp==1,]

members_names<-str_split(membersdf$bioname,",")

#lastnames
membersdf$lastname<-
  sapply(members_names,function(x) x[[1]])
#firstnames
membersdf$firstname<-
  sapply(members_names,function(x) x[[2]])
#suffixes
membersdf$suffix<-sapply(members_names,function(x) {
  if(length(x)>=3) {
    y<-x[[3]]
  } else {
    y<-NA
  }
})
#nicknames
membersdf$suffix<-sapply(members_names,function(x) {
  if(length(x)>=4) {
    y<-x[[4]]
  } else {
    y<-NA
  }
})

#########################################################
#########################################################

#IDENTIFY RACE OF EO'S

#these are black congressmen
setwd(datadir); dir()
cbcdf<-read.csv(
  'congressmen_list.csv',
  stringsAsFactors=F
)
names(cbcdf)<-c(
  "congress",
  "raw",
  "state_alpha2",
  "party",
  "chamber"
)

#tirm
tmp<-cbcdf$raw==""
cbcdf<-cbcdf[!tmp,]

#keep raw
cbcdf$originalraw<-cbcdf$raw

#remove all accents
cbcdf$raw<-str_replace_all(
  cbcdf$raw,
  intToUtf8(0xED),
  "i"
) %>% str_replace_all(
  intToUtf8(0xE1),"a"
) %>% str_replace_all(
  intToUtf8(0xF3),"o"
) %>% str_replace_all(
  intToUtf8(0xFA),"u"
) %>% str_replace_all(
  intToUtf8(0xE9),"e"
)


#get lastname and firstname from orig name
cbcdf$raw
tmpregex<-c(
  "^([A-Z\\s\\'\\-]+)\\,\\s([A-z\\s\\.\\,\\-]+)(\\([A-z\\.\\s]+\\))?([0-9\\s]+)?$"
)
tmp<-str_detect(
  cbcdf$raw,
  tmpregex
)
cbcdf$raw[!tmp]
if(sum(!tmp)>0)
  stop('havent matched all')

#lastname
cbcdf$lastname<-
  str_replace(
    cbcdf$raw,
    tmpregex,
    "\\1"
  ) %>% str_replace(
    "^\\s+|\\s+$",""
  )

#firstname
cbcdf$firstname<-
  str_replace(
    cbcdf$raw,
    tmpregex,
    "\\2"
  ) %>% str_replace(
    "^\\s+|\\s+$",""
  )

#remove suffix from firstname
fnameregex<-"^([A-z\\s\\.\\-]+)(\\,(.*))?$"
tmp<-str_detect(cbcdf$firstname,fnameregex)
if(sum(!tmp)>0)
  stop('havent matched all')
cbcdf$firstname<-str_replace(
  cbcdf$firstname,
  fnameregex,
  "\\1"
) %>% str_replace(
  "^\\s+|\\s+$",""
)
cbcdf$suffix<-str_replace(
  cbcdf$firstname,
  fnameregex,
  "\\3"
) %>% str_replace(
  "^\\s+|\\s+$",""
)


#get congress session info
tmpregex<-"^([0-9]{2,3})(st|nd|rd|th)\\s\\(([0-9]{4})\\-([0-9]{4})\\)$"
tmp<-str_detect(cbcdf$congress,tmpregex)
if(sum(!tmp)>0)
  stop('havent matched all')

cbcdf$session<-str_replace(
  cbcdf$congress,
  tmpregex,
  "\\1"
) %>% as.numeric
cbcdf$startyear<-str_replace(
  cbcdf$congress,
  tmpregex,
  "\\3"
)
cbcdf$endyear<-str_replace(
  cbcdf$congress,
  tmpregex,
  "\\4"
)

#MAKE PERSON-LEVEL
#each row is a person-congress
#we want it to be unique, person-level

#use FI/lastname/state_alpha2
fi<-str_extract(cbcdf$firstname,"^[A-z]{1}")
cbcdf$id<-paste0(
  tolower(fi),
  str_replace(
    tolower(cbcdf$lastname),
    "(\\s|\\.)",""
  ),
  "_",cbcdf$state_alpha2
)

#inspect same ids, different raw
tmpdf<-by(cbcdf,cbcdf$id,function(df) {
  #df<-cbcdf[cbcdf$id=="wclay_MO",]
  tmp<-length(unique(df$raw))
  if(tmp>1) {
    df
  } else {
    data.frame(
      firstname=NA
    )
  }
}) %>% rbind.fill
tmpdf<-tmpdf[!is.na(tmpdf$firstname),]
tmpdf[,c("raw","id")]

#fix the narcissist dups!
tmp<-cbcdf$id=="dpayne_NJ" & 
  str_detect(cbcdf$raw,"Jr")
cbcdf$id[tmp]<-"dpaynejr_NJ"
tmp<-cbcdf$id=="hford_TN" &
  str_detect(cbcdf$raw,"Jr")
cbcdf$id[tmp]<-"hfordjr_TN"
tmp<-cbcdf$id=="wclay_MO" & 
  str_detect(cbcdf$raw,"Jr")
cbcdf$id[tmp]<-"wclayjr_MO"

#now we get uniques
cbcdf<-by(cbcdf,cbcdf$id,function(df) {
  #df<-cbcdf[cbcdf$id=="tscott_SC",]
  returnrow<-data.frame(
    id=unique(df$id),
    firstname=unique(df$firstname),
    lastname=unique(df$lastname),
    startyear=min(df$startyear),
    endyear=max(df$endyear),
    state_alpha2=unique(df$state_alpha2),
    chamber=paste0(
      unique(df$chamber),
      collapse="/"
    ),
    stringsAsFactors=F
  )
  if(nrow(returnrow)>1)
    stop(print(unique(df$id)))
  returnrow
}) %>% rbind.fill

tmp<-cbcdf$state_alpha2%in%membersdf$state_abbrev
cbcdf<-cbcdf[tmp,]

#loop through ssdf,
#match everyone to someone
tmpseq.i<-1:nrow(cbcdf);
tmpoutput<-lapply(tmpseq.i,function(i) {
  #i<-2
  print(i)
  
  thisrow<-cbcdf[i,]
  thisid<-thisrow$id
  thisfname<-thisrow$firstname
  thislname<-thisrow$lastname
  thisa2<-thisrow$state_alpha2
  
  #match!
  
  #trim voters to thisa2
  tmprows<-membersdf$state_abbrev==thisa2
  tmpvars<-c("firstname","lastname","icpsr")
  tmpdf<-membersdf[tmprows,tmpvars,with=F] %>%
    unique
  roworder<-order(tmpdf$lastname)
  tmpdf<-tmpdf[roworder,]
  
  #take all the lastnames here,
  #do we have a match?
  lmatches<-matchme(
    thislname,
    tmpdf$lastname
  )
  
  #take all the firstnames,
  #do we have a match?
  fmatches<-matchme(
    thisfname,
    tmpdf$firstname
  )
  
  #ideally, for a match
  #we want both
  match<-lmatches & fmatches #both
  pmatch<-lmatches | fmatches #either
  
  #browse
  thisrow
  tmpdf[pmatch,]

  ####
  #some matches done manually
  if(thisid=="acarson_IN") 
    match<-tmpdf$icpsr==20757
  if(thisid=="aespy_MS")
    match<-tmpdf$icpsr==15411
  if(thisid=="dpayne_NJ")
    match<-tmpdf$icpsr==15619
  if(thisid=="hford_TN")
    match<-tmpdf$icpsr==14224
  if(thisid=="wclay_MO")
    match<-tmpdf$icpsr==12009
  if(thisid=="wclayjr_MO")
    match<-tmpdf$icpsr==20147
  if(thisid=="wcowan_MA")
    match<-tmpdf$icpsr==41306
  
  ####
  
  
  if(sum(match)==1) {
    
    matchdf<-data.frame(
      row=i,
      match="yes",
      icpsr=tmpdf$icpsr[match],
      firstname=thisfname,
      lastname=thislname,
      firstname.match=tmpdf$firstname[match],
      lastname.match=tmpdf$lastname[match],
      stringsAsFactors=F
    )
    
  } else if(sum(match)>1) {
    
    matchdf<-data.frame(
      row=i,
      match="multi_exact",
      icpsr=NA,
      firstname=thisfname,
      lastname=thislname,
      firstname.match=paste0(
        tmpdf$firstname[pmatch],
        collapse=" / "
      ),
      lastname.match=paste0(
        tmpdf$lastname[pmatch],
        collapse=" / "
      ),
      stringsAsFactors=F
    )
    
  } else if(sum(match)==0) {
    
    #partial match?
    if(sum(pmatch)==1) {
      
      matchdf<-data.frame(
        row=i,
        match="yes_partial",
        icpsr=tmpdf$icpsr[pmatch],
        firstname=thisfname,
        lastname=thislname,
        firstname.match=tmpdf$firstname[pmatch],
        lastname.match=tmpdf$lastname[pmatch],
        stringsAsFactors=F
      )
      
    } else if(sum(pmatch)>1) {
      
      matchdf<-data.frame(
        row=i,
        match="multi_partial",
        icpsr=NA,
        firstname=thisfname,
        lastname=thislname,
        firstname.match=paste0(
          tmpdf$firstname[pmatch],
          collapse=" / "
        ),
        lastname.match=paste0(
          tmpdf$lastname[pmatch],
          collapse=" / "
        ),
        stringsAsFactors=F
      )
      
      
    } else if(sum(pmatch)==0) {
      
      matchdf<-data.frame(
        row=i,
        match="no",
        icpsr=NA,
        firstname=thisfname,
        lastname=thislname,
        firstname.match=NA,
        lastname.match=NA,
        stringsAsFactors=F
      )
      
    }
    
    
  }
  
  
  #return
  matchdf
  
}) 

#ALL YES?
matchdf<-rbind.fill(tmpoutput)
tmp<-matchdf$match!="yes"
if(sum(tmp)>0)
  stop('not all matched')

#########################################################
#########################################################

#use this to identify who is black and who is not
matchdf$black<-1
tmpdf<-matchdf[,c("icpsr","black")]
membersdf<-merge(
  membersdf,
  tmpdf,
  all=T
)
tmp<-is.na(membersdf$black)
membersdf$black[tmp]<-0
table(membersdf$black)

#########################################################
#########################################################

# #this shows where
# membersdf$black<-factor(membersdf$black)
# tmpcolors<-c("grey","black")
# names(tmpcolors)<-levels(membersdf$black)
# 
# require(ggplot2)
# ggplot(
#   membersdf,
#   aes(
#     x=nominate_dim1,
#     y=nominate_dim2,
#     color=black
#   )
# ) +
#   geom_point() +
#   scale_color_manual(
#     values=tmpcolors
#   )

setwd(filesdir); dir()
write.csv(
  membersdf,
  '02_voting_membersdf.csv',
  row.names=F
)



