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

#extra
require(data.table)
require(lme4)
require(rstanarm)

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
source('functions.R')

#set seed
set.seed(23)

#sample N
setwd(filesdir)

#########################################################
#########################################################

#load data
setwd(filesdir); dir()
finaldf<-fread(
  '01po_dataframe.csv'
)

finaldf$raceXquestionXyear<-paste0(
  finaldf$race,
  "_",
  finaldf$question,
  "_",
  finaldf$year
)

finaldf$questionXyear<-paste0(
  finaldf$question,
  "_",
  finaldf$year
)

#########################################################
#########################################################

#SETUP

felist<-list(
  bvrt=c(
    "RACE"
    )#,
  # full=c(
  #   "RACE",
  #   "gender",
  #   "ed",
  #   "age"
  # )
)
relist<-list(
  # simple=c(
  #   "year",
  #   "GEO",
  #   "question"
  #   #####
  # ),
  pref=c(
    "year",
    #"GEO",
    "question",
    #####
    # "RACEXed",
    # "RACEXgender",
    # "RACEXage",
    "RACEXyear"
    #"RACEXGEO",
  )
  # full=c(
  #   "year",
  #   "GEO",
  #   "question",
  #   #####
  #   "RACEXgender",
  #   "RACEXage",
  #   "RACEXquestion",
  #   "RACEXGEOXyear",
  #   "RACEXedXyear"
  # )
)

#########################################################
#########################################################

#MODS
modsdf<-expand.grid(
  dv=c(
    "aff"
  ),
  dimension=c(
    "punitive",
    "anxiety",
    "mistrust"
  ),
  fes=c(
    "bvrt"
  ),
  res=c(
    "pref"
  ),
  race=c(
    "race"
  ),
  sample=c(
    "allpolls"
  ),
  stringsAsFactors=F
)

#modname
modsdf$mname<-apply(
  modsdf,1,paste0,collapse="."
)
modsdf$sampname<-paste0(
  modsdf$dimension,
  ".",
  modsdf$sample
)
unique(modsdf$sampname)

#trim
modsdf$i<-1:nrow(modsdf)

#########################################################
#########################################################

#GET FORMS
tmpseq.i<-1:nrow(modsdf)
forms<-lapply(tmpseq.i,function(i) {
  #i<-1  
  #print(i)
  thisrow<-modsdf[i,]
  #get thisgeo
  thisgeo<-ifelse(
    thisrow$sample=="allpolls",
    "division",
    "state_alpha2"
  )
  thisrace<-ifelse(
    thisrow$race=="race",
    "race",
    "race2"
  )
  #lhs
  lhs<-thisrow$dv
  
  #rhs, fe
  rhs.fe<-felist[[thisrow$fes]] %>% 
    str_replace("GEO",thisgeo) %>%
    str_replace("RACE",paste0("factor(",thisrace,")")) %>%
    paste(collapse=" + ")
  
  #rhs,re
  re.raw<-relist[[thisrow$res]] %>% 
    str_replace("GEO",thisgeo) %>%
    str_replace("RACE",thisrace)
  rhs.re<-paste0(
    "(1 | ",re.raw,")"
  ) %>% 
    paste(collapse=" + ")
  rhs<-paste0(
    rhs.fe," + ",rhs.re
  )
  #put together
  thisform<-paste0(
    lhs,
    " ~ ",
    rhs
  )
  as.formula(thisform)
})
names(forms)<-modsdf$mname

#make sure all vars are prez
allvars<-lapply(forms,all.vars) %>%
  unlist %>% unique
tmp<-allvars%in%names(finaldf)
if(sum(!tmp)>0) {
  print(allvars[!tmp])
  stop()
}

#########################################################
#########################################################

#SAMPS
#each sampname in modsdf
sampnames<-unique(modsdf$sampname)
tmpseq.j<-seq_along(sampnames)
sampspecs<-lapply(tmpseq.j,function(j) {
  #j<-1
  print(j)
  #get all forms w/ this sampname
  this.sampname<-sampnames[j]
  tmprows<-modsdf$sampname==this.sampname
  tmpmods<-modsdf$mname[tmprows]
  tmpdim<-unique(modsdf$dimension[tmprows])
  if(length(tmpdim)>1) stop()
  #these are the vars
  mycols<-lapply(forms[tmpmods],all.vars) %>% 
    unlist %>% unique
  mycols<-c(
    mycols,
    "respid",
    "pollid"
  ) %>% unique
  #these are the rows; all vars present and dimension
  myrows<-complete.cases(finaldf[,mycols,with=F])
  finaldf$race%in%c(1,2) #only blacks/whites
  if(tmpdim!="all") {
    myrows<-myrows & 
      finaldf$dimension==tmpdim
  }  
  #return rows/cols
  list(
    rows=myrows,
    cols=mycols
  )
})
names(sampspecs)<-sampnames

#get info of each samp
sampinfodf<-lapply(tmpseq.j,function(j) {
  listbit<-sampspecs[[j]]
  thisdf<-finaldf[listbit$rows,listbit$cols,with=F]
  data.frame(
    sampname=names(sampspecs)[j],
    N=nrow(thisdf),
    N.resp=length(unique(thisdf$respid)),
    N.questions=length(unique(thisdf$question)),
    N.polls=length(unique(thisdf$pollid)),
    stringsAsFactors=F
  )
}) %>% rbind.fill

#########################################################
#########################################################

# this is time-intensive, so
# preferable to attempt to run this in parallel
require(doParallel)
require(foreach)

# Register a parallel backend
num_cores <- detectCores() - 1
cl <- makeCluster(num_cores)
registerDoParallel(cl)

modslist <- foreach(i=tmpseq.i,.packages='data.table') %dopar% {
  #i<-4
  print(
    paste(
      "Estimating",i,"of",length(tmpseq.i)
    )
  )
  thisrow<-modsdf[i,]
  thisform<-forms[[i]]
  ss<-sampspecs[[thisrow$sampname]]
  thisdf<-finaldf[ss$rows,ss$cols,with=F]
  #####
  #fit mod/getmodtime
  modtime<-system.time(
    m<-lme4::glmer(
      data=thisdf,
      formula=thisform,
      family=binomial(link="logit")
    )
  )
  list(
    m=m,
    modtime=modtime
  )
}
names(modslist)<-modsdf$mname

#########################################################
#########################################################

#save mods/modtimes
setwd(filesdir); dir()
saveRDS(
  modslist,
  "01po_modslist_RandR.RDS"
)



















