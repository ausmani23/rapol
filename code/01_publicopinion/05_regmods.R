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
sample_df<-F
if(!"sample_N"%in%ls())
  sample_N<-as.numeric(readLines('sample_N.txt'))

#########################################################
#########################################################

#load data
setwd(filesdir); dir()
finaldf<-fread(
  '01po_dataframe.csv'
)

#########################################################
#########################################################

#SETUP

felist<-list(
  bvrt=c("RACE"),
  full=c(
    "RACE",
    "gender",
    "ed",
    "age"
  )
)
relist<-list(
  simple=c(
    "year",
    "GEO",
    "question"
    #####
  ),
  pref=c(
    "year",
    "GEO",
    "question",
    #####
    "RACEXed",
    "RACEXgender",
    "RACEXage",
    "RACEXyear",
    "RACEXGEO",
    "RACEXquestion"
  ),
  full=c(
    "year",
    "GEO",
    "question",
    #####
    "RACEXgender",
    "RACEXage",
    "RACEXquestion",
    "RACEXGEOXyear",
    "RACEXedXyear"
  )
)

#########################################################
#########################################################

#MODS
modsdf<-expand.grid(
  dv=c(
    "aff"
  ),
  dimension=c(
    "all",
    "punitive",
    "anxiety",
    "mistrust"
  ),
  fes=c(
    "bvrt",
    "full"
  ),
  res=c(
    "simple",
    "pref",
    "full"
  ),
  race=c(
    "race",
    "race2"
  ),
  sample=c(
    "allpolls",
    "statepolls"
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
tmp<-rep(T,nrow(modsdf))
tmp<-tmp & modsdf$sample%in%c("allpolls")
tmp<-tmp & modsdf$res%in%c("simple","pref","full")
tmp<-tmp & modsdf$fes%in%c("full")
tmp<-tmp & !modsdf$dimension%in%c("all")
tmp<-tmp & modsdf$race=="race"
modsdf<-modsdf[tmp,]
modsdf$i<-1:nrow(modsdf)

####
#not setup to run race, here..
# tmp<-modsdf$race=="race"
# if(sum(tmp)>0)
#   stop('cant estimate race w/o making it a factor')

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

#ESTIMATE 

#DEPRECATED
# tmpseq.i<-1:nrow(modsdf)
# st_time<-proc.time()
# modslist<-lapply(tmpseq.i,function(i) {
#   #i<-1
#   print(
#     paste(
#       "Estimating",i,"of",length(tmpseq.i)
#     )
#   )
#   thisrow<-modsdf[i,]
#   thisform<-forms[[i]]
#   ss<-sampspecs[[thisrow$sampname]]
#   thisdf<-finaldf[ss$rows,ss$cols,with=F]
#   #####
#   #sample to save time?
#   if(sample_df)
#     thisdf<-dplyr::sample_n(thisdf,sample_N)
#   #####
#   #fit mod/getmodtime
#   modtime<-system.time(
#     m<-glmer(
#       data=thisdf,
#       formula=thisform,
#       family=binomial(link="logit")
#     )
#   )
#   list(
#     m=m,
#     modtime=modtime
#   )
# })
# names(modslist)<-modsdf$mname
# duration<-proc.time() - st_time

# this is time-intensive, so
# preferable to attempt to run this in parallel
require(doParallel)
require(foreach)
numCores<-getDoParWorkers()
registerDoParallel(cores=numCores)
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
  #sample to save time?
  if(sample_df)
    thisdf<-dplyr::sample_n(thisdf,sample_N)
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

#IDENTIFY PREFMODS BASED ON FIT

#merge sample info
finaldf<-merge(
  modsdf,
  sampinfodf,
  by="sampname"
)
finaldf<-finaldf[order(finaldf$i),]

#pick pref model
mods<-lapply(modslist,function(x) x$m)
tmpseq.i<-seq_along(mods)
fitdf<-lapply(tmpseq.i,function(i) {
  #i<-4
  m<-mods[[i]]
  m.sum<-summary(m)
  returndf<-data.frame(
    t(m.sum$AICtab)
  )
  returndf$warnings<-paste0(
    m.sum$optinfo$conv$lme4$messages,
    collapse="/"
  )
  returndf
}) %>% rbind.fill
names(fitdf)<-tolower(names(fitdf)) 
fitdf$mname<-names(mods)

#put it together
finaldf<-merge(
  finaldf,
  fitdf,
  by="mname"
)

#########################################################
#########################################################

#obtain prefmods

#rank models within sample
finaldf<-by(finaldf,finaldf$sampname,function(df) {
  #df<-finaldf[finaldf$sampname=="all.allpolls",]
  df$fes<-factor(
    df$fes,
    levels=c("pref","full","bvrt")
  )
  df$res<-factor(
    df$res,
    levels=c("pref","full")
  )
  #by aic
  df$aic.rank<-order(
    df$aic,
    df$fes,
    df$res
  )
  #by bic
  df$bic.rank<-order(
    df$bic,
    df$fes,
    df$res
  )
  #get pref
  df$pref<-F
  df$pref[df$aic.rank==1]<-T
  #return
  df
}) %>% rbind.fill
#retain original order
finaldf<-finaldf[order(finaldf$i),]

#########################################################
#########################################################

#save mod and sampinfo
setwd(filesdir); dir()
write.csv(
  finaldf,
  "01po_regmods_info.csv",
  row.names=F
)

#save mods/modtimes
setwd(filesdir); dir()
saveRDS(
  modslist,
  "01po_modslist.RDS"
)

#########################################################
#########################################################
