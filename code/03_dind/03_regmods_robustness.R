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

#extras
require(haven)
require(readr)
require(plm)

#set dirs
rootdir<-find_root(
  criterion=has_file('_rapol.Rproj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#load helper functions
setwd(dcodedir); dir()
source('beofunctions.R')
source('beofunctions2.R')
source('getlongrun2.R')

#set seed
set.seed(23)
reps<-1000

#########################################################
#########################################################

#load regmods image
# setwd(filesdir); dir()
# # load(
# #   'regmods.RData'
# # )

#load the original beodf
setwd(datadir); dir()
beodfraw<-read.csv(
  'beodf.csv',
  stringsAsFactors=F
)

#add lags of difference of imprt_t_jur
#this is necessary b/c unitroot tests flag it
beodfraw<-by(beodfraw,beodfraw$state_alpha2,function(df) {
  #df<-beodfraw[beodfraw$state_alpha2=='AL',]
  df$L3.D.imprt_t_jur <- dplyr::lag(df$L2.D.imprt_t_jur)
  df$L4.D.imprt_t_jur <- dplyr::lag(df$L3.D.imprt_t_jur)
  df$L3.D.officers_pcap <- dplyr::lag(df$L2.D.officers_pcap)
  df$L4.D.officers_pcap <- dplyr::lag(df$L3.D.officers_pcap)
  df
}) %>% rbind.fill

#we also want five yar
setwd(datadir); dir()
beodf5<-read.csv(
  'beodf5.csv',
  stringsAsFactors=F
)

#load the results
setwd(filesdir); dir()
finaldf<-read.csv(
  '03_dind_regresultsdf.csv',
  stringsAsFactors=F
)

#load the things that go along with it
setwd(filesdir); dir()
reglist<-readRDS(
  '03_dind_regstuff.RDS'
)
forms<-reglist$forms
mods<-reglist$mods
samps<-reglist$samps
sdsdf<-reglist$sdsdf

#get unit root info
setwd(dcodedir); dir()
source('checkunitroots.R')
urvars<-str_replace(
  urvars,
  "\\L.",""
)

iv.urvars<-paste0("L([0-9]+)?\\.",urvars)
dv.urvars<-paste0("^",urvars)
ur.regex<-paste0("(",paste0(c(dv.urvars,iv.urvars),collapse="|"),")")

#quick function to transform
#a formula into a urformula
urtransform<-function(form,asformula=T) {
  #form<-int.f
  #formula=T
  #if it's not a formula, change to one
  if(class(form)!="character") {
    form.string<-as.character(form)
    form<-paste(form.string[2],form.string[1],form.string[3])
  }
  urform<-str_replace_all(form,ur.regex,"D.\\1")
  #adjustment needed, b/c of how i created vars
  #switch the L and the D around
  urform<-str_replace_all(urform,"(D\\.)(L([0-9]+)?\\.)","\\2\\1")
  thisformula<-urform
  if(asformula)
    thisformula<-thisformula %>% as.formula
  return(thisformula)
}

#########################################################
#########################################################

#SET UP ROBMODS LOOP

#models are named externally
setwd(metadir); dir()
rawrobdf<-read.csv(
  '03_dind_robustness.csv',
  stringsAsFactors=F
)

#we will only be running these mods
#for incrt and officers
dvs<-c(
  "imprt_t_jur",
  "officers_pcap"
)

#those which are NA, are deprecated
rawrobdf<-rawrobdf[!is.na(rawrobdf$order),]

# #make a table, for output
# rawrobdf$letter<-
#   paste0(
#     "(",LETTERS[1:nrow(rawrobdf)],")"
#   )
# setwd(outputdir)
# write.csv(
#   rawrobdf[,c("letter","propername")],
#   "tab_robmods.csv",
#   row.names=F
# )

#to be used below
getmodname<-function(mname) {
  mname<-str_replace(mname,"_clSE","")
  rawrobdf$propername[rawrobdf$mname==mname]
}

getmodorder<-function(mname) {
  mname<-str_replace(mname,"_clSE","")
  rawrobdf$order[rawrobdf$mname==mname]
}

#to loop through
robdf<-expand.grid(
  mod=rawrobdf$mname,
  dv=dvs,
  stringsAsFactors=F
)

#########################################################
#########################################################

#LOOP THROUGH

#trim?
tmp<-rep(T,nrow(robdf))
robdf<-robdf[tmp,]
robdf$seq<-1:nrow(robdf)

#LOOP!
tmp.seq<-1:nrow(robdf)
tmpoutput<-lapply(tmp.seq,function(i) {
  
  #i<-12
  #get param
  thisdv<-robdf$dv[i]
  thismod<-robdf$mod[i]
  #track progress
  print("####")
  print(
    paste(
      "Estimating model",
      i,"of",max(tmp.seq)
    )
  )
  print(thisdv)
  print(thismod)
  
  #############
  
  #PREPATORY
  
  #get sample and formula from pref
  tmp<-finaldf$dv==thisdv & 
    finaldf$pref
  #identify sample w/ logicals, 
  #which makes robustness process easier,
  #since we are adding new vars etc.
  thisdf<-beodfraw
  this.samp<-paste0(
    thisdf$state_alpha2,
    thisdf$year
  ) %in% paste0(
    samps[[thisdv]]$state_alpha2,
    samps[[thisdv]]$year
  )
  this.mname<-unique(finaldf$mname[tmp])
  thismodel.arg<-"within" #default pref.
  thisform<-forms[[this.mname]]
  
  #############
  
  #which model?
  #redefine samp/form as necessary
  #and then re-estimate below
  
  #############
  
  #SET UP FORMULA/SAMPLE
  
  if(thismod=="pref") {
    
    #don't change anything
    
  } else if (thismod=="re") {
    
    thismodel.arg<-"random"
    
  } else if (thismod=="pooled") {
    
    thismodel.arg<-"pooling"
    
  } else if (thismod=="5year") {
    
    thisdf<-beodf5
    this.samp<-rep(T,nrow(beodf5))
    
  } else if (thismod=="spending") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-str_replace_all(
      oldform,
      "incrt\\_t\\_jur|imprt\\_t\\_jur",
      "lncorrections_pcap"
    ) %>% str_replace_all(
      "officers\\_pcap",
      "lnpolicesp_pcap"
    ) %>% as.formula
    if(thisdv=="incrt_t_jur" | thisdv=="imprt_t_jur") 
      thisdv<-"lncorrections_pcap"
    if(thisdv=="officers_pcap")
      thisdv<-"lnpolicesp_pcap"
    
  } else if (thismod=="divtrend") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      " + year:division"
    ) %>% as.formula
    
  } else if (thismod=="regtrend") {
    
    oldform<-deparse(thisform) %>%
      paste0(collapse="")
    thisform<-paste0(
      oldform,
      " + year:region"
    ) %>% as.formula
    
  } else if (thismod=='urtransform') {
    
    thisform<-urtransform(thisform)
    if(thisdv%in%urvars) {
      thisdv<-paste0("D.",thisdv)
    }

    
  } else {
    
    stop(
      print(paste(thismod,"not implemented."))
    )
    this.samp<-rep(F,nrow(demdf))
    
  }
  
  #############
  
  #ESTIMATION
  
  #get the df
  tmpdf<-thisdf[this.samp,]
  regvars<-all.vars(thisform)
  regvars<-c("state_alpha2","year",regvars)
  
  #check that they're all present
  tmp<-regvars%in%names(tmpdf)
  if(sum(!tmp)>0) {
    print(regvars[!tmp])
    stop('missing vars')
  }
  
  prez<-complete.cases(tmpdf[,regvars])
  tmpdf<-tmpdf[prez,regvars]
  
  #restimate
  
  m.tmp<-plm(
    data=tmpdf,
    form=thisform,
    model=thismodel.arg
  )
  vcov.tmp<-vcovHC(
    m.tmp,
    type="HC1",
    cluster="group"
  )
  keyvar<-c("beopct_all")
  keyvar.sd<-sdsdf$sd[sdsdf$var==keyvar]
  if(str_detect(thismod,"ratio")) {
    keyvar<-"beoratio"
    keyvar.sd<-tapply(
      tmpdf$L.beoratio,
      tmpdf$state_alpha2,
      sd,na.rm=T
    ) %>% mean(na.rm=T)
  }
  returndf<-getlongrun2(
    m=m.tmp,
    vcov=vcov.tmp,
    dv=thisdv,
    iv=keyvar,
    ivsd=keyvar.sd
  )
  #add standardized
  dvsd<-tapply(
    tmpdf[[thisdv]],
    tmpdf$state_alpha2,
    sd
  ) %>% mean
  returndf$musd<-returndf$mu/dvsd
  returndf$musd.min<-returndf$mu.min/dvsd
  returndf$musd.max<-returndf$mu.max/dvsd
  returndf$iv<-"beopct_all"
  returndf$N<-nrow(m.tmp$model)
  returndf$N.states<-length(unique(tmpdf$state_alpha2))
  returndf$seq<-i
  #return
  return(returndf)
  
})

#make estsdf
robestsdf<-rbind.fill(tmpoutput)
intersect(
  names(robdf),
  names(robestsdf)
)
robestsdf<-merge(
  robdf,
  robestsdf,
  by="seq"
)

#########################################################
#########################################################

#FINALIZE/SAVE OUT

#remove anything that wasn't estimated
tmp<-is.na(robestsdf$mu)
missing<-unique(robestsdf$mod[tmp])
print(missing)
robestsdf<-robestsdf[!tmp,]

#get modname
#robestsdf$modname<-sapply(robestsdf$mod,getmodname)
robestsdf$mname<-paste0(
  robestsdf$dv,".",robestsdf$mod
)

#########################################################
#########################################################

#save out
setwd(filesdir); dir()
#save.image("robusts.RData")
write.csv(
  robestsdf,
  "03_dind_robustnessdf.csv",
  row.names=F
)

#########################################################
#########################################################
