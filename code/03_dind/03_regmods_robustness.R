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

#########################################################
#########################################################

#perform unit root tests

#set params

#vars
tmp<-finaldf$pref &
  finaldf$dv%in%c(
    'imprt_t_jur',
    'officers_pcap'
  )
prefmods<-unique(finaldf$mname[tmp])
vars<-lapply(forms[prefmods],all.vars) %>% 
  unlist %>% unique

#omit some
badvars<-c(
  "year"
)
lagprefs<-c("L.","L2.","L3.","L4.")
dv_vars<-c(
  paste0(lagprefs,"imprt_t_jur"),
  paste0(lagprefs,"officers_pcap")
)
badvars<-c(
  badvars,
  dv_vars
)
tmp<-!vars%in%badvars
vars<-vars[tmp]

#misc parameters
maxlags<-3

#BALANCED TEST

#the data are mostly balanced, 
#so all we want is to loop through
#and pick the 50 states

#where there is an issue, b/c one state is missing
#or for some other reason, we adjust slightly
#this gives us three tests per variable

loopdf<-expand.grid(
  var=vars,
  stringsAsFactors=F
)
loopdf$i<-1:nrow(loopdf)

#put together a balanced dataset for each variable
#i will want at least N.countries, and so I pick the
#T for each variable that gives me this many N.countires
#in a balanced dataset

tmpseq.i<-1:nrow(loopdf)
baldf<-lapply(tmpseq.i,function(i) {
  
  
  #i<-8
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )
  
  ####
  
  thisvar<-loopdf$var[i]
  
  ###
  
  #get countries desired, given N countries
  thisdf<-beodfraw
  tmpa2s<-tapply(
    thisdf[[thisvar]],
    thisdf$state_alpha2,
    function(x) sum(!is.na(x))
  ) %>% sort
  tmpa2s<-tmpa2s[tmpa2s!=0]
  tmptab<-table(tmpa2s)
  if(length(tmptab)!=1) {
    if(thisvar=="imprt_t_jur") {
      minN<-41
    } else if(thisvar=="L.violent_crt") {
      minN<-51
    } else {
      stop('inspect')
    }
  } else {
    minN<-tmpa2s[1]
  }
  tmpa2s<-tmpa2s[tmpa2s==minN]
  
  
  
  ###
  
  #create a tmpdf w/ this many obs,
  #using thesecows
  tmpvars<-c("state_alpha2","year",thisvar)
  tmprows<-thisdf$state_alpha2%in%names(tmpa2s)
  tmpdf<-thisdf[tmprows,tmpvars]
  testdf<-by(tmpdf,tmpdf$state_alpha2,function(df) {
    #df<-tmpdf[tmpdf$cowcode.num==2,]
    df<-df[!is.na(df[[thisvar]]),]
    data.frame(
      thisvar=tail(df[[thisvar]],minN),
      time=1:minN,
      state_alpha2=unique(df$state_alpha2),
      stringsAsFactors=F
    )
  }) %>% rbind.fill
  
  ###
  
  #make this wide rather than long
  is.na(testdf$thisvar) %>% sum
  is.nan(testdf$thisvar) %>% sum
  sum(!is.finite(testdf$thisvar))
  
  #here's my dataframe
  testdf<-spread(
    testdf,
    state_alpha2,
    thisvar
  )
  
  #but remove inel cols (this which don't change)
  badcols<-apply(
    testdf,2,
    function(x) length(unique(x))==1
  )
  testdf<-testdf[,!badcols]
  
  #and more badcols are those w/o sufficient variance
  #purtest() throws an error if these are included
  #and badcols, which are countries in which there isn't much variation
  if(thisvar=="L.demcontrol.klarner") {
    bada2s<-c("HI")
  } else {
    bada2s<-c("")
  }
  badcols<-names(testdf)%in%bada2s
  testdf<-testdf[,!badcols]
  #check variances
  tmpvariances<-apply(
    testdf,2,
    var
  ) 
  sort(tmpvariances)
  
  ###
  
  #run the tests,get restuls
  tests<-c(
    "levinlin",
    "ips",
    "madwu",
    "hadri"
  )
  returndf<-lapply(tests,function(mytest) {
    #mytest<-"levinlin"
    #print(mytest)
    testdf$time<-NULL
    tmptest<-purtest(
      testdf,
      test=mytest,
      exo='intercept',
      lags='AIC',
      pmax=maxlags
    )
    returndf<-data.frame(
      #test=tmptest$statistic$method,
      test=mytest,
      pval=tmptest$statistic$p.value
    )
    if(mytest=="hadri") {
      returndf$unitroot<-ifelse(
        returndf$pval>0.05,"No","Yes"
      )
    } else {
      returndf$unitroot<-ifelse(
        returndf$pval<0.05,"No","Yes"
      )
    }
    returndf
  }) %>% rbind.fill
  
  ###
  
  returndf$N.actual<-ncol(testdf) - 1
  returndf$i<-i
  returndf
  
}) %>% rbind.fill


#merge loopdf and returndf
intersect(
  names(loopdf),
  names(baldf)
)

baldf<-merge(
  loopdf,
  baldf,
  by="i",
  all=T
)

#any potential unitroot vars?
tmp<-baldf$test!="hadri"
tmptab<-tapply(
  baldf$unitroot[tmp],
  baldf$var[tmp],
  function(x) 
    sum(x=="Yes")
)
urvars<-names(tmptab[tmptab>=2])

########################################################
########################################################

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
