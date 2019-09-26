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
require(merTools)
require(doParallel)

#set dirs
rootdir<-find_root(
  criterion=has_file('_rapol.Rproj')
)
codedir<-file.path(rootdir,"code, voting")
setwd(codedir); dir()
source('dirs.R')

#helper functions
setwd(codedir); dir()
source('readpoll_functions.R')
source('getinfo2.R')
source('functions.R')

#load data and mods
setwd(filesdir); dir()
load(file="01R2_prepped.RData")
mods<-readRDS('modslist.RDS')
mods<-lapply(mods,function(x) x$m)

#set seed
set.seed(23)

#########################################################
#########################################################

#USE PREFMODS
#for this exercise, select prefmods
#based on the fit exercise in regmods

#identify prefmods
setwd(metadir)
finaldf<-read.csv(
  'regmods_info.csv',
  stringsAsFactors=F
)
prefmod_names<-finaldf$mname[finaldf$pref]
prefmods<-mods[prefmod_names]

#there should be 4 of these
if(length(prefmods)!=4)
  stop('need to fix this')

#########################################################
#########################################################

#PREDICT FOR DIFFS/MEANS
#median FE vals
#0 with the RE's
#get PI manually, FE only
#using prefmods in each dimension

predictdf<-expand.grid(
  race2=c(1,2),
  gender=c(1),
  ed=2
)
#ensure it works whether race or race2
predictdf$race<-predictdf$race2

#loop through and predictdf
tmpseq.i<-seq_along(prefmods)
fulloutput<-lapply(tmpseq.i,function(i) {
  #i<-1
  print(i)
  thismod<-prefmods[[i]]
  thismodname<-names(prefmods)[i]
  thisdimension<-str_extract(
    thismodname,
    "all|anxiety|punitive|mistrust"
  )
  
  #############
  
  #set up predictdf
  
  #get mrefs
  mrefs<-get.medianrefs(thismod)
  predictdf$year<-mrefs$year
  predictdf$question<-mrefs$question
  predictdf$division<-mrefs$division
  predictdf$age<-2
  predictdf$row<-1:nrow(predictdf)
  
  #now, add all interactions
  loopdf<-expand.grid(
    v1=c(
      "race",
      "race2"
    ),
    v2=c(
      "age",
      "gender",
      "ed",
      "year",
      "region",
      "division",
      "state_alpha2",
      "question"
    ),
    stringsAsFactors=F
  )
  tmpseq.j<-1:nrow(loopdf)
  for(j in tmpseq.j) {
    #print(i)
    #j<-1
    thisrow<-loopdf[j,]
    newname<-paste0(
      thisrow$v1,
      "X",
      thisrow$v2
    )
    #make these all distinct categories
    predictdf[[newname]]<-paste0(
      predictdf[[thisrow$v1]],
      "_",
      predictdf[[thisrow$v2]]
    )
  }
  
  #get predictions w/ merTools
  tmpoutput<-merTools::predictInterval(
    thismod,
    newdata=predictdf,
    which="fixed",
    level=0.95,
    type='probability',
    include.resid.var=F,
    returnSims=T
  )
  simMat<-attr(tmpoutput,"sim.results")
  simMat<-apply(simMat,1,thismod@resp$family$linkinv)
  simMat<-t(simMat) * 100
  
  #summarize the predictdf
  ###AVERAGES
  avgdf<-apply(simMat,1,function(x) {
    summarize.distribution2(x)
  }) %>% rbind.fill
  avgdf$pval<-avgdf$pval.class<-NULL
  avgdf<-cbind(
    avgdf,
    predictdf
  )
  avgdf$dimension<-thisdimension
  avgdf
  ###DIFFERENCE, BASED ON AVG
  sumdist<-apply(simMat,2,function(x) {
    quantile(
      x,
      c(
        0.025,
        0.5,
        0.975
      )
    )
  }) %>% t
  sumdist<-apply(sumdist,1,function(x) {
    rnorm(10000,mean=x[2],sd=abs(x[3]-x[1])/3.92 )
  }) %>% t
  whites<-sumdist[predictdf$race==1,]
  blacks<-sumdist[predictdf$race==2,]
  sumdist<-whites-blacks
  diffdf2<-summarize.distribution2(sumdist)
  diffdf2$dimension<-thisdimension
  ###DIFFERENCES
  whites<-simMat[predictdf$race2==1,]
  blacks<-simMat[predictdf$race2==2,]
  tmpdist<-whites-blacks
  diffdf<-summarize.distribution2(tmpdist)
  diffdf$dimension<-thisdimension
  diffdf
  #returnlist
  list(
    avgdf=avgdf,
    diffdf=diffdf,
    diffdf2=diffdf2
  )
})

#########################################################
#########################################################

#write these out for graph
setwd(filesdir); dir()

#avgs
avgdf<-lapply(fulloutput,function(x) x$avgdf) %>%
  rbind.fill
write.csv(
  avgdf,
  "q_avghat.csv",
  row.names=F
)

#diffs
diffdf<-lapply(fulloutput,function(x) x$diffdf) %>%
  rbind.fill
diffdf$difftype<-"diff"
diffdf2<-lapply(fulloutput,function(x) x$diffdf2) %>%
  rbind.fill
diffdf2$difftype<-"avg"
diffdf<-rbind.fill(
  diffdf,
  diffdf2
)
write.csv(
  diffdf,
  "q_diffhat.csv",
  row.names=F
)

#########################################################
#########################################################

#PREDICT FOR MRP-CELLS
#post-stratifying using census data

#load the census data
setwd(datadir)
cpopdf<-read.csv(
  "cpopdf.csv",
  stringsAsFactors = F
)
cpopdf$X<-NULL

#match our vars
names(cpopdf)<-str_replace(
  names(cpopdf),
  "\\.mrp$",""
)

#make sure all vars are in?
tmpdf<-prefmods[[1]]@frame
tmpnames<-str_replace(names(tmpdf),"factor\\(","") %>%
  str_replace("\\)$","") 
tmpnames<-sapply(tmpnames,function(x) str_split(x,"X")[[1]]) %>%
  unlist %>% unique
tmp<-tmpnames%in%names(cpopdf)
tmp<-tmp | tmpnames%in%c("aff","question")
prez<-tmpnames[tmp]
miss<-tmpnames[!tmp]
if(length(miss)!=0)
  stop('missing vars')

#loop through and predictdf
tmpseq.i<-seq_along(prefmods)
fulloutput<-lapply(tmpseq.i,function(i) {
  
  #i<-1
  print(i)
  ##########
  
  thismod<-prefmods[[i]]
  thismodname<-names(prefmods)[i]
  thisdimension<-str_extract(
    thismodname,
    "all|anxiety|punitive|mistrust"
  )
  
  #where is our predictdf from?
  predictdf<-cpopdf
  
  #now, add dem-based interactions
  loopdf<-expand.grid(
    v1=c(
      "race",
      "race2"
    ),
    v2=c(
      names(cpopdf)[!names(cpopdf)%in%c("race","race2")]
    ),
    stringsAsFactors=F
  )
  tmpseq.i<-1:nrow(loopdf)
  for(i in tmpseq.i) {
    #print(i)
    #i<-1
    thisrow<-loopdf[i,]
    newname<-paste0(
      thisrow$v1,
      "X",
      thisrow$v2
    )
    #make these all distinct categories
    predictdf[[newname]]<-paste0(
      predictdf[[thisrow$v1]],
      "_",
      predictdf[[thisrow$v2]]
    )
  }
  
  ######
  
  
  #FIX NEW LEVELS ISSUE
  
  #loop through all ranef levels
  #make sure they don't appear in the data
  ranefs<-names(ranef(thismod))
  ranefs<-ranefs[ranefs%in%names(predictdf)]
  tmpseq.j<-seq_along(ranefs)
  logicals<-lapply(tmpseq.j,function(j) {
    ##
    #j<-1
    ranefs[j]
    modlevels<-row.names(
      ranef(thismod)[[ranefs[j]]]
    )
    predlevels<-predictdf[[ranefs[j]]] %>%
      unique
    outlevels<-predlevels[!predlevels%in%modlevels]
    ##rows to discard
    predictdf[[ranefs[[j]]]]%in%outlevels
  })
  badrows<-Reduce(f="|",logicals)
  predictdf<-predictdf[!badrows,]
  
  #add missing refs to rpedictdf
  medrefs<-get.medianrefs(thismod)
  ranefs<-names(ranef(thismod))
  tmp<-ranefs%in%names(predictdf) |
    str_detect(ranefs,"X")
  ranefs_miss<-ranefs[!tmp]
  medrefs_miss<-medrefs[ranefs_miss]
  tmpseq.i<-seq_along(medrefs_miss)
  for(i in tmpseq.i) {
    newvar<-names(medrefs_miss)[i]
    newval<-medrefs_miss[[i]]
    predictdf[[newvar]]<-newval
  }
  
  #predict!
  predictdf$dimension<-thisdimension
  predictdf$raceXquestion<-paste0(
    predictdf$race,"_",
    predictdf$question
  )
  
  #single prediction
  spredictdf<-predictdf
  spredictdf$mu<-predict(
    thismod,
    newdata=spredictdf,
    type='response'
  )
  
  #this is very computationally-intensive, 
  #so I do not run this loop; but if 
  #95\% CI's are desired, it could be run
  #best done on the cloud or summat like that
  # #many predictions, w/ CI's
  # mpredictdf<-mpredictdf
  # #registerDoParallel()
  # #getDoParWorkers()
  # tmpoutput<-merTools::predictInterval(
  #   thismod,
  #   newdata=mpredictdf,
  #   which="full",
  #   level=0.95,
  #   type='probability',
  #   include.resid.var=F,
  #   n.sims=10,
  #   returnSims=T
  # )
  # #registerDoSEQ()
  # simMat<-attr(tmpoutput,"sim.results")
  # simMat<-apply(simMat,1,thismod@resp$family$linkinv)
  # simMat<-t(simMat)
  # tmpdf<-data.frame(simMat)
  # mpredictdf<-cbind(mpredictdf,tmpdf)
  # mpredictdf<-gather(
  #   mpredictdf,
  #   rep,
  #   mu,
  #   X1:X10
  # )
  # mpredictdf$rep<-str_replace(mpredictdf$rep,"X","")
  # mpredictdf
  
  returnlist<-list(
    spredictdf=spredictdf#,
    #mpredictdf=mpredictdf
  )
  
})

#loop through these predictions, get sums
tmpseq.i<-seq_along(fulloutput)
pstratdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  print("####")
  print(i)
  print("####")
  #####
  predictdf<-fulloutput[[i]]$spredictdf
  #summarize in different ways
  thisdf<-expand.grid(
    state_alpha2=c(
      unique(predictdf$state_alpha2),
      "all"
    ),
    year=c(
      unique(predictdf$year),
      "all"
    ),
    #can summarize for white/black/other,
    #whether or not it has been modeled this way
    race=c(
      unique(predictdf$race),
      "all"
    ),
    stringsAsFactors=F
  )
  #loop through
  tmpseq.j<-1:nrow(thisdf)
  thisdf$mu<-sapply(tmpseq.j,function(j) {
    #j<-1000
    if(j%%100==0) {
      print(
        paste(j,"of",length(tmpseq.j))
      )
    }

    ######
    thisrow<-thisdf[j,]
    #create logicals from this list
    tmpseq.k<-seq_along(names(thisrow))
    logicals<-lapply(tmpseq.k,function(k) {
      #k<-1
      var<-names(thisrow)[k]
      val<-thisrow[,k]
      if(val=="all") {
        rep(T,nrow(predictdf))
      } else {
        predictdf[[var]]==val
      }
    })
    #combine these logicals
    tmp<-Reduce("&",logicals)
    mypredictdf<-predictdf[tmp,]
    #get the average response,
    #across the pop, weighted accordingly
    weighted.mean(
      mypredictdf$mu,
      mypredictdf$pop
    )
  })
  #return predictions
  thisdf$dimension<-unique(
    predictdf$dimension
  )
  thisdf
}) %>% rbind.fill

#save out
setwd(filesdir); dir()
write.csv(
  pstratdf,
  "pstratdf.csv",
  row.names=F
)


#########################################################
#########################################################

#OUTPUTS POSTSTRAT DATA

#output for regressions and comparison w/ Enns' method
#we want loess/ipolated of all raceXstate combinations
loopdf<-expand.grid(
  state_alpha2=unique(pstratdf$state_alpha2),
  race=unique(pstratdf$race),
  dimension=unique(pstratdf$dimension),
  stringsAsFactors=F
)
tmpseq.i<-1:nrow(loopdf)
podf<-lapply(tmpseq.i,function(i) {
  #i<-1
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )
  ####
  tmp<-pstratdf$state_alpha2==loopdf$state_alpha2[i] &
    pstratdf$race==loopdf$race[i] &
    pstratdf$dimension==loopdf$dimension[i] &
    pstratdf$year!="all"
  thisdf<-pstratdf[tmp,]
  ###
  #add missing years
  yrs<-min(thisdf$year):max(thisdf$year)
  missyrs<-yrs[!yrs%in%thisdf$year]
  extradf<-data.frame(
    year=missyrs
  )
  thisdf<-rbind.fill(
    thisdf,
    extradf
  )
  thisdf<-thisdf[order(thisdf$year),]
  ###
  #ipolate points
  thisdf$mu.i<-zoo::na.approx(
    thisdf$mu,
    na.rm=F
  )
  ###
  #loess
  m.tmp<-loess(
    data=thisdf,
    mu ~ year
  )
  thisdf$mu.loess<-predict(
    m.tmp,
    newdata=thisdf
  )
  ###
  #retrun
  returndf<-data.frame(
    state_alpha2=loopdf$state_alpha2[i],
    year=thisdf$year,
    race=loopdf$race[i],
    dimension=loopdf$dimension[i],
    i=thisdf$mu.i,
    loess=thisdf$mu.loess,
    stringsAsFactors=F
  )
  #return long
  gather(
    returndf,
    var,
    val,
    i:loess
  )
}) %>% rbind.fill

#make podf fully wide
#for use w/ regressions
tmp<-podf$race=="1"
podf$race[tmp]<-"white"
tmp<-podf$race=="2"
podf$race[tmp]<-"black"
tmp<-podf$race=="3"
podf$race[tmp]<-"other"
tmp<-podf$race=="all"
podf$race[tmp]<-"allraces"
podf$dimracevar<-paste0(
  podf$dimension,
  "_",
  podf$race,
  "_",
  podf$var
)
podf$dimension<-
  podf$var<-
  podf$race2<-
  podf$race<-NULL

#finalize as widedf
podf<-spread(
  podf,
  dimracevar,
  val
)

#this is outputdir
setwd(outputdir)
write.csv(
  podf,
  "pubopinion.csv",
  row.names=F
)
