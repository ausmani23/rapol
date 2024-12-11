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
require(lmtest)
require(boot)
require(plm)
require(MASS)

#set dirs
rootdir<-find_root(
  criterion=has_file('_rapol.Rproj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#load helper functions
setwd(dcodedir)
source('beofunctions.R')
source('beofunctions2.R')

#set seed
set.seed(23)
reps<-1000
boot.reps<-10

#########################################################
#########################################################

#PRELIMINARIES

#LOAD BEODF
setwd(datadir); dir()
beodf<-read.csv(
  'beodf.csv',
  stringsAsFactors=F
)
head(beodf)

#LOAD REDISTRICTING INFO
setwd(datadir)
redf<-read.csv(
  'redf.csv',
  stringsAsFactors=F
)
redf$X<-NULL

###define the treatment period
#two dummy variables
#one for before/after point of treatment
#one for the group of states that were treated/not treated
#DinD is the interaction of the two (i.e., treatment period
#in the treated states)
t.year<-1990
t.year<-t.year+1 #because we are using lags of IV's rather than leads of DVs
beodf$post.t<- as.numeric(beodf$year>t.year)
treated<-c("AL","FL","GA","LA","MS","NC","SC","TX","VA","NY")
beodf$t<-as.numeric(beodf$state_alpha2%in%treated)
beodf$t.post.t<-beodf$t * beodf$post.t

#eubanks and fresh ideniti

#we can also interact the treatment variables by 
#the magnitude of the treatment effect, 
#which will be the number of new districts created
redf<-redf[,c("statename","newdistricts_pctalld92")]
names(redf)<-c("statename","newds")

#add all other states, and assume all excluded are 0
allstates<-beodf$statename %>% unique %>% as.character
sum(redf$statename%in%allstates)==length(redf$statename) #names harmonize
newstates<-allstates[!allstates%in%redf$statename]
tmpdf<-data.frame(statename=newstates,newds=0)
redf2<-rbind.fill(redf,tmpdf)

#add this data to beodf
beodf<-merge(
  beodf,
  redf2,
  by=c("statename"),
  all=T
)

#this will be useful, below
median.redistricting<-median(
  redf2$newds[redf2$newds>0 & !is.na(redf2$newds)]
)

#add year2, for estaimation of year trend
beodf$year2<-beodf$year-1990 

#output for graph of beo rep
tmpvars<-c(
  "state_alpha2",
  "year",
  "beopct_all",
  "post.t",
  "t",
  "t.post.t"
)
tmpdf<-beodf[!is.na(beodf$beopct_all),tmpvars]
setwd(filesdir); dir()
write.csv(
  tmpdf,
  '03_dind_beodf_dd.csv',
  row.names=F
)

########################################################
########################################################

#last, to use the bootstrap proceudre, we need to define 
#a function, getbootstats(), which is written to be passed to boot
#store<-NA
getbootstats<-function(x,i,mydf,myformula,b.ols) {  
  #x<-unique(thisdf$state_alpha2)
  #i<-sample(1:length(x),replace=T)
  #mydf<-thisdf[these.obs,]
  #myformula<-thisformula
  #b.ols<-b.ols
  #split df into small dfs, based on these indices
  dfs.list<-lapply(i,function(thisi) subset(mydf,state_alpha2==x[thisi])) 
  #put this list into one giant df
  fulldf<-do.call("rbind",dfs.list)
  #run the desired regression
  m.boot<-lm(data=fulldf,formula=myformula)
  #get the desired stats
  coefs<-coeftest(m.boot,vcov(m.boot))
  myrow<-coefs[str_detect(row.names(coefs),"t.post.t")]
  b.boot<-myrow[1]
  b.se.boot<-myrow[2]
  t.boot<-abs(b.boot-b.ols)/b.se.boot
  # if(is.na(t.boot))
  #   store<<-fulldf
  return(t.boot)
}

########################################################
########################################################

#IDENTIFY VARS
#get dddf dataset
setwd(metadir); dir()
ddvarsdf<-read.csv(
  '03_dind_ddvarsdf.csv',
  stringsAsFactors=F
)
dvs<-ddvarsdf$varname[ddvarsdf$type=="dv"]
ivs<-ddvarsdf$varname[ddvarsdf$type=="iv"]
idvars<-c("state_alpha2","year")
treatment.vars<-c(
  #standard
  "post.t",
  "t",
  "t.post.t",
  #for interaction
  "newds"
)

########################################################
########################################################

#GATHER MODS

#loop through mods
setwd(metadir); dir()
ddmodsdf<-read.csv(
  '03_dind_ddmodsdf.csv',
  stringsAsFactors=F
)

#full space
ddmodsdf<-expand.grid(
  dv=dvs,
  spec=c(ddmodsdf$spec,'statetrend'),
  method=c(
    "normal",
    "aggregation",
    "bootstrap"
  ),
  sample=c(
    'full',
    'jimcrow'
  ),
  stringsAsFactors=F
) 

#identify preferred mod
tmp<-ddmodsdf$dv%in%c(
  'officers_pcap',
  'imprt_t_jur',
  'welfbenefits'
) &
  ddmodsdf$spec=='divtrend' &
  ddmodsdf$method=='normal' &
  ddmodsdf$sample=='full'
prefmodsdf<-ddmodsdf[tmp,]

#don't estimate the whole space
#loop through each condition
#keeping all other choices from prefmods
tmpseq.i<-seq_along(names(prefmodsdf))
ddmodsdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  thisname<-names(prefmodsdf)[i]
  othnames<-names(prefmodsdf)[-i]
  #take thisname from modsdf
  #take othnames from prefmodsdf/tmpdf
  thisperm<-lapply(thisname,function(x)
    unique(ddmodsdf[[x]])
  )
  othperms<-lapply(othnames,function(x)
    unique(prefmodsdf[[x]])
  )
  allperms<-append(
    thisperm,
    othperms
  )
  #return this
  returndf<-expand.grid(allperms,stringsAsFactors = F)
  names(returndf)<-c(thisname,othnames)
  returndf
}) %>% rbind.fill %>% unique

# #add custom rows
# newrow<-data.frame(
#   dv='welfbenefits',
#   spec='controls',
#   method='normal'
# )
# ddmodsdf<-rbind(ddmodsdf,newrow) %>% unique

#also estimate the key mods by aggregation and bootstrap
ddmodsdf$mname<-apply(
  ddmodsdf,1,paste0,collapse="."
)

########################################################
########################################################

#GENERATE FORMULAS

tmpseq.i<-1:nrow(ddmodsdf)
ddforms<-lapply(tmpseq.i,function(i) {
  
  #i<-2
  thisrow<-ddmodsdf[i,]
  this.spec<-thisrow$spec
  thisdv<-thisrow$dv
  
  #lhs
  if(this.spec=="diff") {
    lhs<-paste0("D.",thisdv)
  } else {
    lhs<-thisdv
  }
  
  #rhs
  #keyterms
  if(this.spec=="simple") {
    ddterms<-c("post.t","t","t.post.t") %>%
      paste0(collapse=" + ")
    idterms<-c("")
  } else {
    ddterms<-c("t.post.t")
    idterms<-c("factor(state_alpha2)","factor(year)") %>%
      paste0(collapse=" + ")
  }
  if(this.spec%in%c("newds")) {
    ddterms<-c("t.post.t:newds")
  }
  #controls
  tmp<-ddvarsdf$class%in%c("control")
  controls<-ddvarsdf$varname[tmp]
  if(
    this.spec%in%c(
      "simple",
      "fes",
      "diff",
      "newds",
      "divtrend",
      "regtrend",
      "statetrend"
    )
  ) {
    controlterms<-c("")
  } else if (this.spec=="lags") {
    controlterms<-c(
      paste0("L.",thisdv)#,
      #paste0("L.",controls)
    ) 
  } else if (this.spec=="controls") {
    controlterms<-
      paste0("L.",controls) %>%
      paste0(collapse=" + ")
  }
  
  #default is divisionXyear trends
  if(this.spec%in%c(
    "divtrend",
    "diff",
    "newds",
    "lags",
    "controls")
  ) {
    controlterms<-paste0(
      controlterms," + year:division"
    )
  } else if(this.spec=="regtrend") {
    controlterms<-paste0(
      controlterms," + year:region"
    )
  } else if(this.spec=='statetrend') {
    controlterms<-paste0(
      controlterms," + year2:factor(state_alpha2)"
    )
  }
  
  
  #put form together
  rhs<-paste(
    ddterms,
    idterms,
    controlterms,
    sep=" + "
  )
  
  #get rid of extra + signs
  rhs<-str_replace_all(rhs,"\\+\\s+\\+","+ ") %>%
    str_replace("\\s+\\+\\s+$","") %>%
    str_replace("^\\s+\\+\\s+","")
  
  #put the formula togeehter
  thisform<-paste(
    lhs,"~",rhs
  ) %>% as.formula
  thisform
  
})

names(ddforms)<-ddmodsdf$mname

########################################################
########################################################

#GENERATE DF 
#for each form

ddsampsdf <- ddmodsdf
ddsampsdf$sampname<-apply(
  ddsampsdf,1,paste0,collapse="."
)
tmpseq.i<-1:nrow(ddsampsdf)
ddsamps<-lapply(tmpseq.i,function(i) {
  #get cols
  #i<-21
  print(i)
  thisrow<-ddsampsdf[i,]
  thisdv<-thisrow$dv
  thisspec<-thisrow$spec
  thismethod<-thisrow$method
  thissample<-thisrow$sample
  #this is the core df
  fulldf<-beodf
  #if not, normal
  #these are the cols we want
  allforms<-ddforms[ddmodsdf$dv==thisdv & ddmodsdf$spec==thisspec]
  allvars<-lapply(allforms,all.vars) %>%
    unlist %>%
    unique
  #this gives us rows
  tmp<-allvars%in%names(fulldf)
  if(sum(!tmp)>0) {
    print(allvars[!tmp])
    stop('dont have allvars')
  }
  tmprows<-complete.cases(fulldf[,allvars]) &
    beodf$year<=1996 & #cut analysis in 1996
    beodf$year!=t.year #exclude treatment year
  if(thissample=='jimcrow') {
    #limit to the jim crow states
    tmprows<-tmprows & 
      beodf$statename%in%c(
        ##mulroy and katzenlson, p. 606
        'Missouri',
        'Arkansas',
        'Louisiana',
        'Oklahoma',
        'Texas',
        'Alabama',
        'Kentucky',
        'Mississippi',
        'Tennesee',
        'Delaware',
        'Florida',
        'Georgia',
        'Marlyand',
        'North Carolina',
        'South Carolina',
        'Virginia',
        'West Virginia',
        'Arizona',
        'Kansas',
        'New Mexico'
      )
  }
  #these are extra vars
  idvars<-c(
    "state_alpha2",
    "year"
  )
  extravars<-c(
    'post.t'
  )
  tmpcols<-c(
    idvars,
    extravars,
    allvars
  ) %>% unique
  tmpcols<-tmpcols[tmpcols%in%names(fulldf)]
  #so this is the df
  thisdf<-fulldf[tmprows,tmpcols]
  #but if we are aggregating, aggregate!
  if(thismethod=="aggregation") {
    tmplist<-list(thisdf$post.t,thisdf$state_alpha2)
    thisdf<-aggregate(thisdf,by=tmplist,function(x) {
      if (class(x)=="numeric" | class(x)=="integer") {
        y<-mean(x,na.rm=T)
      } else {
        y<-unique(x)
      }
      return(y)
    })
  }
  #return
  thisdf
})
names(ddsamps)<-ddsampsdf$sampname

ddsampsinfodf<-lapply(tmpseq.i,function(i) {
  #i<-1
  tmpdf<-ddsamps[[i]]
  data.frame(
    sampname=names(ddsamps)[i],
    N=nrow(tmpdf),
    N.states=length(unique(tmpdf$state_alpha2)),
    range=paste0(
      min(tmpdf$year),"-",max(tmpdf$year)
    )
  )
}) %>% rbind.fill

ddsampsdf<-merge(
  ddsampsdf,
  ddsampsinfodf,
  by="sampname"
)

########################################################
########################################################

#ESTIMATE AND RETURN TARGET COEF

this.sequence<-seq_along(ddforms)
tmpoutput<-lapply(this.sequence,function(i) {
  #i<-2
  #progress
  print(
    paste(
      "Estimating model",
      i,"of",max(this.sequence)
    )
  )
  #get params
  thisform<-ddforms[[i]]
  thisdv<-ddmodsdf$dv[i]
  thismethod<-ddmodsdf$method[i]
  this.spec<-ddmodsdf$spec[i]
  this.sample<-ddmodsdf$sample[i]
  thismname<-ddmodsdf$mname[i]
  tmprow<-ddsampsdf$dv==thisdv & 
    ddsampsdf$method==thismethod &
    ddsampsdf$spec==this.spec &
    ddsampsdf$sample==this.sample
  this.sampname<-ddsampsdf$sampname[tmprow]
  thisdf<-ddsamps[[this.sampname]]
  
  if(thismethod%in%c("normal","agg")) {
    
    #if normal, straightforward
    m<-plm(
      data=thisdf,
      formula=thisform,
      index=c(
        "state_alpha2",
        "year"
      ),
      model="pooling"
    )
    #get coefs
    coefs<-m$coefficients
    thisvcov<-vcovHC(
      m,
      type="HC1",
      cluster="group"
    )
    coefs.tested<-coeftest(m,thisvcov)
    #multiplier? 
    if(this.spec=="newds") {
      multiplier<-median.redistricting
    } else {
      multiplier<-1
    }
    #pull out coef of interest
    thevar.regex<-paste0("t.post.t")
    thisrow<-str_detect(row.names(coefs.tested),thevar.regex)
    term<-row.names(coefs.tested)[thisrow]
    est<-coefs.tested[thisrow,"Estimate"]
    se<-coefs.tested[thisrow,"Std. Error"]
    tval.col<-str_detect(colnames(coefs.tested),"t.value")
    t<-coefs.tested[thisrow,tval.col]
    pval<-coefs.tested[thisrow,"Pr(>|t|)"]
    #multiply by iv.sds
    est<-est*multiplier
    se<-se*multiplier
    #now compute
    est.min<-est-1.96*se
    est.max<-est+1.96*se
    #returnrow
    returnrow<-data.frame(
      iv="t.post.t",
      term,
      mu=est,
      mu.min=est.min,
      mu.max=est.max,
      se,
      pval,
      t,
      stringsAsFactors=F
    )
    returnrow$type<-"shortrun"
    if(this.spec=="lags") {
      #add longrun, which is pref
      #all ests involving this var
      tmprows<-str_detect(names(coefs),"t.post.t")
      iv.terms<-names(coefs)[tmprows]
      #all ests with dv
      tmprows<-str_detect(names(coefs),thisdv)
      lagdv.terms<-names(coefs)[tmprows]
      #get the longrun estimate
      means<-c(
        coefs[lagdv.terms],
        coefs[iv.terms]
      )
      #get the vcov matrix
      new.vcov<-thisvcov #from above
      rows<-row.names(new.vcov)%in%c(lagdv.terms,iv.terms)
      cols<-colnames(new.vcov)%in%c(lagdv.terms,iv.terms)
      vcov.useme<-new.vcov[rows,cols]
      #vcov needs to be ordered in the same way as the means
      new.order<-match(names(means),row.names(vcov.useme))
      vcov.useme<-vcov.useme[new.order,new.order]
      #sample from the multivariate distribution defined here
      draws<-mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
      numerator<-apply(draws[,iv.terms] %>% as.matrix,1,sum) #sum iv terms
      denominator<- 1 - apply(draws[,lagdv.terms] %>% as.matrix,1,sum) #sum dv terms
      lrm.distribution<-numerator/denominator
      #put this distribution in meaningful units
      lrm.distribution<-lrm.distribution * multiplier
      returnrow_tmp<-summarize.distribution2(lrm.distribution)
      #return this info (but not w/ lag)
      returnrow_tmp$iv<-"t.post.t"
      returnrow_tmp$type<-"longrun"
      returnrow<-rbind.fill(
        returnrow,
        returnrow_tmp
      )
    }
  } else {
    
    #if this is a bootstrapped procedure, start here
    m.ols<-lm(
      data=thisdf,
      formula=thisform,
      na.action=na.exclude
    )
    these.obs<-!is.na(residuals(m.ols))
    #use standard vcov; duflo (2004) does not say robust SE's
    coefs<-coeftest(
      m.ols,
      vcov(m.ols)
    ) 
    tmprow<-str_detect(row.names(coefs),"t.post.t")
    thiscoef<-coefs[tmprow]
    t.ols<-abs(thiscoef[3])
    b.ols<-thiscoef[1]
    #now we need to block-bootstrap
    #and examine the distributions
    t.distribution<-boot(
      data=unique(thisdf$state_alpha2[these.obs]),
      statistic=getbootstats,
      R=boot.reps,
      mydf=thisdf[these.obs,],
      myformula=thisform,
      b.ols=b.ols
    )
    tmp<-t.distribution$t
    if(sum(is.na(tmp))>0)
      print(paste0(thisformulaname," yields some NAs"))
    t.thresh<-quantile(tmp,c(0.99,0.95,0.90),na.rm=T)
    
    ###THIS GOES TO PVAL.CLASS
    if(t.ols>t.thresh[1]) {
      pval.class<-"at alpha=0.01"
    } else if(t.ols>t.thresh[2]) {
      pval.class<-"at alpha=0.05"
    } else if(t.ols>t.thresh[3]) {
      pval.class<-"at alpha=0.10"
    } else {
      pval.class<-"not sig"
    }
    
    #only other thing to return is b.ols
    #which we make the est
    est<-b.ols
    returnrow<-data.frame(
      iv="t.post.t",
      term="t.post.t",
      type="t.based",
      mu=b.ols,
      pval.class=pval.class,
      stringsAsFactors=F
    )
  }
  
  #returnrow
  returnrow$mname<-thismname
  
  #return 
  returnrow
  
}) %>% rbind.fill

#put together in df
ddestsdf<-rbind.fill(tmpoutput)
ddestsdf

#classify the shortrun pvals into pval class
tmp<-ddestsdf$type=="shortrun"
ddestsdf$pval.class[ddestsdf$pval<0.01 & tmp]<-"at alpha=0.01"
ddestsdf$pval.class[ddestsdf$pval>=0.01 & ddestsdf$pval<0.05 & tmp]<-"at alpha=0.05"
ddestsdf$pval.class[ddestsdf$pval>=0.05 & ddestsdf$pval<0.10 & tmp]<-"at alpha=0.10"
ddestsdf$pval.class[ddestsdf$pval>=0.10 & tmp]<-"not sig"
tmp<-is.na(ddestsdf$pval.class)
if(sum(tmp)>0)
  stop()

########################################################
########################################################

#MERGE
ddfinaldf<-merge(
  ddestsdf,
  ddmodsdf,
  by="mname"
) %>% unique

table(ddestsdf$mname)
table(ddmodsdf$mname)
ddmodsdf$mname%in%ddfinaldf$mname

#standardize
tmp<-sapply(dvs,function(thisdv) {
  #thisdv<-dvs[1]
  tmp<-ddsampsdf$dv==thisdv &
    ddsampsdf$method=="normal" &
    ddsampsdf$spec=='divtrend' &
    ddsampsdf$sample=='full'
  this.sampname<-ddsampsdf$sampname[tmp]
  thisdf<-ddsamps[[this.sampname]]
  tapply(
    thisdf[[thisdv]],
    thisdf$state_alpha2,
    sd,
    na.rm=T
  ) %>% mean(na.rm=T)
})
ddsdsdf<-data.frame(
  dv=dvs,
  dvsd=unname(tmp),
  stringsAsFactors=F
)

#merge it into plotdf
ddfinaldf<-merge(
  ddfinaldf,
  ddsdsdf,
  by="dv"
)
ddfinaldf$musd<-ddfinaldf$mu/ddfinaldf$dvsd
ddfinaldf$musd.max<-ddfinaldf$mu.max/ddfinaldf$dvsd
ddfinaldf$musd.min<-ddfinaldf$mu.min/ddfinaldf$dvsd

########################################################
########################################################

#save out
setwd(filesdir)
#save.image('ddmods.RData')
write.csv(
  ddfinaldf,
  '03_dind_resultsdf.csv',
  row.names=F
)
