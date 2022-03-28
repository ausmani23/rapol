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
setwd(dcodedir)
source('beofunctions.R')
source('beofunctions2.R')


#set seed
set.seed(23)
reps<-1000

#########################################################
#########################################################

#LOAD
setwd(datadir); dir()
beodfraw<-read.csv(
  'beodf.csv',
  stringsAsFactors=F
)

#########################################################
#########################################################

#SET UP REG

#dv
dvs<-c(
  "incrt_t_jur",
  "imprt_t_jur",
  "officers_pcap"
)

#dvlags
dvlags<-c(
  "onelagdv",
  "twolagdv",
  "fourlagdv"
)

#spec
ivspec<-c(
  "bivariate",
  "onelagivs",
  "twolagivs"
)
tmp<-varnamesdf$class%in%c(
  "beo",
  "control"
)
beovars<-
  varnamesdf$varname[tmp & varnamesdf$class=="beo"]
controls<-
  varnamesdf$varname[tmp & varnamesdf$class=="control"]
specslist<-list(
  ###bivariate
  bivariate=list(
    main=paste0("L.",beovars),
    other=paste0("")
  ),
  ###1 lag
  onelagivs=list(
    main=paste0("L.",beovars),
    other=paste0("L.",controls)
  ),
  ###2 lag
  twolagivs=list(
    main=c(
      paste0("L.",beovars),
      paste0("L2.",beovars)
    ),
    other=c(
      paste0("L.",controls),
      paste0("L2.",controls)
    )
  )
)

#lnrealincpc is collinear at multilags, so remove 
tmp<-specslist$twolagivs$other=="L2.lnrealinc_pc"
specslist$twolagivs$other[tmp]<-""

#########################################################
#########################################################

#GET FORMS

modsdf<-expand.grid(
  dv=dvs,
  dvlag=dvlags,
  ivspec=ivspec,
  stringsAsFactors=F
)
modsdf$mname<-apply(
  modsdf,1,paste0,collapse="."
)

tmpseq.i<-1:nrow(modsdf)
forms<-lapply(tmpseq.i,function(i) {
  #i<-1
  #get params
  thisrow<-modsdf[i,]
  thisdv<-thisrow$dv
  ####FORMULA
  ##LHS
  lhs<-thisdv
  ##RHS
  #year fe
  thisfe.yr<-"factor(year)"
  #lagdv
  if(thisrow$dvlag=="onelagdv") {
    thislagdv<-paste0("L.",thisrow$dv)
  } else if(thisrow$dvlag=="twolagdv") {
    thislagdv<-paste0(c("L.","L2."),thisrow$dv) %>%
      paste0(collapse=" + ")
  } else if(thisrow$dvlag=="fourlagdv") {
    thislagdv<-paste0(c("L.","L2.","L3.","L4."),thisrow$dv) %>%
      paste0(collapse=" + ")
  }
  #ivs
  mainiv<-specslist[[thisrow$ivspec]]$main %>% 
    paste0(collapse=" + ")
  otherivs<-specslist[[thisrow$ivspec]]$other %>%
    paste0(collapse=" + ")
  #TOGETHER
  rhs<-paste(
    thisfe.yr,
    thislagdv,
    mainiv,
    otherivs,
    sep=" + "
  )
  #get rid of extra + signs
  rhs<-str_replace_all(rhs,"\\+\\s+\\+","+ ") %>%
    str_replace("\\s+\\+\\s+$","") %>%
    str_replace("^\\s+\\+\\s+","")
  #RETURN
  #put the formula togeehter
  thisform<-paste(
    lhs,"~",rhs
  ) %>% as.formula
  thisform
})

names(forms)<-modsdf$mname
 
#########################################################
#########################################################

#GET SAMPS
#each dv has a sample
sampsdf<-expand.grid(
  dv=unique(modsdf$dv),
  stringsAsFactors=F
)
sampsdf$sampname<-sampsdf$dv

tmpseq.i<-1:nrow(sampsdf)
samps<-lapply(tmpseq.i,function(i) {
  #i<-2
  thisdv<-sampsdf$dv[i]
  #this is the df
  fulldf<-beodfraw
  #these are the cols we want
  allforms<-forms[modsdf$dv==thisdv]
  allvars<-lapply(allforms,all.vars) %>%
    unlist %>%
    unique
  #this gives us rows
  tmprows<-complete.cases(fulldf[,allvars])
  #these are extra vars
  idvars<-c(
    "state_alpha2",
    "year"
  )
  extravars<-c(
    #####
  )
  tmpcols<-c(
    idvars,
    extravars,
    allvars
  ) %>% unique
  tmpcols<-tmpcols[tmpcols%in%names(fulldf)]
  #so this is the df
  thisdf<-fulldf[tmprows,tmpcols]
  #return
  thisdf
})
names(samps)<-sampsdf$sampname

sampinfodf<-lapply(tmpseq.i,function(i) {
  #i<-1
  tmpdf<-samps[[i]]
  data.frame(
    N=nrow(tmpdf),
    N.states=length(unique(tmpdf$state_alpha2)),
    range=paste0(
      min(tmpdf$year),"-",max(tmpdf$year)
    )
  )
}) %>% rbind.fill

#sampsdf
sampsdf<-cbind(
  sampsdf,
  sampinfodf
)
sampsdf

#########################################################
#########################################################

#GET SDS
#all ivs, dvs
#use incrt sample

allvs<-c(
  dvs,
  beovars,
  controls
)
sdsdf<-lapply(allvs,function(thisv) {
  #thisv<-"beopct_all"
  if(thisv%in%dvs) {
    thisdf<-samps[[thisv]] 
    myv<-thisv
  } else {
    thisdf<-samps$incrt_t_jur
    myv<-paste0("L.",thisv)
  }
  print(thisv)
  isdummy<-F
  avg<-mean(thisdf[[myv]],na.rm=T)
  if(isdummy) {
    sd<-rng<-1
  } else {
    sd<-tapply(
      thisdf[[myv]],
      thisdf$state_alpha2,
      sd,na.rm=T
    ) %>% mean(na.rm=T) 
    rng<-tapply(
      thisdf[[myv]],
      thisdf$state_alpha2,
      function(x) {
        diff(
          quantile(x,c(0.2,0.8),na.rm=T)
        ) %>% abs
      } 
    ) %>% mean(na.rm=T)
  }
  data.frame(
    var=thisv,
    avg=avg,
    sd=sd,
    range=rng,
    stringsAsFactors=F
  )
}) %>% rbind.fill

#########################################################
#########################################################

#RUN REGS
this.sequence<-seq_along(forms)
mods<-lapply(this.sequence,function(i) {
  #i<-1
  #progress
  print(
    paste(
      "Estimating model",
      i,"of",max(this.sequence)
    )
  )
  #get params
  thisform<-forms[[i]]
  thisdv<-modsdf$dv[i]
  thisdf<-samps[[thisdv]]
  #estimate
  m.tmp<-plm(
    data=thisdf,
    formula=thisform,
    index=c(
      "state_alpha2",
      "year"
    ),
    model="within"
  )
})
names(mods)<-modsdf$mname

#########################################################
#########################################################

#GET ESTS

#load lmtest
require(lmtest)

this.sequence<-seq_along(mods)
tmpoutput<-lapply(this.sequence,function(i) {
  #i<-1
  #progress
  print(
    paste(
      "Getting results from model",
      i,"of",max(this.sequence)
    )
  )
  #get params
  m<-mods[[i]]
  thismname<-modsdf$mname[i]
  thisdv<-modsdf$dv[i]
  
  #get robustvcov coefs
  coefs<-m$coefficients
  thisvcov<-vcovHC(
    m,
    type="HC1",
    cluster="group"
  )
  coefs.tested<-coeftest(m,thisvcov)
  
  #SHORT-RUN
  #all ivs, and lag dvs
  print("SR")
  shortrunvars<-c(thisdv,beovars,controls)
  sr.sequence<-seq_along(shortrunvars)
  shortrundf<-lapply(sr.sequence,function(j) {
    #j<-2
    #print(j)
    #get params
    thisiv<-shortrunvars[j]
    # #if this is the dv, no multiply
    if(thisiv==thisdv) {
      thisiv.sd<-1
    } else {
      thisiv.sd<-sdsdf$sd[sdsdf$var==thisiv]
    }
    if(length(thisiv.sd)!=1) stop("SD missing.")
    #get the var(s)
    thevar.regex<-paste0(
      "^(L([0-9]+)?\\.)?(D([0-9]+)?\\.)?",
      thisiv,
      "$"
    )
    thisrow<-str_detect(row.names(coefs.tested),thevar.regex)
    notvar.regex<-paste0("X",thisiv)
    thisrow<-thisrow & !str_detect(row.names(coefs.tested),notvar.regex)
    #if(sum(thisrow)>1)
    #stop(paste(thisiv,"is matching >1 terms"))
    #but don't match square terms
    #thisrow<-thisrow & !str_detect(row.names(coefs.tested),paste0(thisiv,"2"))
    #this gives term
    term<-row.names(coefs.tested)[thisrow]
    est<-coefs.tested[thisrow,"Estimate"]
    se<-coefs.tested[thisrow,"Std. Error"]
    tval.col<-str_detect(colnames(coefs.tested),"t.value")
    t<-coefs.tested[thisrow,tval.col]
    pval<-coefs.tested[thisrow,"Pr(>|t|)"]
    #multiply by iv.sds
    est<-est*thisiv.sd
    se<-se*thisiv.sd
    #now compute
    est.min<-est-1.96*se
    est.max<-est+1.96*se
    #return if was in the model
    if(sum(thisrow)>0) {
      returnrow<-data.frame(
        iv=thisiv,
        term,
        mu=est,
        mu.min=est.min,
        mu.max=est.max,
        se,
        pval,
        t,
        stringsAsFactors=F
      )
    } else {
      returnrow<-data.frame(
        iv=thisiv,
        mu=NA
      )
    }
    #return
    return(returnrow)
  }) %>% rbind.fill
  #identify
  shortrundf$type<-"shortrun"
  #chuck all nas
  shortrundf<-shortrundf[!is.na(shortrundf$mu),]
  
  ##########################################
  
  #LONG-RUN
  print("LR")
  longrunvars<-c(beovars,controls)
  lroutput<-lapply(seq_along(longrunvars),function(j) {
    #j<-2
    #print(j)
    #get params
    thisiv<-longrunvars[j]
    # #get sd
    #thisiv.sd<-1
    thisiv.sd<-sdsdf$sd[sdsdf$var==thisiv]
    if(length(thisiv.sd)!=1) stop("SD missing.")
    #did we detect var?
    thevar.regex<-paste0(
      "^(L([0-9]+)?\\.)?(D([0-9]+)?\\.)?",
      thisiv,
      "$"
    )
    tmprows<-str_detect(names(coefs),thisiv)
    if(sum(tmprows)==0) {
      returnrow<-data.frame(
        mu=NA
      )
    } else {
      #all ests involving this var
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
      lrm.distribution<-lrm.distribution * thisiv.sd
      returnrow<-summarize.distribution2(lrm.distribution)
      #return this info (but not w/ lag)
      returnrow$iv<-thisiv
    }
    return(returnrow)
  })
  longrundf<-rbind.fill(lroutput)
  #identify as longrun
  longrundf$type<-"longrun"
  #get rid of this when not applicable
  longrundf<-longrundf[!is.na(longrundf$mu),]
  
  ##########################################
  
  #finalize
  thism.estsdf<-rbind.fill(shortrundf,longrundf)
  thism.estsdf$mname<-thismname
  thism.estsdf$seq<-i
  return(thism.estsdf)
})

#put together in df
estsdf<-rbind.fill(tmpoutput)
estsdf

#classify the shortrun pvals into pval class
tmp<-estsdf$type=="shortrun"
estsdf$pval.class[estsdf$pval<0.01 & tmp]<-"at alpha=0.01"
estsdf$pval.class[estsdf$pval>=0.01 & estsdf$pval<0.05 & tmp]<-"at alpha=0.05"
estsdf$pval.class[estsdf$pval>=0.05 & estsdf$pval<0.10 & tmp]<-"at alpha=0.10"
estsdf$pval.class[estsdf$pval>=0.10 & tmp]<-"not sig"
tmp<-is.na(estsdf$pval.class)
if(sum(tmp)>0)
  stop()

#########################################################
#########################################################

#GET FIT

this.sequence<-seq_along(mods)
fitdf<-lapply(this.sequence,function(i) {
  #i<-10
  #get params
  m<-mods[[i]]
  #track progress
  print(paste("Calc fit for model",i,"of",max(this.sequence)))
  thisrow<-calcfits(m)
}) %>% rbind.fill
fitdf$mname<-modsdf$mname

#########################################################
#########################################################

#PUT TOGETHER

#put model info (from specs)
#together with estimates
#and fitdf

mergelist<-list(modsdf,estsdf,fitdf)
finaldf<-Reduce(
  function(...)
    merge(..., by="mname", all=T),
  mergelist
)

#add sample info
finaldf<-merge(
  finaldf,
  sampsdf,
  by=c("dv")
)
head(finaldf)

#########################################################
#########################################################

#IDENTIFY PREFESTS

finaldf<-by(finaldf,finaldf$dv,function(df) {
  #df<-finaldf[finaldf$dv=="incrt_t_jur",]
  #trim
  rankvars<-c("mname","aic","bic","r2","adjr2")
  tmpdf<-unique(df[,rankvars])
  #rank
  tmpdf$aic.rank<-rank(tmpdf$aic)
  tmpdf$bic.rank<-rank(tmpdf$bic)
  tmpdf$r2.rank<-length(tmpdf$r2) + 1 - rank(tmpdf$r2)
  tmpdf$adjr2.rank<-length(tmpdf$adjr2) + 1 - rank(tmpdf$adjr2)
  #pref by AIC
  tmpdf$pref<-ifelse(tmpdf$aic.rank==1,T,F)
  #pref by BIC
  tmpdf$prefbic<-ifelse(tmpdf$bic.rank==1,T,F)
  #merge back in
  mergevars<-c("mname","pref","prefbic")
  merge(
    df,
    tmpdf[,mergevars],
    by="mname"
  )
}) %>% rbind.fill

#########################################################
#########################################################

#current mu's are semi-standardized
#add standardized estimates for display
tmp<-sdsdf$var%in%dvs
dvsd_df<-sdsdf[tmp,c("var","sd")]
names(dvsd_df)<-c("dv","dvsd")
finaldf<-merge(
  finaldf,
  dvsd_df
)
finaldf$musd<-finaldf$mu/finaldf$dvsd
finaldf$musd.min<-finaldf$mu.min/finaldf$dvsd
finaldf$musd.max<-finaldf$mu.max/finaldf$dvsd

#########################################################
#########################################################

#save out
setwd(filesdir); dir()
write.csv(
  finaldf,
  "03_dind_regresultsdf.csv",
  row.names=F
)

#save forms and other output
saveRDS(
  list(
    forms=forms,
    mods=mods,
    samps=samps,
    sdsdf=sdsdf
  )
  ,
  "03_dind_regstuff.RDS"
)

