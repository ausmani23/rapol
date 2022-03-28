olddir<-getwd()

#HELPER FUNCTIONS
#get the median fixed-effect

get.medianref<-function(ranefs) {
  #ranefs<-ranef(thismod)$personid
  randf<-data.frame(
    group=row.names(ranefs),
    ranef=ranefs[,1],
    stringsAsFactors=F
  )
  randf$dist<-abs(randf$ranef - median(randf$ranef))
  randf<-randf[order(randf$dist),]
  randf$group[1]
}

get.medianrefs<-function(m) {
  #m<-mods[[prefmod]]
  lapply(ranef(m),function(x) {
    get.medianref(x)
  })
}

summarize.distribution2<-function(ests.distribution) {
  #ests.distribution<-tmpdist
  #get quantiles
  quantiles<-quantile(
    ests.distribution,
    c(
      0.01,
      0.025,
      0.05,
      0.5,
      0.95,
      0.975,
      0.99
    )
  )
  #return mu, mu.min, mu.max
  mu<-quantiles["50%"]
  mu.min<-quantiles["2.5%"]
  mu.max<-quantiles["97.5%"]
  #and also a pval classification
  if(mu>=0) {
    if(quantiles["1%"]>0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["2.5%"]>0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["5%"]>0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  } else if(mu<0) {
    if(quantiles["99%"]<0) {
      pval.class<-'at alpha=0.01'
    } else if(quantiles["97.5%"]<0) {
      pval.class<-'at alpha=0.05'
    } else if(quantiles["95%"]<0) {
      pval.class<-'at alpha=0.10'
    } else {
      pval.class<-'not sig'
    }
  }
  # #se
  # #est of se explodes when lagdv coef is over 1
  # #so need something that is robust to that scenario
  # tmpboot<-boot(
  #   ests.distribution,
  #   f.sd,
  #   R=500
  # )
  # se<-mean(tmpboot$t)
  # se.q <- ( quantiles[3] - quantiles[1] ) / 4
  #SE is less rather than more helpful
  se<-NA 
  #se.q<-NA
  #get something like a two-sided pval test
  #pval<-ecdf(ests.distribution)(0)
  #pval<-ifelse(mu<0,(1-pval)*2,pval*2)
  pval<-NA
  #return me
  data.frame(
    mu,
    mu.min,
    mu.max,
    se=se,
    #se.q=se.q,
    pval=pval,
    pval.class=pval.class,
    stringsAsFactors=F
  )
}

########################################################################
########################################################################

#a function to add Latex stars to estimates, based on pval
#obviously, will return a string and not a number
apply.pvals<-function(ests,pvals,markers=c("**","*","+")) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals<0.1 & pvals>=0.05]<-
    paste0(
      ests[pvals<0.1 & pvals>=0.05],
      paste0("\\textsuperscript{",markers[3],"}")
    )
  ests[pvals<0.05 & pvals>=0.01]<-
    paste0(
      ests[pvals<0.05 & pvals>=0.01],
      paste0("\\textsuperscript{",markers[2],"}")
    )  
  ests[pvals<0.01]<-
    paste0(
      ests[pvals<0.01],
      paste0("\\textsuperscript{",markers[1],"}")
    )
  return(ests)
}

#test
apply.pvals(ests=rnorm(30),pvals=rep(c(0.11,0.06,0.01),10))
apply.pvals(ests=sprintf("%.2f",rnorm(30)),pvals=rep(c(0.11,0.06,0.01),10))

#get 
get.pvals.class<-function(pvals) {
  y<-NA
  y[pvals<0.01]<-"at alpha=0.01"
  y[pvals>0.01 & pvals<0.05]<-"at alpha=0.05"
  y[pvals>0.05 & pvals<0.10]<-"at alpha=0.10"
  y[pvals>0.10]<-"not sig"
  return(y)
}

#this function uses class rather than numeric pval
apply.pvals.class<-function(ests,pvals.class,markers=c("**","*","+")) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals.class=="at alpha=0.10"]<-
    paste0(
      ests[pvals.class=="at alpha=0.10"],
      paste0("\\textsuperscript{",markers[3],"}")
    )
  ests[pvals.class=="at alpha=0.05"]<-
    paste0(
      ests[pvals.class=="at alpha=0.05"],
      paste0("\\textsuperscript{",markers[2],"}")
    )  
  ests[pvals.class=="at alpha=0.01"]<-
    paste0(
      ests[pvals.class=="at alpha=0.01"],
      paste0("\\textsuperscript{",markers[1],"}")
    )
  return(ests)
}

#this function takes an est, pval, and se, 
#and gives something for a regtable
gimmie.est<-function(mu,pval,se,nrow=4) {
  tmp1<-apply.pvals(mu,pval)
  tmp2<-paste0("(",sprintf("%.3f",se),")")
  matrix(c(tmp1,tmp2),nrow=nrow)
}

#this function takes an est, a pval class and CI
#and gives something for a regtable
#to be used with simulated long run multipliers
gimmie.est2<-function(mu,pval.class=NULL,mu.min,mu.max,nrow=4) {
  # mu<-estdf$mu
  # pval.class<-estdf$pval.class
  # mu.min<-estdf$mu.min
  # mu.max<-estdf$mu.max
  # nrow<-2
  if(length(mu)==0) {
    tmp1<-tmp2<-""
  } else {
    if(!is.null(pval.class)) {
      tmp1<-apply.pvals.class(mu,pval.class)
    } else {
      tmp1<-format(round(mu,2),2)
    }
    tmp2<-paste0(
      "[",
      format(round(mu.min,2),2),
      ",",
      format(round(mu.max,2),2),
      "]"
    )
  }
  matrix(
    c(tmp1,tmp2),
    nrow=nrow
  )
}

########################################################################
########################################################################


setwd(olddir)