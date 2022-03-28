#stared with this directory
olddir<-getwd()

########################################################################
########################################################################

#read in varnames df

setwd(metadir); dir()
varnamesdf<-read.csv(
  "03_dind_regvarsdf.csv",
  stringsAsFactors=F
)

ddvarnamesdf<-read.csv(
  "03_dind_ddvarsdf.csv",
  stringsAsFactors=F
)

ddmodnamesdf<-read.csv(
  "03_dind_ddmodsdf.csv",
  stringsAsFactors=F
)

########################################################################
########################################################################

#this is a generic conversion function
#given value, from/to, and a df
###(should be written to output 2 possiiblities, etc.)

getcode<-function(x,from,to,df,appx=T,allowdups=F) {
  
  # x<-"McCollum Amendment";
  # from<-"shortname";
  # to<-"amendtitle";
  # df<-plotdf
  # x<-"Labor Intensity"
  # from<-"propername"
  # to<-"order"
  # df<-varnamesdf
  
  ##############
  
  #get the row
  #if you pass me NA
  #return NA
  if(is.na(x)) {
    ###can't do anything
    y<-NA
  } else {
    ###else we can do something
    #convert a;; to lower, 
    #in case these are character matches
    match.from<-tolower(df[[from]])
    x<-tolower(x)
    #get matching rows
    tmp<-match.from==x
    if(sum(tmp)==1) {
      #if just one row, easy
      y<-df[[to]][tmp]
    } else if(sum(tmp)==0) {
      #if nothing matched,
      #this is probably NA
      y<-NA
      #if approx is on
      if(appx) {
        #but we can try converting 
        #the matching column to numeric
        #for appx matches of numbers
        tmpnum<-suppressWarnings(
          as.numeric(match.from)
        )
        tmp<-tmpnum==x & !is.na(tmpnum)
        if(sum(tmp)==1) 
          y<-df[[to]][tmp]
      }
    } else if(sum(tmp)>1) {
      #if more than 1 matches,
      #still return if they're all the same
      y<-df[[to]][tmp] %>%
        unique
      if(length(y)>1) {
        #if more than one matches
        #check if allow dups is on,
        #then we can do something
        #otherwise, don't match more than 1
        if(allowdups) {
          #return a vector
          #this can't be put in df w/o examination
          #because it won't be right length
          y<-df[[to]][tmp]
        } else {
          y<-NA #set back to NA
        }
      }
    }
  }
  
  ##############
  
  return(y)
}

########################################################################
########################################################################

getvarcode<-function(x,append=T,...) {
  #x<-"Derrick A"
  
  #######
  
  #check for std suffixes or prefixes
  
  prefsufs<-c(
    "L[0-9]?.",
    "D[0-9]?."
  )
  prefsufs.propername<-c(
    "Lag",
    "Diff"
  )
  prefsufs.regex<-paste0("(",paste0(prefsufs,collapse="|"),")")
  tmp<-str_detect(x,prefsufs.regex)
  if(tmp) {
    #extract suffix
    x.new<-str_replace_all(x,prefsufs.regex,"")
    y<-getcode(x.new,...)
    #y<-getcode(x.new,"varname","propername",varnamesdf)
    if(append) {
      myprefsufs<-str_extract_all(x,prefsufs.regex)[[1]]
      tmp<-lapply(myprefsufs,function(z) str_detect(z,prefsufs))
      myappender<-prefsufs.propername[sapply(tmp,which)] %>%
        paste(collapse=", ")
      y<-paste0(y," (",myappender,")")
    } 
  } else {
    y<-getcode(x,...) #nothing to do if no suffix
  }
  y
}

#additional wrappers
getvarname<-function(x) {
  getvarcode(x,append=F,"varname","propername",varnamesdf)
}
getvarorder<-function(x) {
  getvarcode(x,append=F,"varname","order",varnamesdf)
}
getvartype<-function(x) {
  getvarcode(x,append=F,"varname","type",varnamesdf)
}
getvar<-function(x) {
  getvarcode(x,append=F,"varname","varname",varnamesdf)
}

# getcfname<-function(x) {
#   getcode(x,"actual","pretty",cfnamesdf)
# }
# getcforder<-function(x) {
#   getcode(x,"actual","order",cfnamesdf)
# }

########################################################################
########################################################################



summarize.distribution2<-function(ests.distribution) {
  #ests.distribution<-lrm.distribution
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
  if(mu>0) {
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

#summarize runs
sumruns<-function(x,sep.me=":") {
  diffs <- c(1, diff(x))
  start_indexes <- c(1, which(diffs > 1))
  end_indexes <- c(start_indexes - 1, length(x))
  coloned <- paste(x[start_indexes], x[end_indexes], sep=sep.me)
  paste0(coloned, collapse=", ")
}

########################################################################
########################################################################
setwd(olddir)