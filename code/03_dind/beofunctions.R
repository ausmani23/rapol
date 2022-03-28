#stared with this directory
olddir<-getwd()

#########################################################

#helper functions
normalize<-function(x,na.rm=T)
  return((x-min(x,na.rm=na.rm))/(max(x,na.rm=na.rm)-min(x,na.rm=na.rm)))
#for diff apply, put na before, given that these are usually end of year counts
diff.apply<-function(var,index,n) {
  #var<-cbeodf$off_pcap
  #index<-cbeodf$id
  #n<-3
  output<-tapply(var,index,function(x) {
    if(length(x)<n) {
      y<-rep(NA,length(x))
    } else {
      y<-c(rep(NA,n),diff(x,n))
    }
  }) %>% unlist
  return(output) 
}

lag.apply<-function(var,index,n) 
  return(unlist(tapply(var,index,function(x) c(dplyr::lag(x,n))))) #lag auto generates NA's
lead.apply<-function(var,index,n) 
  return(unlist(tapply(var,index,function(x) lead(x,n)))) #lead auto generates NA's

#in case I want NA at the end
diff.apply.alt<-function(var,index,n) 
  return(unlist(tapply(var,index,function(x) c(diff(x,n),rep(NA,n))))) 

#########################################################
#########################################################

#interpolation, spitting out old in event of error
require(zoo)
ipolate<-function(x) { 
  output<-tryCatch(
    {
      x.i<-na.approx(x)  
      if (length(x.i)!=length(x)) {
        firstmatch<-min(which(x%in%x.i)) #index of first match
        lastmatch<-max(which(x%in%x.i)) #index of last match
        #return vector of original length, interpolated vals in middle  
        return(c(x[index(x)<firstmatch],x.i,x[index(x)>lastmatch])) 
      }
      return(x.i)
    },error=function(cond) {
      return(x)
    })
  return(output)
} #end function

#test
a<-c(1,2,3,4,NA,NA,10,11,12,NA,14,NA)
ipolate(a)

#########################################################

#a function to add Latex stars to estimates, based on pval
#obviously, will return a string and not a number
apply.pvals<-function(ests,pvals) {
  if(class(ests)=="numeric")
    ests<-sprintf("%.3f",ests)
  ests[pvals<0.1 & pvals>=0.05 & !is.na(pvals)]<-
    paste0(ests[pvals<0.1 & pvals>=0.05  & !is.na(pvals)],
           "\\textsuperscript{+}")
  ests[pvals<0.05 & pvals>=0.01  & !is.na(pvals)]<-
    paste0(ests[pvals<0.05 & pvals>=0.01  & !is.na(pvals)],
           "\\textsuperscript{*}")  
  ests[pvals<0.01  & !is.na(pvals)]<-
    paste0(ests[pvals<0.01  & !is.na(pvals)],
           "\\textsuperscript{**}")
  return(ests)
}

#test
apply.pvals(ests=rnorm(30),pvals=rep(c(0.11,0.06,0.01),10))
apply.pvals(ests=sprintf("%.2f",rnorm(30)),pvals=rep(c(0.11,0.06,0.01),10))

#function to classify pvals into the conventional categories
classify.pval<-function(pval) {
  if(pval<0.1 & pval>=0.05) {
    class<-"At alpha=0.1"
  } else if (pval<0.05 & pval>=0.01) {
    class<-"At alpha=0.05"
  } else if (pval<0.01) {
    class<-"At alpha=0.01"
  } else {
    class<-"Not at conventional levels"
  }
  return(class)
}

classify.pval(0.5)
classify.pval(0.09)

#wrapper for format
mydigits<-2
myformat<-function(x) {
  if(abs(x)<1) {
    mydigits<-2
  } else if(abs(x)<10) {
    mydigits<-2
  } else if(abs(x)<100) {
    mydigits<-1
  } else if(abs(x)<1000) {
    mydigits<-1
  }
  fmt<-paste0("%.",mydigits,"f")
  sprintf(fmt,x)
  #format(x,digits=mydigits,nsmall=mydigits-1)
}

myformat(0.0002)
myformat(63.453)
myformat(105.123)

myformats<-function(xs) sapply(xs,myformat)

myformats(c(3.234,234.2))

#########################################################


#strips a formula

stripf<-function(thisf) {
  str_replace_all(thisf,"\n","") %>%
    str_replace_all("\\s+"," ")
}

#########################################################

#quick helper functions
#for pretty labels on x-axis
toproper <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}

#########################################################

#get pretty and short names from the varnames
setwd(metadir)
varhelperdf<-
  read.csv("03_dind_regvarsdf.csv",stringsAsFactors=F); 
varhelperdf$X<-NULL
head(varhelperdf)

require(stringr)
getname<-function(thisvar,from="actual",to="pretty") {
  #thisvar<-"L.beopct_all"; from="actual"; to="pretty"
  thisrow<-varhelperdf[[from]]==thisvar
  if(sum(thisrow)==0) {
    if(str_detect(thisvar,"^L([0-9]?)\\.")) {
      actualvar<-str_replace(thisvar,"^L([0-9]?)\\.","")
      newvar<-getname(actualvar,from=from,to=to) #call function again
    } else if(str_detect(thisvar,"^D\\.")) {
      actualvar<-str_replace(thisvar,"^D\\.","")
      newvar<-getname(actualvar,from=from,to=to) #call function again
    } else {
      newvar<-NA
    } 
  } else {
    newvar<-varhelperdf[thisrow,to]
  }
  return(newvar)
}

getname("beopct_all")
getname("L.beopct_all")

#tells us how many lags are in this var
getlag<-function(varname) {
  #varname<-"L4.D.beopct_all"
  laginfo<-str_replace(varname,"^(L([0-9]+)?)\\..*$","\\1")
  if(laginfo=="L") {
    y<-1
  } else if (str_detect(laginfo,"L")) {
    y<-str_replace(laginfo,"L","") %>% 
      as.numeric
  } else {
    y<-NA
  }
  return(y)
}
getlag("L2.D.beopct_all")

#########################################################

#make the testnames pretty
testname<-c("llc","ips","madwu","hadri")
simplename<-c("LLC","IPS","Madwu","Hadri")
prettyname<-c("Levin, Lin and Chu (2002)","Im, Pesaran and Shin (2003)",
              "Maddala and Wu (1999)","Hadri (2000)")
unitroot.names<-matrix(c(testname,simplename,prettyname),ncol=3)
getsimplename<-function(testname) {
  return(unitroot.names[unitroot.names[,1]==testname,2])
}

#########################################################

#this is a helper function
#calculates the BIC for PLM objects
#and for LM objects, since my function
#doesn't seem to match the built-in function

calcfits<-function(m) {
  #m<-m.test
  #m<-m
  #is it a plm model?
  plm.fit<-class(m)[1]=="plm"
  #m<-m.cube
  #get residual sum of squares
  res<-m$residuals
  rss<-sum(res^2)
  #get n
  n<-nobs(m)
  #get k
  if(plm.fit) {
    k<-n-m$df.residual
  } else {
    k<-m$rank
  }
  #the stats
  df<-n-k #degrees of freedom
  w<-rep(1,n) #weights
  #log likelihood
  ll<-0.5 * (sum(log(w)) - n * 
               (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))
  df.ll<-k+1
  bic<- -2 * ll + log(n) * df.ll
  aic<- -2 * ll + 2 * df.ll 
  #bic<- n*log(rss/n)+k*log(n) #old way
  #aic<-2*k+n*log(rss) #old way
  #get summary object
  summary.m<-tryCatch(summary(m),error=function(e) "error")
  #temp fix for the plm problem
  if(summary.m[1]=="error") {
    print("summary error")
    r2<-adjr2<-NA
  } else {
    if(plm.fit) {
      r2<-summary.m$r.squared['rsq']
      adjr2<-summary.m$r.squared['adjrsq']
    } else {
      r2<-summary.m$r.squared
      adjr2<-summary.m$adj.r.squared
    }
  }
  #return
  thisrow<-data.frame(bic,aic,r2,adjr2,
                      stringsAsFactors=F)
  return(thisrow)  
}

#test
y<-rnorm(100)
x<-rnorm(100)
m.test<-lm(y ~ x)
BIC(m.test)
calcfits(m.test)

# 
# y<-rnorm(100)
# x<-rnorm(100)
# m.test<-lm(y ~ x) 
# 
# n<-100
# res<-m.test$residuals
# rss<-sum(res^2) 
# k<-3; df<-n-k; w<-rep(1,N) #params, dfs, weights
# ll<-0.5 * (sum(log(w)) - n *
#              (log(2 * pi) + 1 - log(n) + log(sum(w * res^2))))
# ll.stats<-logLik(m.test)
# abs(ll.stats-ll)==0 #same, prob is not here
# 
# bic.mine<-n*log(rss/n)+k*log(n) #formula from wikipedia
# bic.exact<- -2 * ll + log(n) * df #suggestions from comments
# bic.stats<-BIC(m.test) #using stats package
# abs(bic.mine-bic.stats) #mine was off
# abs(bic.exact-bic.stats) #this is still off, though
# 
# 
# 

#########################################################

# require(fUnitRoots)
# require(urca)
# require(metap)
# 
# fishertype.purtest<-function(splitresiduals,lags=3) {
#   #test
#   #splitresiduals<-splitxs
#   #ADF tests on each panel's residuals
#   pvals<-sapply(splitresiduals,function(x) {
#     pval<-tryCatch({
#       #x<-splitresiduals[[2]]
#       #I estimated an ADF regression with constant but no trend
#       #i skip the block F Test b/c I couldn't figure out how to
#       #get p-vals for the F-statistic, in the event that I rejected it
#       #so I rely directly on the tau2 stat. this is not best practice,
#       #but it will have to do for now.
#       output<-urdfTest(x,lags=lags,type=c("c"),doplot=F)
#       #get tau2
#       tau2<-output@test$test@teststat[,'tau2']
#       #get pval of tau1 under MAckinnon distribution
#       pval<-punitroot(tau2,N=length(x),trend="c",statistic="t") %>% as.vector
#     }, error = function(e) {
#       NA
#     })
#     return(pval)
#   })
#   pvals<-pvals[!is.na(pvals)]
#   #using package 'metap' to evaluate all these together,
#   #using Fisher's proposed method
#   fisher<-sumlog(pvals)
#   #return results
#   return(list(logittest=fisher$p,
#               allpvals=pvals))
# }

# #test with some simulated data
# df<-expand.grid(units=letters,t=1:50,x=1)
# df<-df[order(df$units,df$t,df$x),]
# gamma<-0.8
# #make each series unit root with some randomness
# df$x<-tapply(df$x,df$units,function(x) {
#   x[1]<-rnorm(n=1)
#   for(i in 2:length(x))
#     x[i]<-x[i-1]*gamma+rnorm(n=1)
#   return(x)
# }) %>% unlist
# splitxs<-split(df$x,df$units)
# purtest(
#   data.frame(splitxs),
#   test="levinlin",
#   exo="intercept",
#   lags="AIC",
#   pmax=3
# )
# fishertype.purtest(splitxs)


#########################################################


# require(Bolstad2)
# getarea<-function(values,years=NA) {
#   #years<-pred.year
#   #values<-polityhat.normal
#   if(is.na(years[1]))
#     years<-1:length(values)
#   if(max(diff(years))==1) { #if no missing years, no biggie
#     return(sintegral(years,values)$int)
#   } else { #if there are some missing years, need to adjust
#     #need to split this up into running intervals
#     #grab each instance of missing year, and the last year
#     cutpoints<-which(c(diff(years)>1,NA) | years==max(years))
#     cuts<-lapply(seq_along(cutpoints),function(i) {
#       if(i==1) {
#         return(1:cutpoints[i])
#       } else {
#         return((cutpoints[i-1]+1):cutpoints[i])
#       }
#     })
#     #calculate area of each cut, and return
#     area<-sapply(cuts,function(indices) {
#       sintegral(years[indices],values[indices])$int
#     }) %>% sum
#     return(area)
#   }
# }
# 
# #########################################################
# 
# getgain<-function(x,y) { #wrt y
#   if(length(x)!=length(y)) {
#     warning("Different Lengths")
#     avgx<-avgy<-pctgain.avg<-
#       areax<-areay<-pctgain.area<-NA
#   } else {
#     #pct gain by average level
#     avgx<-mean(x)
#     avgy<-mean(y)
#     pctgain.avg<-100*(avgx-avgy)/avgy
#     #pct gain by area covered
#     areax<-getarea(values=x)
#     areay<-getarea(values=y)
#     pctgain.area<-100*(areax-areay)/areay
#   }
#   return(list(avg1=avgx,avg2=avgy,
#               pctgain.avg=pctgain.avg,
#               area1=areax,area2=areay,
#               pctgain.area=pctgain.area))
# }

#########################################################

#takes a model and returns the robust vcov matrix
getrobust<-function(m,cluster) {
  
  #h/t: http://www.r-bloggers.com/easy-clustered-standard-errors-in-r/
  
  require(sandwich,quietly=T)
  require(lmtest,quietly=T)
  
  # m<-lm(data=beodf[fullsamples$oneyrdf$incrate,],
  #       formula = incrt_t_jur ~ L.beopct_all)
  # cluster<-beodf$state_alpha2[fullsamples$oneyrdf$incrate]
  
  M<-length(unique(cluster))
  N<-length(cluster)
  K<-m$rank
  
  #adjust df
  dfc <- (M/(M-1))*((N-1)/(N-K))
  
  #calculate uj's
  uj<-apply(estfun(m),2,function(x) tapply(x,cluster,sum))
  
  #use sandwich to get newvcov
  newvcov<-dfc * sandwich(m,meat=crossprod(uj)/N)
  return(newvcov)
  
}

getrobust.plm<-function(m.plm) {
  
  #m.plm<-allmodels[[1]]
  #cluster<-attr(m.plm$model,"index")[["state_alpha2"]]
  newvcov<-tryCatch({
    vcovHC(m.plm,type="HC0",cluster="group")
  }, error=function(e) {
    print("Couldn't compute clustered SE's")
    print(e)
    return(m.plm$vcov)
  })
  
  return(newvcov)
}


#########################################################

require(MASS)

#this function takes a model,
#the name of the coefficien of the lagged DV
#the name of the coefficient of the lagged IV, 
#optionally, can calc LRM for more than one lagged IV 
#and simulates mvnorm distribution to evaluate SE of LRM
getlongrun<-function(m,lagdv,iv, 
                     reps=5000,
                     plm=T,
                     summary=T,
                     dvfactor=1,
                     ivfactor=1) {
  
  #test input
  #m=m;lagdv=lagdvs;iv=ivs;reps=5000;dvfactor=1;ivfactor=1
  
  #return NA if iv or ivs not found
  if(sum(sapply(iv,function(x) !x%in%names(m$coefficients)))>0)
    return(NA)
  #otherwise:
  means<-c(m$coefficients[lagdv],
           m$coefficients[iv]) 
  #you want the variance-covariance matrix that is adjusted for clustering
  new.vcov<-getrobust.plm(m)
  rows<-row.names(new.vcov)%in%c(lagdv,iv)
  cols<-colnames(new.vcov)%in%c(lagdv,iv)
  vcov.useme<-new.vcov[rows,cols] #just these vars
  #vcov needs to be ordered in the same way as the means
  new.order<-match(names(means),row.names(vcov.useme))
  vcov.useme<-vcov.useme[new.order,new.order]
  #sample from the multivariate distribution defined here
  draws<-mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
  numerator<-apply(draws[,iv] %>% as.matrix,1,sum) #if more than one lag of iv
  denominator<-1-apply(draws[,lagdv] %>% as.matrix,1,sum) #if more than one lag dv
  lrm.distribution<-numerator/denominator
  #put this distribution in meaningful units
  lrm.distribution<-lrm.distribution*ivfactor*dvfactor
  #now compute summary stats for return
  #hist(lrm.distribution)
  estimate<-quantile(lrm.distribution,c(0.5)) #median
  ci<-quantile(lrm.distribution,c(0.025,0.975)) #2.5th, 97.5th percentiles
  pval<-ecdf(lrm.distribution)(0) 
  #adjust pval to be something like a two-sided test
  pval<-ifelse(estimate<0,(1-pval)*2,pval*2)
  estimate.star<-apply.pvals(estimate,pval)
  if(summary) {
    return(list(est=estimate,
                est.output=estimate.star,
                pval=pval,
                ci=c(ci[1],ci[2]),
                se=sd(lrm.distribution),
                se.output=paste0("(",sprintf("%.3f",sd(lrm.distribution)),")")))
  } else {
    return(lrm.distribution)
  }
}

#########################################################

#summarize a distribution, returning mu, etc.
#using the format used already in this project

summarize.distribution<-function(ests.distribution) {
  quantiles<-quantile(ests.distribution,c(0.025,0.5,0.975))
  mu<-quantiles[2]
  mu.min<-quantiles[1]
  mu.max<-quantiles[3]
  #get something like a two-sided pval test
  pval<-ecdf(ests.distribution)(0) 
  pval<-ifelse(mu<0,(1-pval)*2,pval*2)
  #return me
  thisrow<-data.frame(mu=mu,
                      mu.min=mu.min,
                      mu.max=mu.max,
                      se=sd(ests.distribution),
                      pval=pval)
  return(thisrow)
}


#########################################################

#this function takes a model where there is an interaction, 
#and computes the longrun coefficient taking this interaction 
#into account. 

#extra input needed is value of interacted variable at which
#you'd like to calculate the LRM
getlongrun.interaction<-function(m,
                                 lagdvs,
                                 ivs,
                                 int.terms,
                                 intvalues=c(0,1), #default for indicator
                                 reps=5000,
                                 plm=T,
                                 summary=T,
                                 dvfactor=1,
                                 ivfactor=1) {
  # m<-m; lagdvs<-lagdvs; ivs<-ivs[!ivs%in%int.terms];
  # int.terms<-int.terms; intvalues<-intvals
  # reps<-1000; 
  # dvfactor=1; ivfactor=sds[[var]];
  # summary(m)
  not.present<-sum(!ivs%in%names(m$coefficients)) + 
    sum(!int.terms%in%names(m$coefficients))
  if(not.present>0) 
    return(NA) #if can't find IV or interaction term
  means<-c(m$coefficients[lagdvs],
           m$coefficients[ivs],m$coefficients[int.terms]) 
  #you want the variance-covariance matrix that is adjusted for clustering
  new.vcov<-getrobust.plm(m)
  rows<-row.names(new.vcov)%in%c(lagdvs,ivs,int.terms)
  cols<-colnames(new.vcov)%in%c(lagdvs,ivs,int.terms)
  vcov.useme<-new.vcov[rows,cols] #just these vars
  #vcov needs to be ordered in the same way as the means
  new.order<-match(names(means),row.names(vcov.useme))
  vcov.useme<-vcov.useme[new.order,new.order]
  lrm.distributions<-lapply(intvalues,function(intval) {
    #intval<-intvalues[1]
    #sample from the multivariate distribution defined here
    draws<-mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
    #numerator is going to be sum of ivs and ivs:int * intval
    draws[,int.terms]<-draws[,int.terms]*intval
    numerator<-apply(draws[,c(ivs,int.terms)] %>% as.matrix,1,sum)
    denominator<-1-apply(draws[,lagdvs] %>% as.matrix,1,sum)
    lrm.distribution<-(numerator/denominator)*ivfactor*dvfactor
    return(lrm.distribution)
  })
  cum.output<-lapply(lrm.distributions,summarize.distribution) %>% rbind.fill
  cum.output$atval<-intvalues
  #return this dataframe
  return(cum.output)
}

#########################################################

#this function is v. similar to getlongrun.interaction, 
#but it takes a term with its square in the spec, 
#and calculates the LRM at diff values of the IV

getlongrun.sq<-function(m,
                        lagdvs,
                        notsq.terms,
                        sq.terms,
                        sqvals, 
                        reps=5000,
                        plm=T,
                        summary=T,
                        dvfactor=1,
                        ivfactor=1) {
  # m<-m;
  # lagdvs<-lagdvs; 
  # notsq.terms
  # sq.terms
  # sqvals
  # ivfactor<-sds[[var]]
  # reps<-1000; 
  # dvfactor=1; ivfactor=sds[[var]];
  # summary(m)
  not.present<-sum(!sq.terms%in%names(m$coefficients)) + 
    sum(!notsq.terms%in%names(m$coefficients))
  if(not.present>0) 
    return(NA) #if can't find IV or interaction term
  means<-c(m$coefficients[lagdvs],
           m$coefficients[notsq.terms],m$coefficients[sq.terms]) 
  #you want the variance-covariance matrix that is adjusted for clustering
  new.vcov<-getrobust.plm(m)
  rows<-row.names(new.vcov)%in%c(lagdvs,notsq.terms,sq.terms)
  cols<-colnames(new.vcov)%in%c(lagdvs,notsq.terms,sq.terms)
  vcov.useme<-new.vcov[rows,cols] #just these vars
  #vcov needs to be ordered in the same way as the means
  new.order<-match(names(means),row.names(vcov.useme))
  vcov.useme<-vcov.useme[new.order,new.order]
  lrm.distributions<-lapply(sqvals,function(sqval) {
    #sqval<-sqvals[1]
    #sample from the multivariate distribution defined here
    draws<-mvrnorm(n=reps,mu=means,Sigma=vcov.useme)
    #numerator is going to be b1*sds[[var]] + 2*b2*(sds[[var]]^2)*sqval
    draws[,notsq.terms]<-draws[,notsq.terms]*ivfactor
    draws[,sq.terms]<-draws[,sq.terms]*2*sqval*(ivfactor^2)
    #calc numerator and denominator
    numerator<-apply(draws[,c(notsq.terms,sq.terms)] %>% as.matrix,1,sum)
    denominator<-1-apply(draws[,lagdvs] %>% as.matrix,1,sum)
    lrm.distribution<-(numerator/denominator)*dvfactor
    return(lrm.distribution)
  })
  cum.output<-lapply(lrm.distributions,summarize.distribution) %>% rbind.fill
  #give informative return for sqvals
  cum.output$atval<-sqvals
  #return this dataframe
  return(cum.output)
}

##########################################################
##########################################################
##########################################################

#STATE CONVERSION, ETC.

setwd(metadir)
cdf<-read.csv("03_dind_cdf.csv",stringsAsFactors=F); cdf$X<-NULL
names(cdf)[names(cdf)=="state_gallup.old"]<-"state_gallup.60" #valid till 1960

#add divison name and region num
#region
regions<-matrix(c("Northeast","Midwest","South","West",
                  1,2,3,4),ncol=2)

getnum.region<-function(name) {
  tmp<-regions[regions[,1]==name,2] %>% as.numeric
  if(length(tmp)==1) {
    return(tmp) 
  } else {
    return(NA) 
  }
} 
getnum.region("South")  
getnum.region("Blue")
getnum.region(2)
getnum.region(NA)

getname.region<-function(num) {
  tmp<-regions[regions[,2]==num,1] 
  if(length(tmp)==1) {
    return(tmp) 
  } else {
    return(NA) 
  }
} 
getname.region("1")  


#division
divisions<-matrix(c("New England",
                    "Middle Atlantic",
                    "East North Central",
                    "West North Central",
                    "South Atlantic",
                    "East South Central",
                    "West South Central",
                    "Mountain",
                    "Pacific",
                    1:9),ncol=2)
getname.division<-function(num) {
  tmp<-divisions[divisions[,2]==num,1]
  if(length(tmp)==1) {
    return(tmp) 
  } else {
    return(NA) 
  }
}
getname.division(2)   
getname.division(0)
getname.division(NA)  

getnum.division<-function(name) {
  tmp<-divisions[divisions[,1]==name,2] %>% as.numeric
  if(length(tmp)==1) {
    return(tmp) 
  } else {
    return(NA) 
  }
}
getnum.division("New England")

#add region num, division name to cdf
cdf$region.num<-sapply(cdf$region,getnum.region) 
names(cdf)[names(cdf)=="division"]<-"division.num"
cdf$division<-sapply(cdf$division.num,getname.division)

##########################################################

#function to retrieve all info for a given state
getstateinfo<-function(x,from="state_alpha2") {
  if(!is.na(x)) { #if non-NA input
    thisrow<-cdf[cdf[[from]]==x,]
    if(nrow(thisrow)!=1) {
      thisrow<-data.frame(state_alpha2=NA,
                          state_gallup=NA,
                          state_gallup.60=NA,
                          state_fips=NA,
                          statename=NA,
                          region=NA,
                          region.num=NA,
                          division=NA,
                          division.num=NA,
                          state_abc=NA,
                          state_roper=NA) 
    } #if no match found
  } else { #if NA input
    thisrow<-data.frame(state_alpha2=NA,
                        state_gallup=NA,
                        state_gallup.60=NA,
                        state_fips=NA,
                        statename=NA,
                        region=NA,
                        region.num=NA,
                        division=NA,
                        division.num=NA,
                        state_abc=NA,
                        state_roper=NA) 
  }
  return(thisrow)
}

#test 
getstateinfo("AK")
getstateinfo(75,"state_gallup.60")
getstateinfo(NA)
getstateinfo("XY")
getstateinfo(1,"state_roper")

#the plural version
getstateinfos<-function(xs,from="state_alpha2") {
  #xs<-c("AK","AL"); from="state_alpha2"
  return(lapply(xs,getstateinfo,from) %>% rbind.fill)
}

#test
getstateinfos(c(1,2,3,4),"state_abc")

##########################################################

#function to retrieve just region, when given division or state
getregion<-function(x,from="state_alpha2") {
  tmp<-cdf$region[cdf[[from]]==x] %>% unique
  if(length(tmp)==1) {
    return(tmp) 
  } else {
    return(NA) 
  }
}

getregion("AK")
getregion(75,"state_gallup.60")
getregion(NA)
getregion("XY")
getregion(9,"division.num")

##########################################################
##########################################################

#end with this directory
setwd(olddir)

