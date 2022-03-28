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
require(boot)

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

#load data
setwd(filesdir); dir()
finaldf<-fread(
  '01po_dataframe.csv'
)

#########################################################
#########################################################

#HELPER FUNCTION

#uses boot, returns y, ymin, ymax
#for use w/ data.table

mean_se.boot<-function(x,w,raw=T) {
  # x<-df$aff
  # w<-df$weights
  dist<-boot(
    data=x,
    stat=function(x,d) {
      mean(x[d])
    },
    R=1000,
    weights=w
  )
  if(raw==T) {
    return(dist$t)
  } else {
    returndf<-summarize.distribution2(
      dist$t
    )
    returndf$N<-length(x)
    returndf$pval<-
      returndf$pval.class<-NA
    return(returndf)
  }
}

#########################################################
#########################################################


#FOR EACH Q
#return mean/se, by race
#since this is weighted, 
#se should be booted
questions<-unique(finaldf$question) %>%
  sort
tmpseq.i<-seq_along(questions) 

fulloutput<-lapply(tmpseq.i,function(i) {
  #i<-9
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )
  
  thisquestion<-questions[i]
  
  # #examine
  # tmp<-finaldf$question==thisquestion &
  #   finaldf$race==2
  # thisdf<-finaldf[tmp,]
  # sum(thisdf$aff)/nrow(thisdf)
  # sum(thisdf$neg)/nrow(thisdf)
  
  #sum(thisdf$aff[thisdf$race==2])/
  #  sum(thisdf$race==2) #76%
  
  #subset
  thisdf<-finaldf[
    question==thisquestion & 
      !is.na(race) &
      neut==0
    ]
  
  #loop through and return
  #bootstrapped distribution of means
  #for each poll and each race
  loopdf<-expand.grid(
    pollid=unique(thisdf$pollid),
    race=unique(thisdf$race),
    stringsAsFactors=F
  )
  loopdf$name<-apply(
    loopdf,1,paste0,collapse="."
  )
  tmpseq.j<-1:nrow(loopdf)
  fulloutput<-lapply(tmpseq.j,function(j) {
    #j<-1
    df<-thisdf[
      pollid==loopdf$pollid[j] &
        race==loopdf$race[j]
      ]
    if(nrow(df)>0) {
      returndist<-mean_se.boot(
        df$aff,
        df$weights
      )
    } else {
      returndist<-NULL
    }
    100 * returndist
  })
  names(fulloutput)<-loopdf$name
  
  
  # ############
  # #METHOD 1
  # 
  # #AVERAGES
  # #for each race,
  # #put together all boot distributions
  # #and compute quantiles
  # races<-unique(thisdf$race) %>%
  #   sort
  # tmpseq.j<-seq_along(races)
  # avgdf<-lapply(tmpseq.j,function(j) {
  #   #j<-1
  #   thisrace<-races[j]
  #   tmp<-str_detect(
  #     names(fulloutput),
  #     paste0(thisrace,"$")
  #   )
  #   tmpdist<-fulloutput[tmp] %>% 
  #     unlist %>%
  #     unname
  #   returndf<-data.frame(
  #     quantile(
  #       tmpdist,
  #       c(0.5,0.025,0.975)
  #     ) %>% t
  #   )
  #   names(returndf)<-c("mu","mu.min","mu.max")
  #   returndf$question<-thisquestion
  #   returndf$race<-thisrace
  #   returndf
  # }) %>% rbind.fill
  # avgdf$dimension<-getcode(
  #   thisquestion,
  #   "question",
  #   "dimension",
  #   questionsdf
  # )
  # 
  # #to get race diffs
  # #loop through questions
  # #and compute black-white difference
  # #and then compute quantiles
  # polls<-unique(thisdf$pollid)
  # tmpseq.j<-seq_along(polls)
  # diffdist<-lapply(tmpseq.j,function(j) {
  #   #j<-1
  #   thispoll<-polls[j]
  #   tmp<-str_detect(
  #     names(fulloutput),
  #     fixed(thispoll)
  #   )
  #   tmpoutput<-fulloutput[tmp]
  #   #we want black-white difference
  #   whites<-tmpoutput[
  #     str_detect(
  #       names(tmpoutput),
  #       "1$"
  #     )
  #     ] %>% unlist %>%
  #     unname
  #   blacks<-tmpoutput[
  #     str_detect(
  #       names(tmpoutput),
  #       "2$"
  #     )
  #     ] %>% unlist %>% 
  #     unname
  #   if(length(whites)==length(blacks)) {
  #     y<-whites-blacks
  #   } else {
  #     y<-NULL
  #   }
  #   return(y)
  # })
  # tmpdist<-diffdist %>%
  #   unlist %>%
  #   unname
  # diffdf<-data.frame(
  #   quantile(
  #     tmpdist,
  #     c(0.5,0.025,0.975)
  #   ) %>% t
  # )
  # diffdf
  # names(diffdf)<-c("mu","mu.min","mu.max")
  # diffdf$question<-thisquestion
  # diffdf$dimension<-getcode(
  #   thisquestion,
  #   "question",
  #   "dimension",
  #   questionsdf
  # )
  
  ############
  #METHOD 2
  
  
  #GET DIST OF MEANS OF MEANS
  races<-unique(thisdf$race) %>%
    sort
  tmpseq.j<-seq_along(races)
  mydists<-lapply(tmpseq.j,function(j) {
    #j<-1
    thisrace<-races[j]
    tmp<-str_detect(
      names(fulloutput),
      paste0(thisrace,"$")
    )
    tmpoutput<-fulloutput[tmp]
    tmpmat<-do.call(cbind,tmpoutput)
    apply(tmpmat,1,mean)
  })
  names(mydists)<-races
  
  #AVERAGES
  avgdf<-lapply(tmpseq.j,function(j) {
    thisrace<-races[j]
    thisdist<-mydists[[j]]
    returndf<-summarize.distribution2(
      thisdist
    )
    returndf$pval<-returndf$pval.class<-NULL
    returndf$question<-thisquestion
    returndf$race<-thisrace
    returndf
  }) %>% rbind.fill
  avgdf$dimension<-getcode(
    thisquestion,
    "question",
    "dimension",
    questionsdf
  )
  
  #DIFFERENCES
  whites<-mydists[["1"]]
  blacks<-mydists[["2"]]
  tmpdist<-whites-blacks
  diffdf<-summarize.distribution2(
    tmpdist
  )
  diffdf$question<-thisquestion
  diffdf$dimension<-getcode(
    thisquestion,
    "question",
    "dimension",
    questionsdf
  )
  
  ############
  
  #return all this
  list(
    avgdf=avgdf,
    diffdf=diffdf
  )
})

#########################################################
#########################################################

#save out dfs w/ some info

###AVERAGES
infodf<-finaldf[
  !is.na(race) &
    neut==0
  ,
  .(N=length(unique(respid)))
  ,
  by=c(
    "question",
    "race"
  )
  ]
avgdf<-lapply(fulloutput,function(x) x$avgdf) %>% 
  rbind.fill
avgdf<-merge(
  avgdf,
  infodf
)
setwd(filesdir)
write.csv(
  avgdf,
  "01po_q_avgs.csv",
  row.names=F
)

###DIFFERENCES
infodf<-finaldf[
  race%in%c(1,2) &
    !is.na(race) &
    neut==0
  ,
  .(N=length(unique(respid)))
  ,
  by=c(
    "question"
  )
  ]
diffdf<-lapply(fulloutput,function(x) x$diffdf) %>% 
  rbind.fill
diffdf<-merge(
  diffdf,
  infodf
)
setwd(filesdir)
write.csv(
  diffdf,
  "01po_q_diffs.csv",
  row.names=F
)


