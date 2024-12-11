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
require(lme4)
require(merTools)
require(doParallel)

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

#load data and mods
setwd(filesdir); dir()
#finaldf<-fread('finaldf_po.csv')
mods<-readRDS('01po_modslist_RandR.RDS')
modtimes<-lapply(mods,function(x) x$modtime['elapsed']/60)
prefmods<-lapply(mods,function(x) x$m)
lapply(mods,function(x) x$m %>% summary)

#set seed
set.seed(23)
merTools_sims<-100

#########################################################
#########################################################

#PREDICT FOR MRP-CELLS
#post-stratifying using census data

#load the census data
setwd(datadir); dir()
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

#prep cpopdf for prediction
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
  cpopdf[[newname]]<-paste0(
    cpopdf[[thisrow$v1]],
    "_",
    cpopdf[[thisrow$v2]]
  )
}

#add edXyear and divisionXyear
cpopdf$divisionXyear<-paste0(
  cpopdf$division,"_",cpopdf$year
)
cpopdf$edXyear<-paste0(
  cpopdf$ed,"_",cpopdf$year
)
#add others that are missing
cpopdf$raceXdivisionXyear<-paste0(
  cpopdf$race,"_",
  cpopdf$division,"_",
  cpopdf$year
)
cpopdf$raceXedXyear<-paste0(
  cpopdf$race,"_",
  cpopdf$ed,"_",
  cpopdf$year
)

#loop through and predictdf
tmpseq.i<-seq_along(prefmods)
fulloutput<-lapply(tmpseq.i,function(i) {
  
  #i<-3
  print(i)
  ##########
  
  thismod<-prefmods[[i]]
  thismodname<-names(prefmods)[i]
  thisdimension<-str_extract(
    thismodname,
    "all|anxiety|punitive|mistrust"
  )
  predictdf<-cpopdf
  
  #restrict it for temp speed
  # tmp<-predictdf$year%in%c(1960,1990) & 
  #   predictdf$state_alpha2=="AL"
  # predictdf<-predictdf[tmp,]
  
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
  predictdf$raceXquestionXyear<-paste0(
    predictdf$race,"_",
    predictdf$question,"_",
    predictdf$year
  )
  predictdf$questionXyear<-paste0(
    predictdf$question,"_",
    predictdf$year
  )
  
  #single prediction
  spredictdf<-predictdf
  spredictdf$mu<-predict(
    thismod,
    newdata=spredictdf,
    type='response' 
  ) 
  spredictdf<-data.table(spredictdf)
  #collapse to get desired summaries
  #original spredictdf is also saved
  spredictdf_ry<-spredictdf[
    ,
    .(
      mu=weighted.mean(mu,pop)
    )
    ,
    by=c(
      'race',
      'year'
    )
  ]
  
  # #NB: this is very computationally-intensive,
  # #so we use parallel computation to speed it up a bit
  # mpredictdf<-predictdf
  # numCores<-getDoParWorkers()
  # registerDoParallel(cores=numCores)
  # st<-proc.time()
  # tmpoutput<-merTools::predictInterval(
  #   thismod,
  #   newdata=mpredictdf,
  #   which="full",
  #   level=0.95,
  #   type='probability',
  #   include.resid.var=F,
  #   n.sims=merTools_sims, 
  #   #.parallel = T,
  #   #100 X 200k yields about 20 million obs
  #   #1000 reps and we're talking 200 million
  #   returnSims=T
  # )
  # timetorun<-proc.time() - st
  # print(timetorun)
  # 
  # simMat<-attr(tmpoutput,"sim.results")
  # simMat<-apply(simMat,1,thismod@resp$family$linkinv)
  # simMat<-t(simMat)
  # tmpdf<-data.frame(simMat)
  # mpredictdf<-cbind(mpredictdf,tmpdf)
  # gathcols<-names(mpredictdf)[str_detect(names(mpredictdf),"^X")]
  # mpredictdf<-gather_(
  #   mpredictdf,
  #   "rep",
  #   "mu",
  #   gathcols
  # ) %>% data.table
  # mpredictdf$rep<-str_replace(mpredictdf$rep,"X","")
  # #collapse for race year
  # mpredictdf_ry<-mpredictdf[
  #   ,
  #   .(
  #     mu=weighted.mean(mu,pop)
  #   )
  #   ,
  #   by=c(
  #     'race',
  #     'year',
  #     'rep'
  #   )
  # ]
  # #collapse for raceXedXdivisionXyear
  # mpredictdf_redy<-mpredictdf[
  #   ,
  #   .(
  #     mu=weighted.mean(mu,pop)
  #   )
  #   ,
  #   by=c(
  #     'race',
  #     'ed',
  #     'division',
  #     'year',
  #     'rep'
  #   )
  # ]
  
  #returnme
  returnlist<-list(
    spredictdf=spredictdf,
    spredictdf_ry=spredictdf_ry
    #mpredictdf_ry=mpredictdf_ry,
    #mpredictdf_redy=mpredictdf_redy,
    #timetorun=timetorun #how long it takes to run each
  )
  
})

names(fulloutput)<-names(prefmods)

#########################################################
#########################################################

#I. TRENDS OVER TIME
#use spredictdfs to trends

require(ggplot2)

#loop through fulloutput
#to generate trends for each model
tmpseq.i<-seq_along(fulloutput)
trendsdf<-lapply(tmpseq.i,function(i) {
  
  #i<-1
  tmpoutput<-fulloutput[[i]]
  thisdimension<-str_extract(
    names(fulloutput)[i],
    "all|anxiety|punitive|mistrust"
  )
  
  #use spredictdf
  spredictdf<-tmpoutput$spredictdf %>% 
    data.table
  tmpdf<-spredictdf[
    ,
    .(
      mu=weighted.mean(mu,pop)
    )
    ,
    by=c(
      'race',
      'year'
    )
  ]
  
  #add a loess
  returndf<-tmpdf[
    ,
    .(
      mu.loess = loess(
        mu ~ year
      ) %>% predict(min(year):max(year)),
      year=min(year):max(year)
    )
    ,
    by=c(
      'race'
    )
  ]
  
  #put them together
  returndf<-merge(
    tmpdf,
    returndf,
    all=T
  )
  returndf$dimension<-thisdimension
  
  ###
  returndf
  
}) %>% rbind.fill %>% data.table

tmp<-trendsdf$race%in%c(1,2)
plotdf<-trendsdf[tmp,]
plotdf$facet<-'Estimated'

#order race
plotdf$race<-factor(
  plotdf$race,
  levels=c(1,2),
  labels=c("White","Black")
)
tmpcolors<-c('red','blue')
names(tmpcolors)<-levels(plotdf$race)

#dimension
plotdf$dimension<-factor(
  plotdf$dimension,
  levels=c(
    "anxiety",
    "punitive",
    "mistrust"
  ),
  labels=c(
    "Anxiety",
    "Punitiveness",
    "Mistrust"
  )
)

plotdf$facet<-factor(
  plotdf$facet,
  levels=c(
    'Conventional',
    'Estimated'
  ),
  labels=c(
    'Conventional',
    'Estimated'
  )
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=year,
    y=mu.loess,
    group=race,
    color=race
  )
) +
  geom_line(
    size=1
  ) +
  geom_point(
    data=plotdf,
    aes(
      y=mu
    ),
    alpha=0.25
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  facet_grid( 
    dimension ~ facet
  ) +
  xlab("") +
  ylab("P(Affirmative)\n") +
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  ) 

setwd(outputdir)
tmpname<-"fig_po_trends_sparsest.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=4,
  height=8
)

#########################################################
#########################################################

#compare sparsest to trends
setwd(filesdir); dir()
fulloutput<-readRDS('01po_predictions.RDS')
trendsdf2<-lapply(tmpseq.i,function(i) {
  #i<-1
  tmpoutput<-fulloutput[[i]]
  thisdimension<-str_extract(
    names(fulloutput)[i],
    "all|anxiety|punitive|mistrust"
  )
  #use spredictdf
  spredictdf<-tmpoutput$spredictdf %>% 
    data.table
  tmpdf<-spredictdf[
    ,
    .(
      mu=weighted.mean(mu,pop)
    )
    ,
    by=c(
      'race',
      'year'
    )
  ]
  #add a loess
  returndf<-tmpdf[
    ,
    .(
      mu.loess = loess(
        mu ~ year
      ) %>% predict(min(year):max(year)),
      year=min(year):max(year)
    )
    ,
    by=c(
      'race'
    )
  ]
  #put them together
  returndf<-merge(
    tmpdf,
    returndf,
    all=T
  )
  returndf$dimension<-thisdimension
  
  ###
  returndf
  
}) %>% rbind.fill %>% data.table

#comapre the two
trendsdf$model <- 'preferred'
trendsdf2$model <- 'sparsest'
mydf<-rbindlist(
  list(
    trendsdf,
    trendsdf2
  )
) %>% pivot_longer(
  cols=c('mu','mu.loess')
) %>% pivot_wider(
  names_from=model,
  values_from=value
) %>% data.table

#correlated at 0.98-0.99... 
mydf[
  ,
  .(
    cor(preferred,sparsest,use='c')
  )
  ,
  by=c(
    'race',
    'dimension',
    'name'
  )
]



#########################################################
#########################################################
