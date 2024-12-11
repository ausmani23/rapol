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
fulloutput<-readRDS('01po_predictions.RDS')

#set seed
set.seed(23)

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
# #load fonts
# loadfonts(quiet=T) #register w/ pdf
# loadfonts(quiet=T,device = "win") #register w/ windows
# fonts()
# #get ghostscript, for tex output
# gsdir<-file.path(
#   "c:",
#   "Program Files",
#   "gs"
# )
# gsdir_full<-file.path(
#   gsdir,
#   dir(gsdir),
#   "bin",
#   "gswin64c.exe"
# )
# Sys.setenv(
#   R_GSCMD = gsdir_full
# )
# #initialize graphlist
# gs.list<-list()

#quick function to outputdfs
output <- function(df,tmpname) {
  setwd(outputdir)
  if( str_detect(tmpname,"\\.pdf$|\\.png") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$|\\.png$",".csv")
  write.csv(
    df,
    tmpname,
    row.names=F
  )
}

#########################################################
#########################################################

#take predictions
#generate trends w/o SE
#diff between black and white over time
#diff between peak and troughs, by race
#extras:
#diff between black high ed black low ed
#diff between white south white non south

#########################################################
#########################################################

#I. TRENDS OVER TIME
#use spredictdfs to trends

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

#add conventional expectations to this graph
#this is how to generate the points
plotdf$facet<-'Estimated'
loopdf<-expand.grid(
  race=c(1,2),
  dimension=c(
    'punitive',
    'anxiety',
    'mistrust'
  )
)
#quick function to help generate conventional expectations
setwd(codedir); source('genconventional.R')

# tmpseq.i<-1:nrow(loopdf)
# tmpdf<-lapply(tmpseq.i,function(i) {
#   thisrow<-loopdf[i,]
#   if(thisrow$race==2) {
#     m<-(thisrow$endpoint-thisrow$startpoint)/(2014-1955)
#     b<-thisrow$startpoint - (1955 * m)
#     fun.y<-function(x) {
#       m * x + b
#     }
#     yhat<-sapply(
#       1955:2014,
#       fun.y
#     )
#     data.frame(
#       state_alpha2="all",
#       race=thisrow$race,
#       dimension=thisrow$dimension,
#       year=1955:2014,
#       mu=yhat,
#       stringsAsFactors=F
#     )
#   } else {
#     if(thisrow$dimension%in%c('punitive','anxiety')) {
#       data.frame(
#         state_alpha2="all",
#         race=thisrow$race,
#         dimension=thisrow$dimension,
#         year=1955:2014,
#         mu=genconventional(40,65,70)$mu/100,
#         stringsAsFactors=F
#       )
#     } else {
#       data.frame(
#         state_alpha2="all",
#         race=thisrow$race,
#         dimension=thisrow$dimension,
#         year=1955:2014,
#         mu=genconventional(50,35,30)$mu/100,
#         stringsAsFactors=F
#       )
#     }
#   }
# }) %>% rbind.fill
# tmpdf$facet<-'Conventional'
# tmpdf$mu.loess<-tmpdf$mu

# plotdf<-rbind.fill(
#   plotdf,
#   tmpdf
# )

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

g.tmp<-ggplot(
  plotdf[plotdf$facet=='Estimated',],
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
    data=plotdf[plotdf$facet=='Estimated',],
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
tmpname<-"fig_po_trends.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=4,
  height=8
)
output(plotdf,tmpname)

#key points
plotdf[race=='Black' & dimension=='Punitiveness' & year%in%c(1957,1989)]
plotdf[race=='Black' & dimension=='Anxiety']
which.max(plotdf$mu.loess[plotdf$race=='White' & plotdf$dimension=='Anxiety'])

#########################################################
#########################################################

#II. MORE POWERFUL ESTIMATES OF DIFFERENCES
#use mpredictdfs for ests
#of diff by race across years
#and diff by years, by race (trough to peak)

tmpseq.i<-seq_along(fulloutput)
diffoutput<-lapply(tmpseq.i,function(i) {
  
  #i<-1
  mpredictdf<-fulloutput[[i]]$mpredictdf_ry %>%
    data.table
  mpredictdf$type<-"raw"
  thisdimension<-str_extract(
    names(fulloutput)[i],
    "all|anxiety|punitive|mistrust"
  )

  #use loess smooth to get ests
  mpredictdf2<-mpredictdf[
    ,
    .(
      mu = loess(
        mu ~ year
      ) %>% predict(min(year):max(year)),
      year=min(year):max(year)
    )
    ,
    by=c(
      'race',
      'rep'
    )
    ]
  mpredictdf2$type<-"loess"
  mpredictdf<-rbind.fill(
    mpredictdf,
    mpredictdf2
  ) %>% data.table
  
  #compare white to black, by time
  tmpdf<-spread(
    mpredictdf,
    race,
    mu
  ) %>% data.table
  tmpdf$mu<-tmpdf$`2` - tmpdf$`1`
  bwdf<-tmpdf[
    ,
    .(
      mu=quantile(mu,0.5,na.rm=T),
      mu.min=quantile(mu,0.025,na.rm=T),
      mu.max=quantile(mu,0.975,na.rm=T)
    ),
    by=c(
      'year',
      'type'
    )
    ]
  bwdf$dimension<-thisdimension
  
  
  #compare peak to trough, by race
  tmpdf<-spread(
    mpredictdf,
    year,
    mu
  ) %>% data.table
  minyear<-min(as.numeric(names(tmpdf)),na.rm=T)
  gathcols<-names(tmpdf)[!names(tmpdf)%in%c('race','rep','type',minyear)]
  tmpdf<-gather_(
    tmpdf,
    "year",
    "val",
    gathcols
  ) %>% data.table
  tmpdf$mu <- tmpdf$val - tmpdf[[as.character(minyear)]]
  yeardf<-tmpdf[
    ,
    .(
      mu=quantile(mu,0.5,na.rm=T),
      mu.min=quantile(mu,0.025,na.rm=T),
      mu.max=quantile(mu,0.975,na.rm=T)
    ),
    by=c(
      'race',
      'year',
      'type'
    )
    ]
  yeardf$year<-as.numeric(yeardf$year)
  yeardf$dimension<-thisdimension
  
  ###
  diffoutput<-list(
    bwdf=bwdf,
    yeardf=yeardf
  )
  diffoutput
  
})

#plot bw difference
plotdf<-lapply(diffoutput,function(x) x$bwdf) %>% rbind.fill
plotdf<-plotdf[plotdf$type=='loess',]

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

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=mu,
    ymin=mu.min,
    ymax=mu.max
  )
) +
  geom_line(
    size=1
  ) + 
  geom_ribbon(
    alpha=0.25
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed'
  ) +
  facet_wrap(
    ~ dimension,
    ncol=1
  ) +
  xlab("") + 
  ylab("Black-White Gap\n") +
  theme_bw(  )

setwd(outputdir)
tmpname<-"fig_po_trends_blackwhitegap.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=5,
  height=8
)
output(plotdf,tmpname)

#plot difference by year
plotdf<-lapply(diffoutput,function(x) x$yeardf) %>% rbind.fill
plotdf<-plotdf[plotdf$type=="loess" & plotdf$race%in%c(1,2),]

plotdf$race<-factor(
  plotdf$race,
  levels=c(1,2),
  labels=c("White","Black")
)
tmpcolors<-c('red','blue')
names(tmpcolors)<-levels(plotdf$race)

plotdf$dimension<-factor(
  plotdf$dimension,
  levels=c(
    "punitive",
    "anxiety",
    "mistrust"
  ),
  labels=c(
    "Punitiveness",
    "Anxiety",
    "Mistrust"
  )
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    color=race,
    fill=race
  )
) +
  geom_line(
    size=1
  ) + 
  geom_ribbon(
    alpha=0.25
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed'
  ) +
  scale_color_manual(
    guide='none',
    values=tmpcolors
  ) +
  scale_fill_manual(
    guide='none',
    values=tmpcolors
  ) +
  facet_grid(
    dimension ~ race
  ) +
  xlab("") + 
  ylab("Over-Time Change\n") +
  theme_bw( ) +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  )

setwd(outputdir)
tmpname<-"fig_po_trends_byrace.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=10,
  height=8
)
output(plotdf,tmpname)

#########################################################
#########################################################










