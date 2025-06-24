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

#########################################################
#########################################################

#GATHER DATA/COMBINE
#load data
setwd(filesdir); dir()

#AVERAGES
avgdf<-read.csv(
  '01po_q_avgs.csv',
  stringsAsFactors=F
)

#DEPRECATED
# tmpdf<-read.csv(
#   'q_avghat.csv',
#   stringsAsFactors=F
# )
# tmpdf$question<-paste0(
#   tmpdf$dimension,".sum"
# )
# #don't need all.sum
# tmp<-tmpdf$question=="all.sum"
# tmpdf<-tmpdf[!tmp,]
# tmpdf$summary<-T
# 
# #won't show average, 
# #since this is confounded by year
# #so the level is very difficult to interpret
# #it's better interpreted off the 
# tmpdf$mu<-tmpdf$mu.min<-tmpdf$mu.max<-NA
# #if you want to show it, comment out line above
# 
# 
# #before, when estimated black/white
# if("race2"%in%names(tmpdf)) {
#   tmp<-names(tmpdf)=="race2"
#   names(tmpdf)[tmp]<-"race"
# }
# 
# keepvars<-c(
#   "question",
#   "dimension",
#   "race",
#   "summary",
#   "mu",
#   "mu.min",
#   "mu.max"
# )
# avgdf$summary<-F
# avgdf<-rbind.fill(
#   avgdf,
#   tmpdf[,keepvars]
# )


#DIFFERENCES
diffdf<-read.csv(
  '01po_q_diffs.csv',
  stringsAsFactors=F
)

#DEPRECATED
# tmpdf<-read.csv(
#   'q_diffhat.csv',
#   stringsAsFactors=F
# )
# tmpdf$question<-paste0(
#   tmpdf$dimension,".sum"
# )
# #don't need all.sum
# tmp<-tmpdf$question=="all.sum"
# tmpdf<-tmpdf[!tmp,]
# 
# keepvars<-c(
#   "question",
#   "dimension",
#   "difftype",
#   "summary",
#   "mu",
#   "mu.min",
#   "mu.max",
#   "pval.class"
# )
# diffdf$summary<-F
# diffdf$difftype<-"diff"
# tmpdf$summary<-T
# diffdf<-rbind.fill(
#   diffdf,
#   tmpdf[,keepvars]
# )

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(scales)
require(grid)
# #load fonts
# loadfonts(quiet=T) #register w/ pdf
# loadfonts(device = "win",quiet=T) #register w/ windows
# #fonts()
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

#########################################################
#########################################################

#FIGURE 1 - AVERAGE RESPONSES

tmp<-avgdf$race%in%c(1,2)
plotdf<-avgdf[tmp,]

#fix question
#order by dimension,
#whether summary,
#then by black mu
#and make factor 
plotdf$dimension<-factor(
  plotdf$dimension,
  levels=c("anxiety","punitive","mistrust"),
  labels=c("Anxiety","Punitiveness","Mistrust")
)
roworder<-order(
  plotdf$question,
  plotdf$race
)
plotdf<-plotdf[roworder,]
plotdf$blackmu<-tapply(
  plotdf$mu,
  plotdf$question,
  function(x) rep(x[2],2)
) %>% unlist
neworder<-order(
  plotdf$dimension,
  #plotdf$summary,
  plotdf$blackmu
)
tmplevels<-plotdf$question[neworder] %>%
  unique
tmplabels<-sapply(tmplevels,function(x) {
  getcode(x,"question","shortname",questionsdf)
})
plotdf$question<-factor(
  plotdf$question,
  levels=tmplevels,
  labels=tmplabels
)

# #add face
# tmpface<-rep("plain",length(tmplabels))
# tmp<-!str_detect(tmplabels,"\\(")
# tmpface[tmp]<-"bold"

# #get location of dividing lines
# tmpdf<-unique(plotdf[neworder,c("question","dimension")])
# diffs<-tmpdf$dimension %>%
#   as.numeric %>% diff
# hlines<-which(diffs==1) + 0.5

#order race
plotdf$race<-factor(
  plotdf$race,
  levels=c(1,2),
  labels=c("White","Black")
)
tmpcolors<-c('red','blue')
names(tmpcolors)<-levels(plotdf$race)

#dodge positions
dodge<-position_dodge(.6)

g.tmp<- ggplot(
  plotdf,
  aes(
    x=question,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    color=race
  )
) +
  geom_errorbar(
    width=0,
    position=dodge
  ) + 
  geom_point(
    size=1,
    position=dodge
  ) +
  geom_hline(
    yintercept=50,
    linetype='dashed',
    color='grey'
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors,
    guide=guide_legend(
      direction="horizontal"
    )
  ) +
  xlab("") +
  ylab("\n% Anxious, Mistrustful or Punitive") +
  coord_flip() +  
  facet_grid(
    rows=vars(dimension),
    scales='free_y',
    space='free_y'
  ) +
  theme_bw(
    base_size=14
  ) +
  theme(legend.position="top") +
  theme(axis.text.y=element_text(size=8))

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename='fig_po_averages.png',
  width=6,
  height=9,
  dpi=300
)
ggsave(
  plot=g.tmp,
  filename='CleggFig1.pdf',
  width=6,
  height=9,
  dpi=300
)


#########################################################
#########################################################

#FIG X - DIFFERENCES

# #aux plot will take difftype avg
# tmp<-diffdf$difftype=="avg"
# auxplotdf<-diffdf[tmp,]
# tmp<-diffdf$difftype!="avg" 
# plotdf<-diffdf[tmp,]
plotdf<-diffdf

#make this black-white rather than white-black
plotdf$mu <- plotdf$mu * -1
plotdf$mu.min <- plotdf$mu.min * -1
plotdf$mu.max <- plotdf$mu.max * -1

#fix question
#order by dimension,
#then mu
#and make factor 
plotdf$dimension<-factor(
  plotdf$dimension,
  levels=c("anxiety","punitive","mistrust"),
  labels=c("Anxiety","Punitiveness","Mistrust")
)
neworder<-order(
  plotdf$dimension,
  #plotdf$summary,
  plotdf$mu
)
tmplevels<-plotdf$question[neworder] %>%
  unique
tmplabels<-sapply(tmplevels,function(x) {
  getcode(x,"question","shortname",questionsdf)
})
plotdf$question<-factor(
  plotdf$question,
  levels=tmplevels,
  labels=tmplabels
)

# #factor in auxplot, too
# auxplotdf$question<-factor(
#   auxplotdf$question,
#   levels=tmplevels,
#   labels=tmplabels
# )

# #add face
# tmpface<-rep("plain",length(tmplabels))
# tmp<-!str_detect(tmplabels,"\\(")
# tmpface[tmp]<-"bold"

# #get location of dividing lines
# tmpdf<-unique(plotdf[neworder,c("question","dimension")])
# diffs<-tmpdf$dimension %>%
#   as.numeric %>% diff
# hlines<-which(diffs==1) + 0.5

#add pval info to shape of point
plotdf$pval.shp<-NA
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.01"]<-1
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.05"]<-2
plotdf$pval.shp[plotdf$pval.class=="at alpha=0.10"]<-3
plotdf$pval.shp[plotdf$pval.class=="not sig"]<-4
plotdf$pval.shp<-factor(
  plotdf$pval.shp,
  levels=c(1,2,3,4),
  labels=c("at alpha=0.01","at alpha=0.05","at alpha=0.10","not sig")
)
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(plotdf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

g.tmp<-ggplot() + 
  geom_errorbar(
    data=plotdf,
    aes(
      x=question,
      ymin=mu.min,
      ymax=mu.max
    ),
    width=0
  ) + 
  geom_point(
    data=plotdf,
    aes(
      x=question,
      y=mu,
      shape=pval.shp
    ),
    size=1
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='grey'
  ) +
  # geom_vline(
  #   xintercept=hlines[1],
  #   linetype='dashed',
  #   alpha=0.3
  # ) + 
  # geom_vline(
  #   xintercept=hlines[2],
  #   linetype='dashed',
  #   alpha=0.3
  # ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    guide=guide_legend(
      direction="horizontal"
    )
  ) +
  xlab("") +
  ylab("\nBlack-White Gap") +
  coord_flip() +  
  facet_grid(
    rows=vars(dimension),
    scales='free_y',
    space='free_y'
  ) +
  theme_bw(
    #base_family="CM Roman",
    base_size=14
  ) +
  theme(legend.position="top") + 
  theme(axis.text.y=element_text(size=8))

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename='fig_po_diffs.png',
  width=6,
  height=9
)

#########################################################
#########################################################

#ROOT CAUSE RESPONSES

#extras
require(boot)

#load data again
setwd(filesdir); dir()
finaldf<-fread(
  '01po_dataframe.csv'
)

mean_se.boot<-function(x,w) {
  
  #df<-finaldf[question=='adeqprotect.time' & race==1]
  #x<-df$aff
  #w<-df$weights
  dist<-boot(
    data=x,
    stat=function(x,d) {
      mean(x[d])
    },
    R=1000,
    weights=w
  )
  dist$t %>% as.vector
  # if(raw==T) {
  #   return(dist$t)
  # } else {
  #   returndf<-summarize.distribution2(
  #     dist$t
  #   )
  #   returndf$N<-length(x)
  #   returndf$pval<-
  #     returndf$pval.class<-NA
  #   return(returndf)
  # }
}

summarize.distribution3<-function(ests.distribution) {
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
  list(
    mu=mu,
    mu.min=mu.min,
    mu.max=mu.max,
    se=se,
    # #se.q=se.q,
    pval=pval,
    pval.class=pval.class
  )
}

#combine the two more prisons q's
tmpdf <- finaldf
tmpdf[question%in%c('moreprisons.gallup','moreprisons.lat'),question:='moreprisons']

# #summarize rootcause questions
# sumdf <- tmpdf[
#   !is.na(race) & 
#     race%in%c(1,2) &
#     neut==0 & 
#     question%in%c(
#       'useforce.anes',
#       'moreprisons',
#       'moreimppunish.gallup'
#     )
#   ,
#   .(
#     index=1:1000,
#     mu=mean_se.boot(
#       100 * neg,
#       weights
#     )
#   )
#   ,
#   by=c(
#     'race',
#     'question',
#     'year'
#   )
# ][
#   ,
#   summarize.distribution3(mu)
#   ,
#   by=c(
#     'race',
#     'question',
#     'year'
#   )
# ]
# sumdf[order(race,mu)]

#summarize rootcause questions
sumdf <- tmpdf[
  !is.na(race) & 
    race%in%c(1,2) &
    neut==0 & 
    question%in%c(
      'useforce.anes',
      'moreprisons',
      'moreimppunish.gallup'
    )
  ,
  .(
    index=1:1000,
    mu=mean_se.boot(
      100 * neg,
      weights
    )
  )
  ,
  by=c(
    'race',
    'question'
  )
][
  ,
  summarize.distribution3(mu)
  ,
  by=c(
    'race',
    'question'
  )
]
sumdf[order(race,question,mu)]

finaldf[
  question%in%c(
    'useforce.anes',
    'moreprisons.gallup',
    'moreimppunish.gallup',
    'moreprisons.lat'
  ),
  c('question','year')
] %>% unique


