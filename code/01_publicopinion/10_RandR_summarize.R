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
require(ggplot2)

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


#########################################################
#########################################################

####---- R and R comments ----

#in response to R+R:
#unlike in the main analysis, 
#we want to *include* neutrals

#unlike in the main analysis, 
#we also want to see it by poll

#########################################################
#########################################################

#### ----- INCLUDE NEUTRALS ----

#include neutrals, 
#otherwise same analysis

rawdf <- finaldf[
  !is.na(race) & 
    race%in%c(1,2)
  ,
  .(
    index=1:1000,
    mu=mean_se.boot(
      100 * aff,
      weights
    )
  )
  ,
  by=c(
    'dimension',
    'question',
    'race'
  )
]

#AVERAGES
sumdf <- rawdf[
  ,
  summarize.distribution3(mu)
  ,
  by=c(
    'dimension',
    'question',
    'race'
  )
]; sumdf$neutrals <- 'Neutrals Included'
setwd(filesdir); dir()
sumdf2<-fread(
  '01po_q_avgs.csv'
)
sumdf2 <- sumdf2[race%in%c(1,2),]
sumdf2$neutrals <- 'Neutrals Excluded'

#DIFFS
tmpdf <- pivot_wider(
  rawdf,
  names_from=race,
  values_from=mu
) %>% data.table
tmpdf$mu <-  tmpdf$`1` - tmpdf$`2`
diffdf <- tmpdf[
  ,
  summarize.distribution3(mu)
  ,
  by=c(
    'dimension',
    'question'
  )
]; diffdf$neutrals <- 'Neutrals Included'
setwd(filesdir); dir()
diffdf2<-read.csv(
  '01po_q_diffs.csv',
  stringsAsFactors=F
); diffdf2$neutrals <- 'Neutrals Excluded'

#########################################################

#PLOT AVERAGES

#put averages together
plotdf <- rbindlist(
  list(
    sumdf,
    sumdf2
  ),
  fill=T
)

#fix question
#order by dimension,
#whether summary,
#then by black mu
#and make factor 
plotdf$dimension<-factor(
  plotdf$dimension,
  levels=c("anxiety","mistrust","punitive"),
  labels=c("Anxiety","Mistrust","Punitiveness")
)
roworder<-order(
  plotdf$question,
  plotdf$race
)
plotdf<-plotdf[roworder,]
plotdf$blackmu<-tapply(
  plotdf$mu[plotdf$neutrals=='Neutrals Excluded'],
  plotdf$question[plotdf$neutrals=='Neutrals Excluded'],
  function(x) rep(x[2],4)
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
names(tmpcolors)<-levels(plotdf$black)

#dodge positions
dodge<-position_dodge(.6)

g.tmp <- ggplot(
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
    cols=vars(neutrals),
    scales='free_y',
    space='free_y'
  ) +
  theme_bw(
    #base_family="CM Roman",
    base_size=14
  ) +
  theme(legend.position="top") +
  #theme(axis.text.y=element_text(face=tmpface)) +
  theme(axis.text.y=element_text(size=8))

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename='fig_po_averages2.png',
  width=8,
  height=9
)

#correlation/diffs
tmpdf<-plotdf[,c('question','race','mu','neutrals'),with=F]
tmpdf<-spread(
  tmpdf,
  neutrals,
  mu
)
cor(
  tmpdf$`Neutrals Excluded`,
  tmpdf$`Neutrals Included`
)
tmpdf$diff <- tmpdf$`Neutrals Excluded`-tmpdf$`Neutrals Included`
tmpdf<-tmpdf[order(tmpdf$diff),]
median(tmpdf$diff)

tmpdf<-tmpdf[,c('question','race','diff')]
tmpdf<-spread(
  tmpdf,
  race,
  diff
)
cor(tmpdf$White,tmpdf$Black)
tmpdf$diff<-tmpdf$White -tmpdf$Black
median(tmpdf$diff)

#PLOT DIFFERENCES
# #aux plot will take difftype avg
# tmp<-diffdf$difftype=="avg"
# auxplotdf<-diffdf[tmp,]
# tmp<-diffdf$difftype!="avg" 
# plotdf<-diffdf[tmp,]
plotdf<-diffdf

#fix question
#order by dimension,
#then mu
#and make factor 
plotdf$dimension<-factor(
  plotdf$dimension,
  levels=c("anxiety","mistrust","punitive"),
  labels=c("Anxiety","Mistrust","Punitiveness")
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

#########################################################

# PLOT DIFFS

#put diffs together
plotdf <- rbindlist(
  list(
    diffdf,
    diffdf2
  ),
  fill=T
)

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
  plotdf$dimension[plotdf$neutrals=='Neutrals Excluded'],
  #plotdf$summary,
  plotdf$mu[plotdf$neutrals=='Neutrals Excluded']
)
tmplevels<-plotdf$question[plotdf$neutrals=='Neutrals Excluded'][neworder] %>%
  unique
tmplabels<-sapply(tmplevels,function(x) {
  getcode(x,"question","shortname",questionsdf)
})
plotdf$question<-factor(
  plotdf$question,
  levels=tmplevels,
  labels=tmplabels
)

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
    cols=vars(neutrals),
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
  filename='fig_po_diffs2.png',
  width=8,
  height=9
)

#correlation

#differences


#########################################################
#########################################################

#### ----- ENNS-STYLE GRAPH WITH MANUAL TRENDS ----

plotdf <- finaldf[
  !is.na(race) & 
    race%in%c(1,2) &
    neut==0
  ,
  .(
    mu=weighted.mean(
      x=100*aff,
      w=weights
    )
  )
  ,
  by=c(
    'year',
    'dimension',
    'question',
    'race'
  )
]
tmpdf<-finaldf[,c('question','year')] %>% unique
tmptab <- table(tmpdf$question)
goodquestions <- names(tmptab[tmptab>=3])
plotdf <- plotdf[question%in%goodquestions]

plotdf$dimension<-factor(
  plotdf$dimension,
  levels=c("anxiety","punitive","mistrust"),
  labels=c("Anxiety","Punitiveness","Mistrust")
)

#order race
plotdf$race<-factor(
  plotdf$race,
  levels=c(1,2),
  labels=c("White","Black")
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=year,
    y=mu,
    color=question,
    group=question
  )
) +
  geom_point(
    size=1
  ) +
  geom_smooth(
    method='lm',
    size=1,
    se=F
  ) +
  facet_grid(
    rows=vars(dimension),
    cols=vars(race)
  ) +
  scale_color_discrete(name="") +
  theme_bw() +
  #theme(
  #  legend.position='bottom'
  #) +
  xlab('\nYear') +
  ylab('% Anxious, Mistrustful or Punitive\n')

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename='fig_po_raw.png',
  width=8,
  height=6
)


#########################################################
#########################################################

#### ----- BLACK ELITES, BLACK NONELITES ----

#exclude neutrals, 
#otherwise same analysis

#limit to questions for which you have enough respondents!

tmpdf <- finaldf[
  !is.na(race) & 
    race%in%c(2) &
    !is.na(ed) & 
    ed%in%c(1,4) &
    age==3 &
    year>1980 & 
    year<2000 &
    neut==0
  ,
][
  ,
  .(
    .N
  )
  ,
  by=c(
    'dimension',
    'question',
    'ed'
  )
]
#questions for which there aren't enough respondents
badQuestions <- tmpdf$question[tmpdf$N<15]

rawdf <- finaldf[
  !is.na(race) & 
    race%in%c(2) &
    !is.na(ed) & 
    ed%in%c(1,4) &
    #age==3 &
    year>1980 & 
    year<2000 &
    neut==0 &
    !question%in%badQuestions
  ,
  .(
    index=1:1000,
    mu=mean_se.boot(
      100 * aff,
      weights
    )
  )
  ,
  by=c(
    'dimension',
    'question',
    'ed'
  )
]

#FIG 3 - AVERAGES

plotdf <- rawdf[
  ,
  summarize.distribution3(mu)
  ,
  by=c(
    'dimension',
    'question',
    'ed'
  )
]

plotdf$ed<-factor(
  plotdf$ed,
  levels=c(1,4),
  labels=c("HS Dropout","College Grad")
)

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
  plotdf$dimension[plotdf$ed=='HS Dropout'],
  #plotdf$summary,
  plotdf$mu[plotdf$ed=='HS Dropout']
)
tmplevels<-plotdf$question[plotdf$ed=='HS Dropout'][neworder] %>%
  unique
tmplabels<-sapply(tmplevels,function(x) {
  getcode(x,"question","shortname",questionsdf)
})
plotdf$question<-factor(
  plotdf$question,
  levels=tmplevels,
  labels=tmplabels
)

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


#order ed
tmpcolors<-c('#009E73','#CC79A7')
names(tmpcolors)<-levels(plotdf$ed)

#dodge positions
dodge<-position_dodge(.6)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=question,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    color=ed
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
    #base_family="CM Roman",
    base_size=14
  ) +
  theme(legend.position="top") +
  #theme(axis.text.y=element_text(face=tmpface)) +
  theme(axis.text.y=element_text(size=8))

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename='fig3_po_averages_elites.pdf',
  width=6,
  height=8,
  dpi=300
)

#########################################################

#CRIME BILL
#crime bill, specifically
#too small to break out by age/ed
#so get the toplines by race for Section 6

#how many in support?
finaldf[
  !is.na(race) & 
    race==2 & 
    question=='crimebill.gallup' 
  ,
  .(
    index=1:1000,
    mu=mean_se.boot(
      100 * aff,
      weights
    )
  )
  
][
  ,
  summarize.distribution3(mu)
]

#how many opposed?
finaldf[
  !is.na(race) & 
    race==2 & 
    question=='crimebill.gallup' 
  ,
  .(
    index=1:1000,
    mu=mean_se.boot(
      100 * neg,
      weights
    )
  )
  
][
  ,
  summarize.distribution3(mu)
]



#########################################################

#DIFFERENCES

tmpdf <- pivot_wider(
  rawdf,
  names_from=ed,
  values_from=mu
) %>% data.table
tmpdf$mu <-  tmpdf$`4` - tmpdf$`1`
plotdf <- tmpdf[
  ,
  summarize.distribution3(mu)
  ,
  by=c(
    'dimension',
    'question'
  )
]

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
  filename='fig_po_diffs_elites.png',
  width=6,
  height=6
)
















