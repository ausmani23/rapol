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

#set seed
set.seed(23)

#########################################################
#########################################################

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

require(ggplot2)

#########################################################
#########################################################

#LOAD RELEVANT DATA

#load public opinion data
setwd(outputdir)
pundf<-read.csv(
  'fig_po_trends.csv',
  stringsAsFactors=F
)

#load crime data
setwd(datadir)
vdf<-read.csv(
  'tab_longviolence.csv'
)

#load race riots and protests data
setwd(datadir)
riotsdf<-haven::read_dta(
  'race_riot.dta'
)
riotsdf<-by(riotsdf,riotsdf$styr,function(df) {
  #df<-riotsdf[riotsdf$styr==56,]
  data.frame(
    var='riots',
    val=nrow(df[df$race1==1,]),
    year=paste0("19",unique(df$styr)) %>% as.numeric
  )
}) %>% rbind.fill

setwd(datadir)
protestdf<-haven::read_dta(
  'Ethnic_Collect_Action.dta'
)
protestdf<-by(protestdf,protestdf$styr,function(df) {
  #df<-riotsdf[riotsdf$styr==56,]
  data.frame(
    var='protest',
    val=nrow(df[df$race1==1,]),
    year=paste0("19",unique(df$styr)) %>% as.numeric
  )
}) %>% rbind.fill

#load Turchin's dataset
setwd(datadir); dir()
tdf <- fread(
  'american_violence_data_20241016_201046.csv'
) 
tdf$year<-lubridate::year(tdf$date)
tdf <- tdf[
  year%in%c(1950:2023) &
    str_detect(subtypes,'race|ethnic')
  ,
  .(
    var='turchin',
    val=.N
  )
  ,
  by=c('year')
]

#put this in rate basis by dividing by population
tmpvars<-c("year","var","val")
protestdf<-rbind.fill(
  riotsdf[,tmpvars],
  protestdf[,tmpvars],
  tdf[,tmpvars,with=F]
)
setwd(datadir); dir()
popdf<-read.csv(
  'incrates_subnationalstate.csv',
  stringsAsFactors=F
)
tmp<-popdf$statename=="United States" &
  !is.na(popdf$statename)
popdf<-popdf[tmp,c("year","population_census")]
protestdf<-merge(
  protestdf,
  popdf
)
protestdf$val<-10^7*protestdf$val/protestdf$population_census
protestdf$population_census<-NULL

#########################################################
#########################################################

#PLOT PROTESTS AND RIOTS

plotdf<-protestdf
tmplevels<-c(
  "riots",
  "protest",
  "turchin"
)
tmplabels<-c(
  "Riots (Olzak)",
  "Protests (Olzak)",
  "Political Violence (Turchin)"
)
plotdf$var<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)

tmpcolors_riot<-c(
  'grey',
  'darkgrey',
  "black"
)
names(tmpcolors_riot)<-
  levels(plotdf$var)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=val,
    color=var,
    group=var
  )
) +
  geom_line(
    size=2
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors_riot
  ) +
  xlab("") +
  ylab("Events per 10 Million People\n") +
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  )

tmpname<-"fig_po_protests.png"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=8,
  height=6
)
output(plotdf,tmpname)

#########################################################
#########################################################

#FIG - PLOT SHAPE OF PROTESTS/RIOTS

#plot protests/riots/crime and white public opinion
#this supports the point made in the correlations above
#the shape of this curve mirrors the crime rate, not protests..

setwd(outputdir)
pundf<-read.csv(
  'fig_po_trends.csv',
  stringsAsFactors=F
)
pundf$var<-paste0(
  pundf$race,"_",pundf$facet,"_",pundf$dimension
) %>% tolower
tmprows<-str_detect(pundf$var,"estimated") &
  pundf$dimension!="mistrust"
pundf$val<-pundf$mu
tmpcols<-c("year","var","val")
pundf<-pundf[tmprows,tmpcols]

setwd(datadir)
vdf<-read.csv(
  'tab_longviolence.csv'
)

#merge
plotdf<-rbind.fill(
  protestdf,
  pundf,
  vdf
)

#common years
tmp<-plotdf$year%in%1950:2014 &
  !is.na(plotdf$val)
plotdf<-plotdf[tmp,]

#standardize over these common years
plotdf<-by(plotdf,plotdf$var,function(df) {
  df$val<-scale(df$val)
  df
}) %>% rbind.fill

unique(plotdf$var)
tmp<-plotdf$var%in%c(
  "protest",
  "riots",
  "turchin",
  "white_estimated_punitiveness",
  "black_estimated_punitiveness",
  "fbivcrt",
  "fbihom"
)
plotdf<-plotdf[tmp,]

tmplevels<-c(
  "white_estimated_punitiveness",
  "black_estimated_punitiveness",
  "protest",
  "riots",
  "turchin",
  "fbivcrt",
  "fbihom"
)
tmplabels<-c(
  "White Punitiveness",
  "Black Punitiveness",
  "Protests (Olzak)",
  "Riots (Olzak)",
  "Political Violence (Turchin)",
  "Violent Crime",
  "Homicide"
)
plotdf$var<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)
tmpcolors<-c(
  'red',
  'blue',
  'grey',
  'darkgrey',
  "black",
  'blue',
  'darkblue'
)
names(tmpcolors)<-levels(plotdf$var)

#split into two facets
tmp<-plotdf$var%in%c('Protests (Olzak)','Riots (Olzak)','Political Violence (Turchin)')
plotdf$facet[tmp]<-'Protests'
plotdf$facet[!tmp]<-'Crime'
tmp<-plotdf$var%in%c('White Punitiveness','Black Punitiveness')
plotdf$facet <- factor(plotdf$facet,c('Protests','Crime'))
plotdf$facet[tmp]<-NA
tmpdf<-plotdf[is.na(plotdf$facet),]; tmpdf$facet<-NULL

g.tmp<- ggplot(
  plotdf[!is.na(plotdf$facet),],
  aes(
    x=year,
    y=val,
    group=var,
    color=var
  )
) +
  stat_smooth(
    geom="line",
    size=1, 
    se=FALSE,
    alpha=0.5
  ) +
  facet_wrap(
    ~ facet 
  ) +
  geom_smooth(
    data=tmpdf[tmpdf$var!='Black Punitiveness',],
    se=F,
    size=1.5
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors,
    limits = c(
      #'Black Punitiveness',
      'White Punitiveness',
      'Protests (Olzak)',
      'Riots (Olzak)',
      'Political Violence (Turchin)',
      'Violent Crime',
      'Homicide'
    )
  ) +
  xlab("") +
  ylab("Normalized Level\n") +
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction='horizontal',
    legend.text = element_text(size=8)
  )

tmpname<-"fig4_po_correlations.pdf"
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=10,
  height=6,
  dpi=300
)
output(plotdf,tmpname)

#how long is lag between protests/riots peak
#and  punitiveness peak
plotdf$facet<-NULL
tmpdf<-spread(plotdf,var,val)
tmpdf$year[sapply(tmpdf,which.max)]



