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
require(data.table)
require(lme4)
require(rstanarm)

#set dirs
rootdir<-find_root(
  criterion=has_file('_rapol.Rproj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(scales)
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

#quick function to outputdfs
output <- function(df,tmpname) {
  setwd(outputdir)
  if( str_detect(tmpname,"\\.pdf$|\\.png$") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$|\\.png$",".csv")
  write.csv(
    df,
    tmpname,
    row.names=F
  )
}

#########################################################
#########################################################

#citations graph
setwd(datadir); dir()

tmpdf<-read.csv(
  'citecount.csv',
  stringsAsFactors=F
)
tmpdf$citerate<-tmpdf$cites/(2022 - tmpdf$published)

#citerate
tmp<-tmpdf$graph
plotdf<-tmpdf[tmp,]

tmplevels<-plotdf$title[order(plotdf$citerate)]
plotdf$title<-factor(
  plotdf$title,
  tmplevels,
  tmplevels
)

# tmpcolors_bat<-c(
#   rep(,length(plotdf$title)-1),
#   orange
# )
# names(tmpcolors_bat)<-levels(plotdf$title)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=title,
    y=citerate#,
    #color=title,
    #fill=title
  )
) +
  geom_bar(
    stat='identity',
    width=0.3,
    color='black'
  ) +
  # scale_color_manual(
  #   name="",
  #   values=tmpcolors_bat,
  #   guide=F
  # ) +
  # scale_fill_manual(
  #   name="",
  #   values=tmpcolors_bat,
  #   guide=F
  # ) +
  xlab("") +
  ylab("\nCitations Per Year") +
  theme_bw() +
  coord_flip() +
  theme(
    axis.text.x=element_text(size=8)
  )

setwd(outputdir)
tmpname<-"fig_citerate.png"
ggsave(
  filename=tmpname,
  plot=g.tmp,
  width=8,
  height=3
)

#########################################################
#########################################################

#FIG - THE HOCKEY STICK PLOT
setwd(datadir); dir()

#add other countries

plotdf <- fread('histpundf_national_220328.csv')
plotdf$fiveyear<-floor(plotdf$year/5)*5
plotdf$usa<-plotdf$cowcode==2
plotdf<-plotdf[
  statistic%in%c('prisoners','population') & 
    compset=='Rich'
  ,
  .(
    value=median(value)
  )
  ,
  by=c(
    'usa',
    'fiveyear',
    'statistic'
  )
]
plotdf<-spread(plotdf,statistic,value)
plotdf$prisoners_rate <- 10^5 *  plotdf$prisoners/plotdf$population
plotdf<-plotdf[!is.na(plotdf$prisoners_rate),]

tmplevels<-c(T,F)
tmplabels<-c('USA',"Other Rich Countries")
plotdf$usa<-factor(plotdf$usa,tmplevels,tmplabels)

tmpcolors<-c('black','grey')
names(tmpcolors)<-tmplabels
firstyear<-min(plotdf$fiveyear[plotdf$usa=='USA'])

g.tmp<- ggplot(
  plotdf[fiveyear>=firstyear],
  aes(
    x=fiveyear,
    y=prisoners_rate,
    color=usa,
    group=usa
  )
) +
  geom_line(
    size=1
  ) +
  scale_color_manual(
    values=tmpcolors,
    name=""
  ) +
  xlab('') +
  ylab('Prisoners per 100,000\n') +
  theme_bw() +
  theme(
    legend.position='bottom',
    legend.direction='horizontal'
  )

setwd(outputdir)
tmpname<-'fig_comparative.png'
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=8,
  height=4
)
output(plotdf,tmpname)

#########################################################
#########################################################