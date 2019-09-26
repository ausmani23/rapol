#this file does regression prep

#########################################################
#########################################################

#clear workspace
rm(list=ls())

homedir<-file.path(
  "c:",
  "users",
  "adaner",
  "dropbox",
  "data",
  "incarceration, public opinion"
)
metadir<-file.path(
  homedir,
  "meta"
)
filesdir<-file.path(
  homedir,
  "files"
)
codedir<-file.path(
  homedir,
  "code, round 2"
)
outputdir<-file.path(
  homedir,
  "output"
)

#load required packages
require(stringr)
require(plyr)
require(dplyr)
require(tidyr)

#extra
require(data.table)
require(lme4)
require(rstanarm)

#helper functions
setwd(codedir); dir()
source('readpoll_functions.R')
source('getinfo2.R')

#set seed
set.seed(23)

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
#load fonts
loadfonts(quiet=T) #register w/ pdf
loadfonts(quiet=T,device = "win") #register w/ windows
fonts()
#get ghostscript, for tex output
gsdir<-file.path(
  "c:",
  "Program Files",
  "gs"
)
gsdir_full<-file.path(
  gsdir,
  dir(gsdir),
  "bin",
  "gswin64c.exe"
)
Sys.setenv(
  R_GSCMD = gsdir_full
)
#initialize graphlist
gs.list<-list()

#quick function to outputdfs
output <- function(df,tmpname) {
  setwd(outputdir)
  if( str_detect(tmpname,"\\.pdf$") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$",".csv")
  write.csv(
    df,
    tmpname,
    row.names=F
  )
}

#########################################################
#########################################################

#TRENDS GRAPH

plotdfs<-list()

#ARTIFICIAL DATA
#this plots the 'conventional' expectations
#this is how to generate the points
loopdf<-expand.grid(
  race=c(1,2),
  dimension=c(
    'punitive',
    'anxiety',
    'mistrust'
  )
)
#these are start and end points
c<-20
loopdf$startpoint<-c(
  50,50-c,50,50-c,50,50+c
)/100 
loopdf$endpoint<-c(
  50+c,50-c,50+c,50-c,50-c,50+c
)/100
tmpseq.i<-1:nrow(loopdf)
tmpdf<-lapply(tmpseq.i,function(i) {
  thisrow<-loopdf[i,]
  m<-(thisrow$endpoint-thisrow$startpoint)/(2014-1955)
  b<-thisrow$startpoint - (1955 * m)
  fun.y<-function(x) {
    m * x + b
  }
  yhat<-sapply(
    1955:2014,
    fun.y
  )
  data.frame(
    state_alpha2="all",
    race=thisrow$race,
    dimension=thisrow$dimension,
    year=1955:2014,
    mu=yhat,
    stringsAsFactors=F
  )
}) %>% rbind.fill
tmpdf$facet<-'conventional'

plotdfs[['conventional']]<-tmpdf

#ACTUAL DATA
#this plots our best guess of PO trends
setwd(filesdir)
fulldf<-read.csv(
  'pstratdf.csv',
  stringsAsFactors=F
)
tmp<-fulldf$state_alpha2=="all" & 
  fulldf$race!="all" &
  fulldf$year!="all" &
  fulldf$dimension!="all"
tmp<-tmp & fulldf$race!=3
tmpdf<-fulldf[tmp,]
tmpdf$facet<-"actual"
plotdfs[['actual']]<-tmpdf

#combine the dfs
plotdf<-rbind.fill(plotdfs)

plotdf$year<-as.numeric(plotdf$year)

#factors
plotdf$race<-factor(
  plotdf$race,
  levels=c(1,2),
  labels=c("White","Black")
)
tmpcolors<-c('#999999','black')
names(tmpcolors)<-levels(plotdf$black)

tmplevels<-c(
  "punitive",
  "anxiety",
  "mistrust"
)
tmplabels<-c(
  "Punitive",
  "Anxiety",
  "Mistrust"
)
plotdf$dimension<-factor(
  plotdf$dimension,
  levels=tmplevels,
  labels=tmplabels
)

tmplevels<-c(
  "conventional",
  "actual"
)
tmplabels<-c(
  "Conventional",
  "Estimated"
)
plotdf$facet<-factor(
  plotdf$facet,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=mu,
    color=race
  )
) + 
  geom_point(
    size=0.8,
    alpha=0.5
  ) +
  geom_smooth(
    size=1.5,
   se=F
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors#,
    #guide=guide_legend(
    #  direction="horizontal"
    #)
  ) + 
  facet_grid(
    dimension ~ facet
  ) +
  xlab("") +
  ylab("P(A, M or P Response)\n") +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) 
 
tmpname<-"fig_trends.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=6,
  height=6
)
output(plotdf,tmpname)

#########################################################
#########################################################

#OUTPUT
#output graphlist
setwd(outputdir)
this.sequence<-seq_along(gs.list)
for(i in this.sequence) {
  print(
    paste0(
      "saving ",i," of ",length(this.sequence)
    )
  )
  thiselement<-gs.list[[i]]
  ggsave(
    filename="tmp.pdf",
    plot=thiselement$graph,
    width=thiselement$width,
    height=thiselement$height
  )
  #embed font
  embed_fonts(
    file="tmp.pdf",
    outfile=thiselement$filename
  )
  file.remove(
    "tmp.pdf"
  )
}

#also output to main incdir
tmpdir<-file.path(
  "c:",
  "users",
  "adaner",
  "dropbox",
  "data",
  "incarceration",
  "Analysis, Revisions",
  "output"
)
setwd(tmpdir)
this.sequence<-seq_along(gs.list)
for(i in this.sequence) {
  print(
    paste0(
      "saving ",i," of ",length(this.sequence)
    )
  )
  thiselement<-gs.list[[i]]
  ggsave(
    filename="tmp.pdf",
    plot=thiselement$graph,
    width=thiselement$width,
    height=thiselement$height
  )
  #embed font
  embed_fonts(
    file="tmp.pdf",
    outfile=thiselement$filename
  )
  file.remove(
    "tmp.pdf"
  )
}


