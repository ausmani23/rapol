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

#GATHER DATA/COMBINE
#load data
setwd(filesdir); dir()

#AVERAGES
avgdf<-read.csv(
  'q_avgs.csv',
  stringsAsFactors=F
)

tmpdf<-read.csv(
  'q_avghat.csv',
  stringsAsFactors=F
)
tmpdf$question<-paste0(
  tmpdf$dimension,".sum"
)
#don't need all.sum
tmp<-tmpdf$question=="all.sum"
tmpdf<-tmpdf[!tmp,]
tmpdf$summary<-T

#won't show average, 
#since this is confounded by year
#so the level is very difficult to interpret
#it's better interpreted off the 
tmpdf$mu<-tmpdf$mu.min<-tmpdf$mu.max<-NA
#if you want to show it, comment out line above


#before, when estimated black/white
if("race2"%in%names(tmpdf)) {
  tmp<-names(tmpdf)=="race2"
  names(tmpdf)[tmp]<-"race"
}

keepvars<-c(
  "question",
  "dimension",
  "race",
  "summary",
  "mu",
  "mu.min",
  "mu.max"
)
avgdf$summary<-F
avgdf<-rbind.fill(
  avgdf,
  tmpdf[,keepvars]
)


#DIFFERENCES
diffdf<-read.csv(
  'q_diffs.csv',
  stringsAsFactors=F
)
tmpdf<-read.csv(
  'q_diffhat.csv',
  stringsAsFactors=F
)
tmpdf$question<-paste0(
  tmpdf$dimension,".sum"
)
#don't need all.sum
tmp<-tmpdf$question=="all.sum"
tmpdf<-tmpdf[!tmp,]

keepvars<-c(
  "question",
  "dimension",
  "difftype",
  "summary",
  "mu",
  "mu.min",
  "mu.max",
  "pval.class"
)
diffdf$summary<-F
diffdf$difftype<-"diff"
tmpdf$summary<-T
diffdf<-rbind.fill(
  diffdf,
  tmpdf[,keepvars]
)

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
require(scales)
#load fonts
loadfonts(quiet=T) #register w/ pdf
loadfonts(device = "win",quiet=T) #register w/ windows
#fonts()
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

#########################################################
#########################################################

#PLOT AVERAGES
tmp<-avgdf$race%in%c(1,2)
plotdf<-avgdf[tmp,]

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
  plotdf$mu,
  plotdf$question,
  function(x) rep(x[2],2)
) %>% unlist
neworder<-order(
  plotdf$dimension,
  plotdf$summary,
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

#add face
tmpface<-rep("plain",length(tmplabels))
tmp<-!str_detect(tmplabels,"\\(")
tmpface[tmp]<-"bold"

#get location of dividing lines
tmpdf<-unique(plotdf[neworder,c("question","dimension")])
diffs<-tmpdf$dimension %>%
  as.numeric %>% diff
hlines<-which(diffs==1) + 0.5

#order race
plotdf$race<-factor(
  plotdf$race,
  levels=c(1,2),
  labels=c("White","Black")
)
tmpcolors<-c('#999999','black')
names(tmpcolors)<-levels(plotdf$black)

#dodge positions
dodge<-position_dodge(.6)

g.tmp<-ggplot(
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
    width=0.2,
    position=dodge
  ) + 
  geom_point(
    size=1,
    position=dodge
  ) +
  geom_hline(
    yintercept=50,
    linetype='dashed',
    color='red'
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors,
    guide=guide_legend(
      direction="horizontal"
    )
  ) +
  geom_vline(
    xintercept=hlines[1],
    linetype='dashed',
    alpha=0.3
  ) + 
  geom_vline(
    xintercept=hlines[2],
    linetype='dashed',
    alpha=0.3
  ) +
  xlab("") +
  ylab("\nProportion A, M or P") +
  coord_flip() +  
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) +
  theme(legend.position="top") +
  theme(axis.text.y=element_text(face=tmpface))

gs.list[["fig_avgs.pdf"]]<-list(
  graph=g.tmp,
  filename="fig_avgs.pdf",
  width=6,
  height=10
)

#########################################################
#########################################################

#PLOT DIFFERENCES
#aux plot will take difftype avg
tmp<-diffdf$difftype=="avg"
auxplotdf<-diffdf[tmp,]
tmp<-diffdf$difftype!="avg" 
plotdf<-diffdf[tmp,]

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
  plotdf$summary,
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

#factor in auxplot, too
auxplotdf$question<-factor(
  auxplotdf$question,
  levels=tmplevels,
  labels=tmplabels
)

#add face
tmpface<-rep("plain",length(tmplabels))
tmp<-!str_detect(tmplabels,"\\(")
tmpface[tmp]<-"bold"

#get location of dividing lines
tmpdf<-unique(plotdf[neworder,c("question","dimension")])
diffs<-tmpdf$dimension %>%
  as.numeric %>% diff
hlines<-which(diffs==1) + 0.5

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
    width=0.2
  ) + 
  # geom_errorbar(
  #   data=auxplotdf,
  #   aes(
  #     x=question,
  #     ymin=mu.min,
  #     ymax=mu.max
  #   ),
  #   width=0.1,
  #   alpha=0.3,
  #   color='red',
  #   linetype='dashed'
# ) +
geom_point(
  data=plotdf,
  aes(
    x=question,
    y=mu,
    shape=pval.shp
  ),
  size=2
) +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='red'
  ) +
  geom_vline(
    xintercept=hlines[1],
    linetype='dashed',
    alpha=0.3
  ) + 
  geom_vline(
    xintercept=hlines[2],
    linetype='dashed',
    alpha=0.3
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    guide=guide_legend(
      direction="horizontal"
    )
  ) +
  xlab("") +
  ylab("\nWhite-Black Gap") +
  coord_flip() +  
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) +
  theme(legend.position="top")  +
  theme(axis.text.y=element_text(face=tmpface))

gs.list[["fig_diffs.pdf"]]<-list(
  graph=g.tmp,
  filename="fig_diffs.pdf",
  width=6,
  height=10
)

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


