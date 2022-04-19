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
require(haven)
require(readr)
require(plm)

#set dirs
rootdir<-find_root(
  criterion=has_file('_rapol.Rproj')
)
codedir<-file.path(rootdir,"code")
setwd(codedir); dir()
source('dirs.R')

#load helper functions
setwd(dcodedir)
source('beofunctions.R')
source('beofunctions2.R')

#########################################################
#########################################################

#load all the info needed for output
setwd(filesdir); dir()

#dd
dddf<-read.csv(
  '03_dind_resultsdf.csv',
  stringsAsFactors=F
)
dddf$approach<-"dd"

#regs
regdf<-read.csv(
  '03_dind_regresultsdf.csv',
  stringsAsFactors=F
)
regdf$approach<-"reg"

#robustness
robsdf<-read.csv(
  '03_dind_robustnessdf.csv',
  stringsAsFactors=F
)
robsdf$approach<-"reg"
robsdf$pref<-F

#identify the preferred ests in dd
tmp<-dddf$dv%in%c(
  "incrt_t_jur",
  "imprt_t_jur",
  "officers_pcap"
)
tmp<-tmp & 
  dddf$spec%in%c("controls") &
  dddf$method=="normal"
dddf$pref<-F
dddf$pref[tmp]<-T

#add 'conventional' estimates
tmpdf<-data.frame(
  pref=T,
  approach=c('expectation','expectation'),
  dv=c("imprt_t_jur","officers_pcap"),
  musd=c(-0.5,-0.5),
  musd.min=c(-1,-1),
  musd.max=c(0,0),
  pval.class="at alpha=0.05"
)

#add 'conventional' estimates
tmpdf2<-data.frame(
  pref=F,
  approach=c('expectation'),
  spec=c('racialthreat'),
  dv=c("welfbenefits"),
  musd=c(-0.5),
  musd.min=c(-1),
  musd.max=c(0),
  pval.class="at alpha=0.05",
  method='normal'
)

#put them all together
finaldf<-rbind.fill(
  regdf,
  robsdf,
  dddf,
  tmpdf,
  tmpdf2
)

#########################################################
#########################################################

#plotting prelims
require(ggplot2)
require(ggthemes)
require(extrafont)
require(RColorBrewer)
# #load fonts
# loadfonts(quiet=T) #register w/ pdf
# loadfonts(device = "win",quiet=T) #register w/ windows
# #get ghostscript, for tex output
# Sys.setenv(
#   R_GSCMD = gsdir_full
# )
# #initialize graphlist
# gs.list<-list()

#quick function to outputdfs
output <- function(df,tmpname) {
  setwd(outputdir)
  if( str_detect(tmpname,"\\.pdf$|\\.png") ) 
    tmpname<-str_replace(tmpname,"\\.pdf$|\\.png",".csv")
  write.csv(
    df,
    tmpname,
    row.names=F
  )
}

#########################################################
#########################################################

#add shape et al to finaldf
#add pval info to shape of point
finaldf$pval.shp<-NA
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.01"]<-1
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.05"]<-2
finaldf$pval.shp[finaldf$pval.class=="at alpha=0.10"]<-3
finaldf$pval.shp[finaldf$pval.class=="not sig"]<-4
finaldf$pval.shp<-factor(
  finaldf$pval.shp,
  levels=c(1,2,3,4),
  labels=c(
    "at alpha=0.01",
    "at alpha=0.05",
    "at alpha=0.10",
    "not sig"
  )
)
#tmpshapes
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(finaldf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

#get pval fill, for tile
finaldf$pval.fill<-NA
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.01"]<-4
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.05"]<-3
finaldf$pval.fill[finaldf$pval.class=="at alpha=0.10"]<-2
finaldf$pval.fill[finaldf$pval.class=="not sig"]<-1
negmu<-ifelse(finaldf$mu<0,-1,1)
finaldf$pval.fill<-finaldf$pval.fill * negmu
pval.labels<-c("at alpha=0.01","at alpha=0.05","at alpha=0.10","")
tmplabels<-c(
  paste0("- /",pval.labels),
  paste0("+ /",rev(pval.labels))
)
#assign levels,colors
finaldf$pval.fill<-factor(
  finaldf$pval.fill,
  levels=c(-4,-3,-2,-1,1,2,3,4),
  labels=tmplabels
)
#for colors, consult brewer
brewer.pal.info
tmpcolors<-brewer.pal(8,"RdYlGn")
names(tmpcolors)<-levels(finaldf$pval.fill)
fill.labels<-c(
  expression(paste(alpha==0.01,", ",beta<0)),
  expression(paste(alpha==0.05,", ",beta<0)),
  expression(paste(alpha==0.10,", ",beta<0)),
  expression(paste(beta<0)),
  expression(paste(beta>0)),
  expression(paste(alpha==0.10,", ",beta>0)),
  expression(paste(alpha==0.05,", ",beta>0)),
  expression(paste(alpha==0.01,", ",beta>0))
)

#########################################################
#########################################################

#FIG 1 - PREFERRED ESTS

#get preferred ests
tmp<-finaldf$pref & 
  finaldf$dv%in%c(
    "imprt_t_jur",
    "officers_pcap"
  ) 
tmp2<-finaldf$approach=="reg" & 
  finaldf$iv=="beopct_all" &
  finaldf$type=="longrun"
tmp2<-tmp2 | (
  finaldf$approach=="dd" & 
    finaldf$iv=="t.post.t"
)
tmp2<-tmp2 | (
  finaldf$approach=="expectation"
)
tmp<-tmp & tmp2
plotdf<-finaldf[tmp,]

#dv
sapply(plotdf$dv,getvarorder) %>%
  sort %>% names %>% unique
tmplevels<-plotdf$dv %>% unique
tmplabels<-sapply(
  tmplevels,
  getvarname
)
plotdf$dv<-factor(
  plotdf$dv,
  tmplevels,
  tmplabels
)

#approach
tmplevels<-c(
  "expectation",
  "dd",
  "reg"
)
tmplabels<-c(
  "Conventional View",
  "Estimated, D-in-D",
  "Estimated, ADLs"
)
plotdf$approach<-factor(
  plotdf$approach,
  tmplevels,
  tmplabels
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=dv,
    y=musd,
    ymin=musd.min,
    ymax=musd.max,
    shape=pval.shp
  )
) + 
  geom_point(
    size=2
  ) +
  geom_errorbar(
    size=0.4,
    width=0.2
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='black'
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) + 
  scale_color_discrete(
    name=""
  ) +
  ylab("\nStandardized Impact of Black Representation") +
  xlab("") +
  coord_flip() +
  facet_wrap(
    ~ approach,
    ncol=1
  ) + 
  theme_bw()

setwd(outputdir)
tmpname<-"fig_dind_prefests.png"
ggsave(
  plot=g.tmp,
  tmpname,
  width=8,
  height=6
)
output(plotdf,tmpname)


#########################################################
#########################################################

#FIG 2 - WELFARE BENEFITS

#also export welfbenefits
tmp<-finaldf$dv=="welfbenefits" &
  finaldf$spec%in%c(
    "simple",
    "controls",
    "racialthreat"
  ) &
  finaldf$method=="normal"
plotdf<-finaldf[tmp,]

plotdf$dv <- factor(
  plotdf$dv,
  levels='welfbenefits',
  labels='Welfare Benefits'
)

plotdf$spec <- factor(
  plotdf$spec,
  levels=c(
    'racialthreat',
    'simple',
    'controls'
  ) %>% rev,
  labels=c(
    'If Racial Threat',
    'Estimated (Means)',
    'Estimated (Preferred)'
  ) %>% rev
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=spec,
    y=musd,
    ymin=musd.min,
    ymax=musd.max,
    shape=pval.shp
  )
) + 
  geom_point(
    size=2
  ) +
  geom_errorbar(
    size=0.4,
    width=0.2
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed',
    color='black'
  ) +
  scale_shape_manual(
    name="",
    values=tmpshapes,
    labels=shp.labels,
    drop=F
  ) + 
  scale_color_discrete(
    name=""
  ) +
  ylab("\nStandardized Impact of Black Representation") +
  xlab("") +
  coord_flip() +
  facet_wrap(
    ~ dv,
    ncol=1
  ) +
  theme_bw()

setwd(outputdir)
tmpname<-"fig_dind_welfare.png"
ggsave(
  plot=g.tmp,
  tmpname,
  width=8,
  height=3
)
output(plotdf,tmpname)

#is welfare ever negative?
tmp<-finaldf$dv=="welfbenefits" &
  finaldf$method=="normal"
plotdf<-finaldf[tmp,]
plotdf[plotdf$mu<0,]


#########################################################
#########################################################

#FIG 3 - ROBUST ESTS

#which are the pref mods?
tmp<-finaldf$pref
finaldf[tmp,"mname"]

#two secitons, two dvs, one estimate
tmp<-!finaldf$pref
plotdf<-finaldf[tmp,]

setwd(metadir); dir()
tmpdf<-read.csv(
  '03_dind_robdisplay_EDITED.csv',
  stringsAsFactors=F
)
tmp<-tmpdf$facet!=""
tmpdf<-tmpdf[tmp,]
plotdf<-merge(
  tmpdf,
  plotdf,
  all.x=T
)

#restrict to desired coefs
tmp<-plotdf$iv%in%c(
  "beopct_all",
  "t.post.t"
) &
  (
    is.na(plotdf$type) 
    | plotdf$type!="longrun"
  )
plotdf<-plotdf[tmp,]

#there should be one estimate per model
tmptab<-tapply(
  plotdf$musd,
  plotdf$mname,
  function(x) length(unique(x))
)
if(max(tmptab)>1)
  stop()
table(plotdf$x)

#now we want to order the mods
# tmpdf<-unique(plotdf[,c("facet","y")])
# write.csv(
#   tmpdf,
#   "roborder_EDIT.csv",
#   row.names=F
# )
tmpdf<-read.csv(
  '03_dind_roborder_EDITED.csv',
  stringsAsFactors=F
)
plotdf<-merge(
  plotdf,
  tmpdf
)

#factors
tmplevels<-c(
  "dd",
  "reg"
)
tmplabels<-c(
  "D-in-D",
  "ADL"
)
plotdf$facet<-factor(
  plotdf$facet,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "imprt",
  "officers"
)
tmplabels<-c(
  "Incarceration",
  "Police"
)
plotdf$x<-factor(
  plotdf$x,
  tmplevels,
  tmplabels
)

tmporder<-order(plotdf$facet,plotdf$order)
tmplevels<-plotdf$ydisp[tmporder] %>%
  unique
plotdf$ydisp<-factor(
  plotdf$ydisp,
  rev(tmplevels),
  rev(tmplevels)
)

#add text
plotdf$text<-formatC(
  plotdf$musd,
  digits=2,
  replace.zero=T,
  zero.print="0",
  format='f',
  preserve.width=T
)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=x,
    y=ydisp,
    fill=pval.fill,
    label=text
  )
) +
  geom_tile(
    color='black',
    width=1
  ) +
  geom_text(
    color='black'
  ) +
  facet_wrap(
    ~ facet,
    scales='free'
  ) +
  scale_fill_manual(
    name="",
    values=tmpcolors,
    labels=fill.labels,
    drop=F
  ) +
  xlab("") +
  ylab("") +
  theme_bw() +
  theme(
    panel.grid = element_blank(),
    axis.line= element_blank(),
    panel.border=element_blank(),
    axis.ticks = element_blank()
  )

setwd(outputdir)
tmpname<-"fig_dind_robests.png"
ggsave(
  plot=g.tmp,
  tmpname,
  width=8,
  height=4
)
output(plotdf,tmpname)

#########################################################
#########################################################

#FIG - ILLUSTRATE THE RISE IN BLACK REPRESNTATION

setwd(filesdir); dir()
tmpdf<-read.csv(
  '03_dind_beodf_dd.csv',
  stringsAsFactors=F
)
tmpdf<-data.table(tmpdf)
plotdf<-tmpdf[
  year%in%1980:2000,
  .(beopct=mean(beopct_all)),
  by=c("year","t")
]

tmplevels<-c(
  0,1
)
tmplabels<-c(
  "Not Redistricted",
  "Redistricted"
)
plotdf$t<-factor(
  plotdf$t,
  tmplevels,
  tmplabels
)

tmpcolors<-c(
  'blue','red'
)
names(tmpcolors)<-
  levels(plotdf$t)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=beopct,
    group=t,
    color=t
  )
) +
  geom_line(
    size=1
  ) + 
  geom_vline(
    xintercept=1990,
    linetype='dashed',
    color='black'
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  xlab("") + 
  ylab("Black Representatives (%)\n") +
  theme_bw() +
  theme(
    legend.direction='horizontal',
    legend.position='bottom'
  )

tmpname<-paste0("fig_dind_beoshock.png")
setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=6,
  height=6
)
