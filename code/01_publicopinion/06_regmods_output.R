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

#helper functions
setwd(pcodedir); dir()
source('readpoll_functions.R')
source('getinfo2.R')
source('functions.R')

#load data and mods
setwd(filesdir); dir()
finaldf<-fread('01po_dataframe.csv')
mods<-readRDS('01po_modslist.RDS')
#modnames<-names(modslist)
mods<-lapply(mods,function(x) x$m)

#set seed
set.seed(23)

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


#identify prefmods
setwd(filesdir); dir()
regmodsdf<-read.csv(
  '01po_regmods_info.csv',
  stringsAsFactors=F
)

#choose the pref models
prefmodsdf<-by(regmodsdf,regmodsdf$dimension,function(df) {
  #df<-regmodsdf[regmodsdf$dimension=="all",]
  tmp<-df$warnings==""
  if(sum(tmp)>0) {
    tmpdf<-df[tmp,]
    tmpdf[order(tmpdf$bic.rank),][1,]
  } else {
    df[df$pref,][1,]
  }
}) %>% rbind.fill
prefmod_names<-prefmodsdf$mname
prefmods<-mods[prefmod_names]

#there should be 3 of these
if(length(prefmods)!=3)
 stop('problem')

#########################################################
#########################################################

#DEPRECATED

# #get coefs from prefmods
# tmpseq.i<-seq_along(prefmods)
# plotdf<-lapply(tmpseq.i,function(i) {
#   #i<-1
#   print(i)
#   thismod<-prefmods[[i]]
#   thismodname<-names(prefmods)[i]
#   tmpsum<-summary(thismod)
#   tmpcoefs<-tmpsum$coefficients
#   returndf<-data.frame(
#     mname=thismodname,
#     var=row.names(tmpcoefs),
#     mu=tmpcoefs[,1],
#     mu.min=tmpcoefs[,1] - 1.96 * tmpcoefs[,2],
#     mu.max=tmpcoefs[,1] + 1.96 * tmpcoefs[,2],
#     pval=tmpcoefs[,4],
#     stringsAsFactors=F
#   )
#   row.names(returndf)<-NULL
#   returndf
# }) %>% rbind.fill
# 
# #get dimension
# plotdf$dimension<-str_extract(
#   plotdf$mname,
#   "all|anxiety|mistrust|punitive"
# )
# 
# #get rid of intercept
# tmp<-plotdf$var=="(Intercept)" |
#   plotdf$dimension=="all"
# plotdf<-plotdf[!tmp,]
# 
# #pvals
# plotdf$pval.class<-get.pvals.class(plotdf$pval)
# #add pval info to shape of point
# plotdf$pval.shp<-NA
# plotdf$pval.shp[plotdf$pval.class=="at alpha=0.01"]<-1
# plotdf$pval.shp[plotdf$pval.class=="at alpha=0.05"]<-2
# plotdf$pval.shp[plotdf$pval.class=="at alpha=0.10"]<-3
# plotdf$pval.shp[plotdf$pval.class=="not sig"]<-4
# plotdf$pval.shp<-factor(
#   plotdf$pval.shp,
#   levels=c(1,2,3,4),
#   labels=c(
#     "at alpha=0.01",
#     "at alpha=0.05",
#     "at alpha=0.10",
#     "not sig"
#   )
# )
# #tmpshapes
# tmpshapes<-c(8,4,16,1)
# names(tmpshapes)<-levels(plotdf$pval.shp)
# shp.labels<-c(
#   bquote(alpha == 0.01),
#   bquote(alpha == 0.05),
#   bquote(alpha == 0.10)
# )
# 
# #divide by 4 rule
# #(means, roughly shows marginal effect in prob)
# plotdf$mu<-plotdf$mu/4
# plotdf$mu.min<-plotdf$mu.min/4
# plotdf$mu.max<-plotdf$mu.max/4
# 
# #factors
# plotdf$var %>% unique
# tmplevels<-c(
#   "gender",
#   "ed",
#   "age",
#   "factor(race)2",
#   "factor(race)3"
# ) %>% rev
# tmplabels<-c(
#   "Female",
#   "Education",
#   "Age",
#   "Black",
#   "Other"
# ) %>% rev
# plotdf$varname<-factor(
#   plotdf$var,
#   tmplevels,
#   tmplabels
# )
# 
# tmplevels<-c(
#   "anxiety",
#   "mistrust",
#   "punitive"
# ) %>% rev
# tmplabels<-c(
#   "Anxiety",
#   "Mistrust",
#   "Punitiveness"
# ) %>% rev
# plotdf$dimension<-factor(
#   plotdf$dimension,
#   tmplevels,
#   tmplabels
# )
# 
# g.tmp<-ggplot(
#   plotdf,
#   aes(
#     x=varname,
#     y=mu,
#     ymin=mu.min,
#     ymax=mu.max,
#     shape=pval.shp
#   )
# ) + 
#   geom_point(
#     size=2
#   ) +
#   geom_errorbar(
#     size=0.4,
#     width=0.2
#   ) +
#   geom_hline(
#     yintercept=0,
#     linetype='dashed',
#     color='black'
#   ) +
#   scale_shape_manual(
#     name="",
#     values=tmpshapes,
#     labels=shp.labels,
#     drop=F
#   ) + 
#   scale_color_discrete(
#     name=""
#   ) +
#   ylab("\nEffect on Punitiveness, Anxiety, Mistrust ") +
#   xlab("") +
#   coord_flip() +
#   facet_wrap(
#     ~ dimension,
#     ncol=1
#   ) +
#   theme_bw()
# 
# setwd(outputdir)
# ggsave(
#   plot=g.tmp,
#   filename='fig_publicopinion_coefplots.png',
#   width=6,
#   height=6
# )

#########################################################
#########################################################

#FIG X - RACE EFFECT
#the main thing we are interested in showing
#is the effect of race in each of these three dimensions

predictdf<-expand.grid(
  race2=c(1,2),
  gender=c(1,2),
  ed=c(1,2,3,4),
  age=c(1,2,3,4)
)

#ensure it works whether race or race2
predictdf$race<-predictdf$race2

#loop through and predictdf
tmpseq.i<-seq_along(prefmods)
plotdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  print(i)
  thismod<-mods[[i]]
  thismodname<-names(mods)[i]
  thisdimension<-str_extract(
    thismodname,
    "all|anxiety|punitive|mistrust"
  )
  
  #############
  
  #set up predictdf
  
  #get mrefs
  mrefs<-get.medianrefs(thismod)
  predictdf$year<-mrefs$year
  predictdf$question<-mrefs$question
  predictdf$division<-mrefs$division
  predictdf$row<-1:nrow(predictdf)
  
  #now, add all interactions
  loopdf<-expand.grid(
    v1=c(
      "race",
      "race2"
    ),
    v2=c(
      "question",
      "age",
      "ed",
      "gender",
      "year",
      "region",
      "division",
      "state_alpha2"
    ),
    stringsAsFactors=F
  )
  tmpseq.j<-1:nrow(loopdf)
  for(j in tmpseq.j) {
    #print(i)
    #j<-1
    thisrow<-loopdf[j,]
    newname<-paste0(
      thisrow$v1,
      "X",
      thisrow$v2
    )
    #make these all distinct categories
    predictdf[[newname]]<-paste0(
      predictdf[[thisrow$v1]],
      "_",
      predictdf[[thisrow$v2]]
    )
  }
  #add others
  predictdf$raceXdivisionXyear<-paste0(
    predictdf$race,"_",
    predictdf$division,"_",
    predictdf$year
  )
  predictdf$raceXedXyear<-paste0(
    predictdf$race,"_",
    predictdf$ed,"_",
    predictdf$year
  )
  
  #get predictions w/ merTools
  tmpoutput<-merTools::predictInterval(
    thismod,
    newdata=predictdf,
    which="full",
    level=0.95,
    type='probability',
    include.resid.var=F,
    returnSims=T
  )
  simMat<-attr(tmpoutput,"sim.results")
  simMat<-apply(simMat,1,thismod@resp$family$linkinv)
  simMat<-t(simMat) #* 100
  
  ###SUM BLACK-WHITE DIFFERENCE

  tmpdist<-simMat[predictdf$race==1,] - 
    simMat[predictdf$race==2,]
  returndf<-summarize.distribution2(tmpdist)
  returndf$dimension<-thisdimension
  row.names(returndf)<-NULL
  returndf
  
}) %>% rbind.fill


#DEPRECATED..

# #FIG 2 - AGE, ED AND SEX, BY RACE
# #b/c of interactions, main coefs are not sufficient
# 
# stop()
# 
# predictdf<-expand.grid(
#   race2=c(1,2),
#   gender=c(1,2),
#   ed=c(1,2,3,4),
#   age=c(1,2,3,4)
# )
# 
# #ensure it works whether race or race2
# predictdf$race<-predictdf$race2
# 
# #loop through and predictdf
# tmpseq.i<-seq_along(prefmods)
# plotdf<-lapply(tmpseq.i,function(i) {
#   #i<-3
#   print(i)
#   thismod<-mods[[i]]
#   thismodname<-names(mods)[i]
#   thisdimension<-str_extract(
#     thismodname,
#     "all|anxiety|punitive|mistrust"
#   )
#   
#   #############
#   
#   #set up predictdf
#   
#   #get mrefs
#   mrefs<-get.medianrefs(thismod)
#   predictdf$year<-mrefs$year
#   predictdf$question<-mrefs$question
#   predictdf$division<-mrefs$division
#   predictdf$row<-1:nrow(predictdf)
#   
#   #now, add all interactions
#   loopdf<-expand.grid(
#     v1=c(
#       "race",
#       "race2"
#     ),
#     v2=c(
#       "question",
#       "age",
#       "ed",
#       "gender",
#       "year",
#       "region",
#       "division",
#       "state_alpha2"
#     ),
#     stringsAsFactors=F
#   )
#   tmpseq.j<-1:nrow(loopdf)
#   for(j in tmpseq.j) {
#     #print(i)
#     #j<-1
#     thisrow<-loopdf[j,]
#     newname<-paste0(
#       thisrow$v1,
#       "X",
#       thisrow$v2
#     )
#     #make these all distinct categories
#     predictdf[[newname]]<-paste0(
#       predictdf[[thisrow$v1]],
#       "_",
#       predictdf[[thisrow$v2]]
#     )
#   }
#   #add others
#   predictdf$raceXdivisionXyear<-paste0(
#     predictdf$race,"_",
#     predictdf$division,"_",
#     predictdf$year
#   )
#   predictdf$raceXedXyear<-paste0(
#     predictdf$race,"_",
#     predictdf$ed,"_",
#     predictdf$year
#   )
#   
#   #get predictions w/ merTools
#   tmpoutput<-merTools::predictInterval(
#     thismod,
#     newdata=predictdf,
#     which="full",
#     level=0.95,
#     type='probability',
#     include.resid.var=F,
#     returnSims=T
#   )
#   simMat<-attr(tmpoutput,"sim.results")
#   simMat<-apply(simMat,1,thismod@resp$family$linkinv)
#   simMat<-t(simMat) #* 100
#   
#   ###KEY DIFFERENCES
#   
#   base_vals<-list(
#     gender=1,
#     ed=2,
#     age=2
#   )
#   comp_vals<-list(
#     gender=c(2,1),
#     ed=c(4,1),
#     age=c(4,1)
#   )
#   loopdf<-expand.grid(
#     var=c("gender","ed","age"),
#     race2=c(1,2),
#     stringsAsFactors=F
#   )
#   tmpseq.ii<-1:nrow(loopdf)
#   tmpdf<-lapply(tmpseq.ii,function(ii) {
#     #ii<-1
#     thisrow<-loopdf[ii,]
#     #which race
#     tmp<-predictdf$race2==thisrow$race2
#     #and which other rows
#     tmpvars<-loopdf$var[!loopdf$var%in%thisrow$var] %>%
#       unique
#     tmplogicals<-lapply(tmpvars,function(v) predictdf[[v]]==base_vals[[v]]) 
#     tmp<-tmp & Reduce(f="&",tmplogicals)
#     #and now get the contrasts vars
#     tmp_cond1<-tmp & predictdf[[thisrow$var]]==comp_vals[[thisrow$var]][1]
#     cond1<-simMat[tmp_cond1,]
#     tmp_cond2<-tmp & predictdf[[thisrow$var]]==comp_vals[[thisrow$var]][2]
#     cond2<-simMat[tmp_cond2,]
#     tmpdist<-cond1 - cond2
#     summarize.distribution2(tmpdist)
#   }) %>% rbind.fill
#   returndf<-cbind(loopdf,tmpdf)
#   returndf$dimension<-thisdimension
#   returndf
#   
# }) %>% rbind.fill
# 
# #don't need to plot 'all'
# tmp<-plotdf$dimension!="all"
# plotdf<-plotdf[tmp,]
# 
# #factors
# tmplevels<-c(
#   "gender",
#   "ed",
#   "age"
# )
# tmplabels<-c(
#   "Gender",
#   "Education",
#   "Age"
# )
# plotdf$var<-factor(
#   plotdf$var,
#   tmplevels,
#   tmplabels
# )
# 
# tmplevels<-c(
#   1,
#   2
# )
# tmplabels<-c(
#   "Whites",
#   "Blacks"
# )
# plotdf$race2<-factor(
#   plotdf$race2,
#   tmplevels,
#   tmplabels
# )
# 
tmplevels<-c(
  "anxiety",
  "mistrust",
  "punitive"
) %>% rev
tmplabels<-c(
  "Anxiety",
  "Mistrust",
  "Punitiveness"
) %>% rev
plotdf$dimension<-factor(
  plotdf$dimension,
  tmplevels,
  tmplabels
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
  labels=c(
    "at alpha=0.01",
    "at alpha=0.05",
    "at alpha=0.10",
    "not sig"
  )
)
#tmpshapes
tmpshapes<-c(8,4,16,1)
names(tmpshapes)<-levels(plotdf$pval.shp)
shp.labels<-c(
  bquote(alpha == 0.01),
  bquote(alpha == 0.05),
  bquote(alpha == 0.10)
)

g.tmp <- ggplot(
  plotdf,
  aes(
    x=dimension,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    shape=pval.shp
  )
) + 
  geom_point(
    size=2
  ) +
  geom_errorbar(
    size=0.4,
    width=0
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
  ylab("\nAdjusted White-Black Gap") +
  xlab("") +
  coord_flip() +
  # facet_grid(
  #   dimension ~ race2
  # ) + 
  theme_bw()
  # ) +
  # theme_bw(
  #   base_family="CM Roman",
  #   base_size=14
  # ) 

setwd(outputdir)
ggsave(
  plot=g.tmp,
  filename='fig_po_effectofrace.png',
  width=6,
  height=3
)
output(plotdf,'fig_po_effectofrace.png')

#########################################################
#########################################################

# #OUTPUT
# #output graphlist
# setwd(outputdir)
# this.sequence<-seq_along(gs.list)
# for(i in this.sequence) {
#   print(
#     paste0(
#       "saving ",i," of ",length(this.sequence)
#     )
#   )
#   thiselement<-gs.list[[i]]
#   ggsave(
#     filename="tmp.pdf",
#     plot=thiselement$graph,
#     width=thiselement$width,
#     height=thiselement$height
#   )
#   #embed font
#   embed_fonts(
#     file="tmp.pdf",
#     outfile=thiselement$filename
#   )
#   file.remove(
#     "tmp.pdf"
#   )
# }
