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
#require(rstanarm)
require(merTools)

#helper functions
setwd(codedir); dir()
source('readpoll_functions.R')
source('getinfo2.R')
source('functions.R')

#load data and mods
setwd(filesdir); dir()
load(file="01R2_prepped.RData")
mods<-readRDS('modslistEXP.RDS')
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

#FIG 1 - COEF PLOT

#identify prefmods
setwd(metadir)
finaldf<-read.csv(
  'regmods_infoEXP.csv',
  stringsAsFactors=F
)
prefmod_names<-finaldf$mname[finaldf$pref]
prefmods<-mods[prefmod_names]

#there should be 4 of these
# if(length(prefmods)!=4)
#   stop('need to fix this')

#get coefs from prefmods
tmpseq.i<-seq_along(prefmods)
plotdf<-lapply(tmpseq.i,function(i) {
  #i<-1
  thismod<-prefmods[[i]]
  thismodname<-names(prefmods)[i]
  tmpsum<-summary(thismod)
  tmpcoefs<-tmpsum$coefficients
  returndf<-data.frame(
    mname=thismodname,
    var=row.names(tmpcoefs),
    mu=tmpcoefs[,1],
    mu.min=tmpcoefs[,1] - 1.96 * tmpcoefs[,2],
    mu.max=tmpcoefs[,1] + 1.96 * tmpcoefs[,2],
    pval=tmpcoefs[,4],
    stringsAsFactors=F
  )
  row.names(returndf)<-NULL
  returndf
}) %>% rbind.fill

#get dimension
plotdf$dimension<-str_extract(
  plotdf$mname,
  "all|anxiety|mistrust|punitive"
)

#get rid of intercept
tmp<-plotdf$var=="(Intercept)" |
  plotdf$dimension=="all"
plotdf<-plotdf[!tmp,]

#pvals
plotdf$pval.class<-get.pvals.class(plotdf$pval)
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

#divide by 4 rule
#(means, roughly shows marginal effect in prob)
plotdf$mu<-plotdf$mu/4
plotdf$mu.min<-plotdf$mu.min/4
plotdf$mu.max<-plotdf$mu.max/4

#factors
plotdf$var %>% unique
tmplevels<-c(
  "gender",
  "ed",
  "age",
  "factor(race)2",
  "factor(race)3"
) %>% rev
tmplabels<-c(
  "Female",
  "Education",
  "Age",
  "Black",
  "Other"
) %>% rev
plotdf$varname<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)

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

g.tmp<-ggplot(
  plotdf,
  aes(
    x=varname,
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
  ylab("\nMarginal Impact on P(A, M, or P Response) ") +
  xlab("") +
  coord_flip() +
  facet_wrap(
    ~ dimension,
    ncol=1
  ) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) 

tmpname<-"fig_coefplotsEXP.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=6,
  height=6
)

#########################################################
#########################################################

#FIG 2 - AGE, ED AND SEX, BY RACE


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
  #i<-3
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
  
  ###KEY DIFFERENCES
  
  base_vals<-list(
    gender=1,
    ed=2,
    age=2
  )
  comp_vals<-list(
    gender=c(2,1),
    ed=c(4,1),
    age=c(4,1)
  )
  loopdf<-expand.grid(
    var=c("gender","ed","age"),
    race2=c(1,2),
    stringsAsFactors=F
  )
  tmpseq.ii<-1:nrow(loopdf)
  tmpdf<-lapply(tmpseq.ii,function(ii) {
    #ii<-1
    thisrow<-loopdf[ii,]
    #which race
    tmp<-predictdf$race2==thisrow$race2
    #and which other rows
    tmpvars<-loopdf$var[!loopdf$var%in%thisrow$var] %>%
      unique
    tmplogicals<-lapply(tmpvars,function(v) predictdf[[v]]==base_vals[[v]]) 
    tmp<-tmp & Reduce(f="&",tmplogicals)
    #and now get the contrasts vars
    tmp_cond1<-tmp & predictdf[[thisrow$var]]==comp_vals[[thisrow$var]][1]
    cond1<-simMat[tmp_cond1,]
    tmp_cond2<-tmp & predictdf[[thisrow$var]]==comp_vals[[thisrow$var]][2]
    cond2<-simMat[tmp_cond2,]
    tmpdist<-cond1 - cond2
    summarize.distribution2(tmpdist)
  }) %>% rbind.fill
  returndf<-cbind(loopdf,tmpdf)
  returndf$dimension<-thisdimension
  returndf
  
}) %>% rbind.fill

#don't need to plot 'all'
tmp<-plotdf$dimension!="all"
plotdf<-plotdf[tmp,]

#factors
tmplevels<-c(
  "gender",
  "ed",
  "age"
)
tmplabels<-c(
  "Gender",
  "Education",
  "Age"
)
plotdf$var<-factor(
  plotdf$var,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  1,
  2
)
tmplabels<-c(
  "Whites",
  "Blacks"
)
plotdf$race2<-factor(
  plotdf$race2,
  tmplevels,
  tmplabels
)

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

g.tmp<-ggplot(
  plotdf,
  aes(
    x=var,
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
  ylab("\nMarginal Effect on P(A, M, or P Response)") +
  xlab("") +
  coord_flip() +
  facet_grid(
    dimension ~ race2
  ) +
  theme_bw(
    base_family="CM Roman",
    base_size=14
  ) 

tmpname<-"fig_marginalsEXP.pdf"
gs.list[[tmpname]]<-list(
  graph=g.tmp,
  filename=tmpname,
  width=8,
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




