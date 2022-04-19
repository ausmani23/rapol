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

#load classified punitive votes in the house
setwd(filesdir); dir()
mvotesdf<-fread(
  '02_voting_fulldf_classified.csv'
)

#how many
unique(mvotesdf$congress_rollnumber[!mvotesdf$handcoded]) %>% length
unique(mvotesdf$congress_rollnumber[mvotesdf$handcoded]) %>% length
#953 new; 45 handcoded

#fix groups for display clarity
tmp<-mvotesdf$group%in%c('Republicans','Democrats')
mvotesdf$group[tmp]<-paste0('Non-CBC ',mvotesdf$group[tmp])

#########################################################
#########################################################

#SUMMARIZE BY PRESIDENT

#merge info about presidents into this, for display
require(rvest)
require(lubridate)
tmpurl <- "https://history.house.gov/Institution/Presidents-Coinciding/Presidents-Coinciding/"
thishtml <- read_html(tmpurl)
tmpdf<-thishtml %>%
  html_nodes('.manual-table-not-sortable') %>%
  html_table()
tmpdf<-tmpdf[[1]]
names(tmpdf)<-tolower(names(tmpdf))
names(tmpdf)<-c('number','president','vp','years','congresses')
tmpdf$number<-na.locf(tmpdf$number)
tmpdf$startdate<-str_extract(
  tmpdf$years,
  '.*?\\–'
) %>% str_replace("\\–","")
tmpdf$enddate<-str_extract(
  tmpdf$years,
  '\\–.*$'
) %>% str_replace("\\–","")
tmpdf$startdate<-mdy(tmpdf$startdate)
tmp<-tmpdf$enddate=='present'
tmpdf$enddate[tmp]<-paste0(month(today()),'-',day(today()),'-',year(today()))
tmpdf$enddate<-mdy(tmpdf$enddate)

#now, generate full datset
tmpdf<-lapply( 1:nrow(tmpdf),function(i) {
  #i<-60
  #print(i)
  thisrow<-tmpdf[i,]
  data.frame(
    president=str_replace_all(
      thisrow$president,"[0-9]+","" #b/c footnotes, etc
    ) %>% str_trim(), 
    date=seq(
      thisrow$startdate,
      thisrow$enddate,
      by='days'
    )
  )
}) %>% rbind.fill %>% data.table

#keep only those that are in mvotesdf
mvotesdf$date<-ymd(mvotesdf$date)
mvotesdf<-merge(
  mvotesdf,
  tmpdf,
  by='date',
  all.x=T
)

#summarize by president
plotdf<-mvotesdf[
  !is.na(punitive) &
    !is.na(group) &
    group=='CBC'
  ,
  .(
    punitive_pct=100 * mean(punitive_vote,na.rm=T),
    punitive_votes = length(unique(congress_rollnumber)),
    start_date = min(date)
  )
  ,
  by=c(
    "group",
    "president"
  )
]
setorder(plotdf,group,president)

plotdf$lastname<-str_extract(plotdf$president,'[A-z]+$')
tmp<-plotdf$lastname=='Bush'
plotdf$lastname[tmp]<-c('Bush I','Bush II')
tmplevels<-plotdf$lastname[order(plotdf$start_date)]
plotdf$lastname<-factor(plotdf$lastname,tmplevels %>% rev)

plotdf$fill<-plotdf$lastname%in%c('Nixon','Reagan')
tmpfills<-c('red','darkgrey'); names(tmpfills)<-c(T,F)

g.tmp<- ggplot(
  plotdf,
  aes(
    x=lastname,
    y=punitive_pct,
    fill=fill
  )
) + 
  geom_bar(
    stat='identity',
    width=0.5,
    color='black'
  ) +
  scale_fill_manual(
    guide='none',
    values=tmpfills
  ) +
  theme_bw() +
  coord_flip() +
  ylab("") +
  xlab("% of CBC Voting Punitively\n")

setwd(outputdir)
tmpname<-"fig_voting_presidents.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=6,
  height=10
)
output(plotdf,tmpname)

#########################################################
#########################################################

#SUMMARIZE

#because before 1960, sample is sparse
#set everythign before 1960 to 1960
tmp<-mvotesdf$year<=1960
mvotesdf$year[tmp]<-1960

#fit the loess 
sumdf<-mvotesdf[
  !is.na(punitive) &
    !is.na(group)
  ,
  .(
    punitive_pct = 100 * mean(punitive_vote,na.rm=T),
    punitive_votes = length(unique(congress_rollnumber))
  )
  ,
  by=c(
    "group",
    "year"
  )
]
setorder(sumdf,group,year)

# #just the handcoded ones
# sumdf2<-mvotesdf[
#   !is.na(punitive) & 
#     !is.na(group) &
#     handcoded==T
#   ,
#   .(
#     punitive_pct = 100 * mean(punitive_vote,na.rm=T)
#   )
#   ,
#   by=c(
#     'group',
#     'year'
#   )
# ]

sumdf<-by(sumdf,sumdf$group,function(df) {
  #df<-sumdf[sumdf$group=="Democrats",]
  tmp<-loess(
    data=df,
    punitive_pct ~ year
  ) %>% predict(df$year,se=T)
  df$mu.loess<-tmp$fit
  df$se.loess<-tmp$se.fit
  df
}) %>% rbind.fill %>% data.table


#generate diffdf, 
#which is difference
#between cbc and dems
#and cbc and repubs
diffdf<-by(sumdf,sumdf$year,function(df) {
  #df<-sumdf[sumdf$year==1947,]
  cbc<-rnorm(
    1000,
    mean=df$mu.loess[df$group=='CBC'],
    sd=df$se.loess[df$group=='CBC']
  )
  dems<-rnorm(
    1000,
    mean=df$mu.loess[df$group=='Non-CBC Democrats'],
    sd=df$se.loess[df$group=='Non-CBC Democrats']
  )
  repubs<-rnorm(
    1000,
    mean=df$mu.loess[df$group=='Non-CBC Republicans'],
    sd=df$se.loess[df$group=='Non-CBC Republicans']
  )
  tmpdf<-rbind(
    data.frame(quantile(dems - cbc,c(0.025,0.5,0.975)) %>% t),
    data.frame(quantile(repubs - cbc,c(0.025,0.5,0.975)) %>% t)
  )
  names(tmpdf)<-c("mu.min","mu","mu.max")
  tmpdf$group<-c("dems","repubs")
  tmpdf$year<-unique(df$year)
  tmpdf
}) %>% rbind.fill

#########################################################
#########################################################

#PLTO THE AVERAGE

#load
setwd(codedir); source('genconventional.R')

plotdf<-sumdf
plotdf$facet<-"estimated"
plotdf$yhat<-plotdf$mu.loess

#add conventional view
loopdf<-data.frame(
  sumcat=c(
    'CBC',
    'Non-CBC Democrats',
    'Non-CBC Republicans'
  )
)

# c<-30
# loopdf$startpoint<-c(
#   50-c, 
#   50,
#   50+0.9*c
# )
# loopdf$endpoint<-c(
#   50-c,
#   50+c,
#   50+c
# )

tmpseq.i<-1:nrow(loopdf)
tmpdf<-lapply(tmpseq.i,function(i) {
  #i<-2
  print(i)
  endyr<-max(plotdf$year)
  styr<-min(plotdf$year)
  thisrow<-loopdf[i,]
  # m<-(thisrow$endpoint-thisrow$startpoint)/(endyr-styr)
  # b<-thisrow$startpoint - (styr * m)
  # fun.y<-function(x) {
  #   m * x + b
  # }
  # yhat<-sapply(
  #   styr:endyr,
  #   fun.y
  # )
  
  if(thisrow=='CBC') {
    yhat<-20
  } else if(thisrow=='Non-CBC Democrats') {
    yhat<-genconventional(40,70,80)[-(1),2]
  } else if(thisrow=='Non-CBC Republicans') {
    yhat<-genconventional(55,80,80)[-(1),2]
  }
  data.frame(
    group=thisrow,
    year=styr:endyr,
    mu.loess=yhat,
    stringsAsFactors=F
  )
}) %>% rbind.fill
tmpdf$facet<-"conventional"


plotdf<-rbind.fill(
  plotdf,
  tmpdf
)

tmplevels<-c(
  "conventional",
  "estimated"
)
tmplabels<-c(
  "Conventional View",
  "Estimated"
)
plotdf$facet<-factor(
  plotdf$facet,
  tmplevels,
  tmplabels
)

tmplevels<-c(
  "Non-CBC Democrats",
  "Non-CBC Republicans",
  "CBC"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels
)

tmpcolors<-c(
  "Blue",
  "Red",
  "Black"
)
names(tmpcolors)<-levels(plotdf$group)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=mu.loess,
    #ymin=mu.loess - 1.96*se.loess,
    #ymax=mu.loess + 1.96*se.loess,
    group=group,
    color=group
  )
) +
  geom_line(size=1) +
  # geom_ribbon(
  #   alpha=0.25,
  #   color='grey'
  # ) +
  # geom_point(
  #   data=sumdf2,
  #   aes(
  #     y=punitive_pct
  #   ),
  #   alpha=0.25
  # ) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  xlab("") +
  ylab("% Voting Punitive\n") +
  facet_wrap(
    ~ facet,
    ncol=1
  ) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    legend.direction = 'horizontal'
  )

setwd(outputdir)
tmpname<-"fig_voting_levels.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=5,
  height=8
)
output(plotdf,tmpname)


#########################################################
#########################################################

#PLOT THE DIFFERENCE

plotdf<-diffdf

tmplevels<-c(
  "dems",
  "repubs"
)
tmplabels<-c(
  "Non-CBC Democrats",
  "Non-CBC Republicans"
)
plotdf$group<-factor(
  plotdf$group,
  tmplevels,
  tmplabels
)

tmpcolors2<-c(
  "Blue",
  "Red"
)
names(tmpcolors2)<-levels(plotdf$group)

g.tmp<-ggplot(
  plotdf,
  aes(
    x=year,
    y=mu,
    ymin=mu.min,
    ymax=mu.max,
    color=group
  )
) +
  geom_line(
    size=1
  ) +
  geom_hline(
    yintercept=0,
    linetype='dashed'
  ) +
  geom_ribbon(
    alpha=0.25,
    color='grey'
  ) +
  scale_color_manual(
    name="",
    values=tmpcolors2,
    guide='none'
  ) +
  facet_wrap(
    ~ group,
    ncol=1
  ) +
  theme_bw() +
  xlab("") +
  ylab("Punitiveness Gap to CBC\n")

setwd(outputdir)
tmpname<-"fig_voting_differences.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=5,
  height=8
)
output(plotdf,tmpname)

#########################################################
#########################################################

