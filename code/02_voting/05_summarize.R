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
#952 new; 46 handcoded

#fix groups for display clarity
tmp<-mvotesdf$group%in%c('Republicans','Democrats')
mvotesdf$group[tmp]<-paste0('Non-Black ',mvotesdf$group[tmp])

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
    group=='Black'
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
  xlab("% of Black Members Voting Punitively\n")

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
#and this is irrelevant for 'standard story' evaluation
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
    mean=df$mu.loess[df$group=='Black'],
    sd=df$se.loess[df$group=='Black']
  )
  dems<-rnorm(
    1000,
    mean=df$mu.loess[df$group=='Non-Black Democrats'],
    sd=df$se.loess[df$group=='Non-Black Democrats']
  )
  repubs<-rnorm(
    1000,
    mean=df$mu.loess[df$group=='Non-Black Republicans'],
    sd=df$se.loess[df$group=='Non-Black Republicans']
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

# #add conventional view
# loopdf<-data.frame(
#   sumcat=c(
#     'Black',
#     'Non-Black Democrats',
#     'Non-Black Republicans'
#   )
# )
# 
# # c<-30
# # loopdf$startpoint<-c(
# #   50-c, 
# #   50,
# #   50+0.9*c
# # )
# # loopdf$endpoint<-c(
# #   50-c,
# #   50+c,
# #   50+c
# # )

# tmpseq.i<-1:nrow(loopdf)
# tmpdf<-lapply(tmpseq.i,function(i) {
#   #i<-2
#   print(i)
#   endyr<-max(plotdf$year)
#   styr<-min(plotdf$year)
#   thisrow<-loopdf[i,]
#   # m<-(thisrow$endpoint-thisrow$startpoint)/(endyr-styr)
#   # b<-thisrow$startpoint - (styr * m)
#   # fun.y<-function(x) {
#   #   m * x + b
#   # }
#   # yhat<-sapply(
#   #   styr:endyr,
#   #   fun.y
#   # )
#   
#   if(thisrow=='Black') {
#     yhat<-20
#   } else if(thisrow=='Non-Black Democrats') {
#     yhat<-genconventional(40,70,80)[-(1),2]
#   } else if(thisrow=='Non-Black Republicans') {
#     yhat<-genconventional(55,80,80)[-(1),2]
#   }
#   data.frame(
#     group=thisrow,
#     year=styr:endyr,
#     mu.loess=yhat,
#     stringsAsFactors=F
#   )
# }) %>% rbind.fill
# tmpdf$facet<-"conventional"


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
  "Non-Black Democrats",
  "Non-Black Republicans",
  "Black"
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
  # facet_wrap(
  #   ~ facet,
  #   ncol=1
  # ) +
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
  height=5
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
  "Non-Black Democrats",
  "Non-Black Republicans"
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
  ylab("Punitiveness Gap to Black Members\n")

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

#SPLIT SOUTH FROM NON-SOUTH
#focusing just on non-black


#split the sample by 1964 eleciton returns
#goldwater states, all other states, very liberal states
require(rvest)
tmpurl<-'https://en.wikipedia.org/wiki/1964_United_States_presidential_election'
myhtml <- read_html(tmpurl) %>% html_nodes('table')
tmpdf <- myhtml[[15]] %>% html_table
tmpdf <- tmpdf[-(1),c(1,15)] #this is the margin in favor of LBJ
names(tmpdf)<-c('statename','lbjmargin')
tmpdf$lbjmargin<-str_replace_all(tmpdf$lbjmargin,"\\,","")
tmpnegative<-str_detect(tmpdf$lbjmargin,"\\−")
tmpdf$lbjmargin<-str_replace(tmpdf$lbjmargin,"\\−","")
tmpdf$lbjmargin<-as.numeric(tmpdf$lbjmargin)
tmpdf$lbjmargin[tmpnegative]<- -1 * tmpdf$lbjmargin[tmpnegative]
sdf<-data.frame(
  statename=state.name,
  state_abbrev=state.abb
)
tmp<-tmpdf$statename%in%sdf$statename
tmpdf$statename[!tmp] #just DC missing
tmpdf<-merge(
  tmpdf,
  sdf
)
tmpdf$state_1964 <- NA
#5 goldwater states, 5 most liberal states, and the remainder
tmpdf<-tmpdf[order(tmpdf$lbjmargin),]
nrow(tmpdf)
tmpdf$state_1964[1:5]<-'Goldwater'
tmpdf$state_1964[6:45]<-'LBJ (Rest)'
tmpdf$state_1964[46:50]<-'LBJ (Top 5)'
tmpdf$state_1964 <- factor(tmpdf$state_1964,c('Goldwater','LBJ (Rest)','LBJ (Top 5)'))
mvotesdf <- merge(
  mvotesdf,
  tmpdf[,c('state_abbrev','state_1964')],
  all.x=T,
  by=c('state_abbrev')
)

sumdf<-mvotesdf[
  !is.na(punitive) &
    !is.na(group) & 
    group!='Black'
  ,
  .(
    punitive_pct = 100 * mean(punitive_vote,na.rm=T),
    punitive_votes = length(unique(congress_rollnumber))
  )
  ,
  by=c(
    #"group",
    'state_1964',
    "year"
  )
]
setorder(sumdf,year)#group,year)

sumdf<-by(sumdf,sumdf$state_1964,function(df) { #list(sumdf$group,sumdf$state_goldwater),function(df) {
  #df<-sumdf[sumdf$group=="Democrats",]
  tmp<-loess(
    data=df,
    punitive_pct ~ year
  ) %>% predict(df$year,se=T)
  df$mu.loess<-tmp$fit
  df$se.loess<-tmp$se.fit
  df
}) %>% rbind.fill %>% data.table

plotdf<-sumdf
plotdf$facet<-"estimated"
plotdf$yhat<-plotdf$mu.loess

plotdf <- plotdf[plotdf$state_1964%in%c('Goldwater','LBJ (Top 5)')]

g.tmp <- ggplot(
  plotdf,
  aes(
    x=year,
    y=yhat,
    #ymin=mu.loess - 1.96*se.loess,
    #ymax=mu.loess + 1.96*se.loess,
    group=state_1964,
    linetype=state_1964#,
    #color=group
  )
) +
  geom_line(size=1) +
  scale_color_manual(
    name="",
    values=tmpcolors
  ) +
  scale_linetype_discrete(
    name=""
  ) +
  #facet_wrap( ~ group) +
  xlab("") +
  ylab("% Voting Punitive\n") +
  # facet_wrap(
  #   ~ facet,
  #   ncol=1
  # ) +
  theme_bw() +
  theme(
    legend.position = 'bottom',
    legend.direction = 'horizontal',
    panel.spacing.x = unit(2,'lines')
  )

setwd(outputdir)
tmpname<-"fig_voting_levels_south.png"
ggsave(
  plot=g.tmp,
  filename=tmpname,
  width=4,
  height=4
)
output(plotdf,tmpname)

#for calculations
tmpdf<-spread(
  plotdf[,c('year','state_1964','mu.loess')],
  state_1964,
  mu.loess
)
tmpdf$diff <- tmpdf$Goldwater - tmpdf$`LBJ (Top 5)`
tmpdf$diff
goldwater_increase <- (tmpdf$Goldwater[tmpdf$year==2018] - tmpdf$Goldwater[tmpdf$year==1960])/
  tmpdf$Goldwater[tmpdf$year==1960] #105% increase
lbj_increase <- (tmpdf$`LBJ (Top 5)`[tmpdf$year==2018] - tmpdf$`LBJ (Top 5)`[tmpdf$year==1960])/
  tmpdf$`LBJ (Top 5)`[tmpdf$year==1960] #84% increase
lbj_increase/goldwater_increase #80% of the goldwater increase happens in the lbj case, too

