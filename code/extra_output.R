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

# CONVENTIONAL VIEW CITES/GRAPH

setwd(datadir); dir()
tmp<-readLines('conventional.bib',encoding='utf-8')
splitAt <- function(x, pos) unname(split(x, cumsum(seq_along(x) %in% pos)))
tmp<-splitAt(tmp,which(tmp==""))

#loop through and get key info
tmpdf <- lapply(seq_along(tmp),function(i) {
  print(i)
  #i<-19
  x<-tmp[[i]]
  date <- x[str_detect(x,'\\tdate')] %>%
    str_replace(".*?\\{","") %>% 
    str_replace("\\}\\,.*$","")
  author <- x[str_detect(x,'\\tauthor')] %>%
    str_replace(".*?\\{","") %>% 
    str_replace("\\}\\,.*$","") %>%
    str_replace("\\{","") %>%
    str_replace("\\}","")
  if(str_detect(author,"\\,")) {
    author <- str_extract_all(author,"[A-z]+\\,")[[1]] 
    author <- str_replace(author,"\\,","")
  }
  if(length(author)==2) {
    author <- paste0(author,collapse=" and ")
  } else if (length(author)>2) {
    author <- paste0(author[1]," et al.")
  }
  title <- x[str_detect(x,'\\tshorttitle')] %>%
    str_replace(".*?\\{","") %>% 
    str_replace("\\}\\,.*$","")
  if(length(title)==0) { #if there isn't a shorttitle
    title <- x[str_detect(x,'\\ttitle')] %>%
      str_replace(".*?\\{","") %>% 
      str_replace("\\}\\,.*$","")
  }
  data.frame(
    date,
    author,
    title,
    stringsAsFactors=F
  ) 
}) %>% rbind.fill
tmpdf$date <- str_extract(tmpdf$date,"[0-9]{4}") %>%
  str_replace("\\{","") %>% str_replace("\\}","")
tmpdf$author<-str_replace(tmpdf$author,"\\{","") %>%
  str_replace("\\}","")
tmpdf$title<-str_replace(tmpdf$title,"\\*","")
#detect problem titles
spaces <- str_count(tmpdf$title,"\\s")
tmp<-spaces>100
tmpdf$title[tmp]<-str_extract(
  tmpdf$title[tmp],
  ".*?\\s{3}"
) %>%
  str_trim

tmp<-tmpdf$date=='2004' & 
  tmpdf$author=='Bobo and Johnson'
tmpdf$title[tmp]<-'A Taste for Punishment'

tmpdf<-tmpdf[order(tmpdf$date),]
names(tmpdf)<-c('Year','Author','Title')

require(xtable)
tabdf_latex<- xtable(
  tmpdf,
  #align=c('l','l','|','l','l','l','l','l'),
  #caption='Information about Questions in the Public Opinion Sample',
  type='latex'
)
align(tabdf_latex) <- "lp{0.5in}p{2in}p{2.5in}" #here is the change
setwd(outputdir); dir()
print(
  tabdf_latex,
  include.rownames=F,
  floating=F,
  file='tab_conventionalview.tex',
  tabular.environment='longtable'
)




