olddir<-getwd()
##################################################
##################################################

#this is a wrapper to getcode()
#which has to be loaded, obvi
setwd(codedir); dir()
source('getcode.R')
getcode
getvarcode<-function(x,append=T,...) {
  prefsufs<-c(
    "L[0-9]?.",
    "D[0-9]?.",
  )
  prefsufs.propername<-c(
    "Lag",
    "Diff",
  )
  prefsufs.regex<-paste0("(",paste0(prefsufs,collapse="|"),")")
  tmp<-str_detect(x,prefsufs.regex)
  if(tmp) {
    #extract suffix
    x.new<-str_replace_all(x,prefsufs.regex,"")
    y<-getcode(x.new,...)
    #y<-getcode(x.new,"varname","propername",varnamesdf)
    if(append) {
      myprefsufs<-str_extract_all(x,prefsufs.regex)[[1]]
      tmp<-lapply(myprefsufs,function(z) str_detect(z,prefsufs))
      myappender<-prefsufs.propername[sapply(tmp,which)] %>%
        paste(collapse=", ")
      y<-paste0(y," (",myappender,")")
    } 
  } else {
    y<-getcode(x,...) #nothing to do if no suffix
  }
  y
}

#vars
setwd(metadir); dir()
varsdf<-read.csv(
  'varnames.csv',
  stringsAsFactors=F
)
#wrappers
getname<-function(x) {
  getvarcode(x,append=F,"varname","propername",varsdf)
}
getorder<-function(x) {
  getvarcode(x,append=F,"varname","order",varsdf)
}
gettype<-function(x) {
  getvarcode(x,append=F,"varname","type",varsdf)
}
getroot<-function(x) {
  getvarcode(x,append=F,"varname","varname",varsdf)
}

#questions
setwd(metadir); dir()
questionsdf<-read.csv(
  'qinfo.csv',
  stringsAsFactors=F
)
#wrappers
getqname<-function(x) {
  getcode(x,"actual","shortname",questionsdf)
}
getqorder<-function(x) {
  getcode(x,"actual","order",questionsdf)
}

##################################################
##################################################
setwd(olddir)