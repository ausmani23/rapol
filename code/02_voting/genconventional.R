genconventional<-function(s,p,e,syear=1955) {
  #s gives starting level
  #p gives peak height
  #e gives end level
  # s<-25
  # p<-60
  # e<-60
  #
  diff<-p-s
  
  #set up dataframe
  tmpmat<-matrix(
    c(
      syear,s,
      1965,s+diff,
      1968,s+diff,
      1975,s+diff,
      1980,NA,
      1985,NA,
      1990,NA,
      1995,NA,
      2000,NA,
      2005,NA,
      2010,NA,
      2014,e
    ),
    ncol=2,
    byrow=T
  )
  tmpmat[,2]<-
    zoo::na.approx(tmpmat[,2],na.rm=F)
  tmpdf<-data.frame(
    tmpmat
  )
  names(tmpdf)<-c("year","y")
  m.tmp<-loess(data=tmpdf,y ~ year)
  predictdf<-data.frame(
    year=syear:1975
  )
  predictdf$y<-predict(
    m.tmp,
    predictdf
  )
  tmp<-predictdf$y>p
  predictdf$y[tmp]<-p  
  predictdf<-rbind.fill(
    predictdf,
    data.frame(year=1976:2014,y=NA)
  )
  predictdf$y[predictdf$year==2014]<-e
  predictdf$y<-zoo::na.approx(predictdf$y,na.rm=F)
  predictdf
}
genconventional(25,60,60)
genconventional(25,60,52.5)
