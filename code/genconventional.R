genconventional<-function(s,p,e) {
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
      1955,s,
      1965,s+diff/4,
      1968,s+diff/2,
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
    year=1955:2014
  )
  predictdf$mu<-predict(
    m.tmp,
    predictdf
  )
  predictdf
}
genconventional(25,60,60)
genconventional(25,60,52.5)
