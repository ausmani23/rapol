#########################################################
#########################################################

#unit root tests

#set params

#vars
tmp<-finaldf$pref &
  finaldf$dv%in%c(
    'imprt_t_jur',
    'officers_pcap'
  )
prefmods<-unique(finaldf$mname[tmp])
vars<-lapply(forms[prefmods],all.vars) %>% 
  unlist %>% unique

#omit some
badvars<-c(
  "year"
)
lagprefs<-c("L.","L2.","L3.","L4.")
dv_vars<-c(
  paste0(lagprefs,"imprt_t_jur"),
  paste0(lagprefs,"officers_pcap")
)
badvars<-c(
  badvars,
  dv_vars
)
tmp<-!vars%in%badvars
vars<-vars[tmp]

#misc parameters
maxlags<-3

#########################################################
#########################################################

#BALANCED TEST

#the data are mostly balanced, 
#so all we want is to loop through
#and pick the 50 states

#where there is an issue, b/c one state is missing
#or for some other reason, we adjust slightly
#this gives us three tests per variable

loopdf<-expand.grid(
  var=vars,
  stringsAsFactors=F
)
loopdf$i<-1:nrow(loopdf)

#put together a balanced dataset for each variable
#i will want at least N.countries, and so I pick the
#T for each variable that gives me this many N.countires
#in a balanced dataset

tmpseq.i<-1:nrow(loopdf)
baldf<-lapply(tmpseq.i,function(i) {
  
  
  #i<-8
  print(
    paste(
      i,"of",length(tmpseq.i)
    )
  )
  
  ####
  
  thisvar<-loopdf$var[i]
  
  ###
  
  #get countries desired, given N countries
  thisdf<-beodfraw
  tmpa2s<-tapply(
    thisdf[[thisvar]],
    thisdf$state_alpha2,
    function(x) sum(!is.na(x))
  ) %>% sort
  tmpa2s<-tmpa2s[tmpa2s!=0]
  tmptab<-table(tmpa2s)
  if(length(tmptab)!=1) {
    if(thisvar=="imprt_t_jur") {
      minN<-41
    } else if(thisvar=="L.violent_crt") {
      minN<-51
    } else {
      stop('inspect')
    }
  } else {
    minN<-tmpa2s[1]
  }
  tmpa2s<-tmpa2s[tmpa2s==minN]

  
  
  ###
  
  #create a tmpdf w/ this many obs,
  #using thesecows
  tmpvars<-c("state_alpha2","year",thisvar)
  tmprows<-thisdf$state_alpha2%in%names(tmpa2s)
  tmpdf<-thisdf[tmprows,tmpvars]
  testdf<-by(tmpdf,tmpdf$state_alpha2,function(df) {
    #df<-tmpdf[tmpdf$cowcode.num==2,]
    df<-df[!is.na(df[[thisvar]]),]
    data.frame(
      thisvar=tail(df[[thisvar]],minN),
      time=1:minN,
      state_alpha2=unique(df$state_alpha2),
      stringsAsFactors=F
    )
  }) %>% rbind.fill
  
  ###
  
  #make this wide rather than long
  is.na(testdf$thisvar) %>% sum
  is.nan(testdf$thisvar) %>% sum
  sum(!is.finite(testdf$thisvar))
  
  #here's my dataframe
  testdf<-spread(
    testdf,
    state_alpha2,
    thisvar
  )
  
  #but remove inel cols (this which don't change)
  badcols<-apply(
    testdf,2,
    function(x) length(unique(x))==1
  )
  testdf<-testdf[,!badcols]
  
  #and more badcols are those w/o sufficient variance
  #purtest() throws an error if these are included
  #and badcols, which are countries in which there isn't much variation
  if(thisvar=="L.demcontrol.klarner") {
    bada2s<-c("HI")
  } else {
    bada2s<-c("")
  }
  badcols<-names(testdf)%in%bada2s
  testdf<-testdf[,!badcols]
  #check variances
  tmpvariances<-apply(
    testdf,2,
    var
  ) 
  sort(tmpvariances)
  
  ###
  
  #run the tests,get restuls
  tests<-c(
    "levinlin",
    "ips",
    "madwu",
    "hadri"
  )
  returndf<-lapply(tests,function(mytest) {
    #mytest<-"levinlin"
    #print(mytest)
    testdf$time<-NULL
    tmptest<-purtest(
      testdf,
      test=mytest,
      exo='intercept',
      lags='AIC',
      pmax=maxlags
    )
    returndf<-data.frame(
      #test=tmptest$statistic$method,
      test=mytest,
      pval=tmptest$statistic$p.value
    )
    if(mytest=="hadri") {
      returndf$unitroot<-ifelse(
        returndf$pval>0.05,"No","Yes"
      )
    } else {
      returndf$unitroot<-ifelse(
        returndf$pval<0.05,"No","Yes"
      )
    }
    returndf
  }) %>% rbind.fill
  
  ###
  
  returndf$N.actual<-ncol(testdf) - 1
  returndf$i<-i
  returndf
  
}) %>% rbind.fill


#merge loopdf and returndf
intersect(
  names(loopdf),
  names(baldf)
)

baldf<-merge(
  loopdf,
  baldf,
  by="i",
  all=T
)

########################################################
########################################################

#any potential unitroot vars?
tmp<-baldf$test!="hadri"
tmptab<-tapply(
  baldf$unitroot[tmp],
  baldf$var[tmp],
  function(x) 
    sum(x=="Yes")
)
urvars<-names(tmptab[tmptab>=2])
