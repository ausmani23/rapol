#this is a function adapted from 02_regressions, 
#which takes a model and gets the short-run and long-run estimates
#of each variable in the model, along with CI, etc. 

require(lmtest)

getestimates<-function(m, #model
                       dvs, #dvs (i.e., lag terms)
                       ivs, #ivs
                       ivs.sds #sds of the ivs (for useful units)
                       ) { 
  
  #params
  # m<-m
  # dvs<-thisdv.actual
  # ivs<-ivs
  # ivs.sds<-sds
  
  #these are coefs
  coefs<-m$coefficients
  #this is robust vcov
  vcov.robust<-getrobust.plm(m)
  #this is output, given properly
  coefs.tested<-coeftest(m,vcov.robust)
  #loop through the modelvars
  this.sequence<-seq_along(ivs)

  #################################
  
  #SHORT-RUN ESTIMATES
  #first get short run of all coefs
  shortrunvars<-c(dvs,ivs)
  sr.sequence<-seq_along(shortrunvars)
  shortrundf<-lapply(sr.sequence,function(i) {
    #i<-1
    #get params
    thisiv<-shortrunvars[i]
    #track progres
    print(thisiv)
    
    #if this is the dv, don't multiply
    if(thisiv%in%dvs) {
      thisiv.sd<-1
    } else {
      thisiv.sd<-ivs.sds[[thisiv]]
    }
    
    thisrow<-str_detect(row.names(coefs.tested),thisiv)
    term<-row.names(coefs.tested)[thisrow]
    est<-coefs.tested[thisrow,"Estimate"]
    se<-coefs.tested[thisrow,"Std. Error"]
    t<-coefs.tested[thisrow,"t value"]
    pval<-coefs.tested[thisrow,"Pr(>|t|)"]
    #multiply by iv.sds
    est<-est*thisiv.sd
    se<-se*thisiv.sd
    #now compute
    est.min<-est-2*se
    est.max<-est+2*se
    
    #if it wasn't in this model.. 
    if(sum(thisrow)>0) {
      returnrow<-data.frame(dv=dvs,
                            iv=thisiv,
                            term,
                            mu=est,
                            mu.min=est.min,
                            mu.max=est.max,
                            se,
                            pval,
                            t,
                            stringsAsFactors=F)
    } else {
      returnrow<-data.frame(dv=dvs,
                            iv=thisiv,
                            mu=NA)
    }

    #multiply this returnrow
    #by the sds
    return(returnrow)
  }) %>% rbind.fill
  #identify
  shortrundf$est.type<-"shortrun"
  
  #################################
  
  #LONG RUN ESTIMATES
  longrundf<-lapply(this.sequence,function(i) {
    #i<-1
    #get params
    thisiv<-ivs[i]
    thisrow<-str_detect(names(coefs),thisiv)
    thisiv.terms<-names(coefs)[thisrow]
    thisiv.sd<-sds[[thisiv]]
    #track progress
    print(thisiv)
    
    #if it wasn't in this model
    if(sum(thisrow)==0) {
      
      #start of what is to be returned
      returnrow<-data.frame(dv=dvs,
                            iv=thisiv,
                            stringsAsFactors=F)
      
    } else {
      
      #start of what is to be returned
      returnrow<-data.frame(dv=dvs,
                            iv=thisiv,
                            term=thisiv.terms,
                            stringsAsFactors=F)
      
      #get lagdv term
      lagdvs<-names(coefs)[str_detect(names(coefs),dvs)]
      #is there an interaction?
      int.these<-str_detect(ivs,":")
      sq.these<-str_detect(ivs,"_sq")
      if(sum(int.these)==0 & sum(sq.these)==0) {
        #if no interaction or sq term

        ests.distribution<-getlongrun(m=m,
                                      lagdv=lagdvs,
                                      iv=thisiv.terms,
                                      reps=reps,
                                      ivfactor=thisiv.sd,
                                      summary=F)

        cum.output<-summarize.distribution(ests.distribution)
        cum.output$atval<-NA
      }
      returnrow<-cbind(returnrow,cum.output)
    }
    #return returnwo
    return(returnrow)
  }) %>% rbind.fill
  #identify
  longrundf$est.type<-"longrun"
  
  #################################
  
  #combine these in onedf
  estsdf<-rbind.fill(shortrundf,longrundf)
  estsdf<-estsdf[!is.na(estsdf$mu),] #remove anything missing.. 
  return(estsdf)
  
}