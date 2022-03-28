#this file defines functions that help read in the poll data

############################################################ 

#functions to assist

#returns true when row is on given card, given a number of cards in the poll
#card=which card checking, #row=rows from df, #nocards=totalnocards
rowbycard<-function(card,rows,nocards) { 
  if (card!=nocards) 
    return(card==rows%%nocards)
  else 
    return(rows%%nocards==0)
}

isodd<-function(x) x%%2!=0
iseven<-function(x) x%%2==0

howmany0<-function(x) sum(x==0,na.rm=T) #counts zero's in a vector. for checking against reported NR's

genfw<-function(x) { #takes columns you want, and generates fwf request  
  #format has to be startcol,endcol,startcol,endcol,startcol,endcol...  
  fwf<-c()  
  for (i in 1:length(x)) {
    if (i==1) 
      fwf<-append(fwf,-(x[1]-1)) 
    if (isodd(i) & i>1) {
      if ((x[i]-x[i-1])>1) 
        fwf<-append(fwf,-(x[i]-x[i-1])+1)
    }
    if (iseven(i)) 
      fwf<-append(fwf,x[i]-x[i-1]+1)
  }
  if (fwf[1]==0) fwf<-fwf[-1] #if we're getting first col, need to do this 
  return(fwf)
}

#option to make 0' or blanks or .'s NA, since this is what they mean in poll
removezero<-function(x) {
  x[x==0 | x=="" | x=="."]<-NA
  return(x)
}

#normalizes to 0-1, provided underlying var is in order
normalize<-function(x,exclude=F,y) {
  x<-as.numeric(x)
  if (exclude==T) #in the event that max holds DK's or something
    x[x%in%y]<-rep(NA,length(y)) #fills in NA for all the y's to exclude
  z<-(x-min(x,na.rm=T))/(max(x,na.rm=T)-min(x,na.rm=T))
  return(z)
}

#switches around the levels of a numeric var
remake<-function(x,levels1,levels2,excluded=0) {
  if (length(levels1)!=length(levels2)) 
    stop("This won't work with the levels you've given me")
  x2<-rep(NA,length(x))
  for (i in 1:length(levels1)) {
    #print(paste("Changing",levels1[i],"into",levels2[i]))
    x2[x==levels1[i]]<-levels2[i]
  }
  x2[is.na(x)]<-NA #everywhere x is NA, so should x2 be
  #everywere x is not NA but x2 is still NA (b/c levels weren't supplied),
  #i make x2 up to choice. the default is 0, but this is not sensible for 
  #qvars if neutral categories are to be excluded rather than set as reference
  x2[!is.na(x) & is.na(x2)]<-excluded #for depvars, excluded should be NA
  #return the new variable
  return(x2)
}

#test

#easy table function, withNAs
tableNA<-function(x) table(x,useNA="always")


############################################################ 

#the mother function
#this function takes inputs (target vars and their details),
#and some specs (filename and the year of the study),
#and outputs a dataframe named by year of study

#this is basically the same as readgallup, except i've gotten 
#rid of checking NR's and notes, since I didn't end up using them

readpoll<-function(givemeinputs,givemespecs) {
  
  #givemeinputs<-inputs
  #givemespecs<-specs
  
  thefilename<-givemespecs[1] #filename
  theyear<-as.numeric(givemespecs[2]) #year of study
  
  #order the input by cards, and starting col
  cards<-sapply(givemeinputs,function(x) as.numeric(x[1]))
  starts<-sapply(givemeinputs,function(x) as.numeric(x[3]))
  givemeinputs<-givemeinputs[order(cards,starts)]  
  
  #read through cards
  lastcard<-as.numeric(givemespecs[3]) #number of cards in the original file
  
  for (card in unique(cards)) { #loops through each card in order, and each time pulls out relevant df
    
    #card<-2
    
    cols<-c() #empty vector for column starting pos and ending pos
    thisdeckvars<-givemeinputs[sapply(givemeinputs,function(x) as.numeric(x[1]))==card] #the vars on this deck
    
    starts<-sapply(thisdeckvars,function(x) as.numeric(x[3])) #starting col pos's of vars on this deck
    ends<-sapply(thisdeckvars,function(x) as.numeric(x[4])) #ending
    cols<-append(cols,c(starts,ends)) #vector of starts and ending positions
    cols<-cols[order(cols)] #order this vector
    colwidths<-genfw(cols) #use previous function to generate the widths command
    colnames<-sapply(thisdeckvars,function(x) x[2]) #retrieve the associated names from input
    
    carddf<-read.fwf(thefilename,width=colwidths,col.names=colnames,as.is=T) #read the data
    carddf$row<-1:nrow(carddf) #needed to subset
    carddf_ss<-carddf[rowbycard(card,carddf$row,lastcard),] #just the relevant row
    carddf_ss$row<-NULL #don't need this now
    
    if (card==unique(cards)[1]) fulldf<-carddf_ss #if first time round, we use carddf_ss as the starting dataframe
    if (card!=unique(cards)[1]) fulldf<-cbind(fulldf,carddf_ss) #bind this df to the fulldf being created
    
  }
  
  print(paste0("This is df ",theyear))
  
  print("############")
  print("############")
  
  #summary of all the vars, particularly useful where NRs are not reported
  #to see whether the resulting vars make sense given their coding
  for (i in 1:ncol(fulldf)) {
    print("Summary of:")
    print(names(fulldf)[i])
    print(table(fulldf[,i]))    
  }
  
  #save the fulldf 
  row.names(fulldf)<-NULL #no need for rownames 
  
  #remove rows that are all NA (these occur when study has cards which I'm not reading)
  rownas<-apply(fulldf,1,function(x) sum(is.na(x))) #number of na's in each row
  fulldf<-fulldf[rownas!=ncol(fulldf),] #remove all the rows that have all na's
  
  #add filename
  fulldf$filename<-thefilename
  
  #creates a dataframe with the relevant year    
  assign(paste0("df",theyear),fulldf,envir=.GlobalEnv) 
  
}

############################################################ 

#this function takes LA Times and NBC and recodes

recodeNBCLAT<-function(df) {
  
  #df<-df1985
  
  edited<-c()
  vars<-names(df)
  
  ##depvars##
  #policehonesty.nbc, canrehab.lat, rehabilitate.lat, moreprisons.lat
  
  if ("policehonesty.nbc"%in%vars) {    
    #table(df$policehonesty.nbc,df$policehonesty.nbc%>%as.numeric)
    if (class(df$policehonesty.nbc)=="factor") 
      df$policehonesty.nbc<-(as.numeric(df$policehonesty.nbc)-1) #coding off by one
    df$policehonesty.nbc_p<-remake(df$policehonesty.nbc,c(4,5),c(1,1)) #low or very low honesty rating
    df$policehonesty.nbc_np<-remake(df$policehonesty.nbc,c(1,2),c(1,1)) #very high or high honesty rating
    df$policehonesty.nbc_n<-remake(df$policehonesty.nbc,c(3,6),c(1,1)) #average, or DK
    edited<-append("policehonesty.nbc",edited)
  }
  
  if ("canrehab.lat"%in%vars) {    
    df$canrehab.lat_p<-remake(df$canrehab.lat,c(3,4),c(1,1)) #a few or none can rehab
    df$canrehab.lat_np<-remake(df$canrehab.lat,c(1,2),c(1,1)) #most or some can rehab
    df$canrehab.lat_n<-remake(df$canrehab.lat,5,1) #not sure
    edited<-append("canrehab.lat",edited)
  }
  
  ####WORKING HERE####
  
  if ("rehabilitate.lat"%in%vars) {    
    df$rehabilitate.lat_p<-remake(df$rehabilitate.lat,2,1) #punish
    df$rehabilitate.lat_np<-remake(df$rehabilitate.lat,1,1) #rehab
    df$rehabilitate.lat_n<-remake(df$rehabilitate.lat,c(3,4,5),c(1,1,1)) #various neutral 
    edited<-append("rehabilitate.lat",edited)
  }
  
  if ("moreprisons.lat"%in%vars) {    
    df$moreprisons.lat_p<-remake(df$moreprisons.lat,2,1) #spend more on prisons
    df$moreprisons.lat_np<-remake(df$moreprisons.lat,1,1) #attack social
    df$moreprisons.lat_n<-remake(df$moreprisons.lat,3,1) #not sure
    edited<-append("moreprisons.lat",edited)
  }
  
  ##indvars##
  
  if ("race"%in%vars) {    
    if(class(df$race)=="factor") {
      df$black<-remake(as.character(df$race),
             c("White","Black","Hispanic","Oriental","Something else"),
             c(0,1,0,0,0),
             excluded=NA) #dk/refused =NA
      df$white<-remake(as.character(df$race),
                       c("White","Black","Hispanic","Oriental","Something else"),
                       c(1,0,0,0,0),
                       excluded=NA) #dk/refused =NA
    } else {
    df$black<-remake(df$race,
           c(1,2,3,4,5), #diff races for NBC/LAT, but only 6 is not sure/refused
           c(0,1,0,0,0), #black=1, not black=0
           excluded=NA) #dk/ref=NA
    df$white<-remake(df$race,
                     c(1,2,3,4,5), #diff races for NBC/LAT, but only 6 is not sure/refused
                     c(1,0,0,0,0), #black=1, not black=0
                     excluded=NA) #dk/ref=NA
    }
    edited<-append("race",edited)
  }
  
  if ("sex"%in%vars) {
    if (class(df$sex)!="factor") #no missingness to worry about, checked
      df$sex<-factor(df$sex,levels=c(1,2),labels=c("Male","Female"))
    edited<-append("sex",edited)
  }
  
  if ("ed"%in%vars) {
    df$college<-remake(as.numeric(df$ed),c(5,6,7),c(1,1,1))
    df$ed_f<-remake(as.numeric(df$ed),c(2,3,4,5,6,7),
                    c(1,1,2,3,4,4),
                    excluded=NA)
    edited<-append("ed",edited)
  }
  
  if ("ed9"%in%vars) {
    df$college<-remake(df$ed9,4:9,rep(1,6))
    df$ed_f<-remake(df$ed9,1:9,
                    c(1,1,2,3,3,4,4,4,4),
                    excluded=NA)
    edited<-append("ed9",edited)
  }
  
  if ("ed9b"%in%vars) {
    df$college<-remake(df$ed9b,5:9,rep(1,5))
    df$ed_f<-remake(df$ed9b,1:9,
                    c(1,1,1,2,3,3,4,4,4),
                    excluded=NA)
    edited<-append("ed9b",edited)
  }
  
  if ("region8"%in%vars) {
    #this is slightly coarser than divisions,
    #so it can only yield region info
    df$region<-remake(as.numeric(df$region8),1:8,
                      c(1,1,2,2,3,3,4,4))
    df$south<-as.numeric(df$region8) %>%
      remake(c(5,6),c(1,1))
    df$south<-factor(df$south,levels=c(0,1),labels=c("Non-South","South"))
    df$divison
    edited<-append("region8",edited)    
  }
  
  if ("region11"%in%vars) {
    #this is granular enough to yield division info
    df$division<-remake(df$region11,1:11,
                        c(1,1,5,5,6,7,7,3,4,8,9))
    df$south<-remake(df$region11,3:7,rep(1,5))
    df$south<-factor(df$south,levels=c(0,1),labels=c("Non-South","South"))    
    edited<-append("region11",edited)    
  }
  
  if ("agegroup5"%in%vars) {
    df$age<-as.numeric(df$agegroup5) %>%
      remake(c(2,3,4,5,6),c(21,29.5,42,57,70))
    edited<-append("agegroup5",edited)    
  }
  
  if ("agegroup7"%in%vars) {
    df$age<-remake(df$agegroup7,c(1,2,3,4,5,6,7),
                   c(21,34.5,47,52,57,62,70))
    edited<-append("agegroup7",edited)    
  }  
  
  if ("agegroup10"%in%vars) {
    df$agegroup10<-as.numeric(df$agegroup10) # '-' is DK's, so not needed
    df$age<-remake(df$agegroup10,c(1,2,3,4,5,6,7,8,9,0),
                   c(21,27,32,37,42,47,52,57,62,70))
    edited<-append("agegroup10",edited)    
  }
  
  print("These vars were not edited:")
  print(vars[!vars%in%edited])
  
  print("The new df has:")
  print(names(df))
  
  return(df)  
  
}


############################################################ 

#this function takes Time and recodes

recodeTime<-function(df,type=c("fwf","spss")) {
  
  #df<-df1995
  edited<-c()
  vars<-names(df)  
  
  ##depvars##
  #deathpen.time,worryvictim,adeqprotect  
  
  if ("deathpen.time"%in%vars) {
    if(type=="fwf") { #old ones
      df$deathpen.time_p<-remake(df$deathpen.time,1,1)
      df$deathpen.time_np<-remake(df$deathpen.time,2,1)
      df$deathpen.time_n<-remake(df$deathpen.time,3,1) #not sure
    } else { #new ones
      df$deathpen.time_p<-remake(as.character(df$deathpen.time),"Favor",1)
      df$deathpen.time_np<-remake(as.character(df$deathpen.time),"Oppose",1)
      df$deathpen.time_n<-remake(tolower(as.character(df$deathpen.time)),"not sure",1)
    }    
    edited<-append("deathpen.time",edited)
  }
  
  if ("worryvictim"%in%vars) {
    if(type=="fwf") { #old ones
      df$worryvictim_p<-remake(df$worryvictim,1,1) #yes I worry
      df$worryvictim_np<-remake(df$worryvictim,2,1) #no I don't
      df$worryvictim_n<-remake(df$worryvictim,3,1) #not sure
    } else { #new ones
      df$worryvictim_p<-remake(as.character(df$worryvictim),"Yes",1)
      df$worryvictim_np<-remake(as.character(df$worryvictim),"No",1)
      df$worryvictim_n<-remake(tolower(as.character(df$worryvictim)),"not sure",1)
    }    
    edited<-append("worryvictim",edited)
  }
  
  if ("adeqprotect"%in%vars) {
    if(type=="fwf") { #old ones
      df$adeqprotect_p<-remake(df$adeqprotect,2,1) #no I don't 
      df$adeqprotect_np<-remake(df$adeqprotect,1,1) #yes I feel protected
      df$adeqprotect_n<-remake(df$adeqprotect,3,1) #not sure
    } else { #new ones
      remake
      df$adeqprotect_p<-remake(as.character(df$adeqprotect),"No",1) #no I don't 
      df$adeqprotect_np<-remake(as.character(df$adeqprotect),"Yes",1) #yes I feel protected
      df$adeqprotect_n<-remake(tolower(as.character(df$adeqprotect)),"not sure",1) #not sure
    }    
    edited<-append("adeqprotect",edited)
  }
  
  ##indvars##
  
  if ("region"%in%vars) {
    if(type=="fwf") { #old ones
      df$south<-remake(df$region,c(5,6,7),c(1,1,1)) 
      df$south<-factor(df$south,levels=c(0,1),labels=c("Non-South","South"))
    } else { #new ones
      old<-df$region
      df$region<-as.numeric(df$region)
      df$south<-remake(df$region,c(5,6,7),c(1,1,1)) 
      df$south<-factor(df$south,levels=c(0,1),labels=c("Non-South","South"))
      print(table(df$south,old)) #for checking
    }    
    edited<-append("region",edited)
  }
  
  if ("race"%in%vars) {
    if(type=="fwf") {
      df$black<-remake(df$race,
                       c(1,2,3,4,5,6), #2=black, !2=not black,
                       c(0,1,0,0,0,NA), #6=no answer
                       excluded=NA)
      df$white<-remake(df$race,
                       c(1,2,3,4,5,6), 
                       c(1,0,0,0,0,NA), 
                       excluded=NA)
    } else {
      df$black<-remake(as.numeric(df$race),
                       c(1,2,3,4,5,6), #2=black, !2=not black
                       c(0,1,0,0,0,NA), #6=no answer
                       excluded=NA)
      df$white<-remake(as.numeric(df$race),
                       c(1,2,3,4,5,6), 
                       c(1,0,0,0,0,NA),
                       excluded=NA)
    }
    edited<-append("race",edited)
  }
  
  if ("sex"%in%vars) {
    if (type=="fwf")
      df$sex<-factor(df$sex,levels=c(1,2),labels=c("Male","Female"))
    edited<-append("sex",edited)
  }
  
  if ("agegroup"%in%vars) {
    if (type=="fwf") {
      df$age<-remake(df$agegroup,c(1,2,3,4,5,6,7),c(21,27,32,37,44.5,56,70))   
      df$age<-ifelse(df$age==0,NA,df$age)
    } else {
      df$age<-as.numeric(df$agegroup) %>%
        remake(c(1,2,3,4,5,6,7),c(21,27,32,37,44.5,56,70))   
      df$age<-ifelse(df$age==0,NA,df$age)
      print(table(df$age,df$agegroup)) #for checking
    }    
    edited<-append("agegroup",edited)
  }
  
  if ("ed"%in%vars) {
    if (type=="fwf") {
      df$ed_f<-remake(df$ed,c(1,2,3,4,5,6),
                      c(1,1,2,3,4,4),
                      excluded=NA)
      df$college<-remake(df$ed,c(4,5,6),c(1,1,1)) #college, everything below 4 is HS or less
    } else {
      df$ed_f<-as.numeric(df$ed) %>% 
        remake(c(1,2,3,4,5,6),
               c(1,1,2,3,4,4),
               excluded=NA)
      df$college<-as.numeric(df$ed) %>%
        remake(c(4,5,6),c(1,1,1))
      print(table(df$ed,df$college)) #for checking
      edited<-append("ed",edited)
    }
  }
  
  print("These vars were not edited:")
  print(vars[!vars%in%edited])
  
  print("The new df has:")
  print(names(df))
  
  return(df)
  
}


############################################################ 

#this function takes CBS and recodes

recodeCBS<-function(df,type=c("fwf","spss")) {
  
  #df<-df1994
  #type<-"fwf"
  
  edited<-c()
  vars<-names(df)  
  
  ##depvars##
  
  if ("worryvictim.cbs"%in%vars) {
    df$worryvictim.cbs %>% unique
    if(type=="fwf") { #old ones
      df$worryvictim.cbs_p<-remake(df$worryvictim.cbs,
                                   c(1,2),c(1,1))
      df$worryvictim.cbs_np<-remake(df$worryvictim.cbs,
                                    c(3,4),c(1,1))
      df$worryvictim.cbs_n<-remake(df$worryvictim.cbs,9,1) 
    } else { #new ones
      df$worryvictim.cbs_p<-remake(as.character(df$worryvictim.cbs),
                                    c("Some of the time","A lot of the time"),
                                    c(1,1))
      df$worryvictim.cbs_np<-remake(as.character(df$worryvictim.cbs),
                                    c("Never","Hardly ever"),
                                    c(1,1))
      df$worryvictim.cbs_n<-remake(as.character(df$worryvictim.cbs),
                                   c("DK/NA"),1)
    } 
    edited<-append("worryvictim.cbs",edited)
  }
  

  ##indvars##
  #race
  if ("race"%in%vars) {
    if (type=="fwf") {
      df$black<-remake(df$race,
                       c(1,2,3),
                       c(0,1,0),
                       excluded=NA)
      df$white<-remake(df$race,
                       c(1,2,3),
                       c(1,0,0),
                       excluded=NA)
      table(df$black,df$race) #not black=0, refused=NA
    } else {
      df$black<-remake(as.character(df$race),
                       c("White","Black","Asian","Other"),
                       c(0,1,0,0),
                       excluded=NA)
      df$white<-remake(as.character(df$race),
                       c("White","Black","Asian","Other"),
                       c(1,0,0,0),
                       excluded=NA) 
      table(df$black,df$race) #not black=0, refused=NA
    }    
    edited<-append("race",edited)
  }
  
  #sex
  if ("sex"%in%vars) {
    if (type=="fwf") {
      df$female<-remake(df$sex,
                        c(1,2),
                        c(0,1),
                        excluded=NA) #not female=1, DK/NA=NA
      table(df$female,df$sex)
    } else {
      df$female<-remake(as.character(df$sex),
                        c("Male","Female"),
                        c(0,1),
                        excluded=NA) #anyone not female
      table(df$female,df$sex)
    }    
    edited<-append("sex",edited)
  }
  
  if ("age"%in%vars) {
    #for both fwf and spss
    df$age[df$age==99]<-NA
    edited<-append("age",edited)
  }
  
  if ("ed"%in%vars) {
    if (type=="fwf") {
      #ed variable is already ready in the fwf version
    } else {
      df$ed_f<-remake(as.character(df$ed),
                      c("College grad","High School grad","Not a High School grad",
                        "Some college (trade or business)",
                        "Post grad work or degree (Masters, MBA, JD, MD, Ph"),
                      c(4,2,1,3,4),
                      excluded=NA)
      table(df$ed,df$ed_f)
    }    
    edited<-append("ed",edited)
  }
  
  return(df)
  
}


############################################################ 

#this function takes Roper and recodes

recodeRoper<-function(df) {
  
  edited<-c()
  vars<-names(df)  
  
  ##depvars##
  
  if ("crimespend.roper"%in%vars) {   
    df$crimespend.roper_p<-remake(df$crimespend.roper,2,1) #too little money spent
    df$crimespend.roper_np<-remake(df$crimespend.roper,1,1) #too much money spent
    df$crimespend.roper_n<-remake(df$crimespend.roper,c(3,4),c(1,1)) #about right, don't know
    edited<-append("crimespend.roper",edited)
  }
  
  if ("confidence.roper"%in%vars) {    
    df$confidence.roper_p<-remake(df$confidence.roper,3,1) #not at all confident
    df$confidence.roper_np<-remake(df$confidence.roper,1,1) #very confident
    df$confidence.roper_n<-remake(df$confidence.roper,c(2,4),c(1,1)) #only fairly, don't know
    edited<-append("confidence.roper",edited)
  }
  
  if ("deathpen.roper"%in%vars) {
    df$deathpen.roper_p<-remake(df$deathpen.roper,1,1) #favour deathpen
    df$deathpen.roper_np<-remake(df$deathpen.roper,2,1) #don't favour deathpen
    df$deathpen.roper_n<-remake(df$deathpen.roper,3,1) #mixed feelings
    edited<-append("deathpen.roper",edited)
  }
  
  if ("harsher.roper"%in%vars) {
    df$harsher.roper_p<-remake(df$harsher.roper,1,1) #favour harsher sentences
    df$harsher.roper_np<-remake(df$harsher.roper,2,1) #oppose harsher
    df$harsher.roper_n<-remake(df$harsher.roper,3,1) #mixed feelings
    edited<-append("harsher.roper",edited)
  }
  
  ##indvars##
  
  if ("ed"%in%vars) {
    df$college<-remake(df$ed,4,1) #college, everything below 4 is HS or less
    #doesn't have enough info for perfect categories
    #I assume that anyone who went to HS is a HS grad, 
    #even though this is obviously inaccurate
    df$ed_f<-remake(df$ed,c(1,2, #< HS grad
                            3, #some HS & HS grad
                            4), #some college
                    c(1,1,2,3), #No separate HS grad designation, here.
                    excluded=NA)
    edited<-append("ed",edited)
  }
  
  if ("ed7"%in%vars) {
    df$college<-remake(df$ed7,c(5,6,7),c(1,1,1)) #5 and above
    df$ed_f<-remake(df$ed7,c(1,2,3, #<HS grad
                             4, #HS grad
                             5, #>HS grad & <College grad (some college)
                             6,7),#College Grad
                    c(1,1,1,
                      2,
                      3,
                      4,4),
                    excluded=NA)
    edited<-append("ed7",edited)
  }
  
  if ("race"%in%vars) {
    df$black<-remake(df$race,
                     c(1,2,3),
                     c(0,1,0),
                     excluded=NA)
    df$white<-remake(df$race,
                     c(1,2,3),
                     c(1,0,0),
                     excluded=NA)
    edited<-append("race",edited)
  }
  
  if ("sex"%in%vars) {
    if(class(df$sex)!="factor")
      df$sex<-factor(df$sex,levels=c(1,2),labels=c("Male","Female"))
    edited<-append("sex",edited)
  }
  
  if ("region"%in%vars) {
    df$south<-remake(df$region,c(5,6,7),c(1,1,1)) 
    df$south<-factor(df$south,levels=c(0,1),labels=c("Non-South","South"))
    edited<-append("region",edited)
  }
  
  if ("weight"%in%vars) {
    df$weights<-df$weight 
  } else {
    df$weights<-1 
  }
  
  #all we can do with agegroup and agegroup5 is assume that 
  #age is equal to the midpoint or something like it, which might be roughly appropriate
  #if I use this to calculate cohorts
  
  if ("agegroup5"%in%vars) {
    df$age<-remake(df$agegroup5,c(1,2,3,4,5),c(19.5,25,37,52,65))   
    df$age<-ifelse(df$age==0,NA,df$age)
    edited<-append("agegroup5",edited)
  }
  
  if ("agegroup"%in%vars) {
    df$agegroup<-ifelse(as.character(df$agegroup=="-"),11,df$agegroup) #I'm pretty sure these are 11's
    df$age<-remake(df$agegroup,c(1,2,3,4,5,6,7,8,9,10,11),c(19.5,23,27,32,37,42,47,52,57,62,70))   
    df$age<-ifelse(df$age==0,NA,df$age) #blanks are made 0, no way around this. may miss some senior citizens.
    edited<-append("agegroup",edited)
  }
  
  print("These vars were not edited:")
  print(vars[!vars%in%edited])
  
  #print("The new df has:")
  #print(names(df))
  
  return(df)
  
}

############################################################ 

#this function takes ABC and recodes

recodeABC<-function(dfname="",type=c("old","new")) {
  
  year<-as.numeric(str_extract(dfname,"[0-9]{4}"))
  df<-get(dfname)
  edited<-c()
  vars<-names(df)  
  
  #add year and dfname to the df
  df$year<-year 
  df$dfname<-dfname
  
  ##depvars##
  
  if ("deathpen.abc"%in%vars) {
    if(type=="old") { #old ones
      df$deathpen.abc_p<-remake(df$deathpen.abc,1,1)
      df$deathpen.abc_np<-remake(df$deathpen.abc,2,1)
      #neutrals are anything greater than 2 that is not NA
      df$deathpen.abc[df$deathpen.abc>2 & !is.na(df$deathpen.abc)]<-3
      df$deathpen.abc_n<-remake(df$deathpen.abc,3,1)
    } else { #new ones
      df$deathpen.abc_p<-remake(as.character(df$deathpen.abc),"Favor",1)
      df$deathpen.abc_np<-remake(as.character(df$deathpen.abc),"Oppose",1)
      df$deathpen.abc_n<-remake(as.character(df$deathpen.abc) %>% tolower,
                                "dk/no opinion",1)
    }    
    edited<-append("deathpen.abc",edited)
  }
  
  if ("preferdp.abc"%in%vars) {
    df$preferdp.abc_p<-remake(as.character(df$preferdp.abc),"Death penalty",1)
    df$preferdp.abc_np<-remake(as.character(df$preferdp.abc),"Life in prison",1)
    df$preferdp.abc_n<-remake(as.character(df$preferdp.abc_n) %>% tolower,
                              "dk/no opinion",1)
    edited<-append("preferdp.abc",edited)
  }
  
  ##indvars##
  
  if ("weights"%in%vars) {
    if (type=="old") 
      df$weights<-as.numeric(as.character(df$weights))/100
    edited<-append("weights",edited)
  }
  
  if ("race"%in%vars) {    
    if (type=="old") {
      #1 white, 2 black, 3 white hispanic, 4 black hispanic, 5 hispanic, 6 other
      df$race<-remake(df$race,
                      c(1,2,3,4),
                      c(1,2,1,2))      
    } else {
      df$race<-remake(df$race,
                      c("White","Black","White Hispanic","Black Hispanic"),
                      c(1,2,1,2))
    }
    df$race<-factor(df$race,levels=c(1,2,0),labels=c("White","Black","Other"))
    df$black<-ifelse(df$race=="Black",1,0) #add black dummy
    df$white<-ifelse(df$race=="White",1,0) #white dummy
    edited<-append("race",edited)
  }
  
  if ("ed"%in%vars) {
    
    #create the ed_f variable we want
    #less than HS, HS Grad, Some College, College Grad or more
    #these are actually teh same, irrespective
    if (type=="old") {
      df$ed_f<-remake(df$ed,1:6,c(1,1, #some hs or less
                                  2, #hs grad
                                  3, #some college
                                  4,4),excluded=NA) #college grad or postgrad
    } else {
      df$ed_f<-remake(df$ed,c("8th grade or less","Some high school",
                              "Graduated high school",
                              "Some college",
                              "Graduated College","Post-graduate"),
                      c(1,1, #some hs or less
                        2, #hs grad
                        3, #some college
                        4,4),excluded=NA) #college grad or postgrad
    }
    
    if (type=="old") {
      #some college is 4,5,6, less is 1,2,3
      df$college<-remake(df$ed,c(4,5,6),c(1,1,1))
    } else {
      df$college<-remake(df$ed,c("Some College","Graduated College","Post-Graduate"),c(1,1,1))
    }
    edited<-append("ed",edited)      
  }
  
  if ("yearbirth"%in%vars) {
    
    
    df$age<-year-df$yearbirth
    edited<-append("yearbirth",edited)
  }
  
  if ("sex"%in%vars) {
    if (type=="old") {
      df$sex<-factor(df$sex,levels=c(1,2),labels=c("Male","Female"))
    }
    edited<-append("sex",edited)       
  }
  
  if ("region"%in%vars) {
    if (type=="new") df$region<-as.numeric(df$region) #two of them are already coded
    df$south<-remake(df$region,c(5,6,7),c(1,1,1)) 
    df$south<-factor(df$south,levels=c(0,1),labels=c("Non-South","South"))
    edited<-append("region",edited)
  }
  
  print("These vars were not edited:")
  print(vars[!vars%in%edited])
  
  print("The new df has:")
  print(names(df))
  
  return(df)
  
}

###########################################################

#this is a function which takes a dataframe and a set of 
#vars that you want to extract from that dataframe (in order)
#and returns the subsetted dataframe

#I use this to make all the constituent df's identical

subsetme<-function(df,vars) {
  rightorder<-c()
  for (i in 1:length(vars)) {
    rightorder<-append(rightorder,which(names(df)==vars[i]))
  }
  return(df[,rightorder])
}

#########################################################

#this is a function which tells R to wait for me to press 'enter'

readkey <- function() {
  cat ("Press [enter] to continue; Type 's' to stop")  
  line<-readline()
  if (line=="s") stop("I'm fed up")  
}


#########################################################

#this is a function which allows me to enter a string to be evaluated
#as an expression. eases legibility of code

evalme <- function(...) {  
  eval(parse(text=paste0(...))) 
}

#########################################################

#this function 'initializes' a new vector y based on a vector x.
#it makes everything a given value, except where there are NA's

initialize2<-function(x,init) {
  y<-c()
  y[!is.na(x)]<-init #assigns the init level
  return(y)
}

#this function uses initalize to assign a given value for a new vector, 
#given an old vector and certain values that should correspond
#values that don't correspond take an other

ifelse2<-function(x,vals,newlevel=1,otherval=0) {
  y<-initialize2(x,otherval) #initalize y with 'otherval'
  for (v in vals) #loop through each of the vals
    y[x==v]<-newlevel #each time x is equal to the val.. 
  return(y)
}

#########################################################

#this function counts NA, saves somet yping

countNA<-function(x)
  sum(is.na(x))

#########################################################

#defactorize: convert to character, and if everything left is 
#numeric, convert to numeric

defactor<-function(x) { 
  x<-as.character(x)
  #if converting to numeric won't produce additional NA's:
  if (sum(!is.na(x))==sum(!is.na(as.numeric(x)))) x<-as.numeric(x)  
  return(x)
}

#########################################################

#this function reads spss files in and saves some typing
#by automatically producing a dataframe, with lowercase varnames

require(foreign)

readspss<-function(x,...) {  
  #x<-'y97006.por'
  df<-read.spss(x,...)
  df<-as.data.frame(df,stringsAsFactors=F)
  names(df)<-tolower(names(df))  
  df$filename<-x
  return(df)  
}

#and this helps with rename
rename<-function(df,oldnames,newnames) {
  for (i in 1:length(oldnames)) 
    names(df)[names(df)==oldnames[i]]<-newnames[i]
  return(names(df))
}


#########################################################

#this function investigates codings across years, 
#to make sure everything is kosher

tablevar<-function(varname) {
  
  dfs<-ls()[str_detect(ls(),"^df.*")]
  dfs<-dfs[sapply(dfs,function(x) class(get(x)))=="data.frame"] #only data.frames
  
  firsttime<-0 #first time we find the var
  
  for (i in 1:length(dfs)) {
    
    #get the object    
    tempdf<-get(dfs[i])    
    
    if (varname%in%names(tempdf)) {
      
      print("###########")
      print("###########")
      
      
      #first time in this becomes 1, otherwise more
      firsttime<-firsttime+1
      
      #print name of the df
      print(paste0(dfs[i], " has ",varname))
      
      #summarize the variable
      print("This is its summary")
      summary<-sort(100*round(table(tempdf[varname])/
                                sum(table(tempdf[varname])),3),decreasing=T)
      print(summary)
      
      #check max against last max, min against last min
      #this is not infallible, but in addition to eyeballing, 
      #it should help
      
      thismax<-names(which.max(summary))
      thismin<-names(which.min(summary))
      
      if (firsttime>1) { #only run this test if we're on second df
        if (thismax!=oldmax)
          print("ALERT, MAX SUSPECT")
        warning("Max is not the same as last time")
        if (thismin!=oldmin)
          print("ALERT, MIN SUSPECT")
        warning("Min is not the same as last time")          
      }     
      
      oldmax<-thismax
      oldmin<-thismin
      
      print("###########")
      print("###########")
      
    } else {
      
      #print name of the df
      print(paste0(dfs[i], " does not have ",varname)) 
      
    }
  }  
}

#########################################################