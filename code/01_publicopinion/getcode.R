#this is a generic conversion function
#given value, from/to, and a df

getcode<-function(x,from,to,df,appx=T,allowdups=F) {
  
  # x<-"0111";
  # from<-"sic87";
  # to<-"naics02";
  # df<-crosswalks$sic87_naics02
  # x<-"Labor Intensity"
  # from<-"propername"
  # to<-"order"
  # df<-varnamesdf
  
  ##############
  
  #get the row
  #if you pass me NA
  #return NA
  if(is.na(x)) {
    ###can't do anything
    y<-NA
  } else {
    ###else we can do something
    #convert a;; to lower, 
    #in case these are character matches
    match.from<-tolower(df[[from]])
    x<-tolower(x)
    #get matching rows
    tmp<-match.from==x
    if(sum(tmp)==1) {
      #if just one row, easy
      y<-df[[to]][tmp]
    } else if(sum(tmp)==0) {
      #if nothing matched,
      #this is probably NA
      y<-NA
      #if approx is on
      if(appx) {
        #but we can try converting 
        #the matching column to numeric
        #for appx matches of numbers
        tmpnum<-suppressWarnings(
          as.numeric(match.from)
        )
        tmp<-tmpnum==x & !is.na(tmpnum)
        if(sum(tmp)==1) 
          y<-df[[to]][tmp]
      }
    } else if(sum(tmp)>1) {
      #if more than 1 matches,
      #still return if they're all the same
      y<-df[[to]][tmp] %>%
        unique
      if(length(y)>1) {
        #if more than one matches
        #check if allow dups is on,
        #then we can do something
        #otherwise, don't match more than 1
        if(allowdups) {
          #return a vector
          #this can't be put in df w/o examination
          #because it won't be right length
          y<-df[[to]][tmp]
        } else {
          y<-NA #set back to NA
        }
      }
    }
  }
  
  ##############
  
  return(y)
}