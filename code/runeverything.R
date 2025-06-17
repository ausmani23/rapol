require(stringr)
require(rprojroot)

## locate dirs 
rootdir <- find_root(criterion = has_file('_rapol.RProj'))
codedir <- file.path(rootdir, "code")
setwd(codedir); dir()
source('dirs.R')                        

## open log file to record all console output
tmpname <- paste0(
  'runeverything_',
  format(Sys.Date(), "%y%m%d"),
  '.log'
)
log_con <- file(
  file.path(
    outputdir, 
    tmpname,
    ), 
  open = "wt"
)
sink(log_con, type = "output", split = TRUE)   

#helper functions
msg_tee <- function(m) {
  cat(m$message, "\n", file = log_con, append = TRUE)
}
warn_tee <- function(w) {
  cat(
    "WARNING:",
    conditionMessage(w), 
    "\n", 
    file = log_con, 
    append = TRUE
    
  )
}

## run public-opinion scripts (Section 3)
setwd(pcodedir); dir()
myfiles <- dir()[str_detect(dir(), '^[0-9]{2}')]
for (myfile in myfiles) {
  print("######")
  print("Running:")
  print(myfile)
  
  pcodedir <- file.path(
    find_root(criterion = has_file('_rapol.RProj')),
    "code", "01_publicopinion"
  )
  setwd(pcodedir)
  withCallingHandlers(
    #run in its *own* env
    source(
      myfile,
      local = new.env(),
      echo  = TRUE,
      max.deparse.length = Inf
    ),
    message = msg_tee,
    warning = msg_tee
  )
}

## run voting patterns scripts (Section 4)
setwd(vcodedir); dir()
myfiles <- dir()[str_detect(dir(), '^[0-9]{2}')]
for (myfile in myfiles) {
  print("######")
  print("Running:")
  print(myfile)

  pcodedir <- file.path(
    find_root(criterion = has_file('_rapol.RProj')),
    "code", "02_voting"
  )
  setwd(pcodedir)
  withCallingHandlers(
    #run in its *own* env
    source(
      myfile,
      local = new.env(),
      echo  = TRUE,
      max.deparse.length = Inf
    ),
    message = msg_tee,
    warning = msg_tee
  )
}

## run d-in-d scripts (Section 5)
setwd(dcodedir); dir()
myfiles <- dir()[str_detect(dir(), '^[0-9]{2}')]
#myfiles <- dir()[1:3]
for (myfile in myfiles) {
  print("######")
  print("Running:")
  print(myfile)

  pcodedir <- file.path(
    find_root(criterion = has_file('_rapol.RProj')),
    "code", "03_dind"
  )
  setwd(pcodedir)
  withCallingHandlers(
    #run in its *own* env
    source(
      myfile,
      local = new.env(),
      echo  = TRUE,
      max.deparse.length = Inf
    ),
    message = msg_tee,
    warning = msg_tee
  )
}

## close sink + file
sink(type = "output")
close(log_con)
