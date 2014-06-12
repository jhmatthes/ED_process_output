#Setup analysis file structure
base  <- "/projectnb/cheas/paleon/ED_runs/phase1a_spinup/"
out   <- "/projectnb/cheas/paleon/ED_runs/SAS_spinup/phase1a_spinup/"
#sites <- c("PBL","PHA","PMB","PDL","PHO","PUN")
niter <- length(list.dirs(paste(base,sites[1],"/",sep=""),recursive=FALSE)) #iterations/site 
pft   <- c(5,6,8,9,10,11) #set of PFTs used in analysis
dpm <- c(31,28,31,30,31,30,31,31,30,31,30,31)
sufx  <- "g01.h5"

dat.dir    <- paste(base,"PBL","/spin01/histo10/",sep="")
match.files <- grep("-S-",list.files(dat.dir))
files <- list.files(dat.dir)
mon.files  <- files[match.files] #monthly files only

#Get time window
yeara <- as.numeric(strsplit(mon.files,"-")[[1]][3]) #first year
yearz <- as.numeric(strsplit(mon.files,"-")[[length(mon.files)]][3]) #last year
montha <- as.numeric(strsplit(mon.files,"-")[[1]][4]) #first month
monthz <- as.numeric(strsplit(mon.files,"-")[[length(mon.files)]][4]) #first month

htry <- vector()
for (y in yeara:yearz){
    
    #calculate month start/end based on year 
    if (y == yeara){
      month.begin = montha
    }else{
      month.begin = 1
    }
    if (y == yearz){
      month.end = monthz
    }else{
      month.end = 12
    }
    
    for(m in month.begin:month.end){
      #Make the file name. 
      year.now  <-sprintf("%4.4i",y)
      month.now <- sprintf("%2.2i",m)
      day.now   <- sprintf("%2.2i",1)
      hour.now  <- sprintf("%6.6i",0)
      
      dat.dir     <- paste(base,"PBL","/spin01/histo5/",sep="")
      file.now    <- paste("PBL","01spin","-S-",year.now,"-",month.now,"-",day.now,"-"
                           ,hour.now,"-",sufx,sep="")
      
      cat(" - Reading file :",file.now,"...","\n")
      now <- open.ncdf(paste(dat.dir,file.now,sep=""))
      
      ind <- ((m-month.begin)+1)+(y-yeara)*12
      htry[ind] <- get.var.ncdf(now,"HTRY")
      close.ncdf(now)
    }
  }
}
plot(htry)
