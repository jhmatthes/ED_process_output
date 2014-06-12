#plot the monthly ED input met drivers 
#Jaclyn Hatala Matthes, 2/3/14
#jaclyn.hatala.matthes@gmail.com

#Load libraries
ok = require(chron,lib.loc="/usr4/spclpgm/jmatthes/"); if (! ok) stop("Package chron is not available...")
ok = require(ncdf,lib.loc="/usr4/spclpgm/jmatthes/") ; if (! ok) stop("Package ncdf is not available...")
ok = require(maps,lib.loc="/usr4/spclpgm/jmatthes/") ; if (! ok) stop("Package maps is not available...")
ok = require(sp,lib.loc="/usr4/spclpgm/jmatthes/") ; if (! ok) stop("Package raster is not available...")
ok = require(raster,lib.loc="/usr4/spclpgm/jmatthes/") ; if (! ok) stop("Package raster is not available...")
ok = require(colorspace,lib.loc="/usr4/spclpgm/jmatthes/") ; if (! ok) stop("Package raster is not available...")

#Set sites
sites <- c("PBL","PHA","PMB","PDL","PHO","PUN")
pft <- c(5,6,8,9,10,11)
suffix <- "g01.h5" #output file suffixs

for(s in 1:length(sites)){
  #Set directories
  dat.dir    <- paste("/projectnb/cheas/paleon/ED_runs/phase1a_runs/",sites[s],"/analy/",sep="")
  match.files <- grep("-E-",list.files(dat.dir))
  files <- list.files(dat.dir)
  mon.files  <- files[match.files] #monthly files only
  
  #Get time window
  yeara <- as.numeric(strsplit(mon.files,"-")[[1]][3]) #first year
  yearz <- as.numeric(strsplit(mon.files,"-")[[length(mon.files)]][3]) #last year
  montha <- as.numeric(strsplit(mon.files,"-")[[1]][4]) #first month
  monthz <- as.numeric(strsplit(mon.files,"-")[[length(mon.files)]][4]) #first month
  
  agb.pft <- lai.pft <- bsa.pft <- matrix(nrow=(yearz-yeara+1),ncol=length(pft))
  balive <- broot <- bleaf <- bsapa <- bsapb <- sfast <- sslow <- sstruc <- vector(length=(yearz-yeara+1))
  
  #loop over months and aggregate monthly mean data
  for (y in yeara:yearz){
    
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
      year.now  = sprintf("%4.4i",y)
      month.now = sprintf("%2.2i",m)
      day.now   = sprintf("%2.2i",0)
      hour.now  = sprintf("%6.6i",0)
      
      file.now  = paste(sites[s],"spin","-E-",year.now,"-",month.now,"-",day.now,"-"
                        ,hour.now,"-",suffix,sep="")
      
      cat(" - Reading file :",mon.files[y-yeara+1],"...","\n")
      now <- open.ncdf(paste(dat.dir,file.now,sep=""))
      
      #Grab global & patch level variables.
      npoly     <- get.var.ncdf(now,'NPOLYGONS_GLOBAL')
      nsites    <- get.var.ncdf(now,'NSITES_GLOBAL')
      npatches  <- get.var.ncdf(now,'NPATCHES_GLOBAL')
      ncohorts  <- get.var.ncdf(now,'NCOHORTS_GLOBAL')
      ncohort.per.patch    <- get.var.ncdf(now,'PACO_N')
      patch.area.per.site  <- get.var.ncdf(now,'AREA')  
      site.area.per.poly   <- get.var.ncdf(now,'AREA_SI')
      ind.patch.per.site   <- get.var.ncdf(now,'SIPA_ID')
      ind.cohort.per.patch <- get.var.ncdf(now,'PACO_ID')
      lat       <- get.var.ncdf(now,'LATITUDE')
      lon       <- get.var.ncdf(now,'LONGITUDE')
      
      #Grab cohort level variables.
      ipft      <- get.var.ncdf(now,'PFT')
      dbh       <- get.var.ncdf(now,'DBH')
      nplant    <- get.var.ncdf(now,'NPLANT')
      npp       <- get.var.ncdf(now,'MMEAN_NPPDAILY_CO')
      npp.croot <- get.var.ncdf(now,'MMEAN_NPPCROOT_CO')
      npp.froot <- get.var.ncdf(now,'MMEAN_NPPFROOT_CO')
      npp.leaf  <- get.var.ncdf(now,'MMEAN_NPPLEAF_CO')
      npp.leaf  <- get.var.ncdf(now,'MMEAN_NPPWOOD_CO')
      gpp       <- get.var.ncdf(now,'MMEAN_GPP_CO')
      


