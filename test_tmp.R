
path <- "/projectnb/cheas/paleon/ED_runs/met_drivers/phase1a_met/bias_corr_format/PBL_tmp2/"
files <- list.files(path)

for(f in 1:length(files)){
  nc <- open.ncdf(paste(path,files[f],sep=""))
  tmp <- get.var.ncdf(nc,"tmp")-273.15
  close.ncdf(nc)
  if(f==1){
    temp <- tmp
  }else{
    temp <- c(temp,tmp)
  }
}

plot(temp[1:(365*4)])
for(y in 2:20){
  points(temp[((y-1)*365*4+1):(y*365*4)],col=y)
}
