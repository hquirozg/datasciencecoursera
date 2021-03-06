
pollutantmean <- function(directory,pollutant,id = 1:332){
files_list <- list.files(directory,full.names = TRUE)[id]
dat <- data.frame()
i <- 1
  for (fid in id){
    dat <- rbind(dat,read.csv(files_list[i]))
    i <- i + 1  
  }
dat_p <- dat[pollutant]
  average = mean(dat_p[!is.na(dat_p)])
  print(average)
  #print(dat_p[!is.na(dat_p)])
}  ##esta ya jalo asì que ni le muevas


complete <- function(directory,id = 1:332) {
  files_list <- list.files(directory,full.name=TRUE)[id]
  datos <- data.frame()
  #z <- matrix(nrow=length(id),ncol=2)
  z <- data.frame(id=id,nobs=0)
  i <- 1
  nobs <- 0
  for(fid in id){
    datos <- read.csv(files_list[i])
    cond <- complete.cases(datos)
    nobs <- length(datos[cond,"ID"])
    z[i,1] <- id[i]
    z[i,2] <- nobs
    i = i + 1
  }
  z
}

corr <- function(directory, threshold=0) {
  files_list <- list.files(directory,full.name=TRUE)[1:332]
  DF <- data.frame
  r <- matrix()
  i <- 1
  for (fid in files_list){
      DF <- read.csv(files_list[i])
      selcase <- sum(complete.cases(DF))
          if(selcase > threshold){
            DFSub <- DF[complete.cases(DF),]  
            Nitrate <- DFSub["nitrate"]
            Sulfate <- DFSub["sulfate"]
            r <- rbind(r, cor(Sulfate,Nitrate))
          }
      i <- i + 1
  }
  r[-1]
}
