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
z <- matrix(nrow=length(id),ncol=2)
i <- 1
  for(fid in id){
      datos <- read.csv(files_list[i])
      nobs <- length(!is.na(datos))
      z[i,1] <- i
      z[i,2] <- nobs
      i = i + 1
  }
z
}

pollutantmean("specdata","sulfate",1:10)