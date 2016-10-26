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
}  ## Esta ya jala, ni le muevas


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
} ## Esta ya jala, ni le muevas



## Write a function that takes a directory of data files and a threshold for complete cases and calculates the correlation between sulfate and nitrate for monitor locations where the number of completely observed cases (on all variables) is greater than the threshold. The function should return a vector of correlations for the monitors that meet the threshold requirement. If no monitors meet the threshold requirement, then the function should return a numeric vector of length 0. A prototype of this function follows


##For this function you will need to use the 'cor' function in R which calculates the correlation between two vectors. Please read the help page for this function via '?cor' and make sure that you know how to use it.

## You can see some example output from this function. The function that you write should be able to match this output. Please save your code to a file named corr.R. To run the submit script for this part, make sure your working directory has the file corr.R in it.
