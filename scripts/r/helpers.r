#
# Misc. functions used in the code.
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#

getdiph <- function(v, PI=FALSE){
  #
  # Get FPI instances from both corpora.
  #

  n <- c("ascii","ipa","f1","f2","f3","pitch","intensity")

  if (PI==FALSE){
    n <- n[-c(6, 7)]
  }
  
  print('ref / cor')
  print(res.fpi.ref[substring(res.fpi.ref$ascii, 1, 4)==v,])
  print(res.fpi.cor[substring(res.fpi.cor$ascii, 1, 4)==v,])

  ref <- res.fpi.ref[substring(res.fpi.ref$ascii, 1, 4)==v,]
  cor <- res.fpi.cor[substring(res.fpi.cor$ascii, 1, 4)==v,]

  join <- merge(ref,cor, all=TRUE)
  join <- join[,n]
  join$id <- c('Ref.', 'Cor.', 'Ref.', 'Cor.')
  
  return(join)
}

pos.or.neg <- function(x){
  #
  # Return "+" for posive, "-" for negative
  # and "0" for zero.
  #
  if (!(is.numeric(x))){
    stop("x must be numeric.")
  }
  if (x > 0){
    return('+')
  }
  else if (x < 0){
    return('-')
  }
  else if (x == 0){
    return('0')
  }
  else {
    stop("X must be < or > or = from/to zero.")
  }                                   
}

in.barks <- function(rdata){
  #
  # Convert f1, f2 and f3 from Hetrz to Barks
  #
  for (i in names(rdata)){
    if (i %in% c('f1','f2','f3')){
      in.bark <- unlist(lapply(rdata[,i], bark))
      rdata[,i] <- in.bark
    }
  }
    return(rdata)
}

bark <- function(hz, r=2){
  #
  # Convert from Hz to bark
  #
  # Formula from:
  # http://ncslaap.lib.ncsu.edu/tools/norm/norm1_methods.php
  b <- 26.81/(1+1960/hz) - 0.53
  return(round(b, r))
}

rounddata <- function(rdata, roundto=2){
  #
  # Round to a given number.
  #
  for (i in names(rdata)){
    if (is.numeric(rdata[,i])){
      rdata[,i] <- round(rdata[,i], roundto)
    }
  }
    return(rdata)
}

paperreload <- function(){
  print('Reloading paper-main.r...')
  source('paper-main.r')
}

sortres <- function(what, by){
  #
  # Sort data.
  #
  return(what[order(what[,by]),])
}

perc <-function(x, y){
  #
  # Return percentage of y for x
  # and round to r.
  #
  p <- (x/y)*100
  return(round(p, 2))
}

rplasc <- function(letter){
  #
  # Replace ascii character with ipa.
  #
  i <- which(serbian.vowels$ascii==letter)
  return(serbian.vowels$ipa[i])
}

getipa <- function(adata){
  #
  # Replace all characters in adata
  # with IPA.
  #
  tmp.replaced <- unlist(lapply(substr(adata, 1, 1), rplasc))
  tmp.ipa <- paste(tmp.replaced, substr(adata, 2, 2), sep='')
  return(tmp.ipa)
}

