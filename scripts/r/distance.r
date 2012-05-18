#
# Calculate the Euclidean distances in formants.
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#


DiphthongDistance <- function(fq1, fq2){
  #
  # Calculate distances between A with coordinates (F1, F2) and
  # B (F1, F2) for each diphthong.
  #
  # fq1 - first target frequencies
  # fq2 - second target frequencies
  #
  if (nrow(fq1) != nrow(fq2)){
    stop("Number of rows must be the same.")
  }
  
  # Coordinates
  x1 <- fq1$f1
  y1 <- fq1$f2
  
  x2 <- fq2$f1
  y2 <- fq2$f2

  fd <<- rbind(x1, y1, x2, y2)

  #Labels

  ascii <- substr(fq1$ascii, 1, 4)
  ipa <- substr(fq1$ipa, 1, 3)

  distance <- c()
  
  for (i in (1:nrow(fq1))){
    d <- euc.dist2(fd[,i][1], fd[,i][2], fd[,i][3], fd[,i][4])
    distance <- c(distance, as.numeric(d))
  }

  frame.distance <- data.frame(ascii=ascii,
                               ipa=ipa,
                               dist=round(distance, 2))
  
  return(frame.distance)
}


DistanceByTarget <- function(vdata, target='target'){
  # Differences by target
  res <- c()
  cor <- c()
  if (target=='target'){
    ascii <- c('y', 'a', 'w')
    ipa <- c('ɪ', 'ə', 'ʊ')
    a = 2
  }
  else if (target=='len'){
    ascii= c('s', 'l')
    ipa= c('s', 'l')
    a = 4
  }
  else {
    stop('Target not defined.')
  }
  for (i in ascii){
    tmp <- vdata[substring(vdata$ascii, a,a)==i,]
    res <- c(res, mean(tmp$ref))
    cor <- c(cor, mean(tmp$cor))
  }
  data.calculated <- data.frame(ascii=ascii,
                    ipa=ipa,
                    ref=res,
                    cor=cor,
                    diff=res-cor,
                    perc=perc(cor,res))

  return(rounddata(data.calculated))
                    
}


euc.dist2 <- function(x1, y1, x2, y2){
  #
  # Euclidean distance for two dimensions.
  #
  return(sqrt((x1-x2)^2+(y1-y2)^2))
}
