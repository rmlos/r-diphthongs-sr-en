#
# This file contains code for temporal calculations and
# values needed for the graphs.
#
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#



# TIME CALCULATIONS -------------------------------------------------

CalculateTimePerWord <- function(tdata){
  #
  # Calculate times in such way that the resulting
  # frame contains every word with subtracter values.
  #
  result <<- data.frame(word = tdata$word,
                       diph = tdata$diph,
                       diphtime = tdata$time_d,
                       diff = tdata$time_w - tdata$time_d)
  result.agg <<- aggregate(result[,c('diff', 'diphtime')],
                          by=list(result$word),
                          mean)
  result.agg <<- aggregate(tdata[,c('time_w', 'time_d')],
                          by=list(result$word),
                          mean)
  names(result.agg) <- c('word', 'time.w', 'time.d')
  return(result.agg)  
}


CalculateTimeMeans <- function(tdata, what, roundto=3){
  #
  # Calculate mean times for each diphthong.
  #
  if (what == 'diph'){
    d.means = NULL
    for (d in diph.labels$ascii){
      d.data <- subset(tdata, tdata$diph == d)
      d.means <- append(d.means, mean(d.data$time_d))
      rm(d.data)
    }
    # Create data frame for diphthong mean values and theis names.
    means <- data.frame(diph=diph.labels$ascii,
                        times=d.means,
                        ipa=diph.labels$ipa)
    # Correct the order of the diphthongs so they are in the order
    # as above.
    # NB: This with factor ()seems to return the same values,
    # so it is commented out.
    #means$diph <- factor(means$diph,levels=unique(means$diph))
  }
  if (what == 'word'){
    w.means = NULL
    for (w in word.names.all){
      w.data <- subset(tdata, tdata$word == w)
      w.means <- append(w.means, mean(w.data$time_w))
      rm(w.data)
    }
    means <- data.frame(word=word.names.all, times=w.means)
  }
  means$times <- round(means$times, roundto)
  return(means)
}


CalculateTimeRatios <- function(tdata, perc='diph', roundto=2){
  #
  # Calculate ratios that diphthongs/words have within
  # words/diphthongs.
  #
  if (!(perc %in% c('diph', 'word', 'diph.as.word'))){
    stop('Either "diph" or "word"  must be supplied in "perc".')
  }
  if (perc == 'word'){
    tmp <- data.frame(word=tdata$word,
                      perc=perc(tdata$time_d,
                                tdata$time_w))
    tmp.agg <- aggregate(tmp$perc,
                          by=list(tmp$word),
                          mean)
    names(tmp.agg) <- c('word', 'perc')
    tmp.agg$perc <- round(tmp.agg$perc, roundto)
    return(tmp.agg)
  }
  if (perc == 'diph'){
     tmp <- data.frame(diph=tdata$diph,
                       perc=perc(tdata$time_d,
                                 tdata$time_w))
     tmp.agg <- aggregate(tmp$perc,
                          by=list(tmp$diph),
                          mean)
     names(tmp.agg) <- c('diph', 'perc')
     tmp.agg$perc <- round(tmp.agg$perc, roundto)
     tmp.agg$ipa <- diph.labels[order(diph.labels$ascii),]$ipa
     return(tmp.agg)
  }

}


# Filter out polysyllabic words
FilterPolysyllabic <- function(data){
  # Filter out all polysyllabic words from the
  # data.
  if (! 'word' %in% names(data)){
    stop("Data must have the 'word' column.")
  }
  remove.list <- c('thereto', 'daresay', 'graduate')
  res <- data[!data$word %in% remove.list,]
  return(res)
}

# Calculate and store for use
res.t.diph.ref <- CalculateTimeMeans(raw.times.ref, 'diph')
res.t.word.ref <- CalculateTimeMeans(raw.times.ref, 'word')
res.t.diph.cor <- CalculateTimeMeans(raw.times.cor, 'diph')
res.t.word.cor <- CalculateTimeMeans(raw.times.cor, 'word')
 
# Joined data for diphtong times
res.t.diph.join <- data.frame(diph=res.t.diph.ref$diph,
                              ipa=res.t.diph.ref$ipa,
                              ref=res.t.diph.ref$times,
                              cor=res.t.diph.cor$times,
                              diff=res.t.diph.ref$times -
                              res.t.diph.cor$times,
                              per=perc(res.t.diph.ref$time,
                                       res.t.diph.cor$time))
#Joined data for word times
res.t.words <- data.frame(word=res.t.word.ref$word,
                          cor=res.t.word.cor$time,
                          ref=res.t.word.ref$time,
                          diff=res.t.word.ref$time -
                               res.t.word.cor$time)
                          

# Sort
res.t.diph.join <- res.t.diph.join[order(res.t.diph.join$cor),]

# Calculate percentage for each word -----------------------------------

res.t.ratio.word.ref <- CalculateTimeRatios(raw.times.ref, 'word')
res.t.ratio.word.cor <- CalculateTimeRatios(raw.times.cor, 'word')
res.t.ratio.diph.ref <- CalculateTimeRatios(raw.times.ref, 'diph')
res.t.ratio.diph.cor <- CalculateTimeRatios(raw.times.cor, 'diph')

# Put the above in one table
res.t.ratio.word <- data.frame(word=res.t.ratio.word.ref$word,
                               ref=res.t.ratio.word.ref$perc,
                               cor=res.t.ratio.word.cor$perc)

res.t.ratio.diph <- data.frame(word=res.t.ratio.diph.ref$diph,
                               ipa=res.t.ratio.diph.ref$ipa,
                               ref=res.t.ratio.diph.ref$perc,
                               cor=res.t.ratio.diph.cor$perc)

# Calculate word minus diphthong

res.t.wordsmdiph.ref <- CalculateTimePerWord(raw.times.ref)
res.t.wordsmdiph.cor <- CalculateTimePerWord(raw.times.cor)
                               
# Main table for word diffs ------------------------------------------

# Sort by words
res.ratio.words <-
  sortres(res.t.ratio.word.ref, 'word')
# Rename perc to ref
res.ratio.words$ref <- res.ratio.words$perc
res.ratio.words$perc <- NULL
# Add percentage from cor
res.ratio.words$cor <- sortres(res.t.ratio.word.cor, 'word')$perc
# Add differences.
res.ratio.words$diff <- res.ratio.words$ref - res.ratio.words$cor
# Sort by diffrerences
res.ratio.words <- sortres(res.ratio.words, 'diff')

# Main table for diph  diffs ------------------------------------------

# Sort by words
res.ratio.diphs <-
  sortres(res.t.ratio.diph.ref, 'diph')
# Rename perc to ref
res.ratio.diphs$ref <- res.ratio.diphs$perc
res.ratio.diphs$perc <- NULL
# Add percentage from cor
res.ratio.diphs$cor <- sortres(res.t.ratio.diph.cor, 'diph')$perc
# Add differences
res.ratio.diphs$diff <- res.ratio.diphs$ref - res.ratio.diphs$cor
#Sort my diffrerences
res.ratio.diphs <- sortres(res.ratio.diphs, 'diff')

# Correlation for diffrences in diph-word ratio and
# diphthong diffrence lengts.

tmp.w <- sortres(res.ratio.diphs, 'diph')$diff #ratios
tmp.d <- sortres(res.t.diph.join, 'diph')$per #actual lengths
res.correlation.joined <- cor(tmp.w, tmp.d)

tmp.wr <- sortres(res.ratio.diphs, 'diph')$ref #ratios
tmp.dr <- sortres(res.t.diph.join, 'diph')$per #actual lengths
res.correlation.joined <- cor(tmp.w, tmp.d)

# Remove what is not needed

rm(#res.t.diph.ref,
   res.t.word.ref,
   res.t.diph.cor,
   res.t.word.cor,
   tmp.w, tmp.d,
   res.t.ratio.word.ref,
   res.t.ratio.word.cor,
   res.t.ratio.diph.ref,
   res.t.ratio.diph.cor)
