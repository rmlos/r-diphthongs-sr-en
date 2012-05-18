#
# Calculations for segmented formants data. This is
# unfinished.
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#

# DATA PRECALCULATONS --------------------------

## GroupSegments <- function(sdata, sdataname){
##   #
##   # Aggregate data from segments
##   #
##   agg <-
##   aggregate(. ~ paste(sdata[, 'diph'], sdata[, 'segmentid'], sep='_'),
##             data=sdata[,c('f1', 'f2', 'f3')],
##             mean)

##   delstr <- paste('paste(', sdataname, '$diph, ',
##                   sdataname, '$segmentid, sep = "_")',
##                   sep='')
##   print(delstr)
##   tmp  <- agg[, delstr]
##   agg[, delstr] <- NULLy
##   agg$diph <- tmp

##   return(agg)

## }

res.seg.cor <-
  aggregate(. ~ paste(raw.seg.cor$diph, raw.seg.cor$segmentid, sep='_'),
            data=raw.seg.cor[,c('f1', 'f2', 'f3')],
            mean)

# Correct name
tmp  <- res.seg.cor[, 'paste(raw.seg.cor$diph, raw.seg.cor$segmentid, sep = "_")']
res.seg.cor[, 'paste(raw.seg.cor$diph, raw.seg.cor$segmentid, sep = "_")'] <- NULL
res.seg.cor$ascii <- tmp

# Referent speaker -------------------

res.seg.ref <-
  aggregate(. ~ paste(raw.seg.ref$diph, raw.seg.ref$segmentid, sep='_'),
            data=raw.seg.ref[,c('f1', 'f2', 'f3')],
            mean)

# Correct name
tmp  <- res.seg.ref[, 'paste(raw.seg.ref$diph, raw.seg.ref$segmentid, sep = "_")']
res.seg.ref[, 'paste(raw.seg.ref$diph, raw.seg.ref$segmentid, sep = "_")'] <- NULL
res.seg.ref$ascii <- tmp

rm(tmp)

#testseg <- GroupSegments(raw.seg.ref, 'raw.seg.ref')

# ------------------------------------------------------------------------

# Make the copies.

#res.seg.ref.copy1 <- res.seg.ref
#res.seg.ref.copy2 <- res.seg.ref

#sseg <- res.seg.ref.copy1
#sfpi <- res.fpi.ref1

CalculateShifts <- function(sfpi, sseg){
  #
  # Substract the target frequencies from
  # mean values in segments.
  #
  ascii <- substr(sfpi[, 'ascii'], 1, 4)
  for (a in ascii){
    for (i in c('f1','f2', 'f3')){
      formantval <-  sfpi[substr(sfpi$ascii, 1, 4) == a, i]
      dline <- sseg[substr(sseg$ascii, 1, 4) == a, i] - formantval
      sseg[substr(sseg$ascii, 1, 4) == a, i] <- dline
    } 
  }
  return(sseg)
}

shift.ref1 <- CalculateShifts(res.fpi.ref1, res.seg.ref)
shift.ref2 <- CalculateShifts(res.fpi.ref2, res.seg.ref)
shift.cor1 <- CalculateShifts(res.fpi.cor1, res.seg.cor)
shift.cor2 <- CalculateShifts(res.fpi.cor2, res.seg.cor)

shift.corr1 <- CalculateShifts(res.fpi.cor1, res.seg.ref)
shift.corr2 <- CalculateShifts(res.fpi.cor2, res.seg.ref)

shift.ref1 <- transform(shift.ref1, sum=f1+f2)
shift.ref2 <- transform(shift.ref2, sum=f1+f2)
shift.cor1 <- transform(shift.cor1, sum=f1+f2)
shift.cor2 <- transform(shift.cor2, sum=f1+f2)



GetValues <- function(cond, sdata){
  return(sdata[substr(sdata$ascii, 1, 4) == cond, ])

}
