#
# Calulates formants, pitch and intensity
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#

GetFPI <- function(fpidata, calc=mean){
  #
  # Extract formants, pitch and intensity data.
  # Returns matrix with labels.
  #
  # There are 32 diphthong labels: for each of 8 diphthings
  # there are long and short vesion, whereas each version
  # has two targets. In short:
  #
  #          8 diphthongs
  #        x 2 long/short version
  #        x 2 targets
  #          ----------
  #          32 labels
  #
  
  res <-
  aggregate(. ~ fpidata$diph,
            data=fpidata[,c("f1", "f2", "f3", "pitch", "intensity")],
            calc, stringsAsFactors=F)
  res$ipa <- diph.labels.extended[order(diph.labels.extended$ascii),]$ipa
  res$ascii <- diph.labels.extended[order(diph.labels.extended$ascii),]$ascii
  res[, 'fpidata$diph'] <- NULL
  # round data
  res <- rounddata(res)
  # reorder
  res <- res[,c('ascii','ipa','f1','f2','f3','pitch','intensity')]
  return(res)
}

GetFPI.M <- function(fpidata, calc=mean){
  # Extract formants data from Markovic korpus.
  res <-
  aggregate(. ~ fpidata$diph,
            data=fpidata[,c("f1", "f2", "f3", "pitch", "intensity")],
            calc, stringsAsFactors=F)
  res$ipa <- diph.labels.extended[order(diph.labels.extended$ascii),]$ipa
  res$ascii <- diph.labels.extended[order(diph.labels.extended$ascii),]$ascii
  return(res)
}

GetFPIDiffs <- function(fpidata.ref, fpidata.cor){
  #
  # Calculate the diffrences and return the resulting
  # data frame.
  #
  crows <- c('f1', 'f2', 'f3', 'pitch', 'intensity')
  res.diff <- fpidata.ref[,crows] -  fpidata.cor[,crows]
  res.diff[, c('ascii', 'ipa')] <- fpidata.ref[, c('ascii', 'ipa')]
  #res.diff[,crows] <- res.diff[,crows] 
  return(res.diff)
}

GetFPIPerc <- function(fpidata.ref, fpidata.cor){
  #
  # Calculate percentage differences in corpora.
  #
  data.range <- c('f1', 'f2', 'f3', 'intensity', 'pitch')
  data.out <- data.frame(ascii=fpidata.ref$ascii,
                         ipa=fpidata.ref$ipa,
                         f1=perc(fpidata.ref$f1, fpidata.cor$f1),
                         f2=perc(fpidata.ref$f2, fpidata.cor$f2),
                         f3=perc(fpidata.ref$f3, fpidata.cor$f3),
                         intensity=perc(fpidata.ref$intensity,
                           fpidata.cor$intensity),
                         pitch=perc(fpidata.ref$pitch, fpidata.cor$pitch))
  return(data.out)
}

GetTargetDiffs <- function(fdata){
  #
  # Subtract target value f1 from f2.
  #
  data.lab <- diph.labels$ipa
  target1 <- fdata[substr(fdata$ascii, 5, 6) == '_1',]
  target2 <- fdata[substr(fdata$ascii, 5, 6) == '_2',]
  data.out <- data.frame(ascii=substr(target1$ascii, 1, 4),
                         ipa=substr(target1$ipa, 1, 3),
                         f1=target2$f1-target1$f1,
                         f2=target2$f2-target1$f2,
                         f3=target2$f3-target1$f3)
  return(data.out)
}

GetPlusMinusTable <- function(dataref, datacor){
  #
  # Return +/- table for formants.
  #
  data.out <- data.frame(ascii=dataref$ascii,
                         ipa=dataref$ipa,
                         f1ref=unlist(lapply(dataref$f1, pos.or.neg)),
                         f2ref=unlist(lapply(dataref$f2, pos.or.neg)),
                         f3ref=unlist(lapply(dataref$f3, pos.or.neg)),
                         f1cor=unlist(lapply(datacor$f1, pos.or.neg)),
                         f2cor=unlist(lapply(datacor$f2, pos.or.neg)),
                         f3cor=unlist(lapply(datacor$f3, pos.or.neg))
                         )
  return(data.out)
  
}

GetVowelData <- function(vdata){
  #
  # Calculate standard deviation, maximum and
  # minimum for vowels in corpora. 
  #
  vowels <- c()
  all.v <- c()
  vowels.count <- c()
  for (diph in vdata$ipa){
    # Find the target
    target <- as.numeric(substring(diph, 5,5))
    if (!(target %in% c(1,2))){
      stop("Target must be either 1 or 2")
    }
    
    vowel <- substring(diph, target, target)
    all.v <- c(all.v, vowel)
    if (!(vowel %in% vowels)){
      vowels <-c(vowels, vowel)
    }
  }
  # Count diphthongs
  for (i in vowels){
    vowels.count <- c(vowels.count, sum(length(all.v[all.v==i])))
  }
  # Remove NA element
  fun.calls <- c(min, max, mean)
  fun.names <- c('min', 'max', 'sd', 'mean')
  vowel.data <- c()
  all.rows = array()
  for (v in vowels){
    for (el in kElements){
      for (n in fun.names){
        tmp <- get.single(vdata, v, el, n)
        vowel.data <- c(vowel.data, tmp)
        c.name <- paste(el, '.', n, sep='')
      }
    } 
  }
  vowel.data <- round(vowel.data, 2)
  idx <- seq(1, length(vowel.data), by=20)
  vowel.frame <- data.frame(vowel=vowels,
                            count=vowels.count,
                            # F1 ---------------
                            f1min=vowel.data[idx],
                            f1max=vowel.data[idx+1],
                            f1sd=vowel.data[idx+2],
                            f1mea=vowel.data[idx+3],
                            # F2 ---------------
                            f2min=vowel.data[idx+4],
                            f2max=vowel.data[idx+5],
                            f2sd=vowel.data[idx+6],
                            f2mea=vowel.data[idx+7],
                            # F3 ---------------
                            f3min=vowel.data[idx+8],
                            f3max=vowel.data[idx+9],
                            f3sd=vowel.data[idx+10],
                            f3mea=vowel.data[idx+11],
                            # Intensity
                            intmin=vowel.data[idx+12],
                            intmax=vowel.data[idx+13],
                            intsd=vowel.data[idx+14],
                            intmea=vowel.data[idx+15],
                            # Pitch
                            pchmin=vowel.data[idx+16],
                            pchmax=vowel.data[idx+17],
                            pchsd=vowel.data[idx+18],
                            pchmea=vowel.data[idx+19])
  
  return(sortres(vowel.frame, 'count'))
}
  


get.single <- function(vdata, single.vowel, c.name, calc){
  #
  # Get values from FPI
  # regardless of length.
  #
  target <- as.numeric(substring(vdata$ipa, 5, 5))
  vowel.filtered <- vdata[substr(vdata$ipa, target, target)==single.vowel,]
  v <<- vowel.filtered[,c.name]
  v.dat <- vowel.filtered[,c.name]
  return(do.call(calc, list(v.dat)))
}


GetPIDiffs <- function(){
  #
  # Return pitch, intensity and their differences.
  #
  calculated <- data.frame(ascii=res.fpi.ref$ascii,
                           ipa=res.fpi.ref$ipa,
                           pi.ref=res.fpi.ref$pitch,
                           pi.cor=res.fpi.cor$pitch,
                           pi.diff=res.fpi.ref$pitch - res.fpi.cor$pitch,
                           in.ref=res.fpi.ref$intensity,
                           in.cor=res.fpi.cor$intensity,
                           in.diff=res.fpi.ref$intensity -
                                   res.fpi.cor$intensity)
  return(calculated)
}

ending.schwa <- function(fpidata, byword='dare'){
  #
  # Calculate formants by word filtering
  #
  fpidata <- fpidata[fpidata$word==byword & fpidata$diph == 'ea_l_1',]
  schwa <- data.frame(ipa='ə',
                      asci='ea_l_1',
                      f1 = mean(fpidata$f1),
                      f2 = mean(fpidata$f2),
                      f3 = mean(fpidata$f3, na.rm=T),
                      intensity = mean(fpidata$intensity),
                      pitch = mean(fpidata$pitch))
  return(schwa)
  
}


# DIPHTHONG TARGETS -------------------------------------------------



# Explanations
#  res.fpi - the result, calculated values of referent formants
#  cor.fpi - the result, calculated values of corpus formants
#  .cor    - values of all diphthongs
#  .cor1   - values of the 1st targets only
#  .cor2   - values of the 2nd targets only
#  l1      - values of the 1st long targets only
#  s1      - values of the 1st short targets only
#  l2      - values of the 2nd long targets only
#  s2      - values of the 2nd short targets only
#


# TARGETS IN CORPUS
res.fpi.cor <- GetFPI(raw.fpi.cor)
# Targets 1 and 2
res.fpi.cor1 <- res.fpi.cor[substr(res.fpi.cor$ascii, 5, 6)=='_1',]
res.fpi.cor2 <- res.fpi.cor[substr(res.fpi.cor$ascii, 5, 6)=='_2',]
# Long and short.
res.fpi.cors1 <- res.fpi.cor[substr(res.fpi.cor$ascii, 4, 6)=='s_1',]
res.fpi.cors2 <- res.fpi.cor[substr(res.fpi.cor$ascii, 4, 6)=='s_2',]
res.fpi.corl1 <- res.fpi.cor[substr(res.fpi.cor$ascii, 4, 6)=='l_1',]
res.fpi.corl2 <- res.fpi.cor[substr(res.fpi.cor$ascii, 4, 6)=='l_2',]
# All long and short
res.fpi.cors <- res.fpi.cor[substr(res.fpi.cor$ascii, 4, 4)=='s',]
res.fpi.corl <- res.fpi.cor[substr(res.fpi.cor$ascii, 4, 4)=='l',]


# TARGETS IN REFERENT SPEAKER
res.fpi.ref <- GetFPI(raw.fpi.ref)
res.fpi.ref1 <- res.fpi.ref[substr(res.fpi.ref$ascii, 5, 6)=='_1',]
res.fpi.ref2 <- res.fpi.ref[substr(res.fpi.ref$ascii, 5, 6)=='_2',]
# Long and short
res.fpi.refs1 <- res.fpi.ref[substr(res.fpi.ref$ascii, 4, 6)=='s_1',]
res.fpi.refs2 <- res.fpi.ref[substr(res.fpi.ref$ascii, 4, 6)=='s_2',]
res.fpi.refl1 <- res.fpi.ref[substr(res.fpi.ref$ascii, 4, 6)=='l_1',]
res.fpi.refl2 <- res.fpi.ref[substr(res.fpi.ref$ascii, 4, 6)=='l_2',]
#All long and short
res.fpi.refs <- res.fpi.ref[substr(res.fpi.ref$ascii, 4, 4)=='s',]
res.fpi.refl <- res.fpi.ref[substr(res.fpi.ref$ascii, 4, 4)=='l',]

# end targets calculation -------------------------------------------

# Calculations for Serbian vowels, from Markovic corpus

idx <- seq(1,nrow(raw.markovic), by=3)
res.markovic <- data.frame(
	ascii=paste(raw.markovic$vowel, raw.markovic$length, sep="")[idx],
	f1=raw.markovic$frequency[idx],
	f2=raw.markovic$frequency[idx+1],
	f3=raw.markovic$frequency[idx+2])

res.markovic.raw <- res.markovic

#Mean values aggregation
res.tmp <-
  aggregate(. ~ res.markovic$ascii,
            data=res.markovic[,c('f1', 'f2', 'f3')],
            mean)
#Correct name
res.tmp$ascii <- res.tmp[, 'res.markovic$ascii']
res.tmp[, 'res.markovic$ascii'] <- NULL
res.markovic <- NULL
res.markovic <- res.tmp


# For ipa characters --------------------
tmp.ascii <- res.markovic$ascii
tmp.replaced <- unlist(lapply(substr(tmp.ascii, 1, 1), rplasc))
tmp.ipa <- paste(tmp.replaced, substr(tmp.ascii, 2, 2), sep='')
# Add IPA to corpus:
res.markovic$ipa <- tmp.ipa

res.markovic.s <- res.markovic[substr(res.markovic$ascii,2,2)=='s',]
res.markovic.l <- res.markovic[substr(res.markovic$ascii,2,2)=='l',]

# Markovic corpus end -------------------------------------------------

# Calculation of differences

res.diff.fpi <- GetFPIDiffs(res.fpi.ref, res.fpi.cor)
res.diff.fpi2 <- GetFPIDiffs(res.fpi.ref2, res.fpi.cor2)
res.diff.fpi1 <- GetFPIDiffs(res.fpi.ref1, res.fpi.cor1)
res.diff.fpil1 <- GetFPIDiffs(res.fpi.refl1, res.fpi.corl1)
res.diff.fpil2 <- GetFPIDiffs(res.fpi.refl2, res.fpi.corl2)
res.diff.fpis1 <- GetFPIDiffs(res.fpi.refs1, res.fpi.cors1)
res.diff.fpis2 <- GetFPIDiffs(res.fpi.refs2, res.fpi.cors2)

# F1/F2 differences

res.fpi.formdiff.cor <- GetTargetDiffs(res.fpi.cor)
res.fpi.formdiff.ref <- GetTargetDiffs(res.fpi.ref)
res.fpi.formdiff.joined <-
  GetPlusMinusTable(res.fpi.formdiff.ref, res.fpi.formdiff.cor)

# Stats

res.stats.ref <- GetVowelData(res.fpi.ref)
res.stats.cor <- GetVowelData(res.fpi.cor)
res.vowels.ref <- res.stats.ref[,c('vowel', 'f1mea', 'f2mea', 'f3mea')]
res.vowels.cor <- res.stats.cor[,c('vowel', 'f1mea', 'f2mea', 'f3mea')]
res.vowels.diff <- data.frame(vowel=res.vowels.ref$vowel,
                              f1=res.vowels.ref$f1mea-res.vowels.cor$f1mea,
                              f2=res.vowels.ref$f2mea-res.vowels.cor$f2mea,
                              f3=res.vowels.ref$f3mea-res.vowels.cor$f3mea)

# Convert main data frame to barks

res.bark.fpi.ref <- in.barks(res.fpi.ref)
res.bark.fpi.cor <- in.barks(res.fpi.cor)

# Percentage differences
res.fpi.perc <- GetFPIPerc(res.fpi.ref, res.fpi.cor)

# Euclidean distance
res.dist.ref <- DiphthongDistance(res.fpi.ref1, res.fpi.ref2)
res.dist.cor <- DiphthongDistance(res.fpi.cor1, res.fpi.cor2)

res.dist.joined <- data.frame(ascii=res.dist.ref$ascii,
                              ipa=res.dist.ref$ipa,
                              ref=res.dist.ref$dist,
                              cor=res.dist.cor$dist,
                              diff=res.dist.ref$dist - res.dist.cor$dist,
                              perc=perc(res.dist.cor$dist, res.dist.ref$dist))

# Pitch and intensity
res.pi.diffs.all <- GetPIDiffs()
res.pi.diffs.l <- res.pi.diffs.all[substring(res.pi.diffs.all$ascii,4,4)=='l',]
res.pi.diffs.s <- res.pi.diffs.all[substring(res.pi.diffs.all$ascii,4,4)=='s',]
res.pi.diffs.1 <- res.pi.diffs.all[substring(res.pi.diffs.all$ascii,6,6)=='1',]
res.pi.diffs.2 <- res.pi.diffs.all[substring(res.pi.diffs.all$ascii,6,6)=='2',]

rm(idx,
   tmp.ascii,
   tmp.replaced,
   tmp.ipa,
   res.tmp,
   res.dist.ref,
   res.dist.cor)
