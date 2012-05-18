#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#

# DISTANCES

DrawDistanceDifference <- function(dif=res.dist.joined){
  #
  # Bar plot of a distance.
  #
  d.data <- sortres(dif, 'diff')
  dotchart(d.data$diff,
          labels=d.data$ipa,
          xlab='Magnitude (Hz)',
          ylab='Diphthongs',
          main='Magnitude differences',
          pch=15)
}

# COMMANDS FOR DRAWING FORMANT PLOTS -------------------------------------

DrawVowelSpace <- function(what, st=T){
  #
  # Draw a plot with the referent formant values.
  #
  if (what == 'ref1'){
    title = 'Vowel space in referent speaker (1st target)'
    sdata=res.fpi.ref1
    target=1
  }
  else if (what == 'ref2'){
    title = 'Vowel space in referent speaker (2nd target)'
    sdata=res.fpi.ref2
    target=2
  }
  else if (what == 'cor1'){
    title = 'Vowel space in corpus (1st target)'
    sdata=res.fpi.cor1
    target=1
  }
  else if (what == 'cor2'){
    title = 'Vowel space in corpus (2nd target)'
    sdata=res.fpi.cor2
    target=2
  }
  else if (what == 'test'){
    title = 'test'
    sdata=ol
    target=1
  }
  
  DrawPlotFormants(title=title, sdata=sdata, st=st)
  DrawSounds(sdata=sdata, target=target)
}


DrawVowelSpaceJoin <- function(what='ref', dp=T){
  #
  # Draw a plot with the referent formant values.
  #
  if (what == 'ref'){
    DrawPlotFormants(title='Vowel space in referent speaker (both targets)',
                    sdata=res.fpi.ref)
    DrawSounds(res.fpi.ref1, target=1)
    DrawSounds(res.fpi.ref2, target=2)
  }
  else if (what == 'cor'){
    DrawPlotFormants(title='Vowel space in corpus (both targets)',
                    sdata=res.fpi.cor)
    DrawSounds(res.fpi.cor1, target=1)
    DrawSounds(res.fpi.cor2, target=2)
  }
  else if (what == 'joined'){
    DrawPlotFormants(title='Vowel space in corpus and referent data',
                    sdata=res.fpi.cor)
    DrawSounds(res.fpi.cor1, target=1, dp=dp)
    DrawSounds(res.fpi.cor2, target=2, dp=dp)
    DrawSounds(res.fpi.ref1, target=1, lty=3, dp=dp)
    DrawSounds(res.fpi.ref2, target=2, lty=3, dp=dp)
    legend(2600, 820,
         c('Corpus data', 'Referent speaker'),
         cex=0.85, 
         lty=c(1, 3))
  }
  else if (what == 'sreng'){
    DrawPlotFormants(title='Formant Space in Corpora',
                    sdata=res.fpi.cor,
                    st='markovic')
    DrawSounds(res.fpi.cor1, target=1, dp=dp)
    DrawSounds(res.fpi.cor2, target=2, dp=dp)
    DrawSounds(res.fpi.ref1, target=1, lty=3, dp=dp)
    DrawSounds(res.fpi.ref2, target=2, lty=3, dp=dp)
    DrawSounds(res.markovic.s, target=1, lty=4, dp=dp)
    DrawSounds(res.markovic.l, target=1, lty=4, dp=dp)
    legend(2800, 850,
         c('Corpus data', 'Referent speaker', 'Markovi\u107 corpus'),
         cex=0.9, 
         lty=c(1, 3, 4))

  }
  else if (what == 'markovic'){
    DrawPlotFormants(title='Formant Space in Markovi\u107 corpus',
                    sdata=res.fpi.cor,
                    st='markovic')
    DrawSounds(res.markovic.s, target=1, lty=2, dp=dp)
    DrawSounds(res.markovic.l, target=1, lty=1, dp=dp)
    legend(2800, 850,
         c('Short vowels', 'Long wovels'),
         cex=0.9, 
         lty=c(2, 1))

  }
}


DrawDiphMovementSingle <- function(fdata='ref', flen='s'){
  #
  # Draw starting and ending vowel of a diphthong,
  # marked with an arrow.
  #
    # Speaker in title
    if (fdata == 'ref'){
      ptitle <- 'Referent Speaker'
    }
    else {
      ptitle <- 'Corpus Data'
    }
    # Length in title
    if (flen == 's'){
      plen <- 'short diphthongs'
    }
    else {
      plen <- 'long diphthongs'
    }
    # Compile data sources
    cdata <- get(paste('res.fpi.', fdata, flen, sep=''))
    cdata1 <- get(paste('res.fpi.', fdata, flen, '1', sep=''))
    cdata2 <- get(paste('res.fpi.', fdata, flen, '2', sep=''))
    # Compile title
    title <- paste("Target movements in ", ptitle,
                   "\n(", plen, ")", sep='')
    
     DrawPlotFormants(title=title, sdata=cdata)
     DrawMovement(sdata1=cdata1, sdata2=cdata2)
  }

DrawDiphMovementJoinedByLen <- function(flen='s'){
  #
  # Draw the starting and the ending vowel of a diphthong,
  # marked with an arrow.
  # 
    # Length in title
    if (flen == 's'){
      plen <- 'short diphthongs'
      s.ref.1 <- res.fpi.refs1
      s.ref.2 <- res.fpi.refs2
      s.cor.1 <- res.fpi.cors1
      s.cor.2 <- res.fpi.cors2
      s.ref <- res.fpi.refs
    }
    else {
      plen <- 'long diphthongs'
      s.ref.1 <- res.fpi.refl1
      s.ref.2 <- res.fpi.refl2
      s.cor.1 <- res.fpi.corl1
      s.cor.2 <- res.fpi.corl2
      s.ref <- res.fpi.refl
    }
    # Compile title
    title <- paste("Target movements in corpora",
                   "\n(", plen, ")", sep='')
    
     DrawPlotFormants(title=title, sdata=s.ref)

    legend(2600, 830,
         c('Referent data', 'Corpus data'),
         cex=0.9, 
         lty=c(1, 2))
    
     DrawMovement(sdata1=s.ref.1, sdata2=s.ref.2, lt=1)
     DrawMovement(sdata1=s.cor.1, sdata2=s.cor.2, lt=2)    
  }

DrawDiphMovementJoinedByData <- function(fdata='ref'){
  #
  # Draw starting and ending vowel of a diphthong,
  # marked with an arrow.
  # 
    # Length in title
    if (fdata == 'ref'){
      psrc <- 'referent data'
      s.dat.1 <- res.fpi.refs1
      s.dat.2 <- res.fpi.refs2
      l.dat.1 <- res.fpi.refl1
      l.dat.2 <- res.fpi.refl2
      r.dat  <-res.fpi.ref
    }
    else {
      psrc <- 'corpus data'
      s.dat.1 <- res.fpi.cors1
      s.dat.2 <- res.fpi.cors2
      l.dat.1 <- res.fpi.corl1
      l.dat.2 <- res.fpi.corl2
      r.dat <- res.fpi.cor
    }
    # Compile title
    title <- paste("Target movements in ", psrc, sep='')
    DrawPlotFormants(title=title, sdata=r.dat)
    legend(2600, 830,
         c('Long diphthongs', 'Short diphthongs'),
         cex=0.9, 
         lty=c(1, 2))
     DrawMovement(sdata1=l.dat.1, sdata2=l.dat.2, lt=1)
     DrawMovement(sdata1=s.dat.1, sdata2=s.dat.2, lt=2)
  }

# Sample function

DrawDiphMovementSampleFunction <- function(fdata='ref'){
  #
  # Draw starting and ending vowel of a diphthong,
  # marked with an arrow.
  # 
    # Length in title
    if (fdata == 'ref'){
      psrc <- 'referent data'
      s.dat.1 <- res.fpi.refs1[res.fpi.refs1$ascii=='ey_s_1',]
      s.dat.2 <- res.fpi.refs2[res.fpi.refs2$ascii=='ey_s_2',]
      l.dat.1 <- res.fpi.refl1[res.fpi.refl1$ascii=='ey_l_1',]
      l.dat.2 <- res.fpi.refl2[res.fpi.refl2$ascii=='ey_l_2',]
      r.dat  <-res.fpi.ref
    }
    else {
      psrc <- 'corpus data'
      s.dat.1 <- res.fpi.cors1[res.fpi.cors1$ascii=='ey_s_1',]
      s.dat.2 <- res.fpi.cors2[res.fpi.cors2$ascii=='ey_s_2',]
      l.dat.1 <- res.fpi.corl1[res.fpi.corl1$ascii=='ey_1_1',]
      l.dat.2 <- res.fpi.corl2[res.fpi.corl2$ascii=='ey_l_2',]
      r.dat <- res.fpi.cor
    }
    # Compile title
    title <- paste("Target movements in ", psrc, sep='')
    DrawPlotFormants(title=title, sdata=r.dat)
    legend(2600, 830,
         c('Long diphthongs', 'Short diphthongs'),
         cex=0.9, 
         lty=c(1, 2))
     DrawMovement(sdata1=l.dat.1, sdata2=l.dat.2, lt=1)
     DrawMovement(sdata1=s.dat.1, sdata2=s.dat.2, lt=2)
  }


# COMMANDS FOR DRAWING TIME PLOTS ----------------------------------------

DrawTimesDotChart <- function(level='ref', indata=FALSE, nopolysyllabic=FALSE){
  #
  # Draw a plot with referent data.
  #
  #if (indata == FALSE){
  #  stop("Need data in indata")
  #  
  #res <- sortres(res.t.diph.join, level)
  legend.pos.x = 0.1
  legend.pos.y = 16
  res <- sortres(indata, level)
  if (level == 'diff'){
    ptitle <- 'Differences\n(referent minus corpus)'
  }
  else if (level %in% c('ref', 'cor')) {
    if ('word' %in% names(indata)){
      legend.pos.x = 0.55
      legend.pos.y = 4
      ptitle <- 'Word lengths\n(referent and corpus data)'
       # Is removal of polysillabic words needed?
      if (nopolysyllabic == TRUE){
        remove.list <- c('thereto', 'daresay', 'graduate')
        res <- res[!res$word %in% remove.list,]
  }
    }
    else{
      legend.pos.x = 0.1
      legend.pos.y = 16
      ptitle <- 'Diphtong lengths\n(referent and corpus data)'
    }
  }
  else if (level == 'perc'){
    ptitle <- 'Percentage ratio'
  }
  DrawPlotTimeDot(ptitle=ptitle,
                  pdata=res,
                  levels=level)
  legend(legend.pos.x, legend.pos.y,  
         c('Referent speaker', 'Corpus data'),
         cex=0.85, 
         pch=c(2, 17))
}


DrawBarPlotTimeDiff <- function(){
  #
  # Draw a barplot showing time length differences.
  #
  res <- sortres(res.t.diph.join, 'diff')
  barplot(res$diff,
      main='Length Differences in Diphtihongs (ref. - cor.)',
      names.arg = res$ipa)
}

DrawMedianDiphTimesJoined <- function(){
  #
  # Draw a plot with both corpus and referent data.
  #
  DrawPlotTime(ptitle='Mean Diphtong Lengths',
               labels=res.t.diph.join$ipa)
  points(res.t.diph.join$cor, pch=15)
  points(res.t.diph.join$ref, pch=21)
  legend(1, 0.30,
         c('Corpus data', 'Referent speaker'),
         cex=0.85, 
         pch=c(15, 21))
}


DrawMedianDiphthonsTimesRef <- function(){
  #
  # Draw a plot with referent data.
  #
  res <- sortres(res.t.diph.join, 'ref')
  DrawPlotTime(ptitle='Mean Diphtong Lengths - Referent Speaker',
               labels=res$ipa)
  points(res$ref, pch=21)
  legend(1, 0.30,
         c('Referent speaker'),
         cex=0.85, 
         pch=c(21))
}



DrawMedianDiphthonsTimesCor <- function(){
  #
  # Draw a plot with speaker data.
  #
  res <- sortres(res.t.diph.join, 'cor')
  DrawPlotTime(ptitle='Mean Diphtong Lengths - Corpus Data',
               labels=res$ipa)
  points(res$cor, pch=15)
  legend(1, 0.30,
         c('Corpus data'),
         cex=0.85, 
         pch=c(15))
}


# PLOTS FOR PITCH AND INTENSITY  -------------------------------------

DrawPlotIntensityPitchDiffs <- function(ptype='in'){
  # Values
  r.name <- paste(ptype, '.diff', sep='') # column name
  data.scope <- res.pi.diffs.all
  data.diff <- data.frame(ascii=data.scope$ascii,
                          ipa=data.scope$ipa,
                          diff=data.scope[,r.name])
  data.diff <- sortres(data.diff, 'diff')

  if (ptype=='in'){
    title='Differences in intensity\n(all targets)'
    xlab=('Decibels (dB)')
    
  }
  else if (ptype=='pi'){
    title='Differences in pitch\n(all targets)'
    xlab=('Frequency (Hz)')
    
  }
  else {
    stop("ptype must be pi or in")
  }

  dotchart(x = data.diff$diff,
           main = title,
           pch=15,
           labels=data.diff$ipa,
           xlab=xlab,
           ylab='Diphthong targets')
  
}


DrawPlotIntensityPitch <- function(ptype='intensity',
                                   dtype='ref'){
  # Values
  vs1 <<- get(paste('res.fpi.', dtype, 's1', sep=''))
  vs2 <<- get(paste('res.fpi.', dtype, 's2', sep=''))
  vl1 <<- get(paste('res.fpi.', dtype, 'l1', sep=''))
  vl2 <<- get(paste('res.fpi.', dtype, 'l2', sep=''))
  ipal <<- substr(vs1$ipa, 1, 2)
  # Compile the title, values and legend position
  if (ptype == 'intensity'){
    title <- 'Intensity'
    xleg <- 62.5
    yleg <- 2
  }
  else if (ptype == 'pitch'){
    title <- 'Pitch'
    xleg <- 250
    yleg <- 2
  }
  # Compile the rest of title
  if (dtype == 'ref'){
    title <- paste(title, '- Referent Data')
  }
  if (dtype == 'cor'){
    title <- paste(title, '- Corpus Data')
  }
  # Draw the initial plot
  ipal <<- substr(vs1$ipa, 1, 2)
  DrawPitchIntDot(ptitle=title,
               ipal=ipal,
               type=ptype,
               pdata=vs1[, ptype],
               pch=2)
  points(vs2[, ptype], 1:8, pch=22)
  points(vl1[, ptype], 1:8, pch=17)
  points(vl2[, ptype], 1:8, pch=15)
  legend(xleg, yleg,
         c('Short, 1st target.',
           'Short, 2nd target.',
           'Long, 1st target.',
           'Long, 2nd target.'),
         cex=0.85, 
         pch=c(2, 22, 17, 15))
}


# PLOTS FOR ELLIPSES -------------------------------------

DrawPlotEllipse <- function(what='markovic'){
  #
  #
  #
  if (what=='markovic'){
    DrawPlotFormants(title='Formant Space in Serbian (Markovic)',
                    st='markovic')
    DrawEllipses(type='markovics', lty=2)
    DrawEllipses(type='markovicl', lty=1)
    legend(2800, 880,
         c('Short vowel',
           'Long vowel'),
         cex=0.85, 
         lty=c(2,1))
  }
  else if (what=='cor_one'){
    DrawPlotFormants(title='First Target in Corpus',
                     st=T)
    DrawEllipses(type='cor_one', lty=NULL)
     legend(2580, 830,
         c('In short diphthong',
           'In long diphthong'),
         cex=0.85,
         lty=c(2,1))
  }
  else if (what=='cor_two'){
    DrawPlotFormants(title='Second Target in Corpus',
                     st=T)
    DrawEllipses(type='cor_two', lty=NULL)
     legend(2580, 830,
         c('In short diphthong',
           'In long diphthong'),
         cex=0.85,
         lty=c(2,1))
  }
  else if (what=='ref_one'){
    DrawPlotFormants(title='First Target in Referent Speaker',
                     st=T)
    DrawEllipses(type='ref_one', lty=NULL)
     legend(2580, 830,
         c('In short diphthong',
           'In long diphthong'),
         cex=0.85,
         lty=c(2,1))
  }
  else if (what=='ref_two'){
    DrawPlotFormants(title='Second Target in Referent Speaker',
                     st=T)
    DrawEllipses(type='ref_two', lty=NULL)
     legend(2580, 830,
         c('In short diphthong',
           'In long diphthong'),
         cex=0.85,
         lty=c(2,1))
  }
  else {
    stop("No such ellipse graph.")
  }
  
}
