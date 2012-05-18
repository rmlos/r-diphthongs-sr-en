#
# Different elements for creating plots. These are
# used mostly by paper-plots.r
#
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#



kLwd <- 1

# -------------

DrawPitchIntDot <- function(type, pdata,  pch, ptitle, ipal){
  #
  # Draw a plot with custom axes
  #
  if (type == 'pitch'){
    xlab <- 'Pitch (Hz)'
    maxmin <- c(160, 285)
  }
  else if (type == 'intensity'){
    xlab <- 'Intensity (dB)'
    maxmin <- c(62, 85)
  }
  # Plot it!
  dotchart(x = pdata,
           main = ptitle,
           labels = ipal,
           pch = pch,
           xlim = maxmin,
           ylab = 'Diphthongs (short and long)',
           xlab = xlab)
}

DrawPlotTimeDot <- function(pdata, ptitle='NA', levels='ref', sxlab=FALSE){
  #
  # Draw a dotchart with two sets of data.
  #
  # xlim - it was precalculated after examening the data
  #
  #
  pxlab = 'Time (s)'
  # Select which data lavel to display from
  # the dataframe and set the appropriate
  # axis limit.
  if (levels == 'ref'){
    maindata <- pdata[, 'ref']
    pchm <- 2
    secondary <- pdata[, 'cor']
    pchs <- 17
    xlim =  c(0.1, 0.30)
  }
  else if (levels == 'cor'){
    maindata <- pdata[, 'cor']
    pchm <- 17
    secondary <- pdata[, 'ref']
    pchs <- 2
    xlim =  c(0.1, 0.30)
  }
  else if (levels == 'diff'){
    maindata <- pdata[, 'diff']
    secondary <- c()
    pchm <- 2
    xlim =  range(maindata)
  }
  else if (levels == 'perc'){
    maindata <- pdata[, 'perc']
    secondary <- c()
    pchm <- 2
    xlim =  range(maindata)
  }  
  # Are we dealing with words or diphthongs?
  if ('word' %in% names(pdata)){
    # Determine the number of words; there are 32 in
    # the corpus, but some may have been removed for this
    # particular graph.
    points.range <- 1:length(pdata$word)
    plabels = pdata$word
    pylab = ''
    xlim =  range(maindata, secondary)
  }
  else if ('diph' %in% names(pdata)){
    points.range <- 1:16
    plabels = pdata$ipa
    pylab = 'Diphthongs (short and long)'
  }
  else if (levels == 'perc'){
    points.range <- 1:31
    plabels = pdata$word
    pxlab = 'Percent (%)'
  }
  # Should we override xlab?
  if (!(sxlab==FALSE)){
    pxlab <- sxlab
  }
  dotchart(x = maindata,
           labels = plabels,
           pch=pchm,
           xlim =  xlim,
           main = ptitle,
           ylab = pylab, 
           xlab = pxlab
           )
  # If there is a second set of points to
  # draw, do that here.
  if (length(secondary) > 0){
    points(secondary, points.range,  pch=pchs)
  }
}

# --------------

DrawPlotTime <- function(pdata, prange, ptitle, labels){
  #
  # Draw a plot with custom axes
  #
  plot(
    c(1,16), 
    c(0.1, 0.30),
    axes=FALSE,
    main=ptitle,
    type='n',
    xlab = 'Diphthongs (short and long)',
    ylab = 'Time (s)')
  axis(1, lab=labels, at=1:16, lwd=kLwd)
  axis(2, lwd=kLwd)
}

DrawPitchInt <- function(type, ptitle, labels, pylab, prange){
  #
  # Draw a plot with custom axes
  #
  if (type == 'pitch'){
    ylab <- 'Pitch (Hz)'
    maxmin <- c(160, 285)
  }
  else if (type == 'intensity'){
    ylab <- 'Intensity (dB)'
    maxmin <- c(62, 85)
  }
  # Plot it!
  plot(
    c(1,prange), 
    maxmin, #retrieved from median calculations + legend
    axes=FALSE,
    main=ptitle,
    type='n',
    xlab = 'Diphtnong',
    ylab = ylab)
  axis(1, lab=labels, at=1:prange, lwd=kLwd)
  axis(2, lwd=kLwd)
}


DrawEllipses <- function(type, lty=lty){
  #
  # Draw ellipses for several data samples.
  #
  if (type == 'markovics'){ # Short vowels from Markovic
    sdata <- res.markovic.raw
    for (s in serbian.vowels$ascii){
      p  <- sdata[sdata$ascii==paste(s,'s', sep=''),]
      PlaceEllipse(sdata=p, label=rplasc(s), lty=lty)
    }
  }
  else if (type == 'markovicl'){ # Long vowels from Markovic
    sdata <- res.markovic.raw
    for (s in serbian.vowels$ascii){
      p  <- sdata[sdata$ascii==paste(s,'l', sep=''),]
      PlaceEllipse(sdata=p, label=rplasc(s), lty=lty)
    }
  }
  else if (type %in% c('cor_one', 'cor_two', 'ref_one', 'ref_two')){
    if (type == 'cor_one'){
      target.cond <- '1'
      sdata <- raw.fpi.cor
    }
    else if (type == 'cor_two'){
      target.cond <- '2'
      sdata <- raw.fpi.cor
    }
    else if (type == 'ref_one'){
      target.cond <- '1'
      sdata <- raw.fpi.ref
    }
    else if (type == 'ref_two'){
      target.cond <- '2'
      sdata <- raw.fpi.ref
    }
    else {
      stop('No such type of ellipse placement.')
    }
    ds <- diph.labels.extended
    target  <<- ds[substr(ds$ascii, 6, 6)==target.cond,]
    for (s in 1:16){
      p  <<- sdata[sdata$diph==target$ascii[s],]
      length.id <- substr(target$ascii[s], 4, 4)
      #target.id <- as.numeric(substr(target$ipa[s], 6, 6))
      label <- substr(target$ipa[s], 1, 1)
      if (length.id  == 'l'){
        lty <- 1
      }
      else {
        lty <- 2
      }
      PlaceEllipse(sdata=p, label=label, lty=lty)
    }
  }
  else {
    stop('No such ellipse plot ID')
  }
}

#ol <- res.markovic.raw[res.markovic.raw$ascii=='ol',]

PlaceEllipse <- function(sdata, label, lty){
  #
  # Place an elipse onto the plot. 
  #
  dataEllipse(sdata$f2, sdata$f1,
              levels=0.5,
              col='black',
              add=T,
              lwd=1,
              plot.points=F,
              center.cex=1,
              center.pch=0,
              lty=lty)
  # Place the center character (phc above does
  # not work for some reason)
  
  x <- mean(sdata$f2)
  y <- mean(sdata$f1)
  points(x, y, pch=label)

}

DrawSounds <-function(sdata, what='formants', target, lty=1, dp=T){
  #
  # Place sounds on F1/F2 map. 
  #
  # target - 1st or 2nd vowel
  #
  #
  jd <- data.frame(f1=sdata$f2, f2=sdata$f1)
  if (dp == T){
    points(sdata$f2, sdata$f1, pch=substr(sdata$ipa, target, target))
    ltype='c'
  }
  else if (dp == F){
    ltype = 'l'
  }
  otr  <- chull(jd)
  otr <- c(otr, otr[1])
  lines(jd[otr, ]$f1, jd[otr, ]$f2, type=ltype, lty=lty)
}

DrawMovement <- function(sdata1, sdata2, lt=1){
  #
  # Draw the 1st and the 2nd element of the diphthong,
  # joined by an arrow.
  #
  points(sdata1$f2, sdata1$f1, pch=substr(sdata1$ipa, 1, 1))
  points(sdata2$f2, sdata2$f1, pch=substr(sdata2$ipa, 2, 2))
  segments(x0=sdata1$f2, y0=sdata1$f1,
           x1=sdata2$f2, y1=sdata2$f1,
           par(lty=lt))
  arrows(sdata1$f2, sdata1$f1,
         sdata2$f2, sdata2$f1,
         length  = 0.15)
  par.default <- par(no.readonly=TRUE)
}

DrawPlotFormants <-function(st=T, title='Test graph', sdata){
  #
  # Draw F1/F2 plane
  #
  if (st == T){
    f2max <- 2600
    f1max <- 900
    f2min <- 1000
    f1min <- 300
    warning('Using default max/min values for plot axes.')
  }
  else if (st == 'data') {
    f2max <- max(sdata$f2)
    f1max <- max(sdata$f1)
    f2min <- min(sdata$f2)
    f1min <- min(sdata$f1)
  }
  else if (st == 'markovic'){
    f2max <- 2750
    f1max <- 940
    f2min <- 750
    f1min <- 300
  }
  plot(f2max, 
       f1max,
       type = 'n',
       axes = 'T',
       xlab = 'F2 (Hz)',
       ylab = 'F1 (Hz)',
       main = title,  
       xlim = rev(c(f2min, f2max)),
       ylim = rev(c(f1min, f1max)))
}
