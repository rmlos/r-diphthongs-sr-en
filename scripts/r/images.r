#
# This file contains code for creating and
# saving plots as images.
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#

kWidth = 1333
kHeight = 1333

CreateAllPlots <- function(p=kImagesPath,
                           out=png){
  #
  # This will create all graphs.
  #  
  #
  #
  
  i = 0
  
  # Distance -----------------------------------------------------------

  width <- kWidth
  height <- kHeight

  i = i + 1
  file <- paste(p, 'magnitude-difference.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawDistanceDifference()
  dev.off()
  
  
  # Time, diph ----------------------------------------------------------

  width <- kWidth
  
  i = i + 1
  file <- paste(p, 'time-diphthongs-difference.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawTimesDotChart('diff', indata=res.t.diph.join, nopolysyllabic=F)
  dev.off()

  i = i + 1
  file <- paste(p, 'time-diphthongs-ref-first.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawTimesDotChart('ref', indata=res.t.diph.join,  nopolysyllabic=F)
  dev.off()

  i = i + 1
  file <- paste(p, 'time-diphthongs-cor-first.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawTimesDotChart('cor', indata=res.t.diph.join, nopolysyllabic=F)
  dev.off()

  # Time, ratio ---------------------------------------------

  i = i + 1
  file <- paste(p, 'time-ratio-words.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotTimeDot(res.ratio.words,
                  level='diff',
                  ptitle=paste('Ratios of dipthongs within words\n',
                  '(differences, referent minus corpus)', sep=''),
                  sxlab='Percentage')
  dev.off()

  i = i + 1
  file <- paste(p, 'time-ratio-diphthongs.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotTimeDot(res.ratio.diphs,
                  level='diff',
                  ptitle=paste('Ratios of diphthongs within words\n',
                  '(differences, referent minus corpus)', sep=''),
                  sxlab='Percentage')
  dev.off()

  

  # Time, words ------------------------------------------

  i = i + 1
  file <- paste(p, 'time-words-cor-first.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawTimesDotChart('cor', indata=res.t.words)
  dev.off()

  i = i + 1
  file <- paste(p, 'time-words-ref-first.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawTimesDotChart('ref', indata=res.t.words)
  dev.off()

  i = i + 1
  file <- paste(p, 'time-words-cor-first-nopoly.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawTimesDotChart('cor', indata=res.t.words, nopolysyllabic=T)
  dev.off()

  i = i + 1
  file <- paste(p, 'time-words-ref-first-nopoly.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawTimesDotChart('ref', indata=res.t.words, nopolysyllabic=T)
  dev.off()
  
  ## # Charts that cannot be placet in Word, and therefore
  ## # obsolete.
  ## width <- 1000
  
  ## file <- paste(p, 'xxobs-time-diphthongs-difference.png', sep='')
  ## out(file, width=width, units='px')
  ## DrawBarPlotTimeDiff()
  ## dev.off()

  ## file <- paste(p, 'xxobs-time-diphthongs-combined.png', sep='')
  ## out(file, width=width, units='px')
  ## DrawMedianDiphTimesJoined()
  ## dev.off()

  ## file <- paste(p, 'xxobs-time-diphthongs-referent.png', sep='')
  ## out(file, width=width, units='px')
  ## DrawMedianDiphthonsTimesRef()
  ## dev.off()

  ## file <- paste(p, 'xxobs-time-diphthongs-corpus.png', sep='')
  ## out(file, width=width, units='px')
  ## DrawMedianDiphthonsTimesCor()
  ## dev.off()

  # Vowel space ---------------------------------------------------
  
  width = kWidth
  height = kHeight

  i = i + 1
  file <- paste(p, 'vowel-space-ref1.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpace(what='ref1')
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-ref2.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpace(what='ref2')
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-cor1.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpace(what='cor1')
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-cor2.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpace(what='cor2')
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-joined-ref.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpaceJoin(what='ref')
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-joined-cor.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpaceJoin(what='cor')
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-corref-sounds.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpaceJoin(what='joined', dp=T)
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-corref-nosounds.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpaceJoin(what='joined', dp=F)
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-serbian-english-all.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpaceJoin(what='sreng', dp=F)
  dev.off()

  i = i + 1
  file <- paste(p, 'vowel-space-markovic.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawVowelSpaceJoin(what='markovic', dp=T)
  dev.off()

  # Target movements -----------------------------

  width = kWidth
  height = kHeight

  i = i + 1
  file <- paste(p, 'target-movement-short.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawDiphMovementJoinedByLen('s')
  dev.off()

  i = i + 1
  file <- paste(p, 'target-movement-long.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawDiphMovementJoinedByLen('l')
  dev.off()

  i = i + 1
  file <- paste(p, 'target-movement-corpus.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawDiphMovementJoinedByData('cor')
  dev.off()

  i = i + 1
  file <- paste(p, 'target-movement-referent.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawDiphMovementJoinedByData('ref')
  dev.off()

  # Pitch  plots --------------------------------

  width = kWidth

  i = i + 1
  file <- paste(p, 'pitch-reference.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotIntensityPitch('pitch', 'ref')
  dev.off()

  i = i + 1
  file <- paste(p, 'pitch-corpus.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotIntensityPitch('pitch', 'cor')
  dev.off()

  i = i + 1
  file <- paste(p, 'pitch-differences.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotIntensityPitchDiffs('pi')
  dev.off()

   # Intensity  plots --------------------------------

  width = kWidth

  i = i + 1
  file <- paste(p, 'intensity-reference.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotIntensityPitch('intensity', 'ref')
  dev.off()

  i = i + 1
  file <- paste(p, 'intensity-corpus.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotIntensityPitch('intensity', 'cor')
  dev.off()

  i = i + 1
  file <- paste(p, 'intensity-differences.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotIntensityPitchDiffs('in')
  dev.off()

  # Ellipses plots ---------------------------------
 
  width = kWidth
  height = kHeight

  i = i + 1
  file <- paste(p, 'ellipses-serbian-markovic.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotEllipse('markovic')
  dev.off()

  i = i + 1
  file <- paste(p, 'ellipses-corpus-first.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotEllipse('cor_one')
  dev.off()

  i = i + 1
  file <- paste(p, 'ellipses-corpus-second.png', sep='')
  out(file, width=width, height=height, units='px')
  DrawPlotEllipse('cor_two')
  dev.off()

  print(paste("Done.", "There should be", i, "plots in folder", kImagesPath))
  
}
