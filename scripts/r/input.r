#
# Loads the measurements and performs the initial
# cleanup.
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#


# Speakers / corpus:
raw.times.cor <- read.table(
  paste(kPathData, 'cor-times.txt', sep=''), 
  header=1)
# Reference speakers:
raw.times.ref <- read.table(
  paste(kPathData, 'ref-times.txt', sep=''), 
  header=1)
# Formants, pitch and intensity for corpus:
raw.fpi.cor <- read.table(
  paste(kPathData, 'cor-fpi.txt', sep=''), 
  header=1, na.strings = c('--undefined--'))
 # Formants, pitch and intensity for referent speaker:
raw.fpi.ref <- read.table(
  paste(kPathData, 'ref-fpi.txt', sep=''),                         
  header=1, na.strings = c('--undefined--'))

# Serbian vowels data
raw.markovic <- read.table(
  paste(kPathData, 'markovic2.txt', sep=''), 
  header=1)

# Segmented formants data
raw.seg.ref <- read.table(
  paste(kPathData, 'ref-formants_segments-10.txt', sep=''), 
  header=1, na.strings = c('--undefined--'))

# Correct segmentid
raw.seg.ref <- transform(raw.seg.ref, segmentid = as.numeric(segmentid) - 1)


raw.seg.cor <- read.table(
  paste(kPathData, 'cor-formants_segments-10.txt', sep=''),
  header=1, na.strings = c('--undefined--'))

# Correct segmentid
raw.seg.cor <- transform(raw.seg.cor, segmentid = as.numeric(segmentid) - 1)
