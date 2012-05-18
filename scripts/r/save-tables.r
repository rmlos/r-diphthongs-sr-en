#
# Save results in tables
#
# This code is useful in cases when the (R) console does
# not show utf characters. However, I have used a different
# system for saving tables (see paper-main.r).
#
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#

kOutPath <- ''
kRenScript <- 'python "..\\Researh\\scripts\\misc\\diph_hex_unicode.py"'


WriteTable <- function(fname, content){
  #
  # Write data to table
  #
  rows.to.round = c('f1', 'f2', 'f3', 'intensity', 'pitch')
  for (i in rows.to.round){
    if (i %in% names(content)){
      content[,i] <- round(content[,i], 2)
    }
  }
  out.table <- paste(kOutPath, fname, sep='')
  out.file <- file(out.table, 'w',  encoding = 'UTF-8')
  cat('Writing ', fname, '...\n')
  write.csv(content, file = out.file)
  close(out.file)

}

MakeTables <- function(){
  #
  # Write tabels to disk.
  #
  cat('Deleting old CSV files...\n')
  system(paste('ren ', kOutPath, '*.csv', ' *.tmp', sep=''), wait=T)
  cat('Starting the tables...', '\n')
  WriteTable('times-diph.csv', res.t.diph.join)
  WriteTable('fpi-ref1.csv', res.fpi.ref1)
  WriteTable('fpi-ref2.csv', res.fpi.ref2)
  WriteTable('fpi-cor1.csv', res.fpi.cor1)
  WriteTable('fpi-cor2.csv', res.fpi.cor2)
  WriteTable('fpi-cor.csv', res.fpi.cor)
  WriteTable('fpi-ref.csv', res.fpi.ref)
  # Differences in FPI
  WriteTable('res-diff-fpi.csv', res.diff.fpi)
  WriteTable('res-diff-fpi2.csv', res.diff.fpi2)
  WriteTable('res-diff-fpi1.csv', res.diff.fpi1)
  WriteTable('res-diff-fpil1.csv', res.diff.fpil1)
  WriteTable('res-diff-fpil2.csv', res.diff.fpil2)
  WriteTable('res-diff-fpis1.csv', res.diff.fpis1)
  WriteTable('res-diff-fpis2.csv' , res.diff.fpis2)
  # Times
  WriteTable('res-t-diph-join.csv' , res.t.diph.join)
  #
  cat('Starting Python...', '\n')
  system(kRenScript, wait=T)
}

