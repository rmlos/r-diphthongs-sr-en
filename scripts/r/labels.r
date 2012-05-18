#
# IPA and other labels used in the code/plots.
#
# UTF-8 IPA characters are displayed correctly in
# R/ESS console on Linux only. On Windows R shows
# encoded items.
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#                                        

# Diphthongs in ASCII

kDiph <- c('ey', 'ay', 'oy', 'ow', 'aw', 'ia', 'ea', 'ua')

diph.names.all <- c('ey_s', 'ey_l', 'ay_s', 'ay_l', 'oy_s', 'oy_l',
  'ow_s', 'ow_l', 'aw_s', 'aw_l', 'ia_s', 'ia_l',
  'ea_s', 'ea_l', 'ua_s', 'ua_l')

# Diphthongs in IPA, hex values:
diph.ipa <- c('e\u026A', 'a\u026A', '\u0254\u026A', 
             '\u0259\u028A', '\u0251\u028A', '\u026A\u0259', 
             '\u025B\u0259', '\u028A\u0259')

diph.labels.ipa <- c('e\u026A_s', 'e\u026A_l',
                     'a\u026A_s', 'a\u026A_l', 
                     '\u0254\u026A_s', '\u0254\u026A_l',
                     '\u0259\u028A_s', '\u0259\u028A_l',
                     '\u0251\u028A_s', '\u0251\u028A_l',
                     '\u026A\u0259_s', '\u026A\u0259_l',
                     '\u025B\u0259_s', '\u025B\u0259_l',
                     '\u028A\u0259_s', '\u028A\u0259_l'
                     )

# Create labels for 6 x 2 dipthongs. Thanks to Tomath from #R.
dlength <- c('s', 'l')
diph.labels.ipa <- paste(rep(diph.ipa,each=length(dlength)),dlength,sep='')


# Labels for intervals:                                        
tmp <- c()
tmp2 <- c()
for (i in c(1:16)){
  s1 <- paste(diph.names.all[i], '_1', sep='')
  s2 <- paste(diph.names.all[i], '_2', sep='')
  tmp <- append(tmp, s1)
  tmp <- append(tmp, s2)
  i1 <- paste(diph.labels.ipa[i], '_1', sep='')
  i2 <- paste(diph.labels.ipa[i], '_2', sep='')
  tmp2 <- append(tmp2, i1)
  tmp2 <- append(tmp2, i2)
}


# This contains all labels for sounds inside
# diphthongs.

diph.names.extended <- tmp
diph.names.extended.ipa <- tmp2
rm(tmp, s1, s2, i, tmp2, i1, i2)


# Words used in corpus:
word.names.all <- c('bait', 'bows', 'dies', 'Joyce',
                    'doit', 'thereto', 'daresay', 'gourd',
                    'fierce', 'babe', 'bode', 'dais', 'boat',
                    'gouge','abjured', 'dice', 'idiot', 'theirs',
                    'bite', 'fears', 'Job', 'toyed','graduate',
                    'idiom', 'bourse', 'doubt', 'douse', 'daze',
                    'joys', 'bide', 'joke', 'dare')

diph.labels <- data.frame(ascii=diph.names.all, ipa=diph.labels.ipa)
diph.labels.extended <- data.frame(ascii=diph.names.extended,
                                   ipa=diph.names.extended.ipa,
                                   stringsAsFactors=F)

#For Serbian ---------------------------------------------------------
#Only the basic ones
serbian.v <- c('a', 'e', 'i', 'o', 'u')
serbian.i <- c('\u0061', '\u025B', '\u0069', '\u0254', '\u0075')
serbian.vowels <- data.frame(ascii=serbian.v,
                             ipa=serbian.i,
                             stringsAsFactors=F)
#Extended (with short and long)
serbian.v.ext <-  paste(rep(serbian.v,each=length(dlength)),dlength,sep='')
serbian.i.ext <-  paste(rep(serbian.i,each=length(dlength)),dlength,sep='')
serbian.vowels.extended <- data.frame(ascii=serbian.v.ext,
                                      ipa=serbian.i.ext,
                                      stringsAsFactors=F)

# This is no longer needed:
rm(diph.names.all)
rm(diph.names.extended, diph.names.extended.ipa)
rm(serbian.i, serbian.v)
rm(serbian.i.ext, serbian.v.ext)
rm(dlength)
