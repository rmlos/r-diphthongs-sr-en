
 This code was used in analysis of the data measured by the Prat
 program, for the needs of a paper titled: "Pronunciation of English
 Diphthongs by Speakers of Serbian".

 ==== THE R CODE ============================================

The aim was to analyze the data (formants, pitch, intensity) and then
create tables and images.

The tables were created by first displaying data in the ESS console,
and then copy it to LibreOffice/OpenOffice Calc.  Calc neatly
recognized the structure and formatted a table (this copy/paste
procedure did not work in MS Excel).

Images are created in batch by this code, by executing
CreateAllPlots(). You can see the result here:

<http://www.languagebits.com/files/r-plots-diphthongs/>

This is the first time I have written a project in R, hence
repetitions (and other awkwardness) is the code.

INSTALLATION AND RUNNING

On most Debian distros "sudo apt-get r-core" will install R. Then,
enter R in terminal to launch the R interpreter. You will also need
car package, which you can install by entering install.packages() and
following the instruction.

Afterward, use setwd() to set the working directory.

Since this code is created for a specific one-time use, you will need
the specific data (look for "data" folder). Set path kPathData to
point to that folder. Also, adjusts value in kImagesPath for output
images.

 To run the code, type:
 > source('paper-main.r')

 To see all objects, type:
 > ls()

 To create all graphs, type:
 > CreateAllPlots()

The scripts were written on Ubuntu, running Emacs and ESS.  They were
used on Windows and Linux.

Big "thank you" goes to the people in #R channel on Freenode, for
their cordial assistance and patience.

Romeo Mlinar, mlinar [a] languagebits.com
GNU General Public License v. 3

 ==== THE R PRAAT CODE ======================================

The script written for Praat uses the textgrids and signal files
(audio recording) and then creates tab delimited output. You can see
it in "data" folder here.

The script creates 3 files:

 _src-times.txt
 - The list of diphthongs with length.
 - The list of words with lengths.
 - Speaker IDs.

 _src_formants_segments.txt
 - The list with f1, f2 and f3 values marked with 
	segmentid (the number of segments is set by segments_count) 
 - Speaker IDs.

 _src-fpi.txt
 - The list of f1, f2, f3, intensity and pitch targets data
 - Speaker IDs. 

The segmentation in textgids is explaned at:
<http://www.languagebits.com/files/segmentation.pdf>

==== THE PYTHON CODE =================================================

I needed to write several tools in Python. Here is a small script
(checker.py) that uses TextGrid parser from NLTK to check the
integrity of the textgrids.

Regards,
R.

Romeo Mlinar, mlinar [a] languagebits.com
GNU General Public License v. 3
