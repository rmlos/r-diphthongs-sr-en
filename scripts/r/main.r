#
# This R code was used in analysis of the data measured by
# the Prat program, for the needs of a paper titled:
# "Pronunciation of English Diphthongs by Speakers of Serbian".
#
# The aim was to analyze the data (formants, pitch, intensity)
# and then create tables and images.
#
# The tables were created by first displaying data in the ESS
# console, and then copy it to LibreOffice/OpenOffice Calc.
# Calc neatly recognized the structure and formatted a table
# (this copy/paste procedure did not work in MS Excel).
#
# Images are created in batch by this code, by executing
# CreateAllPlots(). You can see the result here:
#
# <http://www.languagebits.com/files/r-plots-diphthongs/>
#
# This is the first time I have written a project in R, hence
# repetitions (and other awkwardness) is the code.
#
# INSTALLATION AND RUNNING
#
# On most Debian distros "sudo apt-get r-core" will install R. Then,
# enter R in terminal to launch the R interpreter. You will also need
# car package, which you can install by entering install.packages() and
# following the instruction.
#
# Afterward, use setwd() to set the working directory.
#
# Since this code is created for a specific one-time use, you will need the
# specific data (look for "data" folder). Set path kPathData to point to
# that folder. Also, adjusts value in kImagesPath for output images.
#
# To run the code, type:
# > source('paper-main.r')
#
# To see all objects, type:
# > ls()
#
# To create all graphs, type:
# > CreateAllPlots()
#
# The scripts were written on Ubuntu, running Emacs and ESS.
# They were used on Windows and Linux.
#
# Big "thank you" goes to the people in #R channel
# on Freenode, for their cordial assistance and patience.
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#


## r-diphthongs-sr-en
## Copyright (C) 2012 Romeo Mlinar

## This program is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, either version 3 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You should have received a copy of the GNU General Public License
## along with this program.  If not, see <http://www.gnu.org/licenses/>.



#Clear the workspace.
rm(list=ls(all=TRUE))

#This package is needed for drawing ellipses.
require(car)


#Constants
kElements <- c('f1', 'f2', 'f3', 'intensity', 'pitch')

# Main path for data measured in Praat
if (.Platform$OS.type == 'windows'){
  kPathData <- "data\\"
  kImagesPath <- "images-generated\\"
}
if (.Platform$OS.type == 'unix'){
  kPathData <- "data/"
  kImagesPath <- "images-generated/"
}

source('helpers.r')
source('input.r')
source('labels.r')
source('distance.r')
source('time.r')
source('fpi.r')
source('fsegments.r')
source('plot-elements.r')
source('plots.r')
source('images.r')
source('save-tables.r')
