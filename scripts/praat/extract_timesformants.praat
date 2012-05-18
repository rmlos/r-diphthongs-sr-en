# 
# PRAAT ANALYSIS SCRIPTS
#
# Author: Romeo Mlinar <mlinar [a] languagebits.com>
# License: GNU General Public License v. 3
#
# This file is a series of commands used to automate the work
# of Praat (http://www.fon.hum.uva.nl/praat/). At this stage
# the script is written for a specific use (my research
# in diphthongs).
#
# All output files are tab delimited text files.
#
# IN
# A folder with WAV and TextGrid files.
#
# OUT
# In _src-times.txt
# - The list of diphthongs with length.
# - The list of words with lengths.
# - Speaker IDs.
#
# In _src_formants_segments.txt
# - The list with f1, f2 and f3 values marked with 
#	segmentid (the number of segments is set by segments_count) 
# - Speaker IDs.
#
# In _src-fpi.txt
# - The list of f1, f2, f3, intensity and pitch targets data
# - Speaker IDs.
#
# Explanation about the textgrids used in this scripts
# is explained at <http://www.languagebits.com/files/segmentation.pdf>
#
# Romeo Mlinar, mlinar [a] languagebits.com
# GNU General Public License v. 3
#


clearinfo

print PRAAT ANALYSYS SCRIPT'newline$'
print Starting...'newline$''newline$'

# Paths ----------------------------------------------------------
print Assigning paths...'newline$'
# Main path for files
path$ = "/media/data/corpus/speaker-reference/"
# path$ = "D:\corpus\analyse\"

# The file for time.
outtime$ = "/home/marw/Researh/data/dev-praatresults/_correction_src-times.txt"
headings_time$ = "name	word	time_w	diph	time_d"

# The file for all formants.
outformant$ = "/home/marw/Researh/data/dev-praatresults/_src_correction_allformants.txt"
headings_ftable$ = "name	diph	word	f1	b1	f2	b2	f3	b3"

# The file for formant segments.
outformantseg$ = "/home/marw/Researh/data/dev-praatresults/_correction_src_formants_segments.txt"
headings_segtable$ = "name	segmentid	diph	word	f1	f2	f3"

# The file for formats, pitch and intensity.
outfpi$ = "/home/marw/Researh/data/dev-praatresults/_corrections_src-fpi.txt"
headings_fpi$ = "name	diph	word	f1	f2	f3	pitch	intensity"

# Segments count
segments_count = 10
# Counter for segments
last_frame_position = 1

# Initial header values----------------------------------------------

print Appending initial header values...'newline$'

fileappend "'outtime$'" 'headings_time$''newline$'
# fileappend "'outformant$'" 'headings_ftable$''newline$'
fileappend "'outfpi$'" 'headings_fpi$''newline$'
fileappend "'outformantseg$'" 'headings_segtable$''newline$'

# Calculate number of TextGrids -------------------------------------

print Getting TextGrids...'newline$'
 
Create Strings as file list... fileList 'path$'*.TextGrid
select Strings fileList
file_count = Get number of strings

print The number of files for processing is 'file_count'.'newline$'

# Loop for all files -----------------------------------------------

print Entering the file loop...'newline$'

for f from 1 to file_count
	select Strings fileList
	file_name$ = Get string... f
	print 'newline$'Loading files at step 'f' of 'file_count':'newline$'
	print The path is 'path$''newline$'
	# Set counter to beginning
	last_frame_position = 1
	# Get speaker ID (truncate ".TextGrid")
	speaker$ = left$ (file_name$, 13)
	print WAV and TextGrid file is 'speaker$'.'newline$'
	print Loading files...'newline$'
	# Load TextGrid
	Read from file... 'path$''file_name$'
	# Now read WAV as well
	Read from file... 'path$''speaker$'.wav
	call MakeFormants 'speaker$'
	call GetTimes 'speaker$' segments_count
	call MakePitchIntensity 'speaker$'
	call GetFormantsPitchIntensity 'speaker$'
	# Remove what is not needed
	select TextGrid 'speaker$'
	plus Sound 'speaker$'
	plus Formant 'speaker$'
	plus Table 'speaker$'
	plus Pitch 'speaker$'
	plus Intensity 'speaker$'
	Remove
endfor


procedure GetFormantsPitchIntensity name$
	#
	# Get formants, pitch and intensity for all
	# points in files. Save the results.
	#
	print Running procedure GetFormantsPitchIntensity...'newline$'
	select TextGrid 'name$'
	points_number = Get number of points... 2
	# Check if the number is correct
	if points_number <> 64
		exit "Number of points is not 64. Aborting"
	endif
	# Go through all points
	for i from 1 to points_number
		select TextGrid 'name$'
		p_label$ = Get label of point... 2 i
		# Get time of the point.
		p_time = Get time of point... 2 i
		# Get the intreval that corresponds to the poit time.
		p_inteval = Get interval at time... 3 p_time
		# Finally, get the word label for corresponding interval.
		p_word$ = Get label of interval... 3 p_inteval
		# To save: p_label, p_word
		# Get formant value
		select Formant 'name$'
		f1 = Get value at time... 1 p_time Hertz Linear
		f2 = Get value at time... 2 p_time Hertz Linear
		f3 = Get value at time... 3 p_time Hertz Linear
		select Pitch 'name$'
		pitch = Get value at time... p_time Hertz Linear
		select Intensity 'name$'
		intensity = Get value at time... p_time Cubic
		tofile$ = "'name$'	'p_label$'	'p_word$'	'f1'	'f2'	'f3'	'pitch'	'intensity''newline$'"
		fileappend "'outfpi$'" 'tofile$'
	endfor
	
endproc


procedure GetTimes name$ segments_number
	#
	# Get and save times for all words/diphthongs.
	# Calculate segments and save results.
	#
	print Running procedure GetTimes...'newline$'
	select TextGrid 'name$'
	intervals_diph = Get number of intervals... 1
	intervals_word = Get number of intervals... 3
	# Check if the number of tiles is the same.
	if intervals_diph <> intervals_word
		exit "Intervals don't have same count."
	else
		print Same count in the file 'name$'. Continuing...'newline$'
	endif
	
	print 'tab$'Calculating times and saving results...'newline$'
	# change to intervals_diph
	for i from 1 to intervals_diph
		select TextGrid 'name$'
		text_d$ = Get label of interval... 1 i
		text_w$ = Get label of interval... 3 i
		if text_d$ <> ""
			# Durations for diphthongs.
			start_d = Get start point... 1 i
			end_d = Get end point... 1 i
			duration_d = end_d - start_d
			
			# print Diphthong 'text_d$' in 'text_w$': starts at 'start_d', end at 'end_d'.'newline$'
			
			# Calculate n segments of the time
			duration_segments = duration_d / segments_number
			# Get formants in each segment and save to file
			
			# Initial values of segment time.
			seg_start = start_d
			
			# Get formant from segments
			for seg from 1 to segments_number
				# Make sure that loop reaches to the
				# end of formants.
				if seg = segments_number
					seg_end = end_d
				else
					seg_end = seg_start + duration_segments
				endif
			
				call GetAllFormants seg_start seg_end 'name$' 'text_d$' 'text_w$' seg
				
				# Move to the next segment.
				seg_start = seg_end
			endfor

			# Select our text grid again, since it was
			# deselected in the previous procedure.
			select TextGrid 'name$'
			
			# Durartions for words.
			start_w = Get starting point... 3 i
			end_w = Get end point... 3 i
			duration_w = end_w - start_w

			tofile$ = "'name$'	'text_w$'	'duration_w'	'text_d$'	'duration_d''newline$'"
			fileappend "'outtime$'" 'tofile$'
			
		endif
	endfor
	print 'tab$'Done.'newline$'
endproc


procedure GetAllFormants pr_seg_start pr_seg_end name$ pr_text_d$ pr_text_w$ segment_id
	#
	# Gets all formant values between times pr_seg_start and pr_seg_end.
	# It uses the table created in MakeFormants.
	#
	select Table 'name$'
	# allframes = Get number of rows
	# Go through the frames.
	frame = last_frame_position
	repeat
		time = Get value... frame time(s)
		if time >= pr_seg_start and time <= pr_seg_end 
			# intens = Get value... frame intensity
			# nformants = Get value... frame nformants
			f1 = Get value... frame F1(Hz)
			# b1 = Get value... frame B1(Hz)
			f2 = Get value... frame F2(Hz)
			# b2 = Get value... frame B2(Hz)
			f3 = Get value... frame F3(Hz)
			# b3 = Get value... frame B3(Hz)
			headings_segtable$ = "'name$''tab$''segment_id''tab$''pr_text_d$''tab$''pr_text_w$''tab$''f1''tab$''f2''tab$''f3'"
			fileappend "'outformantseg$'" 'headings_segtable$''newline$'
		endif
		frame = frame + 1
	until time > pr_seg_end
	last_frame_position = frame - 10
	
endproc

procedure MakePitchIntensity name$
	#
	# Make the pitch and intensity objects
	# for the given file.
	#
	print Running procedure MakePitchIntensity... 'newline$'
	select Sound 'name$'
	To Pitch... 0.0 75.0 600.0
	select Sound 'name$'
	To Intensity... 100 0.0
	print 'tab$'Done.'newline$'
endproc

procedure MakeFormants name$
	#
	# Make formant table.
	#
	print Running procedure MakeFormants... 'newline$'
	select Sound 'name$'
	print 'tab$'Making formant file for the spreaker...'newline$'
	# Each signal file requires different
	# settings for the formants. The values are
	# obtained by examening the spectrograms and
	# waveforms.
	if name$ = "16-speaker-hh"
		To Formant (burg)... 0 3 3400 0.025 50
	elif name$ = "15-speaker-sr"
		To Formant (burg)... 0 3 3600 0.025 50
	elif name$ = "14-speaker-ao"
		To Formant (burg)... 0 3 3500 0.025 50
	elif name$ = "13-speaker-jr"
		To Formant (burg)... 0 3 3900 0.025 50
	elif name$ = "12-speaker-vv"
		To Formant (burg)... 0 3 3600 0.025 50
	elif name$ = "11-speaker-dz"
		To Formant (burg)... 0 3 3600 0.025 50
	elif name$ = "10-speaker-st"
		To Formant (burg)... 0 3 3500 0.025 50
	elif name$ = "09-speaker-tz"
		To Formant (burg)... 0 3 3770 0.025 50
	elif name$ = "08-speaker-ni"
		To Formant (burg)... 0 3 3650 0.025 50
	elif name$ = "07-speaker-lc"
		To Formant (burg)... 0 3 3600 0.025 50
	elif name$ = "06-speaker-ip"
		To Formant (burg)... 0 3 3700 0.025 50
	elif name$ = "05-speaker-gl"
		To Formant (burg)... 0 3 3500 0.025 50
	elif name$ = "04-speaker-ym"
		To Formant (burg)... 0 3 3800 0.025 50
	elif name$ = "03-speaker-im"
		To Formant (burg)... 0 3 3800 0.025 50
	elif name$ = "02-speaker-jj"
		To Formant (burg)... 0 3 3400 0.025 50
	elif name$ = "01-speaker-jk"
		To Formant (burg)... 0 3 3600 0.025 50
	else
		# No speaker ID is found. Abort.
		exit "Error: Invalid speaker ID in the formant calculations."
	endif
	select Formant 'name$'
	print 'tab$'Making table...'newline$'
	Down to Table... 0 1 6 1 3 1 3 1
	print 'tab$'Done.'newline$'
endproc