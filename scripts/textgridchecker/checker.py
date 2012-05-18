#!/usr/bin/python
"""

Checks TextGrid for integrity.

Uses TextGrid parser from NLTK:

http://nltk.googlecode.com/svn/trunk/nltk_contrib/nltk_contrib/textgrid.py

This script is really suitable for the project I was working on. However,
the code can be used as an example.

More on http://www.languagebits.com/?p=749

"""

__author__ = 'Romeo Mlinar'
__mail__ = 'mlinar [a] languagebits.com'
__license__ = 'GNU General Public License version 3'

import os
import sys

from textgrid import TextGrid

PATH = r'/media/data/corpus/speaker-reference'

DIR =  os.path.join(os.path.dirname(__file__))

def get_grid(f):
    """Read the file and return the content."""
    c = file(os.path.join(PATH, f), 'r').readlines()
    return ''.join(c)

def get_paths(path):
    """Get all textgrid files form the path"""
    tgrid = []
    fs = os.listdir(path)
    for i in fs:
        if os.path.splitext(i)[1] == '.TextGrid':
            tgrid.append(i)
    tgrid.sort()
    return tgrid

def get_nonempty(tier):
    """Return the number of no-empty objects"""
    i = 0
    content = []
    for item in tier.simple_transcript:
        # Check if empty
        if item[2] != '':
            content.append(item)
    return content

def check_intervals_match(diph, intervals):
    """Checkk if all intervals match the diphthongs."""
    k = -1
    j = 0
    for ds in diph:
        j = j + 1
        d = ds[2]
        p1, p2 = k+1, k+2
        int1 = intervals[p1][1]
        int2 = intervals[p2][1]
        # Labels
        if (int1[:4] != d) or (int2[:4] != d):
            print 'Wrong interval below diphthong %s, position %s.' % (d, j)
            print 'The value %s is invalid. It should be %s.' \
                  % (int1[:4] if int1[:4] != d else int2[:4], d)
            sys.exit(0)
        # All have _ after the diphthong name
        if not (int1[4] == int2[4] == '_'):
            print "No unerscore for the interval below %s, position %s." \
                  % (d, j)
            sys.exit(0)
        # All have 1 and two at the end
        if not (int1[5] == '1'):
            print "No nubmer 1 below %s, position %s." % (d, j)
            sys.exit(0)
        if not (int2[5] == '2'):
            print "No nubmer 2 below %s, position %s." % (d, j)
            sys.exit(0)
        k = k + 2

def get_checklist():
    """Get list used for checks"""
    check = {}
    f = file(os.path.join(DIR ,'diph.txt'), 'r')
    for line in f:
        line = line.strip()
        c = line.split('\t')
        check[c[1]] = c[0]
    return check

def get_text(tier):
    """Get text value from the tier"""
    t = []
    for i in tier:
        t.append(i[2])
    return t

def all_there(tier, t):
    """Check if all tier ids are in the tier"""
    if t == 'diph':
        ch = checklist.values()
    elif t == 'word':
        ch = checklist.keys()
    else:
        print 'Wrong tier name.'
        sys.exit()
    # Check now
    items = []
    i = 0
    for item in tier:
        i = i + 1
        if item[2] not in ch:
            print 'String "%s" is not allowed but found in tier "%s", at position %s.' \
                  % (item[2], t, i)
            sys.exit()
        items.append(item)

def all_matches(diph, word):
    """Check if diphthongs and words match"""
    i = 0
    for i in range(32):
        if checklist[word[i][2]] != diph[i][2]:
            print 'Mismatch: "%s" not allowed in "%s", at position %s.' \
                  % (diph[i][2], word[i][2], i+1)
            print 'It should say "%s".' % checklist[word[i][2]]
            sys.exit()

def unique(tier):
    """Must return 16"""
    tc = get_text(tier)
    u = tuple(set(tc))
    if len(u) != 16:
        print u
        print "The diphthongs are not paired."
        sys.exit()

def checkwords(tier):
    """Check if all words are present."""
    w = get_text(tier)
    for word in checklist.keys():
        try:
            w.remove(word)
        except ValueError:
            print 'Word "%s" is missing from this tier!' % word
            sys.exit()
    assert len(w) == 0


def check_integrity(f):
    """Check for the integrity"""
    gridobj = TextGrid(get_grid(f))
    # Check if tiers are OK
    print "\tChecking proper tier names..."
    assert gridobj.tiers[0].nameid == 'diph'
    assert gridobj.tiers[1].nameid == 'point'
    assert gridobj.tiers[2].nameid == 'word'
    # Check number of words and diphthings
    print "\tChecking if the tiers contain 32 nonempty items..."
    diph, word = get_nonempty(gridobj.tiers[0]), get_nonempty(gridobj.tiers[2])
    assert len(word) == 32
    assert len(diph) == 32
    print "\tChecking that the number of empty and nonempty tiers is the same..."
    assert len(gridobj.tiers[0].simple_transcript) == len(gridobj.tiers[2].simple_transcript)
    print "\tChecking if all tiers have valid text..."
    all_there(diph, 'diph')
    all_there(word, 'word')
    print "\tChecking if the diphthongs have pairs..."
    unique(diph)
    print "\tChecking if all words are present..."
    checkwords(word)
    print "\tChecking if the words and diphthongs match..."
    all_matches(diph, word)
    print "\tChecking if the number of intervals is 64..."
    intervals = gridobj.tiers[1].simple_transcript
    assert len(intervals) == 64
    print "\tChecking if intervals match diphthongs..."
    check_intervals_match(diph, intervals)
    
# Dictionary used for the testing
checklist = get_checklist()

def run():
    """Check all files in dir"""
    print 'STARTING TEXTGRID CHECKS'
    print 'The path is %s.' % PATH
    # Count the number of files
    paths = get_paths(PATH)
    print "The number of text grids is %s." % len(paths)
    print "Starting the loop..."
    for f in paths:
        print 'Checking file ', f
        check_integrity(f)
        print '\t', 'OK', f
        
           
run()
