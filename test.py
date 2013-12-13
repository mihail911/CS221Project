"""

test.py
-------

Provide tests for functionality.

"""

from readDB import getField
import search
import coursegraph

def testSqlQuery():
    searcher = search.PropertySearch()
    print searcher.search("code CS106A")

def testCourseRelatednessReal():
    data = coursegraph.getData()
    course = [ entry for entry in data if getField(entry, 'code') == "math108" ][0]
    print [ getField(pair[0], 'code') for pair in coursegraph.getRelatedCourses(course) ]

def testCourseRelatedness():
    alldata = [
        ('programming methodology', '', '', '', ''), 
        ('programming abstractions', '', '', '', ''), 
        ('programming ruby', '', '', '', ''), 
        ('sex and gender', '', '', '', ''), 
        ('methodology of gender studies', '', '', '', ''), 
    ]
    coursegraph.featurepriors = coursegraph.createFeaturePriors(alldata)
    for entry1 in alldata:
        for entry2 in alldata:
            feats1 = coursegraph.extractFeatures(entry1)
            feats2 = coursegraph.extractFeatures(entry2)
            print "%s, %s: %s" % (entry1[0], entry2[0],
                                  coursegraph.weight(feats1, feats2))

testCourseRelatednessReal()
# testCourseRelatedness()

# [u'math109', u'math118', u'cs142', u'math16', u'cs103', u'cs105', u'cs106b', u'math104', u'cs109', u'math108']
