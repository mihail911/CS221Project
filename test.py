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
    # cs106A = [ entry for entry in data if entry[2] == "CS106A" ][0]
    math51 = [ entry for entry in data if getField(entry, 'code') == "MATH51" ][0]
    # cs221 = [ entry for entry in data if entry[2] == "CS221" ][0]
    print coursegraph.getRelatedCourses(data, math51, 8)

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
