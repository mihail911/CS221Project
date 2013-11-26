"""

test.py
-------

Provide tests for functionality.

"""

import search
import coursegraph

def testSqlQuery():
    searcher = search.PropertySearch()
    print searcher.search("code CS106A")

def testCourseRelatednessReal():
    data = coursegraph.getData()
    entry = [ entry for entry in data if entry[1] == "CS106A" ][0]
    print coursegraph.getRelatedCourses(data, entry, 8)

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
