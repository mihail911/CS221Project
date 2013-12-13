"""
testquery.py
-----------

Test a series of queries using a point-based metric.

"""

from main import *
from readDB import getField

numresults = 3

def top3(query):
    res = [ entry for (entry, score) in processInput(query)[-3:] ]
    res.reverse()
    return res

multipliers = [1.0, 0.6, 0.4]
    
def searchCode(code):
    entries = top3(code)
    points = 0
    for i in range(numresults):
        points += 50 * (getField(entries[i], 'code') == code)
        
queries = """amelang110a
introduction to computing principles 
amelang 144b
the mathematics of the rubik's cube
math120
third-year persian, second quarter
identity and popular music (femgen 140g, music , csre140g )
math 53
courses taught by julie zelenski 
courses taught by mehran sahami 
leon simon 
amelang128b
what is hemispheric
first-year hausa
first-year hebrew, first quarter (jewishst comparative fictions of ethnicity (amstud 51q, complit , csre51q )
amelang107b and csre14n
growing up bilingual (chilatst 14n, educ math52h and cs105 )
digital dilemmas
amelang129a and cs109""".split("\n")

for query in queries:
    print "%s: %s\n" % (query, [ (getField(entry, 'code'), getField(entry, 'title'), getField(entry, 'instructor')) for entry in top3(query) ])


"""

Scores

amelang110a: 100
introduction to computing principles: 0
amelang 144b: 60
the mathematics of the rubik's cube: 0
math120: 100
third-year persian, second quarter: 0
identity and popular music (femgen 140g, music , csre140g ): 100
math53: 100
courses taught by julie zelenski: 100
courses taught by mehran sahami: 200
leon simon: 100
amelang128b: 100
what is hemispheric: 0
first-year hausa: 0
first-year hebrew, first quarter (jewishst comparative fictions of ethnicity (amstud 51q, complit , csre51q ): 30
amelang107b and csre14n: 100
growing up bilingual (chilatst 14n, educ math52h and cs105 ): 0
digital dilemmas: 100
amelang129a and cs109: 100


"""
