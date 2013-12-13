"""
testquery.py
-----------

Test a series of queries using a point-based metric.

"""

from main import *
from readDB import getField

numresults = 3

def top3(query):
    return reversed([ entry for (entry, score) in processInput(query)[-3:] ])

multipliers = [1.0, 0.6, 0.4]
    
def searchCode(code):
    entries = top3(code)
    points = 0
    for i in range(numresults):
        points +=  getField(entries[i], 'code') == code

    
