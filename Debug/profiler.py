"""

profiler.py
-----------

Author: Michael Dickens, Mihail Eric
Created: 2013-11-26

"""

import cProfile
import readDB
import coursegraph

def profFeatureExtractor():
    math51 = [ entry for entry in coursegraph.alldata if readDB.getField(entry, 'code') == "MATH51" ][0]
    cProfile.run('coursegraph.getRelatedCourses(coursegraph.alldata, math51, 10)')

profFeatureExtractor()
