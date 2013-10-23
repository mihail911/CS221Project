"""

test.py
-------

Provide tests for functionality.

"""

from search import *

courses = {
    "CS107": { "instructors": ["Jerry Cain"], "prerequisites": [ "CS106B" ] }, 
    "MATH108": { "instructors": ["K Sound"], "prerequisites": [ "MATH51" ] }, 
    "CS144": { "instructors": ["Phil", "Nick"], "prerequisites": [ "CS107", "CS110" ] },
    "CS140": { "instructors": ["David Mazieres"], "prerequisites": [ "CS110"] }, 
}

searcher = PropertySearch(courses)
print searcher.search("prerequisites CS110")
