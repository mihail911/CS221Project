#!/usr/bin/python
"""
main.py
--------------

Module we can use to run the entire program.

****
TODO: Combine 'queryparser' and 'coursegraph' to unify the code.
****
"""
import queryparser
import createDatabase
import readDB
import coursegraph

coursecodes="Course Codes"
titles="Titles"
deptcodes="Dept Codes"
instructors="Instructors"

allinstructors=createDatabase.getInstructors()

def processInput(query):
    courseinfo=queryparser.readQuery(query)
    coursesfound = []
    #print 'course info',courseinfo
    #title, course code, instructors, dept codes
    for title in courseinfo[titles]: #iterate over all possible titles
        results=readDB.queryDB('SELECT * FROM courseinfo WHERE title=?', (title.lower(),))
        coursesfound += results
        # print 'title', results
        
    for ccode in courseinfo[coursecodes]:
        results=readDB.queryDB('SELECT * FROM courseinfo WHERE code=?', (ccode.lower(), ))
        coursesfound += results
        # print 'code ', results    
    for inst in courseinfo[instructors]:#iterate over all found instructors
        allcourses=readDB.queryDB('SELECT * from courseinfo') #iterate
        #over all possible instructors
        for acourse in allcourses:
            if inst.lower() in acourse[3]:
                #print 'instructor courses', acourse
                coursesfound.append(acourse)
    for deptcode in courseinfo[deptcodes]:
        allcourses=readDB.queryDB('SELECT * from courseinfo')
        for acourse in allcourses:
            if deptcode.lower() in acourse[2]:
                #print 'deptcode', acourse
                coursesfound.append(acourse)    
    #print 'courses found',coursesfound, '\n\n\n'
    mostrelatedcourses=[]
    for entry in coursesfound: #take most related courses for each course
            mostrelatedcourses+=coursegraph.getRelatedCourses(entry)
    return sorted(mostrelatedcourses,key=lambda pair: pair[1])

# if __name__=='__main__':
#     """
#     Run the program.
#     """
#     processInput()



