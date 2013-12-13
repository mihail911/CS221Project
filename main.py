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

if __name__=='__main__':
	"""
	Run the program.
	"""
	while True:
		courseinfo=queryparser.readQuery()
		coursesfound = []
		#print courseinfo
		allcourseids=[]
		#title, course code, instructors, dept codes
		for title in courseinfo[titles]: #iterate over all possible titles
			results=readDB.queryDB('SELECT * FROM courseinfo WHERE title=?', (title.lower(),))
			# coursesfound += results
			# print 'title', results
			for course in results:
				allcourseids.append(course[0])
		for ccode in courseinfo[coursecodes]:
			results=readDB.queryDB('SELECT * FROM courseinfo WHERE code=?', (ccode.lower(), ))
			# coursesfound += results
			# print 'code ', results
			for course in results:
				allcourseids.append(course[0])	
		for inst in courseinfo[instructors]:#iterate over all found instructors
			allcourses=readDB.queryDB('SELECT * from courseinfo') #iterate over all possible instructors
			for acourse in allcourses:
				if inst.lower() in acourse[3]:
					#print 'instructor courses', acourse
					allcourseids.append(acourse[0])
		for deptcode in courseinfo[deptcodes]:
			allcourses=readDB.queryDB('SELECT * from courseinfo')
			for acourse in allcourses:
				if deptcode.lower() in acourse[2]:
					#print 'deptcode', acourse
					allcourseids.append(acourse[0])	
			#results=readDB.queryDB("SELECT * FROM courseinfo WHERE code LIKE 'CS%'")
			#coursesfound += results
			#print 'deptcode', results	
		related = [ coursegraph.getRelatedCourses(entry)[5] for entry in allcourseids ]
		# print related
		#print 'ids',allcourseids
		readDB.cleanup()
		break



