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
		#title, course code, instructors, dept codes
		print courseinfo
		for title in courseinfo[titles]: #iterate over all possible titles
			results=readDB.queryDB('SELECT * FROM courseinfo WHERE title=?', (title.lower(),))
			coursesfound += results
			print 'title', results
		for ccode in courseinfo[coursecodes]:
			results=readDB.queryDB('SELECT * FROM courseinfo WHERE code=?', (ccode.lower(), ))
			coursesfound += results
			print 'code ', results	
		# for inst in courseinfo[instructors]:
		# 	# for instructor in allinstructors:
		# 	# 	print instructor
		# 	# 	if inst in instructor:
		# 	# 		results=readDB.queryDB('SELECT * FROM courseinfo WHERE instructor=? ',(inst.lower(),))
		# 	# 		print results
		# 	results=readDB.queryDB('SELECT * FROM courseinfo WHERE code LIKE %?%', (inst,))
		# 	coursesfound += results
		# 	print 'instructor', results
		for deptcode in courseinfo[deptcodes]:
			print deptcode
			results=readDB.queryDB("SELECT * FROM courseinfo WHERE code LIKE 'CS%'")
			coursesfound += results
			print 'deptcode', results	
		# related = [ coursegraph.getRelatedCourses(entry)[] for entry in coursesfound ]
		# print related

		readDB.cleanup()
		break



