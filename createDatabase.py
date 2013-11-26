#!/usr/bin/python
"""Provides a series of methods for creating usable databases from a text file
of all available courses."""

import sqlite3

allcoursesdatabase="courseinfodata.db"

def oneClassInfo(f):
	"""Reads in the contents of one file in Python and outputs information as a dictionary."""
	coursedict={}
	courseinfo={}
	coursetitle=' '.join(f.readline()[14:].split())
	courseinstructors=' '.join(f.readline()[20:].split())
	coursecode=' '.join(f.readline()[13:].split())	
	courseunits=' '.join(f.readline()[13:].split())	
	coursedescription=' '.join(f.readline()[20:].split())
	delimiter=f.readline()

	courseinfo['Course Title']=coursetitle
	courseinfo['Course Instructors']=courseinstructors
	courseinfo['Course Units']=courseunits
	courseinfo['Course Code']=coursecode
	courseinfo['Course Description']=coursedescription	
	coursedict[coursetitle]=courseinfo
	return coursedict

def setupDatabase():
	conn=sqlite3.connect(allcoursesdatabase)
	curs=conn.cursor()
        curs.execute('DROP TABLE courseinfo')
	curs.execute('CREATE TABLE courseinfo (id INTEGER PRIMARY KEY NOT NULL,title,code,instructor,units,description)')
	conn.commit()
	conn.close()

def allClassInfo():
	"""Populates COURSEINFO database with all course info."""
	conn=sqlite3.connect(allcoursesdatabase)
	conn.text_factory=str
	curs=conn.cursor()
	with open('courseinfo2.txt') as f:
                id = 0
		while True:
			coursetitle=' '.join(f.readline()[14:].split())
			if not coursetitle: break
                        id += 1
			courseinstructors=' '.join(f.readline()[20:].split())
			coursecode=' '.join(f.readline()[13:].split())	
			courseunits=' '.join(f.readline()[13:].split())
                        print courseunits
			coursedescription=' '.join(f.readline()[20:].split())
			delimiter=f.readline()
			allelems=(id,coursetitle,coursecode,courseinstructors, courseunits, coursedescription)
			curs.execute("INSERT INTO courseinfo VALUES (?,?,?,?,?)", allelems)	
	conn.commit()
	conn.close()

#allClassInfo()

instructordb='instructordatabase.db'
def instructorDBInstance():
	"""Creates instructor database instance.""" 
	conn=sqlite3.connect(instructordb)
	cursor=conn.cursor()
	cursor.execute('CREATE TABLE instructordb (instructor,coursecode)')
	conn.commit()
	conn.close()

#instructorDBInstance()

def makeInstructorDatabase():
	"""Populates INSTRUCTORDB with instructor information where 
	each row is of the form (instructor, coursecode)"""
	conn=sqlite3.connect(instructordb)
	conn.text_factory=str
	curs=conn.cursor()
	with open('courseinfo2.txt') as f:
		while True:
			coursetitle=' '.join(f.readline()[14:].split())
			if not coursetitle: break
			courseinstructors=' '.join(f.readline()[20:].split())
			allinstructors=[inst.strip() for inst in courseinstructors.split(',') if inst.strip()!='']
			coursecode=' '.join(f.readline()[13:].split())	
			courseunits=' '.join(f.readline()[13:].split())
			coursedescription=' '.join(f.readline()[20:].split())
			delimiter=f.readline()
			for inst in allinstructors:
				instelem=(inst,coursecode)
				curs.execute("INSERT INTO instructordb VALUES (?,?)", instelem)	
	conn.commit()
	conn.close()

# makeInstructorDatabase()

setupDatabase()
allClassInfo()
