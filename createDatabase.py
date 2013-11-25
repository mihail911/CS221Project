#!/usr/bin/python
"""Provides a series of methods for creating a usable database from a text file
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
	curs.execute('create table COURSEINFO (title,code,instructor,units,description)')
	conn.commit()
	conn.close()

def allClassInfo():
	conn=sqlite3.connect(allcoursesdatabase)
	conn.text_factory=str
	curs=conn.cursor()
	with open('courseinfo2.txt') as f:
		while True:

			coursetitle=' '.join(f.readline()[14:].split())
			if not coursetitle: break
			courseinstructors=' '.join(f.readline()[20:].split())
			coursecode=' '.join(f.readline()[13:].split())	
			courseunits=' '.join(f.readline()[13:].split())
			coursedescription=' '.join(f.readline()[20:].split())
			delimiter=f.readline()
			allelems=(coursetitle,coursecode,courseinstructors, courseunits, coursedescription)
			curs.execute("insert into COURSEINFO values (?,?,?,?,?)", allelems)	
	conn.commit()
	conn.close()

#allClassInfo()