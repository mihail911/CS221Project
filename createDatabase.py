#!/usr/bin/python
"""Provides a series of methods for creating a usable database from a text file
of all available courses."""

import sqlite3

f=open('courseinfo2.txt','r')

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

dictexample=oneClassInfo(f)
print dictexample
##not working yet!!
def allClassInfo():
	allcourses=[]
	with open('courseinfo.txt') as f:
		while True:
			coursedict={}
			courseinfo={}
			coursetitle=' '.join(f.readline()[14:].split())
			courseinstructors=' '.join(f.readline()[20:].split())
			courseunits=' '.join(f.readline()[13:].split())
			coursedescription=' '.join(f.readline()[20:].split())
			delimiter=f.readline()

			courseinfo['Course Title']=coursetitle
			courseinfo['Course Instructors']=courseinstructors
			courseinfo['Course Units']=courseunits
			courseinfo['Course Description']=coursedescription	
			coursedict[coursetitle]=courseinfo
			allcourses.append(coursedict)	
	return allcourses

def setupDatabase(allcourseinfo):
	conn=sqlite3.connect('courseinfo_db.db')
	curs=conn.cursor()
	curs.execute('create table ALLCOURSES (title,code,instructor,units,description)')

	conn.commit()
	conn.close()
#allcourses=allClassInfo()
#print allcourses