#!/usr/bin/python
"""Provides a series of methods for creating usable databases from a text file
of all available courses."""

import sqlite3, re,pdb

allcoursesdatabase="courseinfodata.db"
instructordb='instructordb_saved.db'
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
	"""
	Create all course info database.
	"""
	conn=sqlite3.connect(allcoursesdatabase)
	curs=conn.cursor()
	curs.execute('DROP TABLE courseinfo')
	curs.execute('CREATE TABLE courseinfo (id INTEGER PRIMARY KEY NOT NULL,title NOT NULL,code NOT NULL,instructor NOT NULL,unitsmin INTEGER NOT NULL,unitsmax INTEGER NOT NULL,description,prereqs)')
	conn.commit()
	conn.close()

def setupDeptCodeTable():
	"""
	Add department code table
	to all course info database.
	"""
	conn=sqlite3.connect(allcoursesdatabase)
	curs=conn.cursor()
	curs.execute('DROP TABLE deptcodes')
	curs.execute('CREATE TABLE deptcodes (departmentcode)')
	conn.commit()
	conn.close()

def populateDeptCodeTable():
	"""
	Update dept code table 
	with list of all dept codes.
	"""
	conn=sqlite3.connect(allcoursesdatabase)
	curs=conn.cursor()
	with open('departmentcodes.txt') as f:
		while True:
			code=f.readline().strip()
			if not code: break
			curs.execute('INSERT INTO deptcodes VALUES (?)', [code])	
	conn.commit()
	conn.close()

def getSetOfDeptCodes():
	"""
	Get set of all dept codes.
	"""
	allcodes=set()
	conn=sqlite3.connect(allcoursesdatabase)
	curs=conn.cursor()
	curs.execute('select * from deptcodes')
	for code in curs.fetchall():
		allcodes.add(code[0]) 
	return allcodes

def getCourseCodes():
	"""
	Return a set of all course codes.
	"""
	coursecodes=set()
	conn=sqlite3.connect(allcoursesdatabase)
	curs=conn.cursor()
	curs.execute('select * from courseinfo')
	for coursetuple in curs.fetchall():
		coursecodes.add(coursetuple[2])
	conn.close()
	return coursecodes

def getInstructors():
	"""
	Return a set of all course instructors.
	"""
	instructors=set()
	conn=sqlite3.connect(instructordb)
	curs=conn.cursor()
	curs.execute('select * from instructordb')
	for instructor in curs.fetchall():
		instructors.add(instructor[0])
	conn.close()
	return instructors

<<<<<<< HEAD
def extractPrereqsRaw(coursedescription, deptcodes):
=======
def getAllCourseTitles():
	"""
	Return a set of all course title.
	"""
	titles=set()
	conn=sqlite3.connect(allcoursesdatabase)
	curs=conn.cursor()
	curs.execute('select * from courseinfo')
	for coursetuple in curs.fetchall():
		titles.add(coursetuple[1])
	conn.close()
	return titles

def extractPrereqs(coursedescription, deptcodes):
>>>>>>> a41a2fc343c3a50503efeff0534279278d72dc83
	"""
	Extract course prereqs from course description.

        return: Raw string containing the prerequisites.
	"""
	#pdb.set_trace()
	prereqindex=coursedescription.find('Prerequisite')
	if prereqindex>=0:
		courseprereqs=coursedescription[(prereqindex+13):]
		modifiedprereqs=re.sub('[\:,/?.()]','', courseprereqs.strip())
		newprereqs=''
		prereqslist=modifiedprereqs.split()
		tokenindex=0
		while tokenindex<len(prereqslist):
			if prereqslist[tokenindex] in deptcodes:
				if (tokenindex+1)<len(prereqslist):
					newprereqs+=(prereqslist[tokenindex]+prereqslist[tokenindex+1]+" ")
					tokenindex+=2
			else:
				newprereqs+=(prereqslist[tokenindex]+" ")
				tokenindex+=1
		return newprereqs.strip()
	return ''

<<<<<<< HEAD
def extractPrereqs(description, deptcodes):
        """
        Extract course prereqs from course description.

        return: List of prerequisite course names.
        """
        raw = extractPrereqsRaw(description, deptcodes)
        words = raw.split()
        for word in words:
                match = re.search("\d", word)
                if match and word[:match.start()] in deptcodes:
                        # add prereq to entry
        
def prereqsToDB():
        deptcodes = getSetOfDeptCodes()
        
        
# deptcodes=getSetOfDeptCodes()
# x='Prerequisite: Placement Test, AMELANG 128C.'
# print extractPrereqs(x,deptcodes)
=======
>>>>>>> a41a2fc343c3a50503efeff0534279278d72dc83
def allClassInfo():
	"""
	Populates 'courseinfo' database with all course info.
	"""
	conn=sqlite3.connect(allcoursesdatabase)
	conn.text_factory=str
	curs=conn.cursor()
	deptcodes=getSetOfDeptCodes()
	with open('courseinfo2.txt') as f:
                id = 0
		while True:
			coursetitle=' '.join(f.readline()[14:].split())
			if not coursetitle: break
                        id += 1
			courseinstructors=' '.join(f.readline()[20:].split())
			coursecode=' '.join(f.readline()[13:].split())
			courseunits=f.readline()[13:].split("-")
			coursedescription=' '.join(f.readline()[20:].split())
			#courseprereqs=extractPrereqs(coursedescription,deptcodes)
			delimiter=f.readline()
			allelems=(id,coursetitle,coursecode,courseinstructors, courseunits[0],courseunits[1],coursedescription,'')
			curs.execute("INSERT INTO courseinfo VALUES (?,?,?,?,?,?,?,?)", allelems)	
	conn.commit()
	conn.close()


def instructorDBInstance():
	"""Creates instructor database instance.""" 
	conn=sqlite3.connect(instructordb)
	cursor=conn.cursor()
	cursor.execute('CREATE TABLE instructordb (instructor,coursecode)')
	conn.commit()
	conn.close()

def getAllInstructors():
	"""
	Return set of all instructor names.
	"""
	allinstructors=set()
	conn=sqlite3.connect(instructordb)
	cursor=conn.cursor()
	cursor.execute('select * from instructordb')
	for insttuple in cursor.fetchall():
		allinstructors.add(insttuple[0])
	conn.close()
	return allinstructors
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

setupDatabase()
allClassInfo()
# # setupDeptCodeTable()
# # populateDeptCodeTable()	
