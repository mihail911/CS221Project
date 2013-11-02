"""Provides a series of methods for creating a usable database from a text file
of all available courses."""

f=open('courseinfo.txt','r')

def oneClassInfo(file):
	"""Reads in the contents of one file in Python and outputs information as a dictionary."""
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
	return coursedict

f.close()