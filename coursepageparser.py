import urllib2
from bs4 import BeautifulSoup

mainurl="http://explorecourses.stanford.edu/"
subsite="http://explorecourses.stanford.edu/search?view=catalog&academicYear=&page=0&q=MKTG&filter-departmentcode-MKTG=on&filter-coursestatus-Active=on&filter-term-Autumn=on"
header={'User-Agent':'Mozilla/5.0'}

instructorlength=11
# req = urllib2.Request(subsite,headers=header)
# page = urllib2.urlopen(req)
# soup = BeautifulSoup(page)
#remember to add 'mainurl' as prefix before each url extracted from main page

def getSoupForPage(url):
	"""Returns soup for a given URL."""
	req = urllib2.Request(url,headers=header)
	page = urllib2.urlopen(req)
	soup = BeautifulSoup(page)
	return soup


def getDepartmentSites(url):
	"""Gets list of all department sites, returned as a 'name of department': url 
	dictionary."""
	soup=getSoupForPage(url)
	alldepartments={}
	listitems=soup.findAll('li')
	for item in listitems:
		hreftag=item.find('a')['href']
		name=' '.join(item.text.split())
		alldepartments[name]=hreftag	

	return alldepartments
	

def parseSinglePage(url):
	"""Parse a single page given the url--return dictionary of class info for each page."""
	soup=getSoupForPage(url)
	allcourses={}
	divs=soup.findAll('div', attrs={'class':'courseInfo'})
	for d in divs: #get info for each class
		courseinfo={}
		coursenumber=d.find('span',attrs={'class':'courseNumber'})
		coursetitle=d.find('span',attrs={'class':'courseTitle'})
		courseattr=d.findAll('div',attrs={'class':'courseAttributes'})
		#print courseattr
		coursedescrip=d.find('div',attrs={'class':'courseDescription'})

		courseinfo['Course Number']=coursenumber.text[:-1]
		courseinfo['Course Title']=coursetitle.text
		courseinfo['Course Description']=coursedescrip.text
		courseAttributes=[]
		#courseinfo['Course Instructor']
		for attr in courseattr:
			attributes=' '.join(attr.text.split())
			if attributes[:11]=='Instructors':
			 	courseinfo['Course Instructors']=attributes[12:]
			courseAttributes.append(attributes) ##REMOVE TABS!!! CHANGE MADE HERE!

		
		courseinfo['Course Attributes']=courseAttributes
		#print courseAttributes
		allcourses[coursetitle.text]=courseinfo
	return allcourses

def writeCourseInfoToFile(courseinfo):
	"""Writes a course info dict to file."""
	allcourses=courseinfo.items() #each dict only will have one pair of course info
	with open('newoutput.txt', 'a') as f:
		for course in allcourses:
			print course, '\n'
			courseitems=course[1].items()
			for item in courseitems:
				if item[0]=='Course Number':
					f.write('Course Number: '+ str(item[1])+'\n\n')
				elif item[0]=='Course Title':
					f.write('Course Title: '+str(item[1])+'\n\n')
				elif item[0]=='Course Instructors':
					f.write('Course Instructors: '+str(item[1])+'\n\n')
				elif item[0]=='Course Description':
					f.write('Course Description: '+str(item[1])+'\n\n')
				else:
					f.write('Course Attributes: ' +str(item[1])+'\n\n')
			f.write('-------------------------------------\n')
		


courseinfo=parseSinglePage(subsite)
writeCourseInfoToFile(courseinfo)
def testDepSite():
	alldep=getDepartmentSites(mainurl)
	url=mainurl+alldep['Astronomy (ASTRNMY)']
	soup=getSoupForPage(url)
	print soup.prettify()

