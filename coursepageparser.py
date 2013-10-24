import urllib2
from bs4 import BeautifulSoup

mainurl="http://explorecourses.stanford.edu/"
subsite="http://explorecourses.stanford.edu/search?view=catalog&academicYear=&page=0&q=MKTG&filter-departmentcode-MKTG=on&filter-coursestatus-Active=on&filter-term-Autumn=on"
header={'User-Agent':'Mozilla/5.0'}
req = urllib2.Request(subsite,headers=header)
page = urllib2.urlopen(req)
soup = BeautifulSoup(page)

def getSoupForPage(url):
	req = urllib2.Request(url,headers=header)
	page = urllib2.urlopen(req)
	soup = BeautifulSoup(page)
	return soup

#Parse a single given the url--return dictionary of class info for each page
def parseSinglePage(url):
	soup=getSoupForPage(url)
	allcourses={}
	divs=soup.findAll('div', attrs={'class':'courseInfo'})
	for d in divs: #get info for each class
		courseinfo={}
		coursenumber=d.find('span',attrs={'class':'courseNumber'})
		coursetitle=d.find('span',attrs={'class':'courseTitle'})
		courseattr=d.findAll('div',attrs={'class':'courseAttributes'})
		coursedescrip=d.find('div',attrs={'class':'courseDescription'})

		courseinfo['Course Number']=coursenumber.text
		courseinfo['Course Title']=coursetitle.text
		courseinfo['Course Description']=coursedescrip.text
		courseAttributes=[]
		for i in courseattr:
			courseAttributes.append(i.text.strip()) ##REMOVE TABS!!!
		courseinfo['Course Attributes']=courseAttributes
		allcourses[coursetitle.text]=courseinfo
	return allcourses


print parseSinglePage(subsite)