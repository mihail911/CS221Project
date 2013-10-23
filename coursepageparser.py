import urllib2
from bs4 import BeautifulSoup

url="http://explorecourses.stanford.edu/search?view=catalog&academicYear=&page=0&q=EARTHSCI&filter-departmentcode-EARTHSCI=on&filter-coursestatus-Active=on&filter-term-Autumn=on"
page=urllib2.urlopen(url)
soup=BeautifulSoup(page.read())
print soup
#def parseSinglePage(url):
