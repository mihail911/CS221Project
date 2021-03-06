#!/usr/bin/python
import nltk, re, pprint,sqlite3
import pdb
import createDatabase

partsofspeechmap={'NNP': 'Proper Noun', 'NNS': 'Plural Noun', 'IN': 'Preposition', 'VB':'Verb Base Form', 'NN':'Singular noun', 'DT': 'Determiner'} #mapping from POS tag to actual meaning

timeofday=['morning', 'afternoon', 'evening']
quarters=['autumn', 'winter', 'spring', 'summer']
namedentitykeys=['ORGANIZATION', 'PERSON', 'LOCATION', 'DATE', 'TIME', 'MONEY', 'PERCENT', 
				'FACILITY', 'GPE']

propernoungrammar1="PNOUN: {<NNP>*}"
propernoungrammar2="PNOUN: {<VBD> <IN> <NNP>*}"
prereqs="PREREQ: {<NN> <IN> <NN>}"

def parseQuery(query):
	"""
	Return query with POS tags.
	"""
	allinstructors=createDatabase.getInstructors()
	words=nltk.word_tokenize(query)
	for inst in allinstructors: 
		for index in range(len(words)):
			if words[index].capitalize() in inst.split():
				words[index]=words[index].capitalize()
	return nltk.pos_tag(words)

def chunkQuery(querypostags, grammar):
	"""
	Chunks input query and returns relevant 
	named entity nodes in list.
	"""
	allnamedentity=[]
	cp=nltk.RegexpParser(grammar)
	tree=cp.parse(querypostags)
	for subtree in tree.subtrees():
		allnamedentity.append(subtree)
	return allnamedentity

def getInstructorNames(namedentitynodes,instructornames):
	"""
	Extract name of instructor given named entity
	nodes, if an instructor name is provided. Else
	return an empty list.
	"""
	allinstructors=[]	
	for n in namedentitynodes:
		instructorname=""
		if hasattr(n,'node'):
			if n.node=='PNOUN':
				sanitizedinstructor=[]	
				for c in n.leaves():
					for inst in instructornames:
						if c[0] in inst and c[0] not in sanitizedinstructor:
							sanitizedinstructor.append(c[0])
				instructorname=' '.join(sanitizedinstructor)
		if instructorname!='': allinstructors.append(instructorname.rstrip())
	return set(allinstructors)

def createBigramTokens(querytokens):
	"""
	Create a list of bigram tokens,
	given the input of unigram tokens.
	"""
	bigramtokens=[]
	for index in range(len(querytokens)-1):
		btoken=querytokens[index].upper()+querytokens[index+1].upper()
		bigramtokens.append(btoken)
	return bigramtokens

def containsCourseCode(querytokens,coursecodes):
	"""
	Checks to see if the query tokens contain a course code.
	If so, return a list of found course codes, else return the 
	empty set.
	"""
	querycoursecodes=[]
	bigramtokens=createBigramTokens(querytokens)
	alltokens=querytokens+bigramtokens
	for code in coursecodes:
		for token in alltokens:
			if code==token.upper():
				querycoursecodes.append(token.upper())
	return set(querycoursecodes)

def containsDeptCode(querytokens,departmentcodes):
	"""
	Checks to see if query tokens contain a 
	department code. If found, return set of codes,
	else return the empty set.
	"""
	deptcodes=set()
	bigramtokens=createBigramTokens(querytokens)
	alltokens=bigramtokens+querytokens	
	for code in departmentcodes:
		for token in alltokens:
			if code==token.upper():
				deptcodes.add(token.upper())
	return deptcodes

def findMatchingCourseTitles(querytokens,coursetitles, deptcodes):
	"""
	Finds whether given query string 
	matches any course titles.
	"""
	alltitles=set()
	capitalizedquery=[querytokens[index].capitalize() for index in range(len(querytokens))]
	newquery=' '.join(capitalizedquery)
	for title in coursetitles:
		if newquery in title and newquery.upper() not in deptcodes:
			alltitles.add(title)
	return alltitles 

def updateCourseInfo(courseinfo,deptcodes,coursecodes,titles, instructors):
	courseinfo['Instructors']=instructors
	courseinfo['Titles']=titles
	courseinfo['Course Codes']=coursecodes
	courseinfo['Dept Codes']=deptcodes	

def readQuery(query):
	"""
	Reads in a query and extracts information
	useful for satisfying the query.
	Returns information as a dict with sets as values.
	"""
	#query=raw_input("Please input desired course search: ")
	courseinfo={}
	coursecodes=createDatabase.getCourseCodes()
	departmentcodes=createDatabase.getSetOfDeptCodes()
	allinstructors=createDatabase.getAllInstructors() 
	coursetitles=createDatabase.getAllCourseTitles()
	cleanquery=re.sub('[\:,/?.()]','', query).strip()
	tokenized=nltk.word_tokenize(cleanquery)
	bigramtokens=createBigramTokens(tokenized)
	coursecodes=containsCourseCode(tokenized,coursecodes)
	deptcodes=containsDeptCode(tokenized,departmentcodes)
	postag=parseQuery(query)
	chunkedinstructor=chunkQuery(postag,propernoungrammar1)
	allinstructors=getInstructorNames(chunkedinstructor,allinstructors)
	updateCourseInfo(courseinfo,deptcodes,coursecodes,findMatchingCourseTitles(tokenized,coursetitles,departmentcodes),allinstructors)
	return courseinfo
