#!/usr/bin/python
import nltk, re, pprint,sqlite3
import pdb
import createDatabase

partsofspeechmap={'NNP': 'Proper Noun', 'NNS': 'Plural Noun', 'IN': 'Preposition', 'VB':'Verb Base Form', 'NN':'Singular noun', 'DT': 'Determiner'} #mapping from POS tag to actual meaning

timeofday=['morning', 'afternoon', 'evening']
quarters=['autumn', 'winter', 'spring', 'summer']
namedentitykeys=['ORGANIZATION', 'PERSON', 'LOCATION', 'DATE', 'TIME', 'MONEY', 'PERCENT', 
				'FACILITY', 'GPE']

propernoungrammar="PNOUN: {<NNP>*}"


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
	#chunked=nltk.ne_chunk(querypostags)
	cp=nltk.RegexpParser(grammar)
	tree=cp.parse(querypostags)
	for subtree in tree.subtrees():
		allnamedentity.append(subtree)
	return allnamedentity

def getInstructorNames(namedentitynodes):
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
				instructorname=' '.join(c[0] for c in n.leaves())
		if instructorname!='': allinstructors.append(instructorname.rstrip())
	return set(allinstructors)

def containsCourseCode(querytokens,coursecodes):
	"""
	Checks to see if the query tokens contain a course code.
	If so, return a list of found course codes, else return the 
	empty set.
	"""
	querycoursecodes=[]
	for code in coursecodes:
		for token in querytokens:
			if code in token:
				querycoursecodes.append(token)
	return set(querycoursecodes)

def containsDeptCode(querytokens,departmentcodes):
	"""
	Checks to see if query tokens contain a 
	department code. If found, return set of codes,
	else return the empty set.
	"""
	deptcodes=set()
	#pdb.set_trace()
	for code in departmentcodes:
		for token in querytokens:
			if code in token:
				deptcodes.add(token)
	return deptcodes

def readQuery():
	"""
	Reads in a query and extracts information
	useful for satisfying the query.
	"""
	query=raw_input("Please input desired course search: ")
	coursecodes=createDatabase.getCourseCodes()
	departmentcodes=createDatabase.getSetOfDeptCodes()
	cleanquery=re.sub('[\:,/?.()]','', query).strip()
	tokenized=nltk.word_tokenize(cleanquery)
	#print 'course codes', coursecodes
	coursecodes=containsCourseCode(tokenized,coursecodes)
	deptcodes=containsDeptCode(tokenized,departmentcodes)
	#print 'department', departmentcodes
	print 'dept', deptcodes
	print 'codes', coursecodes
	postag=parseQuery(query)
	chunkedinstructor=chunkQuery(postag,propernoungrammar)
	allinstructors=getInstructorNames(chunkedinstructor)
	print allinstructors

#readQuery()

#TODO: CREATE LIST OF ALL DEPARTMENT CODES!!!!!!!!!!!!!!!!!!!