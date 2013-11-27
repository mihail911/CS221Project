#!/usr/bin/python
import nltk, re, pprint,sqlite3
import pdb

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
	instructordb='instructordb_saved.db'
	conn=sqlite3.connect(instructordb)
	curs=conn.cursor()
	curs.execute('select * from instructordb')

	words=nltk.word_tokenize(query)
	for instpair in curs.fetchall():
		for index in range(len(words)):
			if words[index].capitalize() in instpair[0].split():
				words[index]=words[index].capitalize()
	conn.close()
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
		# if subtree.node in namedentitykeys:
		allnamedentity.append(subtree)
	return allnamedentity

def getInstructorNames(namedentitynodes):
	"""
	Extract name of instructor given named entity
	nodes, if an instructor name is provided. Else
	return an empty list.
	"""

	allinstructors=[]	
	#pdb.set_trace()
	for n in namedentitynodes:
		instructorname=""
		if hasattr(n,'node'):
			if n.node=='PNOUN':
				instructorname=' '.join(c[0] for c in n.leaves())
		# if n.node=='PERSON':
			# for nameindex in range(len(n)): #append instructor names
			# 	instructorname+=n[nameindex][0]+" "
		if instructorname!='': allinstructors.append(instructorname.rstrip())
	return set(allinstructors)

# query='Jesus Christ and Mehrain Sahami'
# postag=parseQuery(query)
# chunked=chunkquery(postag)
# print getInstructorNames(chunked)

def isCourseCode(querytokens,coursecodes):
	"""
	Checks to see if the query tokens contain a course code.
	If so, return a list of found course codes. Else return the 
	empty list.
	"""
	querycoursecodes=[]
	for code in coursecodes:
		for token in querytokens:
			if code in token:
				querycoursecodes.append(token)
	return set(querycoursecodes)

def readQuery():
	"""
	Reads in a query and extracts information
	useful for satisfying the query.
	"""
	query=raw_input("Please input desired course search: ")
	tokenized=nltk.word_tokenize(query)
	postag=parseQuery(query)
	chunked=chunkQuery(postag,propernoungrammar)
	allinstructors=getInstructorNames(chunked)
	print allinstructors

#readQuery()

#TODO: CREATE LIST OF ALL DEPARTMENT CODES!!!!!!!!!!!!!!!!!!!