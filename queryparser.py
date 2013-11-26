#!/usr/bin/python
import nltk, re, pprint

partsofspeechmap={'NNP': 'Proper Noun', 'NNS': 'Plural Noun', 'IN': 'Preposition', 'VB':'Verb Base Form', 'NN':'Singular noun', 'DT': 'Determiner'} #mapping from POS tag to actual meaning

timeofday=['morning', 'afternoon', 'evening']
quarters=['autumn', 'winter', 'spring', 'summer']
namedentitykeys=['ORGANIZATION', 'PERSON', 'LOCATION', 'DATE', 'TIME', 'MONEY', 'PERCENT', 
				'FACILITY', 'GPE']

def parseQuery(query):
	"""Return query with POS tags."""
	words=nltk.word_tokenize(query)
	return nltk.pos_tag([word.capitalize() for word in words])

def chunkquery(querypostags):
	"""Chunks input query and returns relevant 
	named entity nodes in list."""
	allnamedentity=[]
	chunked=nltk.ne_chunk(querypostags)
	for subtree in chunked.subtrees():
		if subtree.node in namedentitykeys:
			allnamedentity.append(subtree)
	return allnamedentity

def getInstructorNames(namedentitynodes):
	"""Extract name of instructor given named entity
	nodes, if an instructor name is provided. Else
	return an empty list."""
	allinstructors=[]	
	for n in namedentitynodes:
		instructorname=""
		if n.node=='PERSON':
			for nameindex in range(len(n)): #append instructor names
				instructorname+=n[nameindex][0]+" "
		allinstructors.append(instructorname.rstrip())
	return allinstructors

query='courses taught by Jesus Howard Christ'
postag=parseQuery(query)
chunked=chunkquery(postag)
print getInstructorNames(chunked)
