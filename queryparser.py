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

query='courses taught by Abdullah Yousuff'
postag=parseQuery(query)
#print postag
chunked=chunkquery(postag)
#print chunked
print getInstructorNames(chunked)

#parseQuery("courses taught by Jerry Cain")
# x='courses taught by Jerry Cain'
# posx=nltk.pos_tag(nltk.word_tokenize(x))
# chunked=nltk.ne_chunk(posx)
# for i in chunked.subtrees():
# 	if i.node=='PERSON':
# 		print i
#print chunked[0],chunked.node
# leaves=[]
# def traverse(t,leaves):
# 	try:
# 		t.node
# 	except AttributeError:
# 		leaves.append(t)
# 	else:
# 		leaves.append((t.node, ))
# 		for child in t:
# 			traverse(child,leaves)

#traverse(chunked,leaves)
#print leaves