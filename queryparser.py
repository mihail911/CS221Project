import nltk, re, pprint

partsofspeechmap={'NNP': 'Proper Noun', 'NNS': 'Plural Noun', 'IN': 'Preposition', 'VB':'Verb Base Form', 'NN':'Singular noun', 'DT': 'Determiner'} #mapping from POS tag to actual meaning

timeofday=['morning', 'afternoon', 'evening']
quarters['autumn', 'winter', 'spring', 'summer']

def parseQuery(query):
	words=nltk.word_tokenize(query)
	print nltk.pos_tag(words)

parseQuery("courses taught by Jerry Cain")