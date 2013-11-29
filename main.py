#!/usr/bin/python
"""
main.py
--------------

Module we can use to run the entire program.

****
TODO: Combine 'queryparser' and 'coursegraph' to unify the code.
****
"""
import queryparser
import createDatabase
import readDB
import coursegraph

if __name__=='__main__':
	"""
	Run the program.
	"""
	while True:
		queryreply=raw_input("Do you wish to search a query?")
		if queryreply.upper()=='NO':
			print 'All right, jerk. Bye.'
			break
		courseinfo=queryparser.readQuery()
		print courseinfo
