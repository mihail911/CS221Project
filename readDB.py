"""

readDB.py
---------

Author: Michael Dickens, Mihail Eric
Created: 2013-11-25

Utilities to read the course database and provide useful output.

""" 

import sqlite3
import search

allcoursesdbname = "courseinfo-small.db"
instructordbname = "instructordatabase.db"

fields = 'id,title,code,instructor,unitsmin,unitsmax,description'.split(",")
indexes = dict()
for i in range(len(fields)):
    indexes[fields[i]] = i

def getField(entry, field):
    return entry[indexes[field]]

allcoursesdb = sqlite3.connect(allcoursesdbname)
cursor = allcoursesdb.cursor()

def queryDB(query, args=None):
    if args:
        cursor.execute(query, args)
    else:
        cursor.execute(query)
    return cursor.fetchall()

def getData():
    return queryDB("SELECT * FROM courseinfo")

def getRelatedness():
    return queryDB("SELECT * FROM relatedness")

def cleanup():
    """
    Call this before exiting the program.
    """
    allcoursesdb.close()
    instructordb.close()
    
