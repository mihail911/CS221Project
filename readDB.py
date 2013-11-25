"""

readDB.py
---------

Author: Michael Dickens, Mihail Eric
Created: 2013-11-25

Utilities to read the course database and provide useful output.

""" 

import sqlite3

allcoursesdbname = "courseinfodata.db"
instructordbname = "instructordatabase.db"

allcoursesdb = sqlite3.connect(allcoursesdbname)
cursor = allcoursesdb.cursor()

def queryDB(query, args=None):
    if args:
        cursor.execute(query, args)
    else:
        cursor.execute(query)
    return cursor.fetchall()

def cleanup():
    """
    Call this before exiting the program.
    """
    allcoursesdb.close()
    instructordb.close()
    
