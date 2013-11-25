"""

search.py
---------

Perform searches over a database.

"""

from readDB import *

class SearchSchema():
   def search(self, query):
        raise Exception("Not yet implemented: SearchSchena.search()")
        
class PropertySearch(SearchSchema):
    """
    Search for courses with the correct property.
    """
    def search(self, query):
        property, requirement = query.split()
        # TODO: Don't use format or Bobby Tables will get you. See
        # http://docs.python.org/2/library/sqlite3.html
        return queryDB('SELECT * FROM COURSEINFO WHERE %s = "%s"' %
                       (property, requirement))
