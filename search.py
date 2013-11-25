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
        # TODO: This isn't sanitized but I can't get the sanitized
        # version to work.
        return queryDB('SELECT * FROM COURSEINFO WHERE %s = "%s"' %
                       (property, requirement))
