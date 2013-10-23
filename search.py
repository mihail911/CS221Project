"""

search.py
---------

Perform searches over a database.

"""

class SearchSchema():

    def __init__(self, courses):
        # TODO: sanitize course names in self.courses
        self.courses = courses

    def sanitizeQuery(self, query):
        return query
        # TODO: won't work because self.courses is not sanitized yet
        # return query.strip().upper()

    def searchSanitized(self, query):
        """
        Find courses that match `query`.

        Subclasses should implement this method.

        query: Sanitized input string.
        """
        raise Exception("Sub-classes should implement this method.")

    def search(self, query):
        return self.searchSanitized(self.sanitizeQuery(query))

        
class NameSearch(SearchSchema):
    """
    Search simply by finding the course with the given name.
    """
    def searchSanitized(self, query):
        for k in self.courses:
            if query == k:
                return k # kind of useless
        return None

class PropertySearch(SearchSchema):
    """
    Search for courses with the correct property.
    """
    def searchSanitized(self, query):
        property, requirement = query.split()
        return [ k for k in self.courses \
                 if requirement in self.courses[k][property] ]
