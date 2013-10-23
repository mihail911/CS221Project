"""

search.py
---------

Perform searches over a database.

"""

class SearchSchema():

    def __init__(self, courses):
        self.courses = courses

    def search(self, query):
        """
        Find courses that match `query`.

        Subclasses should implement this method.
        """
        raise Exception("not implemented")

        
class SimpleSearch(SearchSchema):
    """
    Search simply by finding keywords in the course list.
    """
    def search(self, query):
        # Right now, if you input a course name, it returns the course data.
        for k in courses:
            if query == k:
                return courses[k]
        return None
