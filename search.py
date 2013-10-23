"""

search.py
---------

Perform searches over a database.

"""

class SearchSchema():

    def __init__(self, filename):
        """
        Initialize by loading `filename` into an internal format.
        """
        # TODO: implement this
        pass

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
        # TODO: implement this
        pass
