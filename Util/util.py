"""

util.py
-------

Author: Michael Dickens, Mihail Eric
Created: 2013-11-25

Miscellaneous utilities.

"""

def product(xs):
    return reduce(lambda x, y: x * y, xs, 1)

def probUpdate(prior, peventgivenprior, pevent):
    """
    Perform a Bayesian probability update: P(A | Event)
    """
    return prior * peventgivenprior / float(pevent)

def combineProbs(probs):
    """
    Find the probability of independent events all occurring. In
    practice, many events will not be independent, but this makes for
    a reasonable heuristic.

    See http://www.paulgraham.com/naivebayes.html
    """
    if len(probs) == 0:
        return 0
    prod = float(product(probs))
    invs = product([ 1 - p for p in probs ])
    return prod / (prod + invs)
