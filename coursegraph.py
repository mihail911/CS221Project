"""

coursegraph.py
--------------

Author: Michael Dickens, Mihail Eric
Created: 2013-11-25

Utility to build a weighted graph of the courses where edge
weights indicate relatedness.

"""

from nltk.stem.porter import PorterStemmer
import re
from collections import Counter
from readDB import *
from util import *

stemmer = PorterStemmer()

def titleFeatures(title):
    return [ ('title', stemmer.stem(w.lower())) for w in title.split() ]

def codeFeatures(code):
    index = re.search('[0-9]', code).start()
    letters = code[:index]
    numbers = re.sub('[a-zA-Z]', '', code[index:]).rjust(3, '0')
    return [ ('codename', letters),
             ('code100s', numbers[0]),
             ('code10s', numbers[1]),
             ('code11s', numbers[1:3])
    ]

def instructorFeatures(title):
    return [ ('instr', w.lower()) for w in title.split(",") ]

def unitFeatures(minunits, maxunits):
    """
    There may be some small correlation between related features and
    number of units. For example, every class in the Math 50-series is
    5 units.
    """
    return [ ('minunits', minunits), ('maxunits', maxunits) ]
    
def descFeatures(desc):
    """
    Features for a course description.
    """
    return [ ('desc', stemmer.stem(w.lower())) for w in desc.split() ]

def extractFeatures(entry):
    """
    Find the features for a given entry. Features are stored in a sparse vector.
    """
    titlefeats = titleFeatures(getField(entry, 'title'))
    codefeats = codeFeatures(getField(entry, 'code'))
    instrfeats = instructorFeatures(getField(entry, 'instructor'))
    unitfeats = unitFeatures(getField(entry, 'unitsmin'),
                             (getField(entry, 'unitsmax')))
    descfeats = descFeatures(getField(entry, 'description'))

    codecombofeats = [ ('codecombo', feat1, feat2)
                       for feat1 in codefeats
                       for feat2 in titlefeats + descfeats + instrfeats ]
    
    return set(titlefeats + codefeats + instrfeats + unitfeats + \
               descfeats + codecombofeats)

features = dict()

def createFeaturePriors(entries):
    """
    For each feature of a course, find the prior probability of that
    feature occurring.

    entries: List of database entries where each entry is a tuple of fields.

    return: Dictionary mapping features to probabilities. If a feature
    is not in the dictionary, its probability is 0.
    """
    allfeatures = Counter()
    for entry in entries:
        feats = extractFeatures(entry)
        features[entry] = feats
        for f in feats:
            allfeatures[f] += 1

    # Use allfeatures to construct a table of probabilities.
    probs = dict()
    for f in allfeatures:
        probs[f] = float(allfeatures[f]) / len(entries)
    return probs

alldata = getData()
relatedness = getRelatedness()
featurepriors = createFeaturePriors(alldata)


def naiveWeight(feats1, feats2):
    """
    Naive algorithm: Just count the proportion of features in common.
    """
    return sum([ feat in feats2 for feat in feats1 ]) / \
        float(len(feats1))

def featFreqWeightedWeight(feats1, feats2):
    """
    Count proportion of features in common, weighted by frequency of those features.
    """
    return sum([ (feat in feats2) * featurepriors[feat] for feat in feats1 ])


def bayesWeight(feats1, feats2):
    """
    Compute weight as the Bayesian probability of two entries sharing
    the features that they do.
    """
    totalnumsharedfeatures = 0
    avgnumsharedfeatures = 0
    
    probs = []
    for feat in feats1:
        """p = P(related | feature) or P(R|F)
        
        P(R) = num related courses / total num courses
        
        P(F) = prior probability of feature occurring in entry
        
        P(F|R) = probability of this feature occurring in both
        given that the entries are related
        
        If two entries are related then they must share at least 1
        feature among `feats1`. There are `2^n - 1` ways to do
        this. There are `2^(n-1)` ways to do this such that `feat`
        is among the chosen features.
        
        Requirements:
         - Update value > 1 for feat in feats2 and < 1 otherwise
         - 0 <= p <= 1

        TODO: The current update is not strong enough, so even two
        identical feature vectors have a low p.

        """
        # prior = float(numrelated) / len(alldata)
        # prior = 1.0 / len(alldata)
        prior = 0.1
        if feat in feats2:
            p = probUpdate(prior, # P(R)
                           0.9, # P(F|R)
                           featurepriors[feat]) # P(F)
        else: 
            # TODO: Should we also update on every feature in feats1 not
            # in feats2?
            """
            P(related | ~feature)
            """
            p = probUpdate(prior, # P(R)
                           0.1, # P(~F|R)
                           1 - featurepriors[feat]) # P(~F)
        probs.append(p)
        # print "%s: %s" % (feat in feats2, p)

    return sum(probs)
    # return combineProbs(probs)

def weight(feats1, feats2):
    """
    Compute the relatedness between two feature sets by examining the features.

    This is not symmetric--weight(a, b) != weight(b, a). Course A may
    be relatively more related to course B than course B is to course
    A.
    """
#    return featFreqWeightedWeight(feats1, feats2)
    return bayesWeight(feats1, feats2)

def buildRelatedCourses(data, entry1, numrelated):
    """
    TODO: This is slow. Try to make it faster.
    
    numrelated: The number of related courses to find.
    
    return: a list of (course, relatedness) sorted by relatedness.
    """
    feats1 = features[entry1]
    
    courses = [ (None, -1) for _ in range(numrelated) ]
    for entry2 in data:
        feats2 = features[entry2]
        relatedness = weight(feats1, feats2)
        if relatedness > courses[0][1]:
            courses[0] = ((getField(entry2,'id'), getField(entry2, 'code'), getField(entry2, 'title')),
                          relatedness)
            # Note: This sort is O(n) because Python uses timsort and
            # only one element is out of order.
            courses.sort(key=lambda pair: pair[1])
    return courses

def getRelatedCourses(entry): 
    """
    Read the relatedness graph to find related courses.
    """
    ids = set([ related_id for (id, related_id) in relatedness \
            if id == getField(entry, 'id') ])
    return [ entry for entry in alldata if getField(entry, 'id') in ids ]
    
def buildGraph():
    for entry in data:
        related = getRelatedCourses(data, desc, 5)
        # TODO: finish this
    
