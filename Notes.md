## Course Graph

### Weights Calculation

First I implemented naiveWeights(), which worked sort of well. Then I implemented bayesWeights(). Finding P(R) and P(F) wasn't too hard, but how to find P(F|R)?

Also, at a certain point I realide I also need to update on a feature not being in the second set as well as a feature being in the second sent--these both provide Bayesian evidence.

Right now for P(F|R) and P(~F|R) I just threw in some constants. This seems to work okay.

Then there's the question of how to combine each conditional probability. First I tried using combineProbs(), but that doesn't seem to work too well. It works a lot better to simply take the sum of all the probabilities. Why?

### Feature Extraction

I started with simple features: just the words in the title and description. Then I added features for instructor names and course codes.

Next I thought to add combination features. I just included `code + (title/description word)` for the sake of efficiency.

One problem: it thinks CS108 and Math108 are closely related. I'm going to remove the code11s feature and see if that helps.

### Profiling

It takes way too long to run. I spent about two hours trying to get the Python profiler to give me something useful, but I couldn't get it to work so I'm rewriting the program in Haskell.

Haskell is also really slow for some reason, but at least the profiler works so I'm going to try to fix it.

We ended up just creating a smaller data set that has about 150 courses instead of 11,000.
