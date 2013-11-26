## Course Graph

### Weights Calculation

First I implemented naiveWeights(), which worked sort of well. Then I implemented bayesWeights(). Finding P(R) and P(F) wasn't too hard, but how to find P(F|R)?

Also, at a certain point I realide I also need to update on a feature not being in the second set as well as a feature being in the second sent--these both provide Bayesian evidence.

Right now for P(F|R) and P(~F|R) I just threw in some constants. This seems to work okay.

Then there's the question of how to combine each conditional probability. First I tried using combineProbs(), but that doesn't seem to work too well. It works a lot better to simply take the sum of all the probabilities. Why?

### Feature Extraction

I started with simple features: just the words in the title and description. Then I added features for instructor names and course codes.

Next I thought to add combination features. I just included `code + (title/description word)` for the sake of efficiency.
