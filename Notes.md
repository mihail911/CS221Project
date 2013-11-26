## Course Graph

First I implemented naiveWeights(), which worked sort of well. Then I implemented bayesWeights(). Finding P(R) and P(F) wasn't too hard, but how to find P(F|R)?

Also, at a certain point I realide I also need to update on a feature not being in the second set as well as a feature being in the second sent--these both provide Bayesian evidence.

Right now for P(F|R) and P(~F|R) I just threw in some constants. This seems to work okay.
