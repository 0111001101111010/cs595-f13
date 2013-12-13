'''
entryfeatures() on p. 137 
takes an entry as an argument, not a string (edits from 2 slides ago would have to be backed out)
looks for > 30% UPPERCASE words
does not tokenize “publisher” and “creator” fields 
actually, the code just does that for “publisher”
for “summary” field, it preserves 1-grams (as before) but also adds bi-grams
For example, “…best songs ever: "Good Life" and "Pink Triangle".” would be split into: 
'''