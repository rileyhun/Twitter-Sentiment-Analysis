positives <- scan('positive-words.txt', what='character', comment.char=';')
positive_words <- c(positives, 'upgrade', 'fleek')
negatives <- scan('negative-words.txt', what='character', comment.char=';')
negative_words <- c(negatives, 'wtf', 'wait', 'fuck', 'waiting', 'epicfail', 'mechanical', 'bs', 'bullshit', 'crap', 'shit', 'b.s.')
