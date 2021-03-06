x <- readLines("3_hypertexts/Fforde, Jasper - Thursday Next 2 - Lost in a Good Book.txt")
ffjorde <- toString(x)

y <- readLines("2_shakespeare/tragedies/hamlet.txt")
hamlet <- toString(y)

hamlet_tokens <- tokenize_ngrams(hamlet, n = 9)

count <-  1

#While loop: identify optimal local alignment for each Hamlet ngram in the whole Ffjorde text.
count <-  1 #must not be zero!
counter <- 0
score_base <- 0
LL <- list()

#While condition = how many ngrams will be compared with the hypertext; attention: all 30k ngrams take at least 30 mins!
while(count < length(hamlet_tokens)) { 
  token <- hamlet_tokens[count]
  result <- (align_local(token, ffjorde))
  if(result$score > score_base){
    score_base <- result$score
  }
    
  #select threshold for alignment score
  if (result$score >= 8){
    #check for duplicate alignments in preceding ngram
    previouscount <- count - 3
    previoustoken <- hamlet_tokens[previouscount]
    previousresult <- (align_local(previoustoken, ffjorde))
    if (result$a_edits != previousresult$a_edits) {
      print(token)
      print(result$score)
      print(result$a_edits)
      print(result$b_edits)
      cat("\n")
      counter <- counter + 1
      LL[counter] <- list(c(count, token, result$score, result$a_edits, result$b_edits))
    }
    
  } else {
    #print("alignment score too low!")
  }
  count <- count + 3 #9grams overlap in 3-word-steps
}


