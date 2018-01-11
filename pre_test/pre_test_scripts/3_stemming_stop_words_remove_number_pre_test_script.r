install.packages("NLP")
install.packages("tm")
install.packages("SnowballC")
install.packages("textstem")
install.packages("textreuse")

library(NLP)
library(tm)
library(SnowballC)
library(textstem)
library(textreuse)

ffjorde <- Corpus(DirSource('3_hypertexts'))
hamlet <- Corpus(DirSource('2_shakespeare/tragedies'))

ffjorde <- tm_map(ffjorde, stripWhitespace)
ffjorde <- tm_map(ffjorde, content_transformer(tolower))
ffjorde <- tm_map(ffjorde, removeNumbers) #TESTEN
ffjorde <- tm_map(ffjorde, removeWords, stopwords("english")) #TESTEN
ffjorde <- tm_map(ffjorde, stemDocument) # TESTEN
ffjorde <- tm_map(ffjorde, removePunctuation)

hamlet <- tm_map(hamlet, stripWhitespace)
hamlet <- tm_map(hamlet, content_transformer(tolower))
hamlet <- tm_map(hamlet, removeNumbers) #TESTEN
hamlet <- tm_map(hamlet, removeWords, stopwords("english")) #TESTEN
hamlet <- tm_map(hamlet, stemDocument) # TESTEN
hamlet <- tm_map(hamlet, removePunctuation)

ffjorde <- toString(ffjorde)
hamlet <- toString(hamlet)
hamlet_tokens <- tokenize_ngrams(hamlet[1], n = 9)

#While loop: identify optimal local alignment for each Hamlet ngram in the whole Ffjorde text.
count <-  1 #must not be zero!
counter <- 0
LL <- list()

#While condition = how many ngrams will be compared with the hypertext; attention: all 30k ngrams take at least 30 mins!
while(count < length(hamlet_tokens)) { 
  token <- hamlet_tokens[count]
  result <- (align_local(token, ffjorde))
    
  #select threshold for alignment score
  if (result$score >= 9){
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
write.csv(LL,file="pre_test_results/3_stemming_stop_words_remove_number_pre_test_script.csv")
