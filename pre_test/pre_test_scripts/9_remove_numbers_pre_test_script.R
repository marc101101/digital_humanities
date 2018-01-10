install.packages("NLP")
install.packages("tm")
install.packages("SnowballC")
install.packages("textstem")
library(NLP)
library(tm)
library(SnowballC)
library(textstem)

x <- readLines("3_hypertexts/Fforde, Jasper - Thursday Next 2 - Lost in a Good Book.txt")
text_corpus <- Corpus(DirSource('3_hypertexts'))
text_corpus #show corpus meta data
ffjorde <- toString(x)

y <- readLines("2_shakespeare/tragedies/hamlet.txt")
hamlet <- toString(y)



ffjorde <- tm_map(ffjorde, stripWhitespace)
ffjorde <- tm_map(ffjorde, content_transformer(toLower))
#x <- tm_map(x, removeWords, stopwords("english")) #TESTEN
ffjorde <- tm_map(ffjorde, removeNumbers) # TESTEN
#x <- lemmatize_strings(x) # TESTEN
ffjorde <- tm_map(ffjorde, removePunctuation)

hamlet <- tm_map(hamlet, stripWhitespace)
hamlet <- tm_map(hamlet, content_transformer(toLower))
#y <- tm_map(y, removeWords, stopwords("english")) #TESTEN
hamlet <- tm_map(hamlet, removeNumbers) # TESTEN
#y <- lemmatize_strings(y) # TESTEN  tis wird zu this. evtl mit dem treetagger testen, dann jedoch andere quellendateien zuerst generieren
hamlet <- tm_map(hamlet, removePunctuation)


hamlet_tokens <- tokenize_ngrams(hamlet, n = 9)

count <-  1

#While loop: identify optimal local alignment for each Hamlet ngram in the whole Ffjorde text.
count <-  1 #must not be zero!
counter <- 0
score_base <- 0
LL <- list()

#While condition = how many ngrams will be compared with the hypertext; attention: all 30k ngrams take at least 30 mins!
while(count < 100) { 
  token <- hamlet_tokens[count]
  result <- (align_local(token, ffjorde))
  if(result$score > score_base){
    score_base <- result$score
  }
    
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
write.csv(LL,file="pre_test_result/9_remove_numbers_pre_test_script.csv")

