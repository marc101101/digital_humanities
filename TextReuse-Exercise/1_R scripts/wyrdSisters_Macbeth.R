#MACBETH VS. WYRD SISTERS
library(textreuse)

#read in WyrdSisters
x <- readLines("3_hypertexts/Pratchett, Terry - Wyrd Sisters.txt")
wyrdSisters <- toString(x) 
wyrdSisters

#read in Macbeth and create overlapping 9grams ...
y <- readLines("2_shakespeare/tragedies/macbeth.txt")
macbeth <- toString(y) 
macbeth_tokens <- tokenize_ngrams(macbeth, n = 9)

token <- macbeth_tokens[110] #fair is foul and foul is fair
token
#align_local(s, t, match = 2L, mismatch = -1L, gap = -1L, edit_mark = "#", progress = interactive()) 
result <- (align_local(token, wyrdSisters))
result$a_edits
result$b_edits
result$score

length(macbeth_tokens) #16568 ngrams total


#read in Macbeth and create overlapping 9grams ...
subcorpus <- readLines("shakespeare/macbeth_11quotes.txt")
z <- toString(subcorpus) 
mac_sub_tok <- tokenize_ngrams(z, n = 9)

token <- macbeth_tokens[11]
token
#Test complete Macbeth (9grams) with complete WyrdSisters
count <-  11 #must not be zero!; error before "11" error ... don't know why yet

while(count < 100) {
  token <- macbeth_tokens[count]
  result <- (align_local(token, wyrdSisters))
  if (result$score >= 3) {
    previouscount <- count - 3
    previoustoken <- macbeth_tokens[previouscount]
    previousresult <- (align_local(previoustoken, wyrdSisters))
    if (result$a_edits != previousresult$a_edits) {
      print(count)
      print(token)
      print(result$score)
      print(result$a_edits)
      print(result$b_edits)
      cat("\n")
    }
    
  } else {
    #print("alignment score too low!")
  }
  #9grams overlap in 3-word-steps
  count <- count + 3 
}


#BEISPIEL FÜR SCHRITTWEISE DURCHFÜHRUNG DER PILOTSTUDIE

#Test A: gold standard quotes 1 by 1
macbeth1 <- paste("When shall we three meet again, In thunder, lightning, or in rain?")
macbeth2 <- paste("Round about the cauldron go; In the poison'd entrails throw.Toad, that under cold stone Days and nights has thirty-one")
macbeth3 <- paste("Eye of newt and toe of frog, Wool of bat and tongue of dog,")
macbeth4 <- paste("Double, double toil and trouble; Fire burn and cauldron bubble.")
macbeth5 <- paste("Make the gruel thick and slab:")
macbeth6 <- paste("How now, you secret, black, and midnight hags!") 
macbeth7 <- paste("If it were done when 'tis done, then 'twere well It were done quickly:")
macbeth8 <- paste("Yet who would have thought the old man to have had so much blood in him.")
macbeth9 <- paste("Infirm of purpose!")
macbeth10 <- paste("To-morrow, and to-morrow, and to-morrow, Creeps in this petty pace from day to day, To the last syllable of recorded time.") 
macbeth11 <- paste("Is this a dagger which I see before me,The handle toward my hand?")

token <- macbeth2
cat(token)
result <- (align_local(token, wyrdSisters))
cat("Text A:", result$a_edits)
cat("Text B:", result$b_edits)
cat("Alignment score:", result$score)
cat("\n\n")

#Test B: goldstandard as 9grams
#11 quotes + blind text
macbeth11quotes <- paste( "When shall we three meet again, In thunder, lightning, or in rain?",
                          "1lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "Round about the cauldron go; In the poison'd entrails throw.Toad,", 
                          "that under cold stone Days and nights has thirty-one",
                          "2lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "Eye of newt and toe of frog, Wool of bat and tongue of dog,",
                          "3lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "Double, double toil and trouble; Fire burn and cauldron bubble.",
                          "4lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "Make the gruel thick and slab:",
                          "5lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "How now, you secret, black, and midnight hags!",
                          "6lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "If it were done when 'tis done, then 'twere well It were done quickly:",
                          "7lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "Yet who would have thought the old man to have had so much blood in him.",
                          "8lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "Infirm of purpose!",
                          "9lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "To-morrow, and to-morrow, and to-morrow, Creeps in this petty pace from day to day,",
                          "To the last syllable of recorded, time.",    
                          "10lorem ipsum dolor si amet se dolet remus merit lipsum dit",
                          "Is this a dagger which I see before me,The handle toward my hand?",
                          "11lorem ipsum dolor si amet se dolet remus merit lipsum dit")

toString(macbeth11quotes)
macbeth_tokens <- tokenize_ngrams(macbeth11quotes, n = 9)
macbeth_tokens
length(macbeth_tokens) #254 ngrams (n=9)

token <- macbeth_tokens[1]
token
result <- (align_local(token, wyrdSisters))
print(result$score)
print(result$a_edits)
print(result$b_edits)
cat("\n")


count <-  55 #must not be zero!; before 14 error ...

while(count < 254) {
  
  token <- macbeth_tokens[count]
  cat(token)
  result <- (align_local(token, wyrdSisters))
  
  if (result$score >= 3) {
    previouscount <- count - 3
    previoustoken <- macbeth_tokens[previouscount]
    previousresult <- (align_local(previoustoken, wyrdSisters))
    
    if (result$a_edits != previousresult$a_edits) {
      
      print(count)
      print(token)
      print(result$score)
      print(result$a_edits)
      print(result$b_edits)
      cat("\n")
    }
  } 
  count <- count + 3   #9grams overlap in 3-word-steps
}






















