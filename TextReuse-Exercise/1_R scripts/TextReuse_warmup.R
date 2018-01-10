#DH-Übung am 20.12.2017

#1 - SETUP
#https://github.com/ropensci/textreuse
#https://www.rdocumentation.org/packages/textreuse/versions/0.1.4/topics/align_local

install.packages("textreuse")
library(textreuse)



#2 - HILFE
vignette("textreuse-introduction", package = "textreuse")
vignette("textreuse-pairwise", package = "textreuse")
vignette("textreuse-minhash", package = "textreuse") 
vignette("textreuse-alignment", package = "textreuse")



#3 - TOKENIZATION
help(paste) #concatenate string
text <- paste("How does it feel, how does it feel?",
              "To be without a home",
              "Like a complete unknown, like a rolling stone")

tokenize_sentences(text)
tokenize_words(text)
tokenize_ngrams(text, n = 3)
tokenize_skip_ngrams(text, n = 3, k = 2)



#4 - JACCARD SIMILARITY
a <- tokenize_words(paste("1 2 3 4 7"))
b <- tokenize_words(paste("1 4 5 7 9"))
a
b
jaccard_similarity(a, b)

x <- tokenize_words(paste("this too too solid flesh"))
y <- tokenize_words(paste("this too too sullied flesh"))
jaccard_similarity(x, y)
jaccard_bag_similarity(x, y)



#5 - LOCAL ALIGNMENT (SMITH-WATERMAN)
#https://www.rdocumentation.org/packages/textreuse/versions/0.1.4/topics/align_local

s <- paste("A B C X D")
t <- paste("A B C D")

#Smith-Waterman-Parameter: macht / mismatch / gap
align_local(s, t, match = 2L, mismatch = -1L, gap = -1L, edit_mark = "#", progress = interactive()) 


#Übung_A: Hamlet "solid flesh" / Fjjord
hamlet <- paste("O that this too too solid flesh would melt,")

ffjorde <- paste("He squeezed my hand and was gone. The world started up again, the TV came back on and there was a muffled plocking from Pickwick, who had managed to lock herself in the airing cupboard again. I let her out and she ruffled her feathers in an embarrassed fashion before going off in search of her water dish.
                 I went into work but there was precious little to do. We had a call from an enraged Mrs Hathaway34, demanding to know when we were going to arrest the ‘unlick’d bear-whelp’ who had cheated her, and another from a student who wanted to know whether we thought Hamlet’s line was ‘this too too solid flesh’ or ‘this too too sullied flesh’, or even perhaps 
                 ‘these two-toed swordfish’. Bowden spent the morning mouthing the lines for his routine, and by noon there had been two attempts to steal Cardenio from Vole Towers. Nothing serious; SO-14 had doubled the guard. This didn’t concern SpecOps 27 in any way, so I spent the afternoon surreptitiously reading the Jurisfiction instruction manual, which felt a little like flicking through Bunty during school. I was tempted to have a go at entering a work of fiction to try out a few of their ‘handy book-jumping tips’ (page 28) but Havisham had roundly forbidden me from doing anything of the sort ‘until I was more experienced’. By the time I was ready to go home I had learned a few tricks about emergency book evacuation procedures (page 34), read about the aims of the Bowdlerizers (page 62), a group of well-meaning yet censorious individuals hell-bent on removing obscenities from fiction. I also read about Heathcliff ’s unexpected three-year career in Hollywood under the name of Buck Stallion and his eventual return to the pages of Wuthering Heights (page 71), the forty-six abortive attempts to illegally save Beth from dying in Little Women (page 74), details of the Character Exchange Program (page 81), using holorimic verse to flush out renegade book people, or PageRunners as they were known (page 96), and how to use spelling mistakes, misprints and double negatives to signal to other Prose Resource Operatives in case emergency book evacuation procedures (page 34) failed (page 105). I was just learning about protocols relating to historical novels (page 122) when it was time to clock off. I joined the general exodus and wished Bowden good luck with his routine. He didn’t seem in the least nervous, but then he rarely did.
                 I got home to find my landlord on my doorstep. He looked around to make sure Miss Havisham was nowhere in sight, then said:") 

align_local(hamlet, ffjorde)


#Übung_B: Hamlet first 5 sentences / Fjjord
hamlet1 <- paste("Who's there?")
hamlet2 <- paste("Nay, answer me. Stand and unfold yourself.") 
hamlet3 <- paste("Long live the King!") 
hamlet4 <- paste("Bernardo?") 
hamlet5 <- paste("He.")

align_local(hamlet1, ffjorde)
align_local(hamlet2, ffjorde)
align_local(hamlet3, ffjorde)
align_local(hamlet4, ffjorde)
align_local(hamlet5, ffjorde)



#6 - TEXTE IMPORTIEREN (SIEHE GRIPS)
#set working directory to GRIPS examples folder "TextReuse-Exercise"
#Test: getwd()

#read in Ffjorde
x <- readLines("3_hypertexts/Fforde, Jasper - Thursday Next 2 - Lost in a Good Book.txt")
x     #alles ausgaben
x[1]  #Zeile 1 ausgeben
ffjorde <- toString(x) #alles zu einem langen String zusammenfügen
ffjorde

#read in Hamlet 
y <- readLines("2_shakespeare/tragedies/hamlet.txt")
hamlet <- toString(y) 



#6 - NGRAM-LOOPING THROUGH HAMLET

#create overlapping 9grams
hamlet_tokens <- tokenize_ngrams(hamlet, n = 18)
length(hamlet_tokens) #wie viele ngrams? --> 30680
hamlet_tokens[1] 

#playing around with the align_local funtion
token <- hamlet_tokens[120]
print(token)
result <- (align_local(token, ffjorde))
result$a_edits
result$b_edits
result$score

#While loop: identify optimal local alignment for each Hamlet ngram in the whole Ffjorde text.
count <-  1 #must not be zero!

#While condition = how many ngrams will be compared with the hypertext; attention: all 30k ngrams take at least 30 mins!
while(count < 100) { 
  token <- hamlet_tokens[count]
  result <- (align_local(token, ffjorde))
  #select threshold for alignment score
  if (result$score >= 5) {
    #check for duplicate alignments in preceding ngram
    previouscount <- count - 3
    previoustoken <- hamlet_tokens[previouscount]
    previousresult <- (align_local(previoustoken, ffjorde))
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
  count <- count + 3 #9grams overlap in 3-word-steps
}








