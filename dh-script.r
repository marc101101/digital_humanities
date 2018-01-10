install.packages("textreuse")
library(textreuse)

dir <- system.file("C:/Users/uids4029/workspace/digital_humanities/TextReuse-Exercise/4_testdaten/2_Hypertexte", package = "textreuse")

minhash <- minhash_generator(200, seed = 235)
ats <- TextReuseCorpus(dir = dir,
                       tokenizer = tokenize_ngrams, n = 5,
                       minhash_func = minhash)

buckets <- lsh(ats, bands = 50, progress = FALSE)
candidates <- lsh_candidates(buckets)
scores <- lsh_compare(candidates, ats, jaccard_similarity, progress = FALSE)

sdata = c("C:/Users/uids4029/dh-", format(Sys.time(), "%d-%m-%Y-%H-%M-%S"), ".csv")
fileNameExport <- paste(sdata[1], sdata[2], sdata[3], sep ='')
write.table(scores, file = fileNameExport ,row.names=FALSE, na="",col.names=TRUE, sep=",")


a <- readLines("4_testdaten/2_Hypertexte/Adams, Douglas - H2G2 The Ultimate Hitchhiker's Guide.txt")
b <- readLines("4_testdaten/3_Hypotexte/hamlet.txt")
adams <- toString(a) #alles zu einem langen String zusammenfügen
hamlet <- toString(b)
align_local(adams, hamlet)
#> TextReuse alignment
#> Alignment score: 7 
#> Document A:
#> this is a good match
#> 
#> Document B:
#> This is a #### match