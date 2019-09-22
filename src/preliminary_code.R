library(udpipe)
vignette("udpipe-tryitout")
library(readtext)
library(tidyverse)
library(magrittr)


files.v <- dir( pattern=".*txt")

t <- readtext(file = files.v[1])
udmodel <- udpipe_load_model(file = "german-gsd-ud-2.4-190531.udpipe" )


x <- udpipe_annotate(udmodel, x = t$text)

x <- as.data.frame(x, detailed = TRUE)



x$feats
str_split(x$feats, "\\|")

# is a punctuation token ever dependent on any real token?

max(x$sentence_id) %>%
  seq_len()

output.v <- NULL
for (i in max(x$sentence_id) %>%
     seq_len()
     )    { 
  y <- x %>%
    filter(sentence_id == i) 
  
  punc_id.v <- y$token_id[which(y$upos == "PUNCT")]
  
 output.v <- append(output.v,  y$head_token_id %in% punc_id.v )
  
  
}

x$head_token_id[which(x$upos == "PUNCT") ]

x[which(output.v == TRUE), ] %>%
  View()

which(output.v == TRUE) %>%
  length() %>%
  divide_by(nrow(x))

x$sentence[which(output.v == TRUE)[3]]

## roughly 15 per 10,000 tokens are punctuation with dependencies.  This seems to happen in the case of a sentence
## containing a direct quotation; the quotation marks somewhow become the head of various words in the matrix clause.
