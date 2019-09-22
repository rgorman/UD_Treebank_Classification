library(udpipe)
vignette("udpipe-tryitout")
library(readtext)
library(tidyverse)


t <- readtext(file = "test_snippet.txt")
udmodel <- udpipe_load_model(file = "german-gsd-ud-2.4-190531.udpipe" )


x <- udpipe_annotate(udmodel, x = t$text)

x <- as.data.frame(x, detailed = TRUE)


write.csv(x, file = "test.snipett.csv", row.names = FALSE, quote = FALSE)

x$feats
str_split(x$feats, "\\|")

# is a punctuation token ever dependent on any real token?

max(x$sentence_id) %>%
  seq_len()

for (i in max(x$sentence_id) %>%
     seq_len()
     )    {
  
}
