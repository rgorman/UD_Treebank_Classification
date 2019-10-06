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

###############
################ start here!

files.v <- dir( pattern=".*txt")

t <- readtext(file = files.v[1])
udmodel <- udpipe_load_model(file = "german-gsd-ud-2.4-190531.udpipe" )


x <- udpipe_annotate(udmodel, x = t$text)

x <- as.data.frame(x, detailed = TRUE)

x <- x %>%
  filter(upos != "PUNCT") # drop punctuation

x <- x %>%
  mutate(self_POS = upos)

x <- x %>%
  mutate(self_rel = dep_rel)




## find morph categories to comprise column names
z <- x$feats %>%
  str_split(., "\\|", simplify = TRUE)

m <- ncol(z)

morphs.v <- NULL
for (i in 1:m) {
  a <- z[, i] %>%
    str_split(., "=") %>%
    sapply(., extract, 1) %>%
    unique()
  
  morphs.v <- append(morphs.v, a)
  
}

morphs.v <-  unique(morphs.v) # remove duplicates

morphs.v <- morphs.v[- which(morphs.v == "")] # remove empty categories
morphs.v[which(is.na(morphs.v))] <- "No_Good"

c_names.v <- paste0("self_", morphs.v)

# add columns

for (i in seq_along(morphs.v)) {
  x[, morphs.v[i]] <- NA
  
}



for (j in 1:m) {
  
  a <- z[, j] %>%
    str_split(., "=") %>%
    sapply(., extract, 1)
  
  a[is.na(a)] <- "No_Good"
  a[which(a == "")] <- "No_Good"
  
  b <- z[, j] %>%
    str_split(., "=") %>%
    sapply(., extract, 2)
  
  for (i in seq_along(a)) {
    x[i, a[i]] <- b[i]
    
  }
  
}


colnames(x)[20:36] <- c_names.v # add "self_" to names of new cols.

# add new parent features


for (i in x$sentence_id %>%
     max() %>%
     seq_len()
) { 
  a <- x %>%
    filter(sentence_id == i) 
  
  
 
             )
  
  
  
  
}


a$head_token_id

a$token_id

head_word.v <- NULL
for (i in nrow(a)) {
  
  which(a$token_id == a$head_token_id[1])
  
}

which(a$token_id == a$head_token_id[1])

a$self_POS[a$head_token_id %>%
             as.numeric() ]

a$head_token_id

###### end here !

d <- a$head_token_id %>%
  as.numeric()
d[which(d == 0) ] <- NA


a$token_id

a$upos[d]


for (i in x$sentence_id %>%
     max() %>%
     seq_len()
     ) {
  a <- x %>%
    filter(sentence_id == i) 
  
}

x$sentence_id %>%
  max() %>%
  seq_len()

a$head_token_id

x[1:2, ] %>%
  View()

p <- a$head_token_id
p[which (p == "0")]<- NA

a$parent_POS <- a$self_POS[p %>%
                  as.numeric()]

which(a$token_id == p[5])

rr <- sapply(p, equals, a$token_id )

qq <- sapply(a$token_id, equals,  p)

 sapply(a$token_id[5], equals,  p)

colnames(r)

a$self_POS[which(a$token_id == colnames(r))]

a$new_test[r] <- a$self_POS[which(a$token_id == colnames(r))]

qq <- qq[-which(is.na(qq))]
pp <- sapply(qq, function(x) {  a$self_POS[which(a$token_id == x)]})

unique(p)


zz <- vector(mode = "character", nrow(a))
sapply(pp, function(x){ return(x)}  )

a$new_test[a$head_token_id ==  names(pp[3]) ] == TRUE

which(a$head_token_id ==  names(pp[3]) )

qq <- p %>%
  unique()

z <- a$head_token_id ==  names(pp[3]) 
z[which(is.na(z))] <- FALSE
zz[which(z == TRUE)] <- pp[3]


parent.fun <- function(x) {
  z <- a$head_token_id ==  names(x) 
  z[which(is.na(z))] <- FALSE
  zz[which(z == TRUE)] <- x
  return(zz)
  
}


sapply(qq, parent.fun)

for (n in seq_along(pp)) {
  z <- a$head_token_id ==  names(pp[n]) 
  z[which(is.na(z))] <- FALSE
  zz[which(z == TRUE)] <- pp[n]
  zz[which(zz == "")] <- NA
  
}
a$new_test <- zz

n <- 1

a[, c(7, 8, 9,  14, 18, 38)] %>%
  View()

a$new_test

is.na(a$head_token_id ==  names(pp[3]) )

which(is.na)

################
i <- 2
a <- x %>%
  filter(sentence_id == i) 

source.names.v <- colnames(a)[18:36] # names of columns to provide new parent values

parent.names.v <- paste0("parent-", source.names.v %>%
                           gsub("self_", "", .)
                         ) # to precede the sentence loop

heads.v <- a$head_token_id # to precede the parent loop; df a must exist (created with filter)
heads.v[which (heads.v == "0")]<- NA
heads.v <- heads.v[-which(is.na(heads.v))]

for (j in seq_along(parent.names.v)) {
  values.v <- sapply(heads.v, function(x) {  a[which(a$token_id == x), source.names.v[j]]})
  zz <- vector(mode = "character", nrow(a))
  
  for (n in seq_along(values.v)) {
    z <- a$head_token_id ==  names(values.v[n]) 
    z[which(is.na(z))] <- FALSE
    zz[which(z == TRUE)] <- values.v[n]
    zz[which(zz == "")] <- NA
    
  } # end of loop n
  a[, parent.names.v[j]] <- zz
  
} # end of loop j


# loop through sentences
sent.count.v <- x$sentence_id %>% # number of sentences in df
  unique() %>%
  length()

source.names.v <- colnames(x)[18:36] # names of columns to provide new parent values

parent.names.v <- paste0("parent-", source.names.v %>%
                           gsub("self_", "", .)
) # to precede the sentence loop

par.df <- matrix(nrow = nrow(x), ncol = length(parent.names.v)) %>% # make data frame to collect new data
  as.data.frame()

colnames(par.df) <- parent.names.v # name columns

row.count.v <- 0

for (i in seq_len(sent.count.v)) {
  
  a <- x %>%
    filter(sentence_id == i) # make df of rows from single sentence
  
 
  
  heads.v <- a$head_token_id # vector of tokens with dependents
  heads.v[which (heads.v == "0")]<- NA
  heads.v <- heads.v[-which(is.na(heads.v))]
  heads.v <- unique(heads.v)
  
  for (j in seq_along(parent.names.v)) {
    values.v <- sapply(heads.v, function(x) {  a[which(a$token_id == x), source.names.v[j]]})
    zz <- vector(mode = "character", nrow(a))
    
    for (n in seq_along(values.v)) {
      z <- a$head_token_id ==  names(values.v[n]) 
      z[which(is.na(z))] <- FALSE
      zz[which(z == TRUE)] <- values.v[n]
      zz[which(zz == "")] <- NA
      
    } # end of loop n
    
    a[, parent.names.v[j]] <- zz
    
  } # end of loop j
  
  if (i == 1) {
    parent.df <- a
  } else {
    parent.df <- rbind(parent.df, a)
  }
  
  
  
}

parent.df[1:100, ] %>%
  View()

############
#########


heads.v[which (heads.v == "0")]<- NA
heads.v <- heads.v[-which(is.na(heads.v))]

heads.v <-unique(heads.v)
values.v <- sapply(heads.v, function(x) {  a[which(a$token_id == x), source.names.v[1]]})

j <- 1
  a[heads.v, source.names.v[1]]