install.packages("udpipe")
library(udpipe)
library(tm)


#lemmatize with udpipe and create dataframe x with all infos

# download german ud model
ud_model <- udpipe_download_model("german")
ud_model <- udpipe_load_model(ud_model)

Text2Lemmatize <- Lina_corpus

x <- udpipe_annotate(ud_model, Text2Lemmatize)
x <- as.data.frame(x)


#check dataframe
x[, c("token", "lemma", "upos")]

#Problem: Some lemmas have NA entries:

# Display the 'tokens' values where 'lemmas' are NA
na_tokens <- x$token[is.na(x$lemma)]


# Replace NA lemmas with original tokens in the lemmas column
x$lemma[is.na(x$lemma)] <- x$token[is.na(x$lemma)]


