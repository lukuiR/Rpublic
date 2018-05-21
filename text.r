#https://www.tidytextmining.com/preface.html

#stopword list
library(stopwords)
sw <- data_frame(line = 1, text =stopwords(language = "pl", source = "stopwords-iso"))

sw <- sw %>%
  unnest_tokens(word, text)
