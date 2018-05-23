#https://www.tidytextmining.com/preface.html
#https://rpubs.com/pjmurphy/265713  ;  http://www.mjdenny.com/Text_Processing_In_R.html

#stopword list
library(stopwords)
sw <- data_frame(line = 1, text =stopwords(language = "pl", source = "stopwords-iso"))

sw <- sw %>%
  unnest_tokens(word, text)


