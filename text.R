#https://www.tidytextmining.com/preface.html
#https://rpubs.com/pjmurphy/265713  ;  http://www.mjdenny.com/Text_Processing_In_R.html

#stopword list
library(stopwords)
sw <- data_frame(line = 1, text =stopwords(language = "pl", source = "stopwords-iso"))

sw <- sw %>%
  unnest_tokens(word, text)
sw<-gsub("ś", "s",sw);
sw<-gsub("ć", "c",sw);
sw<-gsub("ż", "z",sw);
sw<-gsub("ź", "z",sw);
sw<-gsub("ł", "l",sw);
sw<-gsub("ą", "a",sw);
sw<-gsub("ę", "e",sw);
sw<-gsub("ó", "o",sw);

mgr <- tr[order(tr$Temat.znormalizowany,tr$Odebrano),c('Nazwa.nadawcy','Temat.znormalizowany','Temat','Treść','Odebrano')]

mgr$Odebrano <- strptime(mgr$Odebrano, "%d-%m-%y %H:%M")

minn<-aggregate(mgr$Odebrano, by=list(mgr$Temat.znormalizowany), FUN = min)[2]

strptime( aggregate(mgr$Odebrano, by=list(mgr$Temat.znormalizowany), FUN = min)[2], "%d-%m-%y %H:%M")

temp <- tolower(mgr[as.character(mgr$Odebrano) %in% as.character( minn$x),][1,4])

mgr$Temat<-tolower(mgr$Temat)

mgr$Treść<-tolower(mgr$Treść)

mgr$Treść<-gsub("\n", " ; ",mgr$Treść);
mgr$Treść<-gsub("ś", "s",mgr$Treść);
mgr$Treść<-gsub("ć", "c",mgr$Treść);
mgr$Treść<-gsub("ż", "z",mgr$Treść);
mgr$Treść<-gsub("ź", "z",mgr$Treść);
mgr$Treść<-gsub("ł", "l",mgr$Treść);
mgr$Treść<-gsub("ń", "n",mgr$Treść);
mgr$Treść<-gsub("ą", "a",mgr$Treść);
mgr$Treść<-gsub("ę", "e",mgr$Treść);
mgr$Treść<-gsub("ó", "o",mgr$Treść);
 gsub(".*pozdr", " ",mgr$Treść)  gsub("pozdr.*", " ",mgr$Treść)

text_df <- data_frame(line = 1:dim(mgr)[1], text = mgr$Treść)

text_df <- data_frame(line = 1, text = as.character( tr$Treść))


ttt <- text_df %>%
  unnest_tokens(word, text)

tidy_books <- ttt %>%
  anti_join(stopwords(language = "pl", source = "stopwords-iso"))

t4 <- ttt %>%
  anti_join(sw ,by=c('word'='word'))

