#https://www.tidytextmining.com/preface.html
#https://rpubs.com/pjmurphy/265713  ;  http://www.mjdenny.com/Text_Processing_In_R.html

yy=read.csv('raporty odp.csv', header = TRUE, sep=";",quote ='"')

plot_ly(x=t$Var1[t$Var1!="Pors.pl"], y=t$Freq[t$Var1!="Portalribas.pl"], type = "bar")
 
unique(yy$Od)
t=as.data.frame( table(yy$Od))

aggregate(yy$Od, by=list(yy$Od), length)

tt = aggregate(yy$Temat.znormalizowany, by=list(yy$Temat.znormalizowany), length)
tt = tt[tt$x>1,]

tr=yy[yy$Temat.znormalizowany %in% tt$Group.1,]

tr<-tr[tr$Nazwa.nadawcy != 'Portalibas.pl',]
tr<-tr[tr$Nazwa.nadawcy != 'etlserviom.pl',]

mgr <- tr[order(tr$Temat.znormalizowany,tr$Odebrano),c('Nazwa.nadawcy','Temat.znormalizowany','Temat','Treść','Odebrano')]

mgr$Odebrano <- strptime(mgr$Odebrano, "%d-%m-%y %H:%M")

mgr<-mgr[order(mgr$Temat.znormalizowany,mgr$Odebrano),]

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
mgr$Treść<-gsub("ą", "a",mgr$Treść);
mgr$Treść<-gsub("ę", "e",mgr$Treść);
mgr$Treść<-gsub("ó", "o",mgr$Treść);

sub("","\n",temp)

writeLines(as.character( mgr[as.character(mgr$Odebrano) %in% as.character( minn$x),'Treść'][5]))

temp<-writeLines(as.character(temp))

temp<-gsub("\n", " ; ",temp);
temp<-gsub("ś", "s",temp);
temp<-gsub("ć", "c",temp);
temp<-gsub("ż", "z",temp);
temp<-gsub("ź", "z",temp);
temp<-gsub("ł", "l",temp);
temp<-gsub("ą", "a",temp);
temp<-gsub("ę", "e",temp);
temp<-gsub("ó", "o",temp);

sw<-gsub("\n", " ; ",sw);
sw<-gsub("ś", "s",sw);
sw<-gsub("ć", "c",sw);
sw<-gsub("ż", "z",sw);
sw<-gsub("ź", "z",sw);
sw<-gsub("ł", "l",sw);
sw<-gsub("ą", "a",sw);
sw<-gsub("ę", "e",sw);
sw<-gsub("ó", "o",sw);

data.frame(mgr$Treść, gsub(".*pozdr", " ",mgr$Treść))[4,]

iconv(temp, "latin2", "latin1")


text_df <- data_frame(line = 1:dim(mgr)[1], text = mgr$Treść)

text_df <- data_frame(line = 1, text = as.character( tr$Treść))


ttt <- text_df %>%
  unnest_tokens(word, text)

sw <- data_frame(line = 1, text =stopwords(language = "pl", source = "stopwords-iso"))

sw <- sw %>%
  unnest_tokens(word, text)

tidy_books <- ttt %>%
  anti_join(stopwords(language = "pl", source = "stopwords-iso"))

t4 <- ttt %>%
  anti_join(sw ,by=c('word'='word'))

library(dplyr)
library(tidytext)
library(stopwords)

text <- tr$Treść[2]
text_df <- data_frame( tr$Treść[2])

text_df %>%
  unnest_tokens( word, text)

text <- c("Because I could not stop for Death -",
          "He kindly stopped for me -",
          "The Carriage held but just Ourselves -",
          "and Immortality")

text_df <- data_frame(line = 1:4, text = text)

text_df <- data_frame(line = 1, text = as.character( tr$Treść))


ttt <- text_df %>%
  unnest_tokens(word, text)

sw <- data_frame(line = 1, text =stopwords(language = "pl", source = "stopwords-iso"))

sw <- sw %>%
  unnest_tokens(word, text)

tidy_books <- ttt %>%
anti_join(stopwords(language = "pl", source = "stopwords-iso"))

t4 <- ttt %>%
  anti_join(sw)

library(translateR)
res <- translate(content.vec = c("Hello world.", "This is a test."), 
                 google.api.key = 'lukaszjaniszewski1989',
                 source.lang = "en", 
                 target.lang = "de")

library(tm)

docs <- VCorpus(tr$Treść,readerControl = list(reader = reader(x), language = "pl")) 
docs<-tr$Treść
docs2=as.character( docs)
writeLines(as.character(docs[1]))


writeLines(as.character( mgr[as.character(mgr$Odebrano) %in% as.character( minn$x),'Treść'][5]))

removePunctuation(as.character( tr$Treść[2]))
docs2 <- tm_map(as.character(docs),removePunctuation) 

for (j in seq(docs)) {
  docs2[[j]] <- gsub("/", " ", docs[[j]])
  docs2[[j]] <- gsub("@", " ", docs[[j]])
  docs2[[j]] <- gsub("\\|", " ", docs[[j]])
  docs2[[j]] <- gsub("\u2028", " ", docs[[j]])}



