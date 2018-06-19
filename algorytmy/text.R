# sentyment: http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#https://stackoverflow.com/questions/46122591/a-lemmatizing-function-using-a-hash-dictionary-does-not-work-with-tm-package-in


yy=read.csv('raporty odp.csv', header = TRUE, sep=";",quote ='"')

plot_ly(x=t$Var1[t$Var1!="PortalCognosPI@bnpparibas.pl"], y=t$Freq[t$Var1!="PortalCognosPI@bnpparibas.pl"], type = "bar")
 
unique(yy$Od)
t=as.data.frame( table(yy$Od))

aggregate(yy$Od, by=list(yy$Od), length)

tt = aggregate(yy$Temat.znormalizowany, by=list(yy$Temat.znormalizowany), length)
tt = tt[tt$x>1,]

tr=yy[yy$Temat.znormalizowany %in% tt$Group.1,]

tr<-tr[tr$Nazwa.nadawcy != 'PortalCognosPI@bnpparibas.pl',]
tr<-tr[tr$Nazwa.nadawcy != 'etlservicetest@pietl-tst01.fortisbank.com.pl',]

mgr <- tr[order(tr$Temat.znormalizowany,tr$Odebrano),c('Nazwa.nadawcy','Temat.znormalizowany','Temat','Treść','Odebrano')]

mgr$Odebrano <- strptime(mgr$Odebrano, "%d-%m-%y %H:%M")

mgr<-mgr[order(mgr$Temat.znormalizowany,mgr$Odebrano),]

minn<-aggregate(mgr$Odebrano, by=list(mgr$Temat.znormalizowany), FUN = min)[2]

strptime( aggregate(mgr$Odebrano, by=list(mgr$Temat.znormalizowany), FUN = min)[2], "%d-%m-%y %H:%M")

temp <- tolower(mgr[as.character(mgr$Odebrano) %in% as.character( minn$x),][1,4])
mgr1<-mgr
mgr$Temat<-tolower(mgr$Temat)

mgr$Treść<-tolower(mgr$Treść)

mgr$Treść<-gsub("\n", " ; ",mgr$Treść);
mgr$Treść<-gsub("ś", "s",mgr$Treść);
mgr$Treść<-gsub("ć", "c",mgr$Treść);
mgr$Treść<-gsub("ż", "z",mgr$Treść);
mgr$Treść<-gsub("ź", "z",mgr$Treść);
mgr$Treść<-gsub("ń", "n",mgr$Treść);
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

sw$word<-gsub("\n", " ; ",sw$word);
sw$word<-gsub("ś", "s",sw$word);
sw$word<-gsub("ć", "c",sw$word);
sw$word<-gsub("ż", "z",sw$word);
sw$word<-gsub("ź", "z",sw$word);
sw$word<-gsub("ł", "l",sw$word);
sw$word<-gsub("ń", "n",sw$word);
sw$word<-gsub("ą", "a",sw$word);
sw$word<-gsub("ę", "e",sw$word);
sw$word<-gsub("ó", "o",sw$word);



mgr$Treść<- gsub("pozdr.*", " ",mgr$Treść)

mgr$Treść<- gsub("from.*", " ",mgr$Treść)

iconv(temp, "latin2", "latin1")


text_df <- data_frame(line = 1:dim(mgr)[1], text = mgr$Treść)

text_df <- data_frame(line = 1, text = as.character( tr$Treść))


ttt <- text_df %>%
  unnest_tokens(word, text)

sw <- data_frame(line = 1, text =stopwords::stopwords(language = "pl", source = "stopwords-iso"))

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

text_df$text[text_df$line %in% t4$line[t4$word=='wiem']]

gsub(" ;.*"," " ,gsub(".*wiem", " ",text_df$text[text_df$line %in% t4$line[t4$word=='wiem']]))

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
###############

#clustering
#https://www.youtube.com/watch?v=-2Koi-caSZw
########
mgr1<-mgr

mgr<-yy

mgr$Temat<-tolower(mgr$Temat)

mgr$Treść<-tolower(mgr$Treść)

mgr$Treść<-gsub("\n", " ; ",mgr$Treść);
mgr$Treść<-gsub("ś", "s",mgr$Treść);
mgr$Treść<-gsub("ć", "c",mgr$Treść);
mgr$Treść<-gsub("ż", "z",mgr$Treść);
mgr$Treść<-gsub("ź", "z",mgr$Treść);
mgr$Treść<-gsub("ń", "n",mgr$Treść);
mgr$Treść<-gsub("ł", "l",mgr$Treść);
mgr$Treść<-gsub("ą", "a",mgr$Treść);
mgr$Treść<-gsub("ę", "e",mgr$Treść);
mgr$Treść<-gsub("ó", "o",mgr$Treść);


writeLines(as.character(mgr$Treść[1]))

library(stopwords)
sw <- data_frame(line = 1, text =stopwords(language = "pl", source = "stopwords-iso"))

sw$word<-gsub("\n", " ; ",sw$word);
sw$word<-gsub("ś", "s",sw$word);
sw$word<-gsub("ć", "c",sw$word);
sw$word<-gsub("ż", "z",sw$word);
sw$word<-gsub("ź", "z",sw$word);
sw$word<-gsub("ł", "l",sw$word);
sw$word<-gsub("ń", "n",sw$word);
sw$word<-gsub("ą", "a",sw$word);
sw$word<-gsub("ę", "e",sw$word);
sw$word<-gsub("ó", "o",sw$word);



mgr$Treść<- gsub("pozdr.*", " ",mgr$Treść)

mgr$Treść<- gsub("from.*", " ",mgr$Treść)

mgr$Treść<- gsub(".*czesc", " ",mgr$Treść)

iconv(temp, "latin2", "latin1")


text_df <- data_frame(line = 1:dim(mgr)[1], text = mgr$Treść)


ttt <- text_df %>%
  unnest_tokens(word, text)

sw <- data_frame(line = 1, text =stopwords::stopwords(language = "pl", source = "stopwords-iso"))

sw <- sw %>%
  unnest_tokens(word, text)

tidy_books <- ttt %>%
  anti_join(stopwords(language = "pl", source = "stopwords-iso"))

t4 <- ttt %>%
  anti_join(sw ,by=c('word'='word'))

as.matrix.tabular( t4)

library(tm)

corpus <- Corpus(VectorSource(mgr$Treść))

cleanset <- tm_map(corpus, removeWords, stopwords::stopwords(language = "pl", source = "stopwords-iso"))

dtm <- DocumentTermMatrix(cleanset)

dtm_<- weightTfIdf(dtm)

m<-as.matrix(dtm_)

rownames(m) <- 1:nrow(m)

norm_e <- function(m)
  m/apply(m, 1, function(x) sum(x^2)^.5)

m_nor <- norm_e(m)

res<-kmeans(na.omit( m_nor),2)

findFreqTerms(dtm_[res$cluster==2,],2)


########################

############

######  CHORD DIAGRAM    ########

#https://jokergoo.github.io/circlize_book/book/the-chorddiagram-function.html


library(circlize)
#


set.seed(999)
mat = matrix(sample(18, 18), 3, 6) 
rownames(mat) = paste0("S", 1:3)
colnames(mat) = paste0("E", 1:6)
mat

df = data.frame(from = rep(rownames(mat), times = ncol(mat)),
                to = rep(colnames(mat), each = nrow(mat)),
                value = as.vector(mat),
                stringsAsFactors = FALSE)
chordDiagram(mat)

unlist(strsplit(as.character( yy$Do), ";"))
cbind(yy$Od, strsplit(as.character( yy$Do), ";"))

sp<-strsplit(as.character( yy$Do), ";")

library(tidyr)

lis<-data.frame(yy$Od,sp)

myDataFrame <- data.frame(row = c(1:507))

myDataFrame$col1 <- yy$Od
myDataFrame$col2 <- sp

chord<-unnest(myDataFrame, col2)[,2:3]
ccc<-table(chord)


chordDiagram(chord[1:200,],annotationTrack = "grid", preAllocateTracks = 1)

circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name,cex=0.4, facing = "clockwise", niceFacing = TRUE, adj = c(-0.1, 0.6))
  circos.axis(h = "top", labels.cex = 0.5, major.tick.percentage = 0.2, sector.index = sector.name, track.index = 2)
}, bg.border = NA)



