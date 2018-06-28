
# http://clip.ipipan.waw.pl/LRT pilish text analysis
# sentyment: http://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm
#https://stackoverflow.com/questions/46122591/a-lemmatizing-function-using-a-hash-dictionary-does-not-work-with-tm-package-in

# read and prepare data

yy=read.csv('raporty odp.csv', header = TRUE, sep=";",quote ='"')

t=as.data.frame( table(yy$Od))
library(plotly)
plot_ly(x=t$Var1[t$Var1!=adres1], y=t$Freq[t$Var1!=adres1], type = "bar")

#prepare data emails with respond 
tt = aggregate(yy$Temat.znormalizowany, by=list(yy$Temat.znormalizowany), length)
tt = tt[tt$x>1,]

tr=yy[yy$Temat.znormalizowany %in% tt$Group.1,]

tr<-tr[tr$Nazwa.nadawcy != adres1,]
tr<-tr[tr$Nazwa.nadawcy != adres2,]

mgr <- tr[order(tr$Temat.znormalizowany,tr$Odebrano),c('Nazwa.nadawcy','Temat.znormalizowany','Temat','Treść','Odebrano')]

mgr$Odebrano <- strptime(mgr$Odebrano, "%d-%m-%y %H:%M")

mgr<-mgr[order(mgr$Temat.znormalizowany,mgr$Odebrano),]

mgr1<-mgr#backup
#to lower
mgr$Temat<-tolower(mgr$Temat)

mgr$Treść<-tolower(mgr$Treść)

#remove not significant part of emails
mgr$Treść<- gsub("pozdr.*", " ",mgr$Treść)

mgr$Treść<- gsub("from.*", " ",mgr$Treść)

#remove polish letters
#mgr$Treść<-gsub("\n", " ; ",mgr$Treść);
mgr$Treść<-gsub("ś", "s",mgr$Treść);
mgr$Treść<-gsub("ć", "c",mgr$Treść);
mgr$Treść<-gsub("ż", "z",mgr$Treść);
mgr$Treść<-gsub("ź", "z",mgr$Treść);
mgr$Treść<-gsub("ń", "n",mgr$Treść);
mgr$Treść<-gsub("ł", "l",mgr$Treść);
mgr$Treść<-gsub("ą", "a",mgr$Treść);
mgr$Treść<-gsub("ę", "e",mgr$Treść);
mgr$Treść<-gsub("ó", "o",mgr$Treść);


library(dplyr)
library(tidytext)

text_df <- data_frame(line = 1:dim(mgr)[1], text = mgr$Treść)

ttt <- text_df %>%
  unnest_tokens(word, text)

# stop words

library(stopwords)

sw <- data_frame(line = 1, text =stopwords::stopwords(language = "pl", source = "stopwords-iso"))
sw <- data_frame(line = 1, text =stopwords::stopwords(language = "pl", source = "stopwords-iso"),encoding = 'UTF-8')

polish_stopwords <- strsplit(readLines('lem/polish_stopwords.txt', encoding = 'UTF-8'), ", ")[[1]]


sw <- sw %>%
  unnest_tokens(word, text)

sw$word<-gsub("ś", "s",sw$word);
sw$word<-gsub("ć", "c",sw$word);
sw$word<-gsub("ż", "z",sw$word);
sw$word<-gsub("ź", "z",sw$word);
sw$word<-gsub("ł", "l",sw$word);
sw$word<-gsub("ń", "n",sw$word);
sw$word<-gsub("ą", "a",sw$word);
sw$word<-gsub("ę", "e",sw$word);
sw$word<-gsub("ó", "o",sw$word);

tidy_books <- ttt %>%
  anti_join(stopwords(language = "pl", source = "stopwords-iso"))

t4 <- ttt %>%
  anti_join(sw ,by=c('word'='word'))









################ lemantyzacja

library(data.table)
library(dplyr)
library(magrittr)

polish_lematization <- fread('lem/polimorfologik-2.1.txt', data.table = FALSE, encoding = 'UTF-8')

lower_lema <- polish_lematization %>%
  set_colnames(c('to', 'from', 'no_need')) %>%
  select(-no_need) %>%
  mutate(to   = tolower(to),
         from = tolower(from))

splits_num <- 1:20
lower_lema_split <- 
  split(lower_lema, 
        f = 
          sample(
            splits_num,
            size = nrow(lower_lema),
            replace = TRUE))


#### stem ####


text = 
  mgr$Treść %>%
  lapply(iconv, "UTF-8", "ASCII", sub="") %>%
  lapply(tolower) %>%
  lapply(tm::removeWords, c("\"", "ul.", "godz.", polish_stopwords, 0:3000)) %>% 
  stri_extract_all_words()

dict <- data_frame(
  words   = unlist(text),
  article = mapply(rep,
                   1:length(text),
                   lapply(text, length)) %>%
    unlist()
)
for(i in splits_num) {
  dict$words <- dict$words %>%
    plyr::mapvalues(warn_missing = FALSE,
                    from = lower_lema_split[[i]]$from,
                    to   = lower_lema_split[[i]]$to)
}



library(tm)
library(pbapply)
library(stringi)




###


arts_words <- dict %>% 
  group_by(article) %>%
  mutate(words_in_art = paste0(words, collapse = " ")) %>% 
  select(article, words_in_art) %>%
  unique() %>% 
  ungroup() %>%
  as_data_frame()


#clustering
#https://www.youtube.com/watch?v=-2Koi-caSZw
########


corpus <- Corpus(VectorSource(arts_words$words_in_art[arts_words$words_in_art!="NA"]))

cleanset <- tm_map(corpus, removeWords, stopwords::stopwords(language = "pl", source = "stopwords-iso"))

dtm <- DocumentTermMatrix(cleanset)

dtm_<- weightTfIdf(dtm)

m<-as.matrix(dtm_)

rownames(m) <- 1:nrow(m)

norm_e <- function(m)
  m/apply(m, 1, function(x) sum(x^2)^.5)

m_nor <- norm_e(m)

res<-kmeans(m_nor,5)

findFreqTerms(dtm_[res$cluster==5,],1)

#####################
####
############## Word cloud  ###################

#https://www.r-bloggers.com/awesome-twitter-word-clouds-in-r/
#https://www.littlemissdata.com/blog/wordclouds

require(devtools)
install_github("lchiffon/wordcloud2")

library(tidytext)
library(dplyr)
library(stringr)
library(rtweet)
library(wordcloud2)

#Unnest the words - code via Tidy Text
hmtTable <- data_frame(line = 1:dim(mgr)[1], text = mgr$Treść)

hmtTable <-arts_words

hmtTable <- hmtTable %>% 
  unnest_tokens(word, words_in_art)

#remove stop words - aka typically very common words such as "the", "of" etc
data(stop_words)

hmtTable <- hmtTable %>%
  anti_join(sw ,by=c('word'='text'))

hmtTable <- hmtTable %>%
  count(word, sort = TRUE) 
hmtTable 

#Remove other nonsense words
hmtTable <-hmtTable %>%
  filter(!word %in% c('t.co', 'https','http', 'handmaidstale', "handmaid's", 'season', 'episode', 'de', 'handmaidsonhulu',  'tvtime', 
                      'watched', 'watching', 'watch', 'la', "it's", 'el', 'en', 'tv',
                      'je', 'ep', 'week', 'amp'))

wordcloud2(hmtTable, size=0.3, color=rep_len( redPalette, nrow(hmtTable) ) )

#better wordcloud nie działa moze przez brak updatu

#Create Palette
redPalette <- c("#5c1010", "#6f0000", "#560d0d", "#c30101", "#940000")
redPalette

#Download images for plotting
url = "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/twitter_wordcloud/handmaiden.jpeg"
handmaiden <- "handmaiden.jpeg"
download.file(url, handmaiden) # download file
url = "https://raw.githubusercontent.com/lgellis/MiscTutorial/master/twitter_wordcloud/resistance.jpeg"
resistance <- "resistance.jpeg"
download.file(url, resistance) # download file

#plots
wordcloud2(hmtTable, size=0.3, figPath = handmaiden, color=rep_len( redPalette, nrow(hmtTable) ) )
wordcloud2(hmtTable, size=1.6, figPath = resistance, color=rep_len( redPalette, nrow(summary4) ) )
wordcloud2(hmtTable, size=1.6, figPath = resistance, color="#B20000")



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

############################
#############

#############
##################################


