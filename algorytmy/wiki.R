#https://github.com/mkearney/wiki_data

library('data.table')

untar('https://dumps.wikimedia.org/other/pageviews/2018/2018-12/pageviews-20181231-230000.gz')


t<-read.table(gzfile(fread(paste("https://dumps.wikimedia.org/other/pageviews/2019/2019-01/pageviews-20190128-230000.gz"))))[1]


file_name<-"pageviews-20190124-120000.gz"


tcv<-read.table(gzfile("clickstream-plwiki-2018-12.tsv.gz"), fill = TRUE)

te<-read.table(gzfile("clickstream-enwiki-2018-12.tsv.gz"), fill = TRUE)

source("read_wiki_data.R")

pwe<-read.table(gzfile(file_name), fill = TRUE)

x <- wiki_data(file_name, skip = 100, n_max = 100)

unique( pwe$V1[pwe$V1 %like% 'app'])

i=0
j=0
while (i==0) {
  x <- wiki_data(file_name, skip = j, n_max = 1)
  if(x[1]!="pl"){
    i=1
  }
  j=100000+j
}

min_en=1303001
max_en=3603001

min_pl=6003001
max_pl=6303001
x <- wiki_data(file_name, skip = j-50000, n_max = 1)

byDayTime[order(-byDayTime$count),][1:100,]
