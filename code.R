library(httr)
library(jsonlite)
library(tokenizers)
library(stopwords)
library(SnowballC) # for stemming
library(tm)
library(hunspell)
library(tidyverse)
library(corpus)
library(textstem)
library(quanteda)
library(data.table)
library(tidytext)
library(dplyr)
library(Matrix)
library(reshape2)
library(igraph)
library(stringr)
library(words)

data("words")
print(words)


api_key = "12d2886a-95d4-4f98-80c7-9d62c20655d2"
query1 = "Terror"
query2 = "Floods"

# To get terror articles
current_page = 1
article_count = 0
terror_articles = c()
while (article_count < 100)
{
  baseurl = "http://content.guardianapis.com/search"
  params = list("api-key" = api_key, "page" = current_page, "page-size"= '200', "q"= query1, "show-fields" = "bodyText")
  request = GET(baseurl, query = params)
  json_data = fromJSON(httr::content(request, as="text"))

  article_range = 1:200
  for (x in article_range)
  {
    if ((grepl("terror", json_data$response$results$webTitle[x]) | grepl("Terror", json_data$response$results$webTitle[x])))
    {
      if(grepl("Pakistan", json_data$response$results$fields$bodyText[x]))
      {
        if (article_count < 100)
        {
          article_count = article_count + 1
          terror_articles = append(terror_articles, paste(json_data$response$results$webTitle[x]))
        }
      }
    } 
  }
  
  current_page = current_page + 1
}

# To get floods articles
current_page = 1
article_count = 0
flood_articles = c()
while (article_count < 100)
{
  baseurl = "http://content.guardianapis.com/search"
  params = list("api-key" = api_key, "page" = current_page, "page-size"= '200', "q"= query2, "show-fields" = "bodyText")
  request = GET(baseurl, query = params)
  json_data = fromJSON(httr::content(request, as="text"))
  
  article_range = 1:200
  for (x in article_range)
  {
    if ((grepl("floods", json_data$response$results$webTitle[x]) | grepl("Floods", json_data$response$results$webTitle[x])))
    {
      if (grepl("Pakistan", json_data$response$results$fields$bodyText[x]))
      {
        if (article_count < 100)
        {
          article_count = article_count + 1
          flood_articles = append(flood_articles, paste(json_data$response$results$webTitle[x]))
        }
      }
    } 
  }
  
  current_page = current_page + 1
}

# making the co-occurence matrix

corpus = Corpus(VectorSource(as.vector(flood_articles)))
corpus = Corpus(VectorSource(as.vector(terror_articles)))

t = tokenize_words(paste(unlist(terror_articles), collapse = ''), strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords::stopwords("en"))
t = lemmatize_words(t[[1]])
t = unique(t)

extrawords <- c('the', 'can', 'get', 'got', 'can', "'s", "-", "s")
# https://stackoverflow.com/questions/30994194/quotes-and-hyphens-not-removed-by-tm-package-functions-while-cleaning-corpus
removeSpecialChars <- function(x) gsub("[^a-zA-Z0-9 ]","",x)
corpusclean <- corpus %>%
  tm::tm_map(removePunctuation) %>%
  tm::tm_map(removeNumbers) %>%
  tm::tm_map(tolower) %>%
  tm::tm_map(removeWords, stopwords()) %>%
  tm::tm_map(removeWords, extrawords) %>%
  tm::tm_map(removeSpecialChars)

s = 1

for (j in corpusclean$content)
{
  t = tokenize_words(j, strip_punct = TRUE, strip_numeric = TRUE, stopwords = stopwords::stopwords("en"))
  t = lemmatize_words(t[[1]])
  t = unique(t)
  
  new_range = 1:length(t)
  listof = c()
  
  for (x in new_range)
  {
    if(is_empty(hunspell_stem(t[x])[[1]])) # no stemming occurred
    {
      listof = append(listof, t[x])
    }
    else
    {
      if (is.na(hunspell_stem(t[x])[[1]][2])) # NA 
      {
        listof = append(listof, hunspell_stem(t[x])[[1]][1])
      }
      else
      {
        if(length(hunspell_stem(t[x])[[1]][[2]]) < 2) # if word is being stemmed to a letter
        {
          listof = append(listof, t[x])
        }
        else
        {
          listof = append(listof, hunspell_stem(t[x])[[1]][2])
        }
      }
    }
  }  
  listof = lemmatize_words(listof)
  listof = unique(listof)
  
  w = c()
  for (x in new_range)
  {
    if (wordStem(listof[x]) %like% "i$")
    {
      w = append(w, listof[x])
    }
    else
    {
      wi = wordStem(listof[x])
      wi = removePunctuation(wi)
      w = append(w, wi)
    }
  }
  
  co = c()
  for (x in new_range)
  {
    if (is.na(stemCompletion(w[x], dictionary = t[[1]], type = "shortest")))
    {
      co = append(co, listof[x][1])
    }
    else
    {
      co = append(co, stemCompletion(w[x], dictionary = t[[1]], type = "shortest")[[1]][1])
    }
  }
  
  co = lemmatize_words(co)
  co = unique(co) 
  j = paste(unlist(co), collapse = " ")
  corpusclean[[s]] = j
  s = s + 1
}

# https://ladal.edu.au/coll.html
corpus_DTM <- DocumentTermMatrix(corpusclean, control = list(wordLengths = c(2, Inf)))
corpusdtm <- Matrix::sparseMatrix(i = corpus_DTM$i, j = corpus_DTM$j, 
                                   x = corpus_DTM$v, 
                                   dims = c(corpus_DTM$nrow, corpus_DTM$ncol),
                                   dimnames = dimnames(corpus_DTM))
# calculate co-occurrence counts
coocurrences <- t(corpusdtm) %*% corpusdtm
# convert into matrix
collocates <- as.matrix(coocurrences)

# make edge list
obj = melt(collocates)
colnames(obj) = c("word1", "word2", "frequency")
print(obj[obj$word1 != obj$word2 & obj$frequency > 1,])

# https://stackoverflow.com/questions/22756392/deleting-reversed-duplicates-with-r
c_sort_collapse <- function(...){
  c(...) %>% 
    sort() %>% 
    str_c(collapse = ".")
}

v = obj %>% 
  mutate(x_y = map2_chr(obj$word1, obj$word2, c_sort_collapse)) %>% 
  distinct(x_y, .keep_all = TRUE) %>% 
  select(-x_y)

edge_list = v[v$word1 != v$word2 & v$frequency > 1,]
print(edge_list)


# graph
g = graph_from_data_frame(edge_list, directed = F)
E(g)$width = edge_list$frequency
plot(g, vertex.size= 0.01, layout = layout.auto, vertex.label = V(g)$names, vertex.shape = "none")
print(degree(g))

