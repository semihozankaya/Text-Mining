library(tidyverse)
library(tidytext)
library(stringr)
library(DT)
library(igraph)
library(ggraph)
library(topicmodels)
library(tm)
library(wordcloud)
library(h2o)
library(textdata)

df <- read_csv("https://raw.githubusercontent.com/BobAdamsEE/SouthParkData/master/All-seasons.csv")

top_chars <- df %>% group_by(Character) %>% tally(sort = TRUE)

## Most active characters
ggplot(head(top_chars,10), aes(x = reorder(Character, n), y = n)) +
  geom_bar(stat='identity',colour="white", fill = "Dark Grey") +
  geom_text(aes(x = Character, y = 1, label = paste0("(",n,")",sep="")),
  hjust=0, vjust=.5, size = 4, colour = 'black',
  fontface = 'bold') +
  labs(x = 'Character', y = 'Sentence Count') +
  coord_flip() + 
  theme_bw()
## Top words
top_words <- df %>%
  unnest_tokens(word, Line) %>%
  filter(!word %in% stop_words$word) %>%
  count(word,sort = TRUE) %>%
  ungroup() %>%
  mutate(Word = factor(word, levels = rev(unique(word)))) %>% select(Word, Count = n)

ggplot(head(top_words, 20), aes(x = Word,y = Count)) +
  geom_bar(stat='identity',colour="white", fill = "Dark Grey") +
  geom_text(aes(x = Word, y = 1, label = paste0("(",Count,")",sep="")),
  hjust=0, vjust=.5, size = 4, colour = 'black',
  fontface = 'bold') +
  labs(x = 'Word', y = 'Word Count') + 
  coord_flip() + 
  theme_bw()


### Word cloud

top_words %>% head(50) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))


### Use parts of speech to determine different words grouped by it being an adjective, noun and so on.
## Adjective
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Adjective") %>%
  ungroup()  %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Adverb  
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Adverb") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Conjunction
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Conjunction") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Definite Article
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Definite Article") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Interjection
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Interjection") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Noun
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Noun") %>%
  ungroup()  %>%
  with(wordcloud(Word, Count, max.words = 150,colors=brewer.pal(8, "Dark2")))
## Noun Phrase
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Noun Phrase") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Plural
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Plural") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Preposition
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Preposition") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Pronoun
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Pronoun") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))
## Verb (intransitive)
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Verb (intransitive)") %>%
  ungroup()  %>%
  with(wordcloud(Word, Count, max.words = 100,colors=brewer.pal(8, "Dark2")))
## Verb (transitive)
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Verb (transitive)") %>%
  ungroup()  %>%
  with(wordcloud(Word, Count, max.words = 100,colors=brewer.pal(8, "Dark2")))
## Verb (usu participle)
top_words %>% merge(parts_of_speech, by.x = "Word", by.y = "word")  %>%
  filter(pos == "Verb (usu participle)") %>%
  ungroup()  %>%
  head(100) %>%
  with(wordcloud(Word, Count, max.words = 50,colors=brewer.pal(5, "Dark2")))

## TF IDF

#Get the Top 20 Characters

top10_chars <- head(top_chars, 10)$Character

Words <- df %>%
  unnest_tokens(word, Line) %>%
  count(Character, word, sort = TRUE) %>%
  ungroup()

Words[which(Words$word == "mkay"), 2] <- "m'kay"
Words[which(Words$word == "hm'kay"), 2] <- "m'kay"
Words[which(Words$word == "uhkay"), 2] <- "m'kay"

total_words <- Words %>% 
  group_by(Character) %>% 
  summarize(total = sum(n))
  
Words <- left_join(Words, total_words)

WordsFull <- Words %>%
  filter(!is.na(Character)) %>%
  bind_tf_idf(word, Character, n)

Words_top10 <- Words %>% filter( Character %in% top10_chars) %>%
  bind_tf_idf(word, Character, n)

plot_td_idf <- Words_top10 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_td_idf %>% 
  top_n(15) %>%
  ggplot(aes(word, tf_idf, fill = Character)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() + scale_fill_brewer(type = "qual") + 
  theme_bw()
## There seems to be a problem with Kenny's lines. When taking a better look, we can see that some
## of the words are coming from the episode 'Titties and Dragons' where Kenny plays an anime princess
## and helps Sony win a war against Microsoft. The sentences doesn't make much sense and all consist of 
## made up words.  They are quite repetitive and make the td-idf measures get misleading results.

## Words like quack, te and io is also misleading. io and te comes from a spanish-like singing he makes
## at 4th season episode 3. quack is a repetitive word from the sentence quack-quack. This is misleading
## as well.

## Let's add them to our stop word lists so that we can have more insightfull results.

my_stop_words <- stop_words

mydata <- df %>% filter(Character == "Kenny")
kennys_stop_words <- filter(df, Character == 'Kenny')[which(grepl("Kenni", mydata$Line)),] %>% 
  unnest_tokens(word, Line) %>% select(word)

my_stop_words <- append(my_stop_words$word, kennys_stop_words[[1]], after = nrow(my_stop_words))
my_stop_words <- append(my_stop_words, c("quack", "io","te"), after = length(my_stop_words))



Words <- df %>%
  unnest_tokens(word, Line) %>% filter(!word %in% my_stop_words) %>%
  count(Character, word, sort = TRUE) %>%
  ungroup()

Words[which(Words$word == "mkay"), 2] <- "m'kay"
Words[which(Words$word == "hm'kay"), 2] <- "m'kay"
Words[which(Words$word == "uhkay"), 2] <- "m'kay"

total_words <- Words %>% 
  group_by(Character) %>% 
  summarize(total = sum(n))

Words <- left_join(Words, total_words)

WordsFull <- Words %>%
  filter(!is.na(Character)) %>%
  bind_tf_idf(word, Character, n)

Words_top10 <- Words %>% filter( Character %in% top10_chars) %>%
  bind_tf_idf(word, Character, n)

plot_td_idf <- Words_top10 %>%
  arrange(desc(tf_idf)) %>%
  mutate(word = factor(word, levels = rev(unique(word))))

plot_td_idf %>% 
  top_n(15) %>%
  ggplot(aes(word, tf_idf, fill = Character)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip() + scale_fill_brewer(type = "qual") + 
  theme_bw()

## Now it looks better. m'kay, woohoo or loo still doesn't make much sense but the characters are well known for
## these lines so I decided not to adjust them. Same goes for the names, i.e. Sharon, Stanley and Randy. 
## Sharon and Randy seems to communicate with each other by addressing their names first. 

bigrams <- df %>%
    unnest_tokens(bigram, Line, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% my_stop_words,
           !word2 %in% my_stop_words) %>%
    count(word1, word2, sort = TRUE)
  
bigrams <- bigrams %>% filter(word1 != word2)


bigram_graph <- bigrams %>%
  filter(n > 25) %>%
  graph_from_data_frame()

set.seed(09052021)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

## Sentiment Analysis

sentiments <- Words %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    group_by(Character) %>%
    summarize(score = sum(value * n) / sum(n)) 

top20_chars <- head(top_chars, 20)$Character

sentiments <- sentiments %>% filter(Character %in% top20_chars)
sentiments <- sentiments %>% arrange(-score)
  
sentiments %>%
    mutate(Name = reorder(Character, score)) %>%
    ggplot(aes(Name, score, fill = score > 0)) +
    geom_col(show.legend = TRUE) +
    coord_flip() +
    ylab("Average sentiment score") + theme_bw()

## Sentiments by words

contributions <- df %>%
  unnest_tokens(word, Line) %>%
  count(Character, word, sort = TRUE) %>%
  ungroup() %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  group_by(word) %>%
  summarize(occurences = n(),
            contribution = sum(value))



contributions %>%
  top_n(20, abs(contribution)) %>%
  mutate(word = reorder(word, contribution)) %>%
  ggplot(aes(word, contribution, fill = contribution > 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() + theme_bw()

#Topic Modelling



#########################################################################################

mydf <- df %>%
  select(Character,Line)

corpus <- Corpus(VectorSource(mydf$Line))

#Pre-process data
LowIDF <- WordsFull %>%
  arrange((idf)) %>%
  select(word,idf)

Low_IDF <- unique(LowIDF$word)

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removeWords, Low_IDF[1:50])
corpus <- tm_map(corpus, stemDocument)


dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms

dtm <- removeSparseTerms(dtm, 0.997)

# Create data frame

labeledTerms <- as.data.frame(as.matrix(dtm))
labeledTerms <- labeledTerms[rowSums(abs(labeledTerms)) != 0,]

##############################################################################

#LDA Modelling Starts

###############################################################################



# set a seed so that the output of the model is predictable

spark_lda <- LDA(labeledTerms, k = 2, control = list(seed = 09052021))



#The tidytext package provides this method for extracting the per-topic-per-word probabilities, 

# called   β  (“beta”), from the model

spark_topics <- tidy(spark_lda, matrix = "beta")

spark_top_terms <- spark_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)

spark_top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() + theme_bw()


###### predictive modeling

corpus <- Corpus(VectorSource(df$Line))

# Pre-process data

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms

dtm <- removeSparseTerms(dtm, 0.997)


# Create data frame

labeledTerms <- as.data.frame(as.matrix(dtm))

my_df <- df %>%
  mutate(is_Cartman = 0)

my_df <- my_df %>%
  mutate(is_Cartman=replace(is_Cartman, Character == 'Cartman', 1)) %>%
  as.data.frame()



labeledTerms$is_Cartman = as.factor(my_df$is_Cartman)

## Preparing the features for the XGBoost Model

features <- colnames(labeledTerms)



for (f in features) {
  
  if ((class(labeledTerms[[f]])=="factor") || (class(labeledTerms[[f]])=="character")) {
    levels <- unique(labeledTerms[[f]])
    labeledTerms[[f]] <- as.numeric(factor(labeledTerms[[f]], levels=levels))
  }
  
}

## Creating the XGBoost Model

labeledTerms$is_Cartman = as.factor(labeledTerms$is_Cartman)


h2o.init()

h2o_data <- as.h2o(labeledTerms)
splitted_data <- h2o.splitFrame(h2o_data, ratios = c(0.2), seed = 20210905)

data_train <- splitted_data[[1]]
data_test <- splitted_data[[2]]

y <- "is_Cartman"
X <- setdiff(names(h2o_data), c(y))

# For modeling
foresting_params <- list(
  ntrees = c(10, 25), 
  mtries = c(10, 15), 
  sample_rate = c(0.2),
  max_depth = c(10, 20)
)

forester_grid <- h2o.grid(
  "randomForest", x = X, y = y,
  training_frame = splitted_data[[1]],
  grid_id = "forester1",
  nfolds = 5,
  seed = 20210905,
  hyper_params = foresting_params
)
best_forest <- h2o.getModel(
  h2o.getGrid(forester_grid@grid_id, "auc")@model_ids[[sum(lengths(h2o.getGrid(forester_grid@grid_id, "auc")@model_ids))]]
)
h2o.auc(h2o.performance(best_forest))
h2o.auc(h2o.performance(best_forest, data_test))

plot(h2o.performance(best_forest, xval = TRUE), type = "roc")
h2o.varimp_plot(best_forest)

## Lets do Stan


corpus <- Corpus(VectorSource(df$Line))

# Pre-process data

corpus <- tm_map(corpus, tolower)
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, stemDocument)
dtm <- DocumentTermMatrix(corpus)

# Remove sparse terms

dtm <- removeSparseTerms(dtm, 0.997)


# Create data frame

labeledTerms <- as.data.frame(as.matrix(dtm))

my_df <- df %>%
  mutate(is_Stan = 0)

my_df <- my_df %>%
  mutate(is_Stan =replace(is_Stan, Character == 'Stan', 1)) %>%
  as.data.frame()



labeledTerms$is_Stan = as.factor(my_df$is_Stan)

## Preparing the features for the XGBoost Model

features <- colnames(labeledTerms)



for (f in features) {
  
  if ((class(labeledTerms[[f]])=="factor") || (class(labeledTerms[[f]])=="character")) {
    levels <- unique(labeledTerms[[f]])
    labeledTerms[[f]] <- as.numeric(factor(labeledTerms[[f]], levels=levels))
  }
  
}

## Creating the XGBoost Model

labeledTerms$is_Stan = as.factor(labeledTerms$is_Stan)



h2o_data <- as.h2o(labeledTerms)
splitted_data <- h2o.splitFrame(h2o_data, ratios = c(0.2), seed = 20210905)

data_train <- splitted_data[[1]]
data_test <- splitted_data[[2]]

y <- "is_Stan"
X <- setdiff(names(h2o_data), c(y))

# For modeling
foresting_params <- list(
  ntrees = c(10, 25), 
  mtries = c(10, 15), 
  sample_rate = c(0.2),
  max_depth = c(10, 20)
)

forester_grid_stan <- h2o.grid(
  "randomForest", x = X, y = y,
  training_frame = splitted_data[[1]],
  grid_id = "forester1_stan",
  nfolds = 5,
  seed = 20210905,
  hyper_params = foresting_params
)
best_forest_stan <- h2o.getModel(
  h2o.getGrid(forester_grid_stan@grid_id, "auc")@model_ids[[sum(lengths(h2o.getGrid(forester_grid_stan@grid_id, "auc")@model_ids))]]
)
h2o.auc(h2o.performance(best_forest_stan))
h2o.auc(h2o.performance(best_forest_stan, data_test))

plot(h2o.performance(best_forest_stan, xval = TRUE), type = "roc")
h2o.varimp_plot(best_forest_stan)
