#' ---
#' title: Inauguration Speech Analysis
#' author: Carlos Blancarte
#' date: '`r format(Sys.Date(), "%B %d, %Y")`'
#' ---

#+ message = FALSE, warning = FALSE
# Libraries
library(tidyverse)
library(magrittr)
library(stringr)
library(stringi)
library(tidytext)
library(ggthemes)
library(scales)
library(knitr)

#' ## Read in the data
data <- read_csv("/users/carlosblancarte/Documents/code/inauguration_speeches/data/speech_data_clean.csv")

#' ## Analysis
#' 
#' I'm primarily interested in detecting and comparing sentiment between
#' presidential inauguration speeches. It's likely that these speeches tend
#' to be uplifting and more positive than most, but I believe there may be
#' notable differences between presidents. Especially given the contentious
#' 2000 and 2016 elections.  
#' 
#' I'd also like to visualize the sentiment throughout the narrative *time* of
#' the speech. Do speeches begin with very low sentiment and build up at the
#' end?  
#+ include=FALSE

#' ## Data Prep
#' 
#' Part of the reason for going through with this project was because I wanted
#' to play around with the `tidytext` package. The package contains many
#' useful functions for data wranglin' text that align with *tidy* principles.
#' We'll begin by restructuring the data using `unnest_tokens` to tokenize
#' sentences first, and single words afterward. 
#' 

# double unnest_tokens! 
dataLong <- unnest_tokens(
    tbl=data,
    output=sentence,
    input=content,
    to_lower=TRUE,
    token='sentences'
    ) %>%
  group_by(speech_date, president) %>%
  mutate(sentence_index = row_number(),
         total_sentences = max(sentence_index)) %>%
  ungroup() %>%
  unnest_tokens(
    tbl=.,
    output=word,
    input=sentence,
    token='words',
    drop=FALSE
  ) %>%
  ungroup() %>%
  select(speech_date, president, sentence, sentence_index, total_sentences, word)

#' ### removing stop words
#' 
#' If we want to remove stopword we can utilize the dataset `stop_words` and
#' `anti_join` it to our dataset. This will ensure that pesky filler words like
#' *too*, *then*, *the*, etc.
#' 

# load data
data('stop_words')

#+ include=FALSE
# totalSentences
totalSentences <- dataLong %>%
  select(speech_date, total_sentences) %>%
  distinct()

# create new dataframe which omits stopwords and count num words
dataClean <- dataLong %>%
  anti_join(., stop_words, by = 'word') %>%
  select(-total_sentences) %>%
  # fills in implicit missing values with 0
  complete(sentence_index, nesting(speech_date, president),
           fill=list(sentence=NA, word=NA))  %>%
  group_by(speech_date, president, sentence_index) %>%
  mutate(sentence_word_count = max(row_number())) %>%
  ungroup() %>%
  group_by(speech_date, president) %>%
  mutate(total_words = n()) %>%
  ungroup() %>%
  left_join(., totalSentences, by='speech_date') %>%
  group_by(speech_date) %>%
  filter(sentence_index <= total_sentences) %>%
  ungroup()

#' ## descriptive stats
#' 
#' Now we can take a look at a few high-level stats about the 
#' speeches as a whole:

#+ echo=F
avgWordsPerSentence <- dataClean %>%
  select(speech_date,
         president,
         sentence_index,
         sentence_word_count,
         total_sentences) %>%
  group_by(speech_date, president) %>%
  filter(sentence_index <= total_sentences) %>%
  summarise(`Average Words Per Sentence` = mean(sentence_word_count))

# speech length
dataClean %>%
  count(speech_date, president, total_sentences, total_words) %>%
  select(-n) %>%
  left_join(., avgWordsPerSentence, by = c('speech_date', 'president')) %>%
  kable()


#' Of the five inauguration speeches Barack Obama's first speech is by far
#' the longest with a total of 110 sentences and over 800 words (keep in mind
#' that this is AFTER removing stop words). Trump's speech is by far the
#' shortest with only 61 sentences and 327 words. In addition, the average
#' number of words per sentence appears to be significantly lower among the
#' Trump speech.  
#' 
#' Which words are used most frequently?

# count top10 highest frequency words
dataClean %>%
  count(speech_date, president, word, total_words) %>%
  ungroup() %>%
  arrange(speech_date, president, desc(n)) %>%
  group_by(speech_date, president) %>%
  mutate(percent_of_total_words = percent(n/total_words)) %>%
  slice(1:3) %>%
  kable()

#+ echo=FALSE
# plot it
freqWords <- dataClean %>%
  count(speech_date, president, word, total_words) %>%
  ungroup() %>%
  arrange(speech_date, desc(n)) %>%
  mutate(percent_of_total_words = n/total_words) %>%
  group_by(speech_date, president) %>%
  arrange(speech_date, desc(percent_of_total_words)) %>%
  slice(1:10) %>%
  ungroup() %>%
  arrange(speech_date, president, percent_of_total_words) %>%
  ungroup() %>%
  mutate(order= row_number())

  ggplot(freqWords, aes(x=order, y=percent_of_total_words)) +
  geom_col(fill='#3182bd') +
  facet_wrap(paste0('Speech Date: ', speech_date) ~ president, scales = 'free') +
  scale_x_continuous(breaks = freqWords$order, labels = freqWords$word, expand = c(0,0)) +
  scale_y_continuous(label=percent) +
  coord_flip() +
  theme_minimal() +
  xlab('Word') +
  ylab('% of Words in Speech') +
  ggtitle('Top 10 Word Freqeuencies', subtitle = '(as percentage of total words after removing stop words)')

#' No real surprise here... the president is addressing the nation in these
#' inauguration speeches so it's no surprise that words like `america`,
#' `freedom`, and `liberty` all make it to the top of the list. It's also
#' worth noting that relatively low frequency with which these words are used
#' - hovering around 1 to 3 percent.  
#' 
#' BUT, a couple interesing tidbits do pop up. For example, `spirit` and`crisis`
#' in Obama's 2009 address (amid all of the financial turmoil that the country
#' was embroiled in at the time). As for Trump, `jobs` was a major focus of his
#' campaign and thus continued to be a central focus of his inauguration
#' address.
#+ include=F
  
#' # Sentiment
#' 
#' Performing any kind of *sentiment* analysis can be tricky since we're
#' essentially dealing with a bag of words and assigning each word a score
#' without taking context or negation into account. Thusly, it's
#' important to review the output to ensure that the scoring is consistent
#' with what you would expect. We know that the results won't be perfect, but
#' at least we can still gain some sort of insight.  
#+ include=F
  
#' ## BING sentiment
#' 
#' We'll begin by using the `bing` lexicon to score our words. This lexicon
#' scores words as either being negative or positive. The implementation in the
#' `tidytext` package currently contains over 6,000 words!  
#' 
#' Using an `inner_join` we'll restrict our data to only include words found
#' in the lexicon and then count the number of positive and negative words
#' for each sentence in each speech. Given that these are official transcripts
#' i'm  not too concerned about misspelled words getting cut out of the analysis
#' from using an inner join. 

# create bing scored data
  dataBing <- dataClean %>%
  inner_join(get_sentiments('bing'), by='word') %>%
  count(president, speech_date, sentence_index, sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive - negative) %>%
  ungroup() %>%
  # fills in implicit missing values with 0
  complete(sentence_index, nesting(speech_date, president),
    fill=list(negative=0, positive=0, sentiment=0)) %>%
  arrange(speech_date, sentence_index) %>%
  left_join(., totalSentences, by = 'speech_date') %>%
  group_by(speech_date) %>%
  filter(sentence_index <= total_sentences) %>%
  ungroup()

#+ echo=F  
dataBing %>%
  ggplot(., aes(x=sentence_index, y=sentiment)) +
  geom_col(fill='#3182bd') +
  facet_wrap(paste0('Speech Date: ', speech_date) ~ president, scales='free_x') +
  theme_minimal() +
  xlab('Sentence Index') +
  ylab('Sentiment') +
  ggtitle('Inauguration Speech Sentiment', subtitle='(Bing Sentiment)')

#' The first thing that jumps out me is how less varied Trump's speech is. Not
#' only is it shorter than all the other speeches (which we've already
#' mentioned) but it doesn't pack the same punch in terms of sentiment. It seems
#' to be much more neutral when compared to the rest of the speeches. Positive
#' sentiment tops out at 3 while negative sentiment tops out at 2.

#+ echo=F
dataBing %>%
  group_by(speech_date, president) %>%
  summarise(sentiment_minimum = min(sentiment),
            sentiment_maximum = max(sentiment)) %>%
  kable()

#+ echo=F
# top3 obs with highest sentiment
bingMaxSentiment <- dataBing %>%
  group_by(speech_date, president) %>%
  top_n(3, sentiment) %>%
  arrange(speech_date, desc(sentiment)) %>%
  mutate(order = row_number()) %>%
  filter(order <= 3)

#+ echo=F
#bottom3 obs with most negative sentiment
bingMinSentiment <- dataBing %>%
  group_by(speech_date, president) %>%
  top_n(3, -sentiment) %>%
  arrange(speech_date, sentiment) %>%
  mutate(order = row_number()) %>%
  filter(order <= 3)

#' Let's take a look at some of the sentences with the min/max sentiment
#' scores for each president:
#+ include=FALSE

#' ### Most positive sentences
#+ echo = FALSE
dataLong %>%
  select(speech_date, sentence_index, sentence) %>%
  distinct() %>%
  inner_join(., bingMaxSentiment, by=c('speech_date', 'sentence_index')) %>%
  select(speech_date, president, sentiment, sentence) %>%
  arrange(speech_date, desc(sentiment)) %>%
kable()

#' I tend to agree that most of these sentences are genearlly positive in their
#' message. There's nothing that immediately stands out to as looking *off* or
#' like it doesn't belong.  
#' 
#'  We can also focus on the sentences that scored the highest on negative
#'  sentiment: 
#+ include=FALSE

#' ### Most negative sentences
#+ echo = FALSE
dataLong %>%
  select(speech_date, sentence_index, sentence) %>%
  distinct() %>%
  inner_join(., bingMinSentiment, by=c('speech_date', 'sentence_index')) %>%
  select(speech_date, president, sentiment, sentence) %>%
  arrange(speech_date, sentiment) %>%
kable()

#' these also look pretty good - comments about oppression, poverty, and
#' hardships all make an appearance. Overall, I feel pretty good about the
#' results.  
#' 
#' turning our attention back to the graph: There isn't a discernable pattern
#' prevalent in all of the speeches. Sure, most addresses tend to begin
#' positively and  build up a bit before turning negative (mostly in the form
#' of bringing up social and international issues) before ending on a generally
#' positive note. Again, the Trump speech stands out for it's neutrality. I
#' assume that a lot of thought goes into writing a presidental address and the
#' choice of wording is very deliberate. Perhaps his address was designed so it
#' wouldn't be soo contentious? Or perhaps it just so happens that the
#' vocabulary used was less likely to be included in the `bing` lexicon?  
#' 
#' Let's try normalzing the sentiment by number of words in each sentence. This
#' will give us a fairer comparison between presidents, assuring that the scale
#' is between negative 1 and positive 1. In essence, we're computing the
#' average sentement per word.
#' 

#+ echo=FALSE
# only among
wordsPerSentence <- dataClean %>%
  select(speech_date,
         president,
         sentence_index,
         sentence_word_count,
         total_sentences) %>%
  distinct() %>%
  group_by(speech_date, president) %>%
  filter(sentence_index <= total_sentences) %>%
  ungroup() %>%
  select(-total_sentences)


#+ echo=F  
dataBing %>%
  left_join(., wordsPerSentence, by=c('speech_date', 'president', 'sentence_index')) %>%
  mutate(normalized_sentiment = sentiment / sentence_word_count) %>%
  ggplot(., aes(x=sentence_index, y=normalized_sentiment)) +
  geom_col(fill='#3182bd') +
  facet_wrap(paste0('Speech Date: ', speech_date) ~ president, scales='free_x') +
  theme_minimal() +
  xlab('Sentence Index') +
  ylab('Sentiment') +
  ggtitle('Inauguration Speech (Normalized) Sentiment', subtitle='(Bing Sentiment)')

#' Well, well, well... what have we here?
#' #+ include=FALSE


#' ## scoring with nrc
#' 
#' I need to do a little more research, but using `nrc` for sentiment
#' scoring provides additional emotional measurements such as 
#' anger, anticipation, disgust, fear, joy, etc. 

# nrcScored <- dataLong %>%
#   anti_join(., stop_words, by='word') %>%
#   inner_join(get_sentiments('nrc'), by='word') %>%
#   count(president, speech_date, sentence_index, sentiment) %>%
#   spread(sentiment, n, fill=0) %>%
#   mutate(sentiment = positive - negative) %>%
#   #gather(emotion, score, anger:joy, sadness:trust) %>%
#   ungroup() %>%
#   complete(sentence_index, nesting(speech_date, president),
#            fill=list(anger=0,
#                      anticipation=0,
#                      disgust=0,
#                      fear=0,
#                      joy=0,
#                      negative=0,
#                      positive=0,
#                      sadness=0,
#                      surprise=0,
#                      trust=0,
#                      sentiment=0)) %>%
#   arrange(speech_date, sentence_index)
#   
# # plot Trumps speech
# nrcScored %>%
#   filter(grepl('trump', tolower(president))) %>%
#   gather(sentiment, score, anger:sentiment) %>%
#   ggplot(., aes(x=sentence_index, y=score)) +
#   geom_bar(stat='identity') +
#   facet_wrap(~sentiment)
# 
# # plot 'trust'
# nrcScored %>%
#   filter(emotion %in% 'trust') %>%
#   ggplot(., aes(x=sentence_n, y=score)) +
#   geom_bar(stat='identity') +
#   facet_wrap(speech_date ~ president)
