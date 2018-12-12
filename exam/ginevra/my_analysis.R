library(feather)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(tidytext)

# set current dir 
setwd("/home/ginevracoal/MEGA/Università/DSSC/semester_3/data-visualization/exam")

# DATA PREPROCESSING

#load db and separate in different years, primary key given by year, number_edition, number_article
df=read_feather(path="my_data2.feather")
df$date=as.Date(df$date)
df$year=year(df$date)
df$ID <- seq.int(nrow(df))
#df=filter(df,year!=2014) # now removing any year now
#adding a single number for edition
df$unique_edition=df$number_edition+((df$year-2015)*51)

# DATA CLEANING

# Clean text from wrongly parsed html
df$article=gsub("Get 3 free articles per week, daily \n newsletters and more.","",df$article)

# remove rows having empty string as an article
# sum(df[!(df$article==""), ]) # there are 3 empty articles we have to remove
new_df <- df[!(df$article==""), ]
df <- new_df

# SENTIMENT ANALYSIS

# split article column into single words
df=df%>%unnest_tokens(word,article)

#removing stop words
data("stop_words")
df = df%>% anti_join(stop_words)

# selecting only negative sentiments

# CTRL + SHIFT + C to comment multiple rows
# cw=tidyNoStopPoliticalDf%>%
#   group_by(flag,year)%>%
#   count(word,sort = TRUE)
# 
# #Plotting word frequency
# tidyNoStopPoliticalDf%>%
#   count(word, sort = TRUE) %>%
#   filter(n > 2000) %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n)) +
#   geom_col() +
#   xlab(NULL) +
#   coord_flip()

#dataset from tidytext
sentiments

# sentiments expressed in words
nrc=get_sentiments(lexicon = "nrc")

# scores from -5 to 5
afinn=get_sentiments(lexicon = "afinn")

# positive and negative
bing=get_sentiments(lexicon = "bing")
loughran=get_sentiments(lexicon = "loughran")

#let's filter all negative sentiments
negatives_nrc <- nrc %>%
  filter(sentiment %in% c("anger", "disgust", "fear", "negative", "sadness"))
negatives_afinn <- afinn %>% filter(score < -1)
negatives_bing_loug <- bing %>% 
  left_join(loughran, by = "sentiment") %>% 
  filter(sentiment == "negative")

negative_words <- unique(c(unique(negatives_nrc$word),
                    unique(negatives_afinn$word),
                    unique(negatives_bing_loug$word.y)))

# counting the most frequent negative words
negative_df <- df %>% 
  filter(word %in% negative_words)

# top 10 negative words
top_negative_df <- negative_df %>% count(word, sort = TRUE) 
top_negative_words <- unique(top_negative_df[1:8,]$word)

# Now I want to investigate how the object of negative 
# sentiment changed over time

# now I want to plot the whole dataset, 
# with the same label for all positive words

# I have to separate negative sentiments 
# from positive and neutral
binary_df <- df

# positive or neutral
binary_df$word[!(binary_df$word %in% negative_words)] <- "positive or neutral"

# negative but not top 10 negative
binary_df$word[binary_df$word %in% setdiff(negative_words, top_negative_words)] <- "other negative"

# there are 12 possible words now!
unique(binary_df$word)

# showing only negative sentiments
binary_df %>% filter(binary_df$word %in% negative_words | binary_df$word == "other negative") %>% 
  ggplot(aes(x=date, fill=as.factor(word)))+
  geom_histogram(position = "fill", binwidth = 15)+
  #facet_grid("flag")+
  labs(x="Date (15 days bins)",
       y="Frequency in the articles",
       title="Distribution of negative sentiment in 'The Economist' articles",
       subtitle = "Top 10 words associated to negative sentiments",
       fill="",
       caption = "Based on the online european editions of \"The Economist\"")+
  # annotate(geom="text",x=as.Date(min(top10_negativeDf$date)),
  #          y=0.5,label=min(top10_negativeDf$date))+
  # annotate(geom="text",x=as.Date(max(top10_negativeDf$date)),
  #          y=0.5,label="End ")+
  theme_dark()
# scale_fill

# It makes sense also to only look at the top 10 words

negative_df %>%
  filter(word %in% top_negative_words) %>% 
  ggplot(aes(x=date,fill=as.factor(word)))+
  geom_histogram(position = "fill",binwidth = 15)+
  #facet_grid("flag")+
  labs(x="Date (15 days bins)",
       y="Frequency in the articles",
       title="Distribution of negative sentiment in 'The Economist' articles",
       subtitle = "Top 10 words associated to negative sentiments",
       fill="",
       caption = "Based on the online european editions of \"The Economist\"")+
  geom_vline(aes(xintercept = as.numeric(as.Date("2015-09-01"))), col = "red")+
  scale_fill_brewer(palette="Set2")

  
  # annotate(geom="text",x=as.Date(min(top10_negativeDf$date)),
  #          y=0.5,label=min(top10_negativeDf$date))+
  # annotate(geom="text",x=as.Date(max(top10_negativeDf$date)),
  #          y=0.5,label="End ")+
  #theme_dark() # scale_fill_gdocs()


# How are the peaks related to specific events?

