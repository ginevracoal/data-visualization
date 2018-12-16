# Here I am adapting Doma's code to what I wanted to extract
# from the data.
# My idea is to analyze the top negative sentiments in the 
# whole sentiment analysis of our dataset (since it's 
# impossible to show them all) and see how these sentiments 
# where distributed on different object of interest, based 
# on historical events.

library(feather)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(tidytext)

top = 8

# set current dir 
setwd("/home/ginevracoal/MEGA/Università/DSSC/semester_3/data-visualization/exam")

# DATA PREPROCESSING

#load db and separate in different years, primary key given by year, number_edition, number_article
data=read_feather(path="my_data2.feather")
data$date=as.Date(data$date)
data$year=year(data$date)
data$ID <- seq.int(nrow(data))
#df=filter(df,year!=2014) # now removing any year now
#adding a single number for edition
data$unique_edition=data$number_edition+((data$year-2015)*51)

# DATA CLEANING

# Clean text from wrongly parsed html
#data$article=gsub("Get 3 free articles per week, daily \n newsletters and more.","",data$article)

# remove rows having empty string as an article
# sum(df[!(df$article==""), ]) # there are 3 empty articles we have to remove
data <- data[!(data$article==""), ]

# SENTIMENT ANALYSIS

# split article column into single words
df=data%>%unnest_tokens(word,article)

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

# #########################################################
# # negative sentiments
# 
# #let's filter all negative sentiments
# negatives_nrc <- nrc %>%
#   filter(sentiment %in% c("anger", "disgust", "fear", "negative", "sadness"))
# negatives_afinn <- afinn %>% filter(score < -1)
# negatives_bing_loug <- bing %>% 
#   left_join(loughran, by = "sentiment") %>% 
#   filter(sentiment == "negative")
# 
# negative_words <- unique(c(unique(negatives_nrc$word),
#                     unique(negatives_afinn$word),
#                     unique(negatives_bing_loug$word.y)))
# 
# # counting the most frequent negative words
# negative_df <- df %>% 
#   filter(word %in% negative_words)
# 
# # top negative words
# negative_count <- negative_df %>% count(word, sort = TRUE) 
# top_negative_words <- unique(negative_count[1:top,]$word)
# 
# # Now I want to investigate how the object of negative 
# # sentiment changed over time
# 
# # now I want to plot the whole dataset, 
# # with the same label for all positive words
# 
# # I have to separate negative sentiments 
# # from positive and neutral
# #triplet_df <- df
# 
# #df$sentim_categ <- ""
# 
# # positive or neutral
# #df$sentim_categ[!(df$word %in% negative_words)] <- "positive or neutral"
# 
# # negative but not top 10 negative
# #df$sentim_categ[df$word %in% setdiff(negative_words, top_negative_words)] <- "other negative"
# 
# # top negative
# #df$sentim_categ[df$word %in% top_negative_words] <- "top negative"
# 
# # there are 3 possible sentiment categories now
# #unique(df$sentim_categ)
# 
# 
# 
# # showing only negative sentiments
# df %>% 
#   filter(df$sentim_categ != "positive or neutral") %>% 
#   ggplot(aes(x=date, fill=as.factor(sentim_categ)))+
#   geom_histogram(position = "fill", binwidth = 15)+
#   #facet_grid("flag")+
#   labs(x="Date (15 days bins)",
#        y="Frequency in the articles",
#        title="Distribution of negative sentiment in 'The Economist' articles",
#        subtitle = "Top 10 words associated to negative sentiments",
#        fill="",
#        caption = "Based on the online european editions of \"The Economist\"")+
#   # annotate(geom="text",x=as.Date(min(top10_negativeDf$date)),
#   #          y=0.5,label=min(top10_negativeDf$date))+
#   # annotate(geom="text",x=as.Date(max(top10_negativeDf$date)),
#   #          y=0.5,label="End ")+
#   theme_dark()
# # scale_fill
# 
# # It makes sense also to only look at the top 10 words
# 
# # potrei colorare le righe con il colore del sentimento 
# # predominante e metterci accanto una copertina dell'economist
# # (direttamente nella presentazione, fuori dal plot)
# negative_df %>%
#   filter(word %in% top_negative_words) %>% 
#   ggplot(aes(x=date, fill=as.factor(word)))+
#   geom_histogram(position = "fill",binwidth = 7)+
#   #facet_grid("flag")+
#   labs(x="Date (7 days bins)",
#        y="Frequency in the articles",
#        title="Distribution of negative sentiment in 'The Economist' articles",
#        subtitle = "Top 10 words associated to negative sentiments",
#        fill="",
#        caption = "Based on the online european editions of \"The Economist\"")+
#   geom_vline(aes(xintercept = as.numeric(as.Date("2015-08-10"))), col = )+
#   geom_vline(aes(xintercept = as.numeric(as.Date("2015-12-22"))), col = "red")+
#   geom_vline(aes(xintercept = as.numeric(as.Date("2016-12-01"))), col = "red")+
#   # https://www.bbc.com/news/world-asia-41291650
#   geom_vline(aes(xintercept = as.numeric(as.Date("2017-09-16"))), col = "red")+
#   # https://www.theguardian.com/politics/2017/mar/29/theresa-may-triggers-article-50-with-warning-of-consequences-for-uk
#   geom_vline(aes(xintercept = as.numeric(as.Date("2017-03-29"))), col = "red")+
#   
#   # https://www.nbcnews.com/news/world/robert-mugabe-world-s-oldest-leader-finally-resigns-one-week-n822896
#   geom_vline(aes(xintercept = as.numeric(as.Date("2017-11-21"))), col = "red")+
#   scale_fill_brewer(palette="Set2")
#   
#   # annotate(geom="text",x=as.Date(min(top10_negativeDf$date)),
#   #          y=0.5,label=min(top10_negativeDf$date))+
#   # annotate(geom="text",x=as.Date(max(top10_negativeDf$date)),
#   #          y=0.5,label="End ")+
#   #theme_dark() # scale_fill_gdocs()

#######################################################

# let's filter all extreme positive and negative sentiments

# loughran negative or positive or litigious
loughran_neg <- loughran %>% 
  filter(sentiment %in% c("negative", "litigious"))
loughran_pos <- loughran %>% 
  filter(sentiment == "positive")

# afinn <-1 and >1
afinn_neg <- afinn %>% filter(score < -2)
afinn_pos <- afinn %>% filter(score >2)

extreme_words <- c(loughran_neg$word, loughran_pos$word,afinn_neg$word,afinn_pos$word)

# I only want the dataset to have the extreme sentiment words
extreme_df <- df %>% 
  filter(word %in% extreme_words)

# initializing sentiment column
extreme_df$sentim_categ <- NA

# positive words
extreme_df$sentim_categ[extreme_df$word %in% c(loughran_pos$word, afinn_pos$word)] <- 1

# negative words
extreme_df$sentim_categ[extreme_df$word %in% c(loughran_neg$word, afinn_neg$word)] <- -1

# there are 3 possible sentiment categories now
unique(extreme_df$sentim_categ)


#######################################################

# How are the peaks related to specific events?
# I am not interested in the top words but in the 
# significant differences from the mean value!

# avg occurrence of all words in the month
avg_daily_freq <- extreme_df %>% 
  group_by("month"=month(date), word) %>% 
  count(word) %>% 
  summarise(avg_daily_freq = mean(n)/30)

# then I consider all the differences from the mean of
# current month
daily_counts <- extreme_df %>% 
  group_by(date, word, sentim_categ) %>% 
  count(word) %>% 
  rename(freq=n) %>% 
  mutate(month=month(date))

# finally, for each day I calculate the word having 
# the highest distance from the monthly mean
daily_extremes <- daily_counts %>%  
  full_join(avg_daily_freq, by=c("month","word")) %>% 
  mutate(std_freq=freq-avg_daily_freq) %>% 
  group_by(date) %>% 
  filter(std_freq==max(std_freq)) %>%
  arrange(desc(std_freq))

daily_extremes


# Now i want to create a column indicating the elements 
# I want to label in my plot
num_labels = 20
  
max=daily_extremes$std_freq[num_labels]

daily_extremes <- daily_extremes %>% 
  mutate(extr_values=as.logical(std_freq > max)) %>% 
  filter(std_freq >= 0) 

daily_extremes$word[daily_extremes$extr_values == FALSE] <- NA
daily_extremes <- daily_extremes[!duplicated(daily_extremes[c('date')]),]

#write.csv(daily_extremes, file = "daily_extremes.csv")
#png(filename="ginevra/word_frequency.png", width = 1200, height= 600)

# Daily word occurrences
# We want to investigate the significant changes in the occurrence of
# specific words. 

daily_extremes %>%
  group_by(sentim_categ) %>% 
  ggplot(aes(x=date, weight=std_freq, fill=word))+
  geom_histogram(binwidth = 1, labels = word)+
  #facet_grid("flag")+
  labs(x="Date",
       y="Frequency increment (%)",
       title="Words having the most significant increment in frequency",
       subtitle = "Calculated on monthly averages",
       fill="",
       caption = "Source: online european editions of \"The Economist\"")+
  geom_text(aes(x=date, y=std_freq+1, label=word, color=word))+
  theme(legend.position="none")+
  #scale_y_continuous(labels = function(x) paste0(x, "%"))+
  theme(panel.background = element_blank())
  #scale_x_date(date_breaks = "6 months",date_labels = "%Y-%b") 
 #+ geom_vline(aes(xintercept = as.numeric(as.Date("2017-10-30"))))

#dev.off()
