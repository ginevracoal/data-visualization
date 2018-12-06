library(feather)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(tidytext)

#load db and separate in different years, primary key given by year, number_edition, number_article
df=read_feather(path="my_data2.feather")
df$date=as.Date(df$date)
df$year=year(df$date)
df$ID <- seq.int(nrow(df))
df=filter(df,year!=2014)
#adding a single number for edition
df$unique_edition=df$number_edition+((df$year-2015)*51)
#retain articles talking about trump (mentioning his name at least once)
trump=df%>%
  filter(str_detect(article,"Donald Trump"))%>%
  add_column(flag="Trump")
#same for Clinton
clinton=df%>%
  filter(str_detect(article,"Hillary Clinton"))%>%
  add_column(flag="Clinton")
#merging
politicalDf=bind_rows(trump,clinton)
  
#plotting relative frequencies
ggplot(politicalDf,aes(x=date,fill=as.factor(flag)))+
  geom_histogram(position = "fill",binwidth = 20)+
  labs(x="Date (bins are 20 days)",
       y="Normalized frequency",
       title="Trump vs Clinton",
       fill="",
       caption = "Based on the online european editions of \"The Economist\"")+
  annotate(geom="text",x=as.Date("2015-06-16"),
           y=0.5,label="Start ")+
  annotate(geom="text",x=as.Date("2016-11-08"),
           y=0.5,label="End ")+
  theme_economist()
  

#Let's start with the sentyment analysis

#First some data cleaning

# Clean text from wrongly parsed html
politicalDf$article=gsub("Get 3 free articles per week, daily \n newsletters and more.","",politicalDf$article)

tidyPoliticalDf=politicalDf%>%
  unnest_tokens(word,article)

#removing stop words
data("stop_words")
tidyNoStopPoliticalDf=tidyPoliticalDf%>%
  anti_join(stop_words)

cw=tidyNoStopPoliticalDf%>%
  group_by(flag,year)%>%
  count(word,sort = TRUE)

#Plotting word frequency
tidyNoStopPoliticalDf%>%
  count(word, sort = TRUE) %>%
  filter(n > 2000) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#dataset from tidytext
sentiments

nrc=get_sentiments(lexicon = "nrc")
afinn=get_sentiments(lexicon = "afinn")
bing=get_sentiments(lexicon = "bing")
loughran=get_sentiments(lexicon = "loughran")

#let's search for different sentiments

politicalSentiment=inner_join(tidyNoStopPoliticalDf,bing,by = "word")

#I want to plot sentiment as function of time like before!
#plotting relative frequencies
politicalSentiment%>%
  ggplot(aes(x=date,fill=as.factor(sentiment)))+
  geom_histogram(position = "fill",binwidth = 90)+
  facet_grid("flag")+
  labs(x="Date (bins are 50 days)",
       y="Normalized frequency",
       title="Clinton vs Trump bing lexicon",
       fill="",
       caption = "Based on the online european editions of \"The Economist\"")+
  annotate(geom="text",x=as.Date("2015-06-16"),
           y=0.5,label="Start ")+
  annotate(geom="text",x=as.Date("2016-11-08"),
           y=0.5,label="End ")+
  theme_economist()+
  scale_fill_gdocs()


politicalSentiment=inner_join(tidyNoStopPoliticalDf,loughran,by = "word")
politicalSentiment%>%
  ggplot(aes(x=date,fill=as.factor(sentiment)))+
  geom_histogram(position = "fill",binwidth = 90)+
  facet_grid("flag")+
  labs(x="Date (bins are 50 days)",
       y="Normalized frequency",
       title="Clinton vs Trump Loughran lexicon",
       fill="",
       caption = "Based on the online european editions of \"The Economist\"")+
  annotate(geom="text",x=as.Date("2015-06-16"),
           y=0.5,label="Start ")+
  annotate(geom="text",x=as.Date("2016-11-08"),
           y=0.5,label="End ")+
  theme_economist()+
  scale_fill_gdocs()


politicalSentiment=inner_join(tidyNoStopPoliticalDf,nrc,by = "word")
politicalSentiment%>%
  ggplot(aes(x=date,fill=as.factor(sentiment)))+
  geom_histogram(position = "fill",binwidth = 90)+
  facet_grid("flag")+
  labs(x="Date (bins are 50 days)",
       y="Normalized frequency",
       title="Clinton vs Trump nrc lexicon",
       fill="",
       caption = "Based on the online european editions of \"The Economist\"")+
  annotate(geom="text",x=as.Date("2015-06-16"),
           y=0.5,label="Start ")+
  annotate(geom="text",x=as.Date("2016-11-08"),
           y=0.5,label="End ")+
  theme_economist()+
  scale_fill_gdocs()




