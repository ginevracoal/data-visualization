#Paragraph analysis

library(feather)
library(tidyverse)
library(lubridate)
library(ggthemes)
library(tidytext)
library(wordcloud)
#load db and separate in different years, primary key given by year, number_edition, number_article
df=read_feather(path="my_data2_v1.feather")
df$date=as.Date(df$date)
df$year=year(df$date)
df$ID <- seq.int(nrow(df))
df=filter(df,year!=2014)
#adding a single number for edition
df$unique_edition=df$number_edition+((df$year-2015)*51)
#cleaning html parsing problems
df$article=gsub("Get 3 free articles per week, daily \n newsletters and more.","",df$article)
df$article=gsub("Upgrade your inbox and get our Daily Dispatch and Editor's Picks.","",df$article)
#Clean sections column
df$section=gsub("print-edition icon Print edition \\|"," ",df$section)
df$section=gsub("^\\s+","",df$section)
df$location=gsub("\\| ","",df$location)
unique(df$section)


n_distinct(df$section) #26 is ok
n_distinct(df$location) #way too much
#Length of articles
df=df%>%
  mutate(Nwords=str_count(article," ")-1)

#filter very short articles
df = df %>%
  filter(Nwords>250)

#estraiamo i paragrafi

trump=df%>%
  unnest_tokens(paragraph,article, token = stringr::str_split, pattern = "\\.")%>%
  filter(str_detect(paragraph,"trump"))%>%
  add_column(flag="Trump")
clinton=df%>%
  unnest_tokens(paragraph,article, token = stringr::str_split, pattern = "\\.")%>%
  filter(str_detect(paragraph,"clinton"))%>%
  add_column(flag="Clinton")
brexit=df%>%
  unnest_tokens(paragraph,article, token = stringr::str_split, pattern = "\\.")%>%
  filter(str_detect(paragraph,"brexit"))%>%
  add_column(flag="Brexit")
test_base=df%>%
  unnest_tokens(paragraph,article, token = stringr::str_split, pattern = "\\.")%>%
  add_column(flag="test")


#sent anal
test_base1=sample_n(test_base,20000)
politicalDf=bind_rows(trump,clinton,brexit,test_base1)

tidyPoliticalDf=politicalDf%>%
  unnest_tokens(word,paragraph)
tidyNoStopPoliticalDf=tidyPoliticalDf%>%
  anti_join(stop_words)


#Afinn mean value
afinn=get_sentiments(lexicon = "afinn")
politicalSentiment=inner_join(tidyNoStopPoliticalDf,afinn,by = "word")
politicalSentiment=politicalSentiment%>%
  #group_by(ID)%>%
  mutate(mean=median(score))




politicalSentiment%>%
  filter(flag=="Trump"|flag=="Clinton"| flag=="test")%>%
  ggplot(aes(x=date,y=score,color=flag))+
  #geom_point()+
  geom_smooth(method = "loess", size = 1.5)+
  #geom_smooth()+
  labs(x="Date",
       y="Normalized frequency",
       title="Trump Afinn lexicon only paragraphs loess",
       fill="",
       caption = "Based on the online european editions of \"The Economist\"")+
  annotate(geom="text",x=as.Date("2015-06-16"),
           y=0.5,label="Start ")+
  annotate(geom="text",x=as.Date("2016-11-08"),
           y=0.5,label="End ")+
  theme_economist()+
  scale_fill_economist()+
  scale_color_gdocs()


#nrc 
nrc=get_sentiments(lexicon = "nrc")
politicalSentiment=inner_join(tidyNoStopPoliticalDf,nrc,by = "word")
politicalSentiment%>%
  ggplot(aes(x=date,fill=as.factor(sentiment)))+
  geom_histogram(position = "fill",binwidth = 30)+
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

#bing
bing=get_sentiments(lexicon = "bing")

politicalSentiment=inner_join(tidyNoStopPoliticalDf,bing,by = "word")
politicalSentiment%>%
  
  ggplot(aes(x=date,fill=as.factor(sentiment)))+
  geom_histogram(position = "fill",binwidth = 30)+
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

#loughrain
loughran=get_sentiments(lexicon = "loughran")
politicalSentiment=inner_join(tidyNoStopPoliticalDf,loughran,by = "word")
politicalSentiment%>%
  ggplot(aes(x=date,fill=as.factor(sentiment)))+
  geom_histogram(position = "fill",binwidth = 30)+
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

#
