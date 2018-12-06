library(feather)
library(tidyverse)
library(lubridate)
library(ggthemes)

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
  
  





