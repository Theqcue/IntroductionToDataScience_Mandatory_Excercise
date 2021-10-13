library("tidyverse")
library("readxl")
library("skimr")
library("dplyr")
library("stringr")
library("ggplot2")
library("readtext")
library("tidytext") 
library("text2vec")
library("wordcloud2") 


speakers <- read_excel("speakers.xlsx")
load("ungd.Rdata")

ungd_speakers<-left_join(ungd_files, speakers, by = c("session" = "Session", "country" = "ISO", "year" ="Year"))

#Summarize your data by meaningfully applying grouping, summaries, and transformations when needed. In your own words,
#offer a brief summary of some interesting insight(s) from the data. It is more than enough to offer 2-3 summaries
#you deem most useful and that demonstrate that you can apply the functions introduced in the course.

#Finding the countries who have attended the United nation general debate least amount of times and the first year they attended and the latest year. 

NumberOfTimesCountriesSpoke<- ungd_speakers %>% group_by(country) %>%  count() %>% arrange(n)

Countries_First_year <- ungd_speakers %>% group_by(country,year) %>%  count()
minYear <- aggregate(year ~ country, data = Countries_First_year, FUN = min) %>% rename(yearMin = year)
maxYear <- aggregate(year ~ country, data = Countries_First_year, FUN = max) %>% rename(yearMax = year)

TimesCountriesSpokenWithFirstYear <-left_join(NumberOfTimesCountriesSpoke, minYear, by = c("country" = "country"))
TimesCountriesSpoken_With_First_And_lase_Year <-left_join(TimesCountriesSpokenWithFirstYear, maxYear, by = c("country" = "country"))

TimesCountriesSpoken_With_First_And_lase_Year_Not_2020 <- TimesCountriesSpoken_With_First_And_lase_Year %>% filter(yearMax < 2020)%>% head(20)

#Finding out how many countries attended per year

Countries_Attended_Per_Year <- ungd_speakers %>% group_by(year) %>%  count()  %>% rename(Number_Of_Countries = n) %>% ungroup()

#Countries attending the geberal debate per year
Countries_Attended_Per_Year_Plot <- Countries_Attended_Per_Year %>%
  ggplot( aes(x=year, y= Number_Of_Countries)) +
  geom_line() +
  geom_point() +
  xlab("Year") + 
  ylab("Number of Countyres") +
  labs(title="Number of countries attending the United Nations General Debate", 
       caption="Source: United Nations General Debate Corpus")


#Summarize and compare any metric of speech length. You can compare across years or between countries, or
#both, it is entirely up to you.

Average_No_of_word_In_Speech_Per_Year <- ungd_speakers %>% mutate(length = lengths(strsplit(text, '\\S+'))) %>% select(-text)%>% group_by(year)%>% summarize(avg_length = mean(length))

Average_No_of_word_In_Speech_Per_country <- ungd_speakers %>% mutate(length = lengths(strsplit(text, '\\S+'))) %>% select(-text)%>% group_by(country)%>% summarize(avg_length = mean(length))

#Plot for average number of words in years:
Average_No_of_word_In_Speech_Per_Year_plot <- Average_No_of_word_In_Speech_Per_Year %>%
  ggplot( aes(x=year, y=avg_length)) +
  geom_line() +
  geom_point() +
  xlab("Year") + 
  ylab("Average length of speech per word") +
  labs(title="Average number of words per speech \nfor countries attending \nthe United Nations General Debate per year", 
       caption="Source: United Nations General Debate Corpus")

#Compare and discuss word usage differences between presidents and all other type of speakers.

Average_Word_Uasge_post <- ungd_speakers %>% mutate(length = lengths(strsplit(text, '\\S+'))) %>% 
  select(-text)%>% 
  group_by(post)%>% 
  summarize(avg_length = mean(length), number_of_obs = n())

Average_Word_Uasge_post_more_then_10 <- Average_Word_Uasge_post %>% filter(number_of_obs >=10) %>% arrange(desc(avg_length))

Average_Word_Uasge_post_more_then_10_No_NA <-Average_Word_Uasge_post_more_then_10 %>% drop_na()

#Plot - word usage with more then 10 observations

Average_Word_Uasge_post_more_then_10_No_NA_plot <- ggplot(data=Average_Word_Uasge_post_more_then_10_No_NA, aes(y=post, x=avg_length)) + 
  geom_bar(stat="identity", width=.5, fill="tomato3") + 
  xlab("Average number of words used") + 
  ylab("Average no of words in speech") +
  labs(title="Average nubmer of words used by different posts\n at the United Nations General Debate", 
       caption="Source: United Nations General Debate Corpus") + 
  #theme(axis.text.x = element_text(angle=90, vjust=0)) +
  geom_text(aes(label= round(avg_length,2)), position=position_dodge(width=0.1), vjust=0.35,hjust=-0.25)+
  scale_x_continuous(expand = c(0, 0, 0.3, 0))

Average_Word_Uasge_post_more_then_10_No_NA_plot
#Sentimental analysis

#Use any sentiment dictionary and any approach (tidytext or quanteda) to compare the sentiment of the
#speeches throughout time or between countries.

getsentiment <- get_sentiments("bing")

sentimentCountry_Most_positive <- ungd_speakers %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(country, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(polarity = (positive - negative)/(positive + negative))%>% arrange(desc(polarity)) %>% head(7)

sentimentCountry_Most_negative <- ungd_speakers %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(country, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(polarity = (positive - negative)/(positive + negative))%>% arrange(polarity) %>% head(7)


sentimentYear <- ungd_speakers %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>%
  count(year, sentiment) %>%
  pivot_wider(names_from = sentiment, values_from = n, values_fill = 0) %>% 
  mutate(polarity = (positive - negative)/(positive + negative)) %>% arrange(desc(polarity))


sentimentPositiveWord <- ungd_speakers %>%  
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% filter(sentiment == "positive") %>%
  count(word) %>%arrange(desc(n))

sentimentNegatveWord <- ungd_speakers %>% 
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("bing")) %>% filter(sentiment == "negative") %>%
  count(word) %>%arrange(desc(n))

WordCloudPositive <- wordcloud2(data=sentimentPositiveWord, size=1.2)
WordCloudNegative<- wordcloud2(data=sentimentNegatveWord, size=0.9)



#Sentiment polarity over timer
Sentiment_analysis_years <- sentimentYear %>%
  ggplot( aes(x=year, y=polarity)) +
  geom_line() +
  geom_point() +
  xlab("Year") + 
  ylab("Sentiment polarity of speeches") +
  labs(title="Analysis of Sentiment polarity of speeches thoughout the years", 
       caption="Source: United Nations General Debate Corpus")


#Write out CVS files of table to insert into report
write.table(TimesCountriesSpokenWithFirstYear_Not_2020, file = "TimesCountriesSpoken_With_First_And_lase_Year_Not_2020.txt", sep = ",", quote = FALSE, row.names = F)
write.table(sentimentCountry_Most_positive, file = "sentimentCountry_Most_positive.txt", sep = ",", quote = FALSE, row.names = F)
write.table(sentimentCountry_Most_negative, file = "sentimentCountry_Most_negative.txt", sep = ",", quote = FALSE, row.names = F)


#Write out PDF of figures to insert into report
ggsave(filename="Figures/Countries_Attended_Per_Year_Plot.pdf", plot=Countries_Attended_Per_Year_Plot, width=7, height=6)
ggsave(filename="Figures/Average_No_of_word_In_Speech_Per_Year_plot.pdf", plot=Average_No_of_word_In_Speech_Per_Year_plot, width=7, height=6)
ggsave(filename="Figures/Average_Word_Uasge_post_more_then_10_No_NA_plot.pdf", plot=Average_Word_Uasge_post_more_then_10_No_NA_plot, width=10, height=8)
ggsave(plot=Sentiment_analysis_years, filename="Figures/Sentiment_analysis_years.pdf",  width=8, height=6)






