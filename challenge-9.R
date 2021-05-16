# Challenge 9
# P. Kessling, 29.04.2021

# Heute wollen wir das Kommunikationsverhalten von Twitter-User*innen zu einem
# bestimmtem Thema untersuchen. Suche dafür ein aktuelles Thema und nutze die Twitter-API.
# Durchsuche Twitter nach dem Thema, beachte dabei, dass wir zu mindestens 1500 Tweet
# brauchen.
# Für die einhundert aktivsten Nutzer bestimmen wir das Kommunikations verhalten genauer.
# Lade für diese Nutzer jeweils die letzten 1000 Tweets, falls so viele vorhanden sind
# und Werte visuell nach der Zeit aus.
# ACHTUNG: das Laden der Daten kann eine ganze Weile in Anspruch nehmen.
# * Erstelle eine Visualisierung des gesamten Kommunikatinosaufkommen, aufgelöst in 5 Minuten-Intervallen.
# * Erstelle eine Visualisierung der aktivsten Nutzer.
# * Erstelle eine Visualisierung der von diesen Nutzern am häufigsten genutzen Hashtags.

# load libraries
library(rtweet)
library(tidyverse)
library(stopwords)
library(ggplot2)
library(plotly)
library(ggwordcloud)

# get token
token <- get_token()
token


#(Musste neues Thema Laden, da ich Urpsrüngliche Tweets zum Thema "Neubauer" nicht gespeichert hatte.)
# (Nur noch Timelines waren vom alten Thema vorhanden. Timelines zum neuen Thema "Rundfunkbeitrag" ließen sich nicht laden.)

# get tweets
tweets_zum_rundfunkbeitrag <- search_tweets("Rundfunkbeitrag", n = 2000)

# save as rds 
#write_rds(tweets_zum_rundfunkbeitrag, "rundfunkbeitrag.rds")
#tweets_zum_rundfunkbeitrag <- readRDS("tweets_zum_rundfunkbeitrag.rds")

# filter for most active users
users <- tweets_zum_rundfunkbeitrag %>% 
  group_by(screen_name) %>% 
  mutate(tweets_per_user = n())

users <- users %>% 
  group_by(screen_name, tweets_per_user) %>% 
  summarise() %>% 
  arrange(desc(tweets_per_user))

aktivste_user <- users %>% 
  ungroup(screen_name) %>% 
  slice(1:100)

# save as rds
#write_rds(aktivste_user, "aktivste_user.rds")
#aktivste_user <- readRDS("aktivste_user.RDS")

# get their recent tweets
timelines <- get_timelines(aktivste_user$screen_name, check = F, n = 1000, token = token, retryonratelimit = T)

#timelines <- timelines %>% saveRDS("timelines.rds")
timelines <- readRDS("timelines.rds")

# create plots
# 1st tweet : 2021-05-16 16:38
# last tweet : 2021-05-16 21:03

# calculate bins
bins = ((21-16)*60 + 3 - 38) / 5

# 1st plot: Kommunikationsaufkommen
ggplot(tweets_zum_rundfunkbeitrag) +
  geom_freqpoly(aes(created_at, color = is_retweet), position = "stack", bins = bins)+
  labs(
    x = "Zeit",
    y = "Anzahl Tweets",
    color = "Retweet", 
    title = "Kommunikationsaufkommen auf Twitter zum Thema Rundfunkbeitrag",
    subtitle = "am 16.05.2021 in 5-Minuten-Intervallen"
  )+
  theme_minimal()

# 2nd plot: Aktivste User

# identify most active users on specific topic "Neubauer"
tweets_filtered <- timelines %>% 
  filter(str_detect(text, paste ("Neubauer")))

aktiv_neubauer <- tweets_filtered %>% 
  group_by(screen_name) %>% 
  summarise(tweets_total = n()) %>%  
  arrange(desc(tweets_total))

# bar chart
plot_ly(data = aktiv_neubauer,
        x = ~reorder(screen_name, -tweets_total),
        marker=list(color=toRGB("lightblue")),
        y = ~tweets_total,
        type = "bar") %>% 
  layout(title= "Die 100 aktivsten Twitter-User zum Thema Neubauer", 
         xaxis = list(title = "Username"),
         yaxis = list(title = "Anzahl Tweets"))

# 3rd plot: Häufigste Hashtags

# define hashtag pattern and extract from text
hashtag_pat <- "#[a-zA-Z0-9_-ー\\.]+"
hashtag <- str_extract_all(timelines$text, hashtag_pat)

# unlist hashtags
hashtag_word <- unlist(hashtag)
hashtag_word

# create data frame
hashtag_frame<- data.frame(hashtag_word)
hashtag_frame<- hashtag_frame %>% 
  group_by(hashtag_word) %>% 
  summarise(anzahl=n()) %>% 
  arrange(desc(anzahl)) 

# create wordcloud
cloud <-hashtag_frame %>% 
  slice(1:20) %>% 
  ggplot(aes(label=hashtag_word, size = anzahl)) +
  geom_text_wordcloud(shape="diamond") +
  theme_light() +
  scale_size_area(max_size = 20)

cloud

