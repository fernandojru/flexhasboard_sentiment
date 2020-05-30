library(rtweet)
library(tidyverse)
library(ggmap)
library(leaflet)
library(tm)
library(wordcloud2)

#======================================Query Twitter API===============================

## Authenticate via web browser, token request on user behalf
#This will retrive the tweets as data frame 

tweets <- search_tweets(
  "matrimonioigualitario", n = 2000, include_rts = FALSE
)

save(tweets, file = "matrimonio_igualitario_26-midnight.RData") #Save the data

#Retrive user level data
user_info <- lookup_users(unique(tweets$user_id)) 


##======================================GOOGLE API========================================================

register_google(key="KEY PROVIDED BY GOOGLE, NOT SHOWED FOR PRIVACY") #Key provided by google
geocode("5	Altstadt-Lehel, Múnich ") #Test, it should retrive lat and lon

load("matrimonio_igualitario_26-midnight.RData") #Load the tweets

#Obtain the unique locations, this is done to avoid requesting more than one repeated location, as the API usage is limitated
Unique_tweet_locations<- data.frame(Name_Location=unique(tweets$location)) 
Unique_tweet_locations$Name_Location<-as.character(Unique_tweet_locations$Name_Location)

#Clean a couple of locations 
Unique_tweet_locations$Name_Location[Unique_tweet_locations$Name_Location=="Costa Rica"]<-"San Jose, Costa Rica"
Unique_tweet_locations$Name_Location[Unique_tweet_locations$Name_Location=="Nicaragua "]<-"Managua, Nicaragua"
is.character(Unique_tweet_locations$Name_Location) #Must be True

#Test the first 5 locations:
#Test<-data.frame(Name_Location=Unique_tweet_locations[1:5,])
#Test$Name_Location<-as.character(Test$Name_Location)
#is.character(Test$Name_Location)
  
#Test<-Test %>% mutate_geocode(Name_Location)

#Make the request and wait...
Unique_tweet_locations <- Unique_tweet_locations %>% mutate_geocode(Name_Location)

#Save the location + coordinates table as a file.
save(Unique_tweet_locations, file="Locations_tweets_1.RData")
load("Locations_tweets_1.RData")

#==================================SENTIMENT ANALYSIS=======================================

#Sentimental score function
sentimental.score <- function(text, positive.words, negative.words) 
{
  sentimental.score <-  lapply(text,function(text, positive.words, negative.words)
  {
    # Independent words
    words = unlist(str_split(text, " "))
    # Count positive words
    positive = !is.na(match(words, positive.words))
    #  Count negative words
    negative = !is.na(match(words, negative.words))
    # Diff between positive and negative words
    score = sum(positive) - sum(negative)
    # return text and score
    out <- list(text = text, score =  ifelse(score > 0,"Positive",
                                             ifelse(score == 0,"Neutral","Negative")))
    return(out)
  }, 
  positive.words, negative.words)
  # Convert to a data frame and format columns
  out <- data.frame(matrix(unlist(sentimental.score),ncol = 2,byrow = T),stringsAsFactors = F)
  colnames(out) <- c("text","score")
  return(out)
}

#Separate spanish and english tweets
Tweets_ES<-tweets %>% filter(lang=="es")
Tweets_EN<-tweets %>% filter(lang=="en")

#Negative and positive words in both languages
negative <- read_file("Negative_words.csv", locale = locale(encoding = "Windows-1252")) %>% str_split("\r\n") %>% flatten_chr()
positive <- read_file("Positive_words.csv", locale = locale(encoding = "Windows-1252")) %>% str_split("\r\n") %>% flatten_chr()

negativas <- read_file("Palabras-Negativas-V2.csv", locale = locale(encoding = "Windows-1252")) %>% str_split("\r\n") %>% flatten_chr()
positivas <- read_file("Palabras-Positivas-V2.csv", locale = locale(encoding = "Windows-1252")) %>% str_split("\r\n") %>% flatten_chr()

#1. Spanish
Text_tweets_ES<-Tweets_ES$text 

#Lower case and remove punctuations
Text_tweets_ES<- str_to_lower(Text_tweets_ES)
Text_tweets_ES<- str_replace_all(Text_tweets_ES,"[[:punct:]]","")

Score_Tweets_ES<- sentimental.score(Text_tweets_ES,positivas,negativas) 

Tweets_ES$score<-Score_Tweets_ES$score

#2. English
Text_tweets_EN<-Tweets_EN$text 

#Lower case and remove punctuations
Text_tweets_EN<- str_to_lower(Text_tweets_EN)
Text_tweets_EN<- str_replace_all(Text_tweets_EN,"[[:punct:]]","")

Score_Tweets_EN<- sentimental.score(Text_tweets_EN,positive,negative) 

Tweets_EN$score<-Score_Tweets_EN$score

#Join spanish and english tweets
Tweets<- rbind(Tweets_ES,Tweets_EN)

#===================================Visualization code==================================

#1. Leaflet Map

Tweets<-Tweets %>% select(status_id,screen_name,text, location,score) #select only the requiered variables

#Change location names to match the query made to Google Maps API
Tweets$location[Tweets$location=="Costa Rica"]<-"San Jose, Costa Rica"
Tweets$location[Tweets$location=="Nicaragua "]<-"Managua, Nicaragua"

#Join tweet data with lat and long (froom Google query)
Tweets<- Tweets %>% left_join(Unique_tweet_locations, by=c("location"="Name_Location"))

#Create a color palette
Palette <- colorFactor(
  palette = c(rgb(189/255,55/255,48/255), "#e7b416", rgb(103/255,201/255,77/255)), #c('red', 'blue', 'green')
  domain = Tweets$score
)

#Make the map
leaflet() %>% 
  addTiles()%>% 
  setView(lng = -80, lat = 10, zoom = 3.0) %>%
  addCircleMarkers(data = Tweets, 
                   lng = ~ lon,
                   lat = ~ lat,
                   popup = ~ paste(screen_name, ": ",text),
                   color = ~Palette(score),
                   label = ~ location) 


#2. Positive and negative tweets word cloud

clean_words<-function(data, num_words = 100,language="english",empty_words=c("The","And"))
{
  # If text is provided, convert it to a dataframe of word frequencies
  if (is.character(data)) {
    corpus <- Corpus(VectorSource(data))
    corpus <- tm_map(corpus, tolower)
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, removeWords, stopwords(language))
    corpus <- tm_map(corpus, removeWords,tolower(empty_words))
    tdm <- as.matrix(TermDocumentMatrix(corpus))
    data <- sort(rowSums(tdm), decreasing = TRUE)
    data <- data.frame(word = names(data), freq = as.numeric(data))
  }
  
  # Make sure a proper num_words is provided
  if (!is.numeric(num_words) || num_words < 3) {
    num_words <- 3
  }  
  
  # Grab the top n most common words
  data <- head(data, n = num_words)
  if (nrow(data) == 0) {
    return(NULL)
  }
  
  return(data)
}

#Negative words

Tweets_Neg<- Tweets %>% filter(score=="Negative")

#Clean the tweets
Cleaned_tweets_neg<-clean_words(Tweets_Neg$text,num_words = 2000, language = "spanish",
                            empty_words=c("matrimonioigualitario","costa","rica", "costarica","hoy",
                                          "covid","país","día","solo","así","felicidades","lucha",
                                          "personas","amor","gracias"))

#Create a color palette
colors=c("#403780","#356986","#3E8A46","#9A9B42","#966B2D","#8E3D41")
ran<-sample(1:6,500,replace = TRUE)
color_vector=colors[ran]

wordcloud2(Cleaned_tweets_neg, color =color_vector, fontFamily="Domine")

#wordcloud2(Cleaned_tweets_neg, color =color_vector, fontFamily="Domine",figPath = "Theater_negative.png", backgroundColor = "black")

#Positive words
Tweets_Pos<- Tweets %>% filter(score=="Positive")

#Clean the tweets
Cleaned_tweets_pos<-clean_words(Tweets_Pos$text,num_words = 1000, language = "spanish",
                                empty_words=c("matrimonioigualitario","costa","rica", "costarica","país"))

#Create a color palette
colors=c(rgb(208/255,24/255,40/255),rgb(255/255,116/255,33/255),rgb(239/255,194/255,43/255),
         rgb(0/255,127/255,38/255),rgb(0/255,69/255,126/255),rgb(148/255,45/255,194/255))
ran<-sample(1:6,500,replace = TRUE)
color_vector=colors[ran]


wordcloud2(Cleaned_tweets_pos, color =color_vector, fontFamily="Domine")

wordcloud2(Cleaned_tweets_pos, color =color_vector, fontFamily="Domine",figPath = "Theater_positive.png", backgroundColor = "black")

#Neutral words
Tweets_Neu<- Tweets %>% filter(score=="Neutral")

#Clean the tweets
Cleaned_tweets_neu<-clean_words(Tweets_Neu$text,num_words = 3000, language = "spanish",
                                empty_words=c("matrimonioigualitario","costa","rica", "costarica","the",
                                              "tan","partir"))

#Create a color palette
colors=c(rgb(208/255,24/255,40/255),rgb(255/255,116/255,33/255),rgb(239/255,194/255,43/255),
         rgb(0/255,127/255,38/255),rgb(0/255,69/255,126/255),rgb(148/255,45/255,194/255))
ran<-sample(1:6,3000,replace = TRUE)
color_vector=colors[ran]

wordcloud2(Cleaned_tweets_neu, color =color_vector, fontFamily="Domine")

wordcloud2(Cleaned_tweets_neu, color =color_vector, fontFamily="Domine",figPath = "Theater_2.png",backgroundColor = "black")
