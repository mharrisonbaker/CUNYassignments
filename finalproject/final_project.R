library("tidyverse")
library(wrapr)
library("httr")
library(XML)
library("jsonlite")
library(RCurl)
library("RSocrata")
library("tibble")
library("magrittr")
library("soql")
library('ggplot2')
library('rjson')
library('stringr')
library('lubridate')

#The original aim of my project was to combine streaming music data and weather data.  It looked like a promising idea
#based on the documentation of the Echo Nest API...
#The Echo Nest has disabled it's API and Spotify does not offer much on its own API, but last.fm API still lives
#Unfortunately last.fm API is no longer returning updated geographical data (despite the documentation)
#further, the old geographic data can not be queried by date ranges
#I was able to find a list of two million user profiles extracted from the last.fm API around Dec 2012
#this includes the country code of the user, which is the only geo data from a streaming music service that I can find


#socrata has a database of ~2 million Last.FM user names... which I will use in the Last.FM API
socrata_fm_endpoint<- c("https://opendata.socrata.com/resource/5vvd-truf.json")
lastfm_users <- read
socrata_endpoint_test<-c("https://soda.demo.socrata.com/resource/6yvf-kk3n.json?select=depth region?where=region=Virgin Islands region")
soctestdf <- read.socrata(socrata_endpoint_test)
as_tibble(soctestdf)


#this builds a Socrata Query using "SoQL" ... I think bots were getting higher play_counts so I filter them out in this query

my_url <- soql() %>%
  soql_select("id,username,playlists,play_count,country,age,gender") %>%
  soql_where("play_count < 50000") %>%
  soql_order("play_count", desc=TRUE) %>%
  soql_group("play_count,country,age,gender,playlists,id,username")
  as.character()
  


#builds query url
socrata_built <- soql_add_endpoint(my_url,socrata_fm_endpoint)


#build df from Socrata API
socrata_df <- read.socrata(socrata_built)
socrata_df <- as_tibble(socrata_df)
socrata_df <- drop_na(socrata_df)

#summarise some of the socrata dataset
socrata_by_country <- group_by(socrata_df, country)
socrata_stats<-summarise(socrata_by_country, count=n()) %>% arrange(desc(count))
print(socrata_stats, n=50)

#dplyr, filter out (probably) fake ages and removed duplicates
#choose 5 countries with small geographic area (smallest locale available)
user_df <- filter(socrata_df, country == "UK" | country == "DE" | country == "MX" | country == "NL" | country == "ES")
user_df <- user_df %>% filter(age < 65)
user_df <- distinct(user_df)
user_df<- select(user_df, -playlists)
summarize(user_df)

#plot the chosen country data
g<-ggplot(user_df,aes(country))
g+geom_bar(aes(fill=gender))

#i like NL because it is small geographically and the climate is relatively uniform country wide
nl_temps<-read.csv('https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/finalproject/temperature.csv', header = TRUE)
nl_temps
g2<-ggplot(nl_temps,aes(week,temperature)) +
  geom_point() +
  geom_smooth()

#take top 20 women and men from each country by play_count
top_df_f = user_df %>% filter(gender %in% "f") %>% group_by(country) %>% do(head(.,20))
top_df_f
top_df_m = user_df %>% filter(gender %in% "m") %>% group_by(country) %>% do(head(.,20))
top_df_m
top_users_df = bind_rows(top_df_f, top_df_m)
top_users_df



#here is a table of dates from 2012-2014 (considered the best lastfm data due to service and API changes)
dates_df = read.csv('https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/finalproject/justdates.csv', header = TRUE)
dates_df <- as_tibble(dates_df)
dates_df


#weekly range
from_x = '&from='
to_x = '&to='
weekly_range_df <- c()
for(i in 1:(nrow(dates_df)-1)){
  n<-paste(from_x,dates_df[i,2],to_x,dates_df[i+1,2],sep="")
  weekly_range_df=append(weekly_range_df, n)
}
head(weekly_range_df)


#last fm url and api
lastfm_url = 'http://ws.audioscrobbler.com/2.0'
lastfm_apikey = '&api_key=b92c60ae0994e92981c6e2a02f66676c' # use your own api key here
lastfm_json = '&format=json'


#top 50 tags used on lastFM (for genre identification)
tag_method = '/?method=chart.gettoptags'
tags_url = paste(lastfm_url, tag_method, lastfm_apikey, lastfm_json, sep="")
tags_json <- GET(tags_url) %>% 
  rawToChar(.$content) %>%
  fromJSON %>%
  data.frame 
tags_json
as_tibble(tags_json) %>% select(tags.tag.name) -> tags_df
rename(tags_df, tag = tags.tag.name) 
tags_json[,1]


#use NL M users as a test group (i found that males were more consistent users)
nl_users_df <- top_users_df %>% filter(country =='NL') %>% filter (gender %in% 'm')
nl_users_df2<-nl_users_df

#dummy col to add 52 weeks to table
dummytibble<-tibble(name = paste("Name", 1:52))
columnsToAdd <- paste("week", 1:104,sep="")
nl_users_df2[,columnsToAdd]=NA
nl_users_df2

#top artist per user per week
lastfm_weeklytrack_method = '/?method=user.getWeeklyTrackChart&user='
call_url = paste(lastfm_url, lastfm_weeklytrack_method, "bartkl", lastfm_apikey, "&from=1356998400&to=1357603200", lastfm_json, sep="") 


method:'user.getWeeklyTrackChart'
user:nl_users_df2[,6]
api_key:lastfm_apikey
limit:200
page: page || 1


#fill in user tag
for (i in 1:nrow(nl_users_df2)){
    for (j in 1:52){
      nl_users_df2=append(nl_users_df2[i,j+6])
    }
}

#get top artist for 52 weeks for each user for 52 weeks (create week range as its own column?)



# for (i in 1:nrow(nl_users_df[,6])){
#     for (j in 1:nrow(weekrange[,1]))
# }


this.raw.content <- rawToChar(raw.result$content)
this.raw.content
jsonlite::fromJSON(raw.result$content)