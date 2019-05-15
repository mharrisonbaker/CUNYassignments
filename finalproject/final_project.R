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
library('reshape2')
library('RMySQL')
library('RLastFM')
library('sqldf')
library('anytime')

# library('analyzelastfm')
#RLastFM can be installed by using instructions at 
#hhttps://rstudio-pubs-static.s3.amazonaws.com/236264_81312ba4d795474c8641dd0e2af83cba.html
#devtools::install_github("zappingseb/analyze_last_fm")


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

#i like NL because it is small geographically and the climate is relatively uniform country wide, also has a temp swing
nl_temps<-read.csv('https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/finalproject/temperature.csv', header = TRUE)
g2<-ggplot(nl_temps,aes(week,temperature)) +
  geom_point() +
  geom_smooth()
g2

#top artists played in NL
nl_artists<-geo.getTopTracks('Netherlands', key = api.key, parse = TRUE)
nl_artists$artist

#clean the usernames
user_df2<-user_df
user_df2$username <- iconv(user_df$username, "UTF-8", sub='')

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
fmapikey='b92c60ae0994e92981c6e2a02f66676c'



#top 50 tags used on lastFM (for genre identification), ranked
tag_method = '/?method=chart.gettoptags'
tags_url = paste(lastfm_url, tag_method, lastfm_apikey, lastfm_json, sep="")
tags_json <- tags_json <- jsonlite::fromJSON(tags_url) %>%
  data.frame
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

###
user_nm<-nl_users_df2$username
user_id <- seq(1:length(user_nm))
user_dfx <- data.frame(cbind(user_id,user_nm),stringsAsFactors=FALSE)



# function to retrive users' top artists
# Using custom function rather than RLastFM functon to obtain more refined results (From rcrastinate)

user.getTopArtistsMod <- function(user, key=fmapikey, period="12month", limit=100, page=1){
  base_url <- "http://ws.audioscrobbler.com/2.0/?method="
  method_string <- "user.gettopartists"
  user_string <- "&user="
  key_string <- "&api_key="
  format_string <- "&format=json"
  period_string <- "&period="
  limit_string <- "&limit="
  page_string <- "&page="
  return (jsonlite::fromJSON(paste0(base_url,method_string, user_string,user,key_string,key,format_string,
                          period_string, period,limit_string,limit,page_string,page)))
} 

user_dfz<-c("Thepekingduck","bartkl", "mrc314")

# obtain user/top artist combinations
user_artist <- data.frame(name = character(), playcount = integer(),mbid= character(), user_id = integer())
for (name in user_dfz){
  temp <- user.getTopArtistsMod(name)$topartists$artist[c("name","playcount","mbid")]
  temp$user_id <- user_dfx$user_id[user_dfx$user_nm == name]
  user_artist <- rbind(user_artist,temp)
  Sys.sleep(0.2)
}

# user_artist2<-user_artist
# arrange(user_artist2,desc(playcount))


# artists data frame
artists <- unique(user_artist[c("name","mbid")])
artist_id <- seq(1:nrow(artists)) 
artists <- cbind(artists, artist_id)
artists <- subset(artists,select=c(artist_id,name,mbid))

# artists data frame
artists <- unique(user_artist[c("name","mbid")])
artist_id <- seq(1:nrow(artists)) 
artists <- cbind(artists, artist_id)
artists <- subset(artists,select=c(artist_id,name,mbid))

# clean up user_artist dataframe for database export
user_artist <-inner_join(user_artist, artists, by="name")
user_artist <- subset(user_artist, select=c(user_id,artist_id,playcount))

# Make sure all artist names are UTF-8
artists$name <- iconv(artists$name, "UTF-8", sub='')



# artist/tag combination data frame
artist_tag <- data.frame(artist_id = integer(),tag = character(),count = integer() ) 

# clean up artist_tag table
artist_tag$tag <- tolower(artist_tag$tag)
artist_tag <- artist_tag %>%
  group_by(artist_id,tag) %>%
  summarise(count = sum(count))
artist_tag <- data.frame(artist_tag)

# unique tags data frame
tags <- artist_tag %>%
  group_by(tag) %>%
  summarise(count = sum(count))
tag_id <- seq(1:nrow(tags))
tags$tag_id <- tag_id
tags <- subset(tags, select=c(tag_id,tag,count)) 
tags <- data.frame(tags)

#
as_tibble(tags2)<-tags
tags2
# tags3<-melt(tags, id.vars = tag)
g3<-ggplot(tags, aes(tag, counts))+geom_bar(stat ="identity")
g3

g3 <- ggplot(tags, aes(reorder(tag, count), count))
g3 + geom_bar() + scale_y_continuous + coord_flip()


# prepare artist_tags data frame for database export
artist_tag <- inner_join(artist_tag,tags, by="tag")
artist_tag <- subset(artist_tag, select=c(artist_id,tag_id,count.x))
names(artist_tag) <- c("artist_id","tag_id","count")

# make sure all tag names are UTF-8 compliant
tags$tag <- iconv(tags$tag, "UTF-8", sub='')

# part 1: populate artist_tags data frame by matching on unique artist mbid (musicbrainz id), where available
for (i in 1:nrow(artists)){
  if (artists[i,]$mbid != ""){
    temp <- tryCatch(artist.getTopTags(artist=NA, mbid=artists[i,]$mbid),error=function(e) NULL)
    temp_tag <- temp$tag
    temp_ct <- temp$count
    temp_id <- rep(artists[i,]$artist_id,length(temp_tag))
    temp_col <- data.frame( artist_id= temp_id, tag=temp_tag,count=temp_ct)
    artist_tag <- rbind(temp_col, artist_tag)
    Sys.sleep(0.2)
  }
  
}   
# unique artist_ids from part 1
artist_unq <- unique(artist_tag$artist_id)













#top artist per user per week
lastfm_weeklytrack_method = '/?method=user.getWeeklyTrackChart&user='
call_url = paste(lastfm_url, lastfm_weeklytrack_method, "bartkl", lastfm_apikey, "&from=1356998400&to=1357603200", lastfm_json, sep="") 


rooturl:'http://ws.audioscrobbler.com/2.0/'
api_key='b92c60ae0994e92981c6e2a02f66676c'
method:'user.getWeeklyTrackChart'
user=nl_users_df2[,6]
api_key=lastfm_apikey
from=dates_df[i,2]
to=dates_df[i+1,2]

call1<-paste(rooturl)



#fill in user tag
for (i in 1:nrow(nl_users_df2)){
    for (j in 1:52){
      nl_users_df2=append(nl_users_df2[i,j+6])
    }
}



# for (i in 1:nrow(nl_users_df[,6])){
#     for (j in 1:nrow(weekrange[,1]))
# }


# function to retrive users' top artists
# Using custom function rather than RLastFM functon to obtain more refined results
my_key=api_key

user.getTopArtistsMod <- function(user, key=my_key, period="12month", limit=100, page=1){
  base_url <- "http://ws.audioscrobbler.com/2.0/?method="
  method <- "user.gettopartists"
  user_string <- "&user="
  key_string <- "&api_key="
  format_string <- "&format=json"
  period_string <- "&period="
  limit_string <- "&limit="
  page_string <- "&page="
  return (fromJSON(paste0(base_url,method, user_string,user,key_string,key,format_string,
                          period_string, period,limit_string,limit,page_string,page)))
} 

# obtain user/top artist combinations
user_artist <- data.frame(name = character(), playcount = integer(),mbid= character(), user_id = integer())
for (name in user_df$user_nm){
  temp <- user.getTopArtistsMod(name)$topartists$artist[c("name","playcount","mbid")]
  temp$user_id <- user_df$user_id[user_df$user_nm == name]
  user_artist <- rbind(user_artist,temp)
  Sys.sleep(0.5)
}


# artists data frame
artists <- unique(user_artist[c("name","mbid")])
artist_id <- seq(1:nrow(artists)) 
artists <- cbind(artists, artist_id)
artists <- subset(artists,select=c(artist_id,name,mbid))


#at this point i could probably make a pretty good recommender, but i'm stuck on getting the song tags timestamped

#as a workaround I will use a database of songs with energy+tempo values, and some user listening data from prolific NL users that I manually identified
mydb = dbConnect(MySQL(), user='matt', password='cunymsds1!', dbname='songsdata', host='35.239.118.183')
dbListTables(mydb)
rs = dbSendQuery(mydb, "select * from songtable")
songdata = fetch(rs, n=-1)
dbDisconnect(mydb)
as_tibble(songdata)

#
mrc314<-read.csv('https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/finalproject/mrc314.csv', header = TRUE)
bartkl<-read.csv('https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/finalproject/bartkl.csv', header = TRUE)


as_tibble(bartkl)
as_tibble(mrc314)


#
names(bartkl)[1] <- "listener_artist"
names(bartkl)[2] <- "listener_Album"
names(bartkl)[3] <- "listener_Song"
names(bartkl)[4] <- "listener_timestamp"
names(mrc314)[1] <- "listener_artist"
names(mrc314)[2] <- "listener_Album"
names(mrc314)[3] <- "listener_Song"
names(mrc314)[4] <- "listener_timestamp"


#
bartkl_data<-left_join(songdata, bartkl, by =c("title"="listener_Song"))
mrc314_data<-left_join(songdata, mrc314, by =c("title"="listener_Song"))

#drop NA from timestamp row
bartkl_listened_data  <- (drop_na(bartkl_data, listener_timestamp))
bartkl_TL<- select(bartkl_listened_data, tempo, loudness, listener_timestamp)

mrc314_listened_data  <- (drop_na(mrc314_data, listener_timestamp))
mrc314_TL<- select(mrc314_listened_data, tempo, loudness, listener_timestamp)
mrc314_TL2 <-  mutate(mrc314_TL, listener_timestamp = as.POSIXct(strptime(listener_timestamp,tz ="GMT", format = "%m/%d/%Y %H:%M")))
mrc314_TL2 <-  mutate(mrc314_TL2, loudness = abs(loudness))


#no real pattern here that i can discern
g4<-ggplot(data=mrc314_TL2,aes(x = as.Date(listener_timestamp), y = tempo)) + geom_point() + 
  scale_x_date(date_breaks = "2 month", 
               limits = as.Date(c('2013-01-01','2014-01-01')))
g4


#nice summer peak for loudness
g6<-ggplot(data=mrc314_TL2,aes(x = as.Date(listener_timestamp), y = loudness)) + geom_point() + 
  scale_x_date(date_breaks = "2 month", 
               limits = as.Date(c('2013-01-01','2014-01-01')))
g6

