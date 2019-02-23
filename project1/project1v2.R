library(tidyverse)


chesstextread<-readLines("https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/project1/tournamentinfo.txt")
chesstext<-chesstextread[-(1:4)]
chesstext2<- chesstext[seq(1, length(chesstext), 3)]
chesstext2

#chessdf <- data.frame("Player's Name", "Player's State", "Total Number of Points", "Player's Pre-Rating", "Average Pre Chess Rating of Opponents")
chessdf <- data.frame()


#make a player number vector
playernumber<-c(1:64)


#name extraction
namepat <- ("([A-Z]+[[:space:]]){2,4}")
name <- unlist(str_extract_all(chesstext, namepat))

#state extraction
statepat <- ("[[:space:]][A-Z]{2}[[:space:]]\\|")
state <- unlist(str_extract_all(chesstext, statepat))
state <- substr(state, 2, 3)


#pre-rating extraction
preratingpat <- c("\\:[[:space:]]{1,2}[[:digit:]]{3,4}")
prerating <- unlist(str_extract_all(chesstext, preratingpat))
prerating<-as.numeric(gsub("\\D", "", prerating))
prerating

#total number of points extraction
tpointspat <-c("\\|[[:digit:]]\\.?[[:digit:]]?")
tpoints <- unlist(str_extract_all(chesstext, tpointspat))
tpoints <- as.numeric(gsub("\\|", "", tpoints))
tpoints

#chessdf with variables so far
chessdf <- data.frame(name, state, prerating, tpoints)
chessdf
chessdf <- as_tibble(chessdf)
chessdf

#opponent grid as dataframe
chesstext2
oppvec<-(str_extract_all(chesstext2, "[[:space:]][[:digit:]]{1,2}"))
oppvec
oppdf<-plyr::ldply(oppvec, rbind)
oppdf <-oppdf[,-1]
colnames(oppdf) <- c("opp1", "opp2", "opp3", "opp4", "opp5", "opp6", "opp7")
oppdf<-apply(oppdf,2,function(x)gsub('\\s+', '',x))
oppdf

#number of opponents
playedgames <-lengths(oppvec)
playedgames
#or
#find number of opponents played as list
numberplayed <- rapply(oppvec, length, how="list")


#use lookup table to convert opponent numbers to opponent rating
lut <- data.frame(chessdf$playernumber, chessdf$prerating)
colnames(lut) <- c("pnum", "prating")
lut$pnum = as.character(as.numeric(lut$pnum))
lut$prating = as.character(as.numeric(lut$prating))


#opponent rating dataframe
oppratingdf <- oppdf #make a copy
oppratingdf[is.na(oppratingdf)] <- 0
oppratingdf[] <- lut$prating[match(unlist(oppratingdf), lut$pnum)]
oppratingdf
oppratingdf<-as_tibble(oppratingdf)
oppratingdf$opp1<-as.numeric(oppratingdf$opp1)
oppratingdf$opp2<-as.numeric(oppratingdf$opp2)
oppratingdf$opp3<-as.numeric(oppratingdf$opp3)
oppratingdf$opp4<-as.numeric(oppratingdf$opp4)
oppratingdf$opp5<-as.numeric(oppratingdf$opp5)
oppratingdf$opp6<-as.numeric(oppratingdf$opp6)
oppratingdf$opp7<-as.numeric(oppratingdf$opp7)
# oppratingdf %>%
#   rowwise() %>%
#   mutate(mymean=mean(c(opp1, opp2, opp3, opp4, opp5, opp6, opp7), na.rm = TRUE)) %>%
#   select(opp1, opp2, opp3, opp4, opp5, opp6, opp7, mymean) -> oppratingdf3

oppratingdf %>% 
  mutate(means=rowMeans(., na.rm=TRUE)) -> oppratingdf3
  


chessdf$avgopprating<- oppratingdf3$means
chessdf$avgopprating %>% 
    mutate(recode = as.numeric(factor(avgopprating))) -> chessdf
print(tbl_df(chessdf), n=64)

chessdf %>% write.csv(.,file = "C:/Users/Matt/Documents/CUNY/DATA/PROJECT1/chessoutput.csv")
  
