#xml method
#this makes a nice dataframe
library("XML")
library("methods")
data1<-xmlParse("C:/Users/Matt/Documents/CUNY/DATA/WEEK7/ABC/teddybooks.xml")
xmldataframe <- xmlToDataFrame("C:/Users/Matt/Documents/CUNY/DATA/WEEK7/ABC/teddybooks.xml")


#json method
#this makes a wide table, interestingly
#it repeats the rows with a single author
library("jsonlite")
library(plyr)
result <- fromJSON(file = "C:/Users/Matt/Documents/CUNY/DATA/WEEK7/ABC/teddybooks.json")
result
result2<-ldply(result, stack)
json_data_frame <- as.data.frame(result)
print(json_data_frame)

#html method
#
library(XML)
library(RCurl)
library(rlist)
tables <- readHTMLTable("C:/Users/Matt/Documents/CUNY/DATA/WEEK7/ABC/teddybooks.html")
tables <- list.clean(tables, fun = is.null, recursive = FALSE)
html_data_frame <- as.data.frame(tables)
colnames(html_data_frame) <-c("Author", "Year", "Publisher", "ISBN-10")
print(html_data_frame)