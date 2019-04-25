library("neo4r")
library("RMySQL")
library("magrittr")

##connect to a GCP mySQL instance
##if you use user='cunygrader', password='cunyrules' and professor Catlin's IP address from week2 it should login to this host
mydbcon = dbConnect(MySQL(), user='matt', password='cunymsds1!', dbname='moviesdb', host='35.239.118.183')


##check to see the tables and their fields 
#find and print table names
table_names<-dbListTables(mydbcon)
table_names
#find and print table fields
table_fields<-lapply(table_names, dbListFields , conn = mydbcon)
table_fields


##dataframes
#generate data frames from sql tables
dfmovie<-dbReadTable(mydbcon, 'movie')
dfmovie

dfrating<-dbReadTable(mydbcon, 'rating')
dfrating

dfreviewer<-dbReadTable(mydbcon, 'reviewer')
dfreviewer

##disconnect from db
dbDisconnect(mydbcon)


# #csv export all sql query
# selectall <- "SELECT *"
# 
#csv URLs
movieURL <- c("https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/week12/movie.csv")
ratingURL <- c("https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/week12/rating.csv")
reviewerURL <- c("https://raw.githubusercontent.com/mharrisonbaker/CUNYassignments/master/week12/reviewer.csv")

##df to csv
g


#graph password cunymsds1!
## create neo4j connection
con <- neo4j_api$new(
  url = "http://localhost:7474",
  user = "matt", 
  password = "cunymsds1!"
)


#test neo4j connection
con$reset_user("neo4j")
con$ping()

#load CSV to neo4j server
load_csv(con=con, url=movieURL, header=TRUE, 
         as = "csvLine", 
         on_load = 'CREATE (:Movie { title: csvLine.title, director: csvLine.director, year: toInteger(csvLine.year), mID: toInteger(csvLine.mID)})')

load_csv(con=con, url=movieURL, header=TRUE, 
         as = "csvLine", 
         on_load = 'CREATE (:Rating { mID: toInteger(csvLine.mID), rID: toInteger(csvLine.rID), stars: toInteger(csvLine.stars), ratingID: toInteger(csvLine.ratingID)})')

load_csv(con=con, url=movieURL, header=TRUE, 
         as = "csvLine", 
         on_load = 'CREATE (:Reviewer { name: csvLine.name, rID: toInteger(csvLine.rID)})')

##neo4j create 
call_neo4j(query = 'CREATE INDEX ON :Product(productID);',con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Movie(director);',con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Movie(title);',con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Movie(year);',con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Movie(mID);',con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Rating(mID);',con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Rating(rID);',con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Rating(stars);' ,con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Rating(reviewID);' ,con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Reviewer(name);',con=con, type = "row")
call_neo4j(query = 'CREATE INDEX ON :Reviewer(rID);',con=con, type = "row")

#create relationships of 
load_csv(con=con, url=movieURL, header=TRUE, 
         as = "csvLine", 
         on_load = 'CREATE (:Reviewer { name: csvLine.name, rID: toInteger(csvLine.rID)})')

