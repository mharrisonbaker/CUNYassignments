/*
  assignment2.sql
*/

DROP DATABASE moviesdb;
CREATE DATABASE moviesdb;

DROP TABLE IF EXISTS movie;
DROP TABLE IF EXISTS rating;
DROP TABLE IF EXISTS reviewer;

CREATE TABLE movie 
(
mID INT(11), 
title TEXT, 
year INT(11), 
director TEXT
);

CREATE TABLE rating 
(
mID INT(11), 
rID INT(11), 
stars INT(11)
);

CREATE TABLE reviewer 
( 
rID INT(11), 
name TEXT
);

SELECT * FROM movie;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/movie.csv' 
INTO TABLE movie
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(mID, title, year, director)
;
SELECT * FROM movie;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/rating.csv' 
INTO TABLE rating
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(mID, rID, stars)
;
SELECT * FROM movie;

LOAD DATA INFILE 'C:/ProgramData/MySQL/MySQL Server 8.0/Uploads/reviewer.csv' 
INTO TABLE reviewer
FIELDS TERMINATED BY ',' 
ENCLOSED BY '"'
LINES TERMINATED BY '\n'
(rID, name)
;

SELECT * FROM movie;
SELECT * FROM rating;
SELECT * FROM reviewer;
SELECT COUNT(*) FROM movie;
SELECT COUNT(*) FROM rating;
SELECT COUNT(*) FROM reviewer;