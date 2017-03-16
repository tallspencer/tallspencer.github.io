#STA 141 Assignment 5
#Spencer Phillips
#998887568
#11/23/15
install.packages("sqldf")
library(sqldf)
install.packages('RSQLite')
library(RSQLite)

db = dbConnect(drv = SQLite(), dbname = 'G:/Library/Spencer/Documents/Spencer/School/College Junior/STA 141/imdb_data')

#Q1
numactors = as.integer(dbGetQuery(db, 'SELECT COUNT(idactors) FROM actors;'))
nummovies = as.integer(dbGetQuery(db, 'SELECT COUNT(idmovies) FROM movies;'))

#Q2
years = dbGetQuery(db, 'SELECT DISTINCT year FROM movies;')
sort(years$year)
dbGetQuery(db, 'SELECT * FROM movies WHERE year<1891;')
dbGetQuery(db, 'SELECT MAX(year) FROM movies;')

#Q3
girlactors = dbGetQuery(db, 'SELECT * FROM actors WHERE gender IS NULL;')
nrow(girlactors)/numactors
guyactors = dbGetQuery(db, 'SELECT * FROM actors WHERE gender = "1";')
nrow(guyactors)/ numactors


#Q4
dbGetQuery(db, 'SELECT DISTINCT type FROM movies;')
dbGetQuery(db, 'SELECT * FROM movies WHERE type IS NULL LIMIT 25')
dbGetQuery(db, 'SELECT * FROM movies WHERE type = "1" LIMIT 25')
as.integer(dbGetQuery(db, 'SELECT COUNT(*) FROM movies WHERE type = "1" '))/nummovies
as.integer(dbGetQuery(db, 'SELECT COUNT(*) FROM movies WHERE type = "2" '))/nummovies
as.integer(dbGetQuery(db, 'SELECT COUNT(*) FROM movies WHERE type = "3" '))/nummovies
as.integer(dbGetQuery(db, 'SELECT COUNT(*) FROM movies WHERE type IS NULL '))/nummovies

#Q5
dbGetQuery(db, 'SELECT DISTINCT COUNT(genre) FROM genres;')
genres = dbGetQuery(db, 'SELECT DISTINCT genre FROM genres;')
paste(genres)

#Q6
dbGetQuery(db, 'SELECT * FROM genres;')
dbGetQuery(db, 'SELECT COUNT(*) FROM movies_genres WHERE idgenres = "32";')
genrecount = dbGetQuery(db, 'SELECT idgenres, COUNT(*) FROM movies_genres GROUP BY idgenres;')
for(i in 29:32)
{
  genrecount[i,] = c(i,0)
}
genrecount$names = dbGetQuery(db, 'SELECT genre FROM genres')
genrecount$proportions = genrecount$`COUNT(*)`/sum(genrecount$`COUNT(*)`)
genrecount[,3:4]

#Q7
dbGetQuery(db, 'CREATE TEMPORARY TABLE superkeyword AS SELECT * FROM movies_keywords INNER JOIN keywords ON movies_keywords.idkeywords = keywords.idkeywords;')
dbGetQuery(db, 'CREATE TEMPORARY TABLE supermovies AS SELECT * FROM superkeyword AS key INNER JOIN movies ON movies.idmovies = key.idmovies;')
dbGetQuery(db, 'SELECT * FROM supermovies WHERE keyword = "space" LIMIT 25;')
dbGetQuery(db, 'SELECT * FROM keywords WHERE keyword = "space";')
dbGetQuery(db, 'CREATE TEMPORARY TABLE spacemovies AS SELECT * FROM movies_keywords WHERE idkeywords = "1266";')

dbGetQuery(db, 'SELECT COUNT(*) FROM spacemovies')
dbGetQuery(db, 'CREATE TEMPORARY TABLE MovieSpace AS SELECT * FROM spacemovies INNER JOIN movies ON spacemovies.idmovies = movies.idmovies;')
dbGetQuery(db, 'SELECT * FROM acted_in WHERE idmovies = "36986" AND billing_position <6')

dbGetQuery(db, 'SELECT movies.idmovies, title, year 
            FROM keywords, movies_keywords, movies 
            WHERE keyword = "space"
            AND type = "1"
            AND keywords.idkeywords = movies_keywords.idkeywords
            AND movies.idmovies = movies_keywords.idmovies')
dbGetQuery(db, 'SELECT DISTINCT MAX(year), MIN(year)
            FROM keywords, movies_keywords, movies 
            WHERE keyword = "space"
            AND keywords.idkeywords = movies_keywords.idkeywords
            AND movies.idmovies = movies_keywords.idmovies')
dbGetQuery(db, 'SELECT DISTINCT year
            FROM keywords, movies_keywords, movies 
            WHERE keyword = "space"
            AND keywords.idkeywords = movies_keywords.idkeywords
            AND movies.idmovies = movies_keywords.idmovies')
dbGetQuery(db, 'SELECT DISTINCT lname, fname, movies.idmovies, title, billing_position 
            FROM keywords, movies_keywords, movies, acted_in, actors 
           WHERE keyword = "space"
           AND keywords.idkeywords = movies_keywords.idkeywords
           AND movies.idmovies = movies_keywords.idmovies
           AND billing_position < 6
           AND movies.idmovies = acted_in.idmovies
           AND acted_in.idactors = actors.idactors;')

dbGetQuery(db, 'SELECT DISTINCT lname || ", " || fname 
            FROM keywords, movies_keywords, movies, acted_in, actors 
           WHERE keyword = "space"
           AND keywords.idkeywords = movies_keywords.idkeywords
           AND movies.idmovies = movies_keywords.idmovies
           AND billing_position < 6
           AND movies.idmovies = acted_in.idmovies
           AND acted_in.idactors = actors.idactors;')

#Q8
yeargenre = dbGetQuery(db, 'SELECT COUNT(movies.idmovies) AS count, year, genre
           FROM genres, movies_genres, movies
           WHERE genres.idgenres = movies_genres.idgenres
           AND movies_genres.idmovies = movies.idmovies GROUP BY genre, year')
dbGetQuery(db, 'SELECT genre, year, COUNT(movies.idmovies)
           FROM genres, movies_genres, movies
           WHERE genres.idgenres = movies_genres.idgenres
           AND movies_genres.idmovies = movies.idmovies GROUP BY genre, year LIMIT 10')

blah = order(yeargenre$year)
yeargenre = yeargenre[blah,]
yeargenre = na.omit(yeargenre)
tester = split(yeargenre, yeargenre$genre)
library(ggplot2)
ggplot(yeargenre, aes(year, count, colour = genre)) +geom_line() +xlab("Year") + ylab("Count") + ggtitle("Genres of Movies Per Year")

#Q9
dbGetQuery(db, 'SELECT lname, fname, COUNT(acted_in.idactors)
           FROM actors, acted_in, movies
           WHERE movies.idmovies = acted_in.idmovies
           AND actors.idactors = acted_in.idactors
           GROUP BY lname, fname 
           LIMIT 20')

dbGetQuery(db, 'CREATE TEMP TABLE counts AS 
           SELECT lname, fname, COUNT(acted_in.idactors) AS count 
           FROM acted_in, actors
           WHERE acted_in.idactors = actors.idactors
           GROUP BY acted_in.idactors')
counts = dbGetQuery(db, 'SELECT fname, lname, count FROM counts')
blerg = order(counts$count, decreasing = TRUE)
counts = counts[blerg,]
head(counts, n = 20)

#Q10
dbGetQuery(db, 'CREATE TEMP TABLE bestcounts AS 
           SELECT lname, fname, COUNT(acted_in.idactors) AS count, MIN(year), MAX(year) 
           FROM acted_in, actors, movies
           WHERE acted_in.idactors = actors.idactors
           AND movies.idmovies = acted_in.idmovies
           AND billing_position < "4"
           GROUP BY acted_in.idactors')
dbGetQuery(db, 'SELECT fname, lname, count, "MIN(year)", "MAX(year)" FROM bestcounts ORDER BY count DESC LIMIT 10')

#Q11

# dbGetQuery(db, 'CREATE TEMP TABLE num11 AS
#           SELECT fname, lname, COUNT(acted_in.idactors) AS count, year 
#            FROM acted_in, actors, movies
#            WHERE acted_in.idactors = actors.idactors
#            AND movies.idmovies = acted_in.idmovies
#            GROUP BY acted_in.idactors, year ORDER BY year')
dbGetQuery(db, 'CREATE TEMP TABLE num11 AS
           SELECT fname, lname, COUNT(acted_in.idactors) AS count, year 
           FROM acted_in, actors, movies
           WHERE acted_in.idactors = actors.idactors
           AND movies.idmovies = acted_in.idmovies
           AND movies.type = 1
           GROUP BY year, acted_in.idactors')

q11 = dbGetQuery(db, 'SELECT DISTINCT * FROM num11 
                 WHERE year > "1878"
                 ORDER BY count DESC')

q11limit = dbGetQuery(db, 'SELECT DISTINCT * FROM num11 
                 WHERE year > "1878" 
                 ORDER BY count DESC 
                 LIMIT 10')

#dbGetQuery(db, 'SELECT fname, lname, title FROM actors, movies, acted_in WHERE actors.idactors = "3500163" AND acted_in.idactors = actors.idactors AND acted_in.idmovies = movies.idmovies')
dbGetQuery(db, 'DROP TABLE num11;')









#Q12
dbGetQuery(db, 'SELECT fname, lname, count(aka_names.idactors) AS count 
           FROM aka_names, actors 
           WHERE aka_names.idactors = actors.idactors
           GROUP BY aka_names.idactors ORDER BY count DESC LIMIT 10')
dbGetQuery(db, 'SELECT * FROM aka_names LIMIT 50')


#Q13
#actor ID 753934 WE JGL NOW edition
install.packages('igraph')
library(igraph)

q13db = dbConnect(drv = SQLite(), dbname ='G:/Library/Spencer/Documents/Spencer/School/College Junior/STA 141/lean_imdbpy_2010.db')
dbGetQuery(q13db, 'SELECT * FROM name2 WHERE name = "Gordon-Levitt, Joseph"')

dbGetQuery(q13db, 'SELECT * FROM title2 WHERE kind_id =  LIMIT 10')

dbGetQuery(q13db,'SELECT DISTINCT movie_id, title FROM cast_info2, title2 
           WHERE person_id = 753934  
           AND kind_id = 1
           AND cast_info2.movie_id = title2.id;')
dbGetQuery(q13db,'CREATE TEMP TABLE jglmovies AS
           SELECT DISTINCT movie_id, title FROM cast_info2, title2 
           WHERE person_id = 753934  
           AND kind_id = 1
           AND cast_info2.movie_id = title2.id;')
firstIDs = dbGetQuery(q13db, 'SELECT movie_id FROM jglmovies')
firstIDs = as.list(firstIDs$movie_id)

# getactors = function(movieID)
# {
#   dbGetQuery(q13db, 'SELECT DISTINCT name, name2.id 
#              FROM cast_info2, name2 WHERE movie_id = "movieID"
#              AND cast_info2.person_id = name2.id')
# }
# 
# lapply(firstIDs, function(i) getactors(firstIDs[[i]]))
# 
# getactors(2631851)
dbGetQuery(q13db,'CREATE TABLE  title AS 
SELECT * FROM title2 
WHERE kind_id = 1;')

dbGetQuery(q13db,'CREATE TABLE  cast_info AS 
           SELECT * FROM cast_info2
           WHERE movie_id IN (SELECT id FROM title);')
           
dbGetQuery(q13db, 'CREATE TABLE  name AS 
  SELECT * FROM name2
   WHERE id IN (SELECT person_id FROM cast_info);')         

castinfo = dbReadTable(q13db, "cast_info")
actornames = dbReadTable(q13db, "name")
movietitle = dbReadTable(q13db, "title")

getmovielist = function(actorid)
{
  movieIDs = castinfo[castinfo$person_id == actorid,]
  movieIDs = movieIDs[!duplicated((movieIDs$movie_id)),]
  final = movieIDs$movie_id
  return(final)
}

jglmovieIDs = getmovielist(753934)

#got from stackoverflow
completeFun = function(data, desiredCols) 
{
  completeVec = complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

getactors = function(movieID)
{
  otheractors = castinfo[castinfo$movie_id == movieID & castinfo$nr_order < 5,]
  theids = completeFun(otheractors, "nr_order")
  actors = theids$person_id
  return(actors)
}

level1actors = lapply(1:length(jglmovieIDs), function(i) getactors(jglmovieIDs[i]))
level1actors = unlist(level1actors)
level1actors = level1actors[!duplicated(level1actors)]

level2movies = lapply(1:length(level1actors), function(i) getmovielist(level1actors[i]))
level2movies = unlist(level2movies)
level2movies = level1actors[!duplicated(level2movies)]

level2actors = c()
for(i in 1:722)
{
  val = getactors(level2movies[i])
  level2actors[i] = val   
}

level2actors = lapply(1:7, function(i) getactors(level2movies[i]))
level2actors = unlist(level2actors)
level2actors = level2actors[!duplicated(level2actors)]

install.packages("network")
library(network)
network()

