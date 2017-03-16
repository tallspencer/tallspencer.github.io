#Spencer Phillips
#998887568
#STA141 Assignment 6
#12/9/15

#part 1
#install packages
install.packages('RCurl')
install.packages('XML')
library(XML)
library(RCurl)

#URL time
url = "https://stackoverflow.com/questions/tagged/r?page=3&sort=newest&pagesize=50"
lasturl = "https://stackoverflow.com/questions/tagged/r?page=2335&sort=newest&pagesize=50"
blerg = getURLContent(url, verbose = TRUE)
lastblerg = getURLContent(lasturl, verbose = TRUE)
lasthtml = htmlParse(lastblerg)
html = htmlParse(blerg)
saveXML(html, "hn1.html")


 #######################
#FUNCTION TO GET POSTERS#
 #######################
getposter = function(html)
{
  poster = getNodeSet(html, "//div[@class = 'user-details']")
  for(i in 1:length(poster))
  {
    user = getNodeSet(poster[[i]], ".//a/text()")
    if(is.null(user))
    {
      poster[[i]] = NA
    }
    else
    {
      change = xmlValue(user[[1]])
      poster[[i]] = change
    }
  }
   poster = unlist(poster)
  poster = as.data.frame(poster, stringsAsFactors = FALSE)
  return(poster)
}

# path = "//div[@class = 'user-details']/a"
#  users = getNodeSet(html, "//div[@class = 'user-details']")
#  
# xpathSApply(users, )


 ########
#GET TIME#
 ########
whenposted = function(html)
{
  thetime = getNodeSet(html, "//div[@class = 'user-action-time']/span/@title")
  thetime = as.data.frame(as.character(thetime), stringsAsFactors = FALSE)
  return(thetime)
}

#  test = getNodeSet(html, "//div[@class = 'user-action-time']/span/@title")
# # test$title
#  test = as.data.frame(as.character(test))

 ################
#GET NAME OF POST#
 ################

getname = function(html)
{
  namelist = getNodeSet(html, "//div[@class = 'summary']/h3/a/text()")
  namelist = unlist(namelist)
  namelist = sapply(namelist,xmlValue)
  namelist = as.data.frame(namelist, stringsAsFactors = FALSE)
  return(namelist)
}

# test = getNodeSet(html, "//div[@class = 'summary']/h3/a/text()")
# test = unlist(test)
# test = sapply(test, xmlValue)
# test = as.data.frame(test)

 ##########################
#FUNCTION TO GET REPUTATION#
 ##########################
getreputation = function(html)
{
  reputation = getNodeSet(html, "//div[@class = 'user-details']")
  for(i in 1:length(reputation))
  {
    rep = getNodeSet(reputation[[i]], ".//div[@class = '-flair']/span[@class = 'reputation-score']/text()")
    change = xmlValue(rep[[1]])
    if(is.null(rep))
    {
      reputation[[i]] = NA
    }
    else if(grepl("k", change))
    {
      special = getNodeSet(reputation[[i]], ".//div[@class = '-flair']/span/@title")[1]
      special = as.character(special)
      special = gsub("([^0-9]*)","",special)
      reputation[[i]] = special
    }
    else if(grepl(",", change))
    {
      reputation[[i]] = gsub(",", "", change)
    }
    else
    {
      reputation[[i]] = change
    }
  }
  reputation = unlist(reputation)
  reputation = as.data.frame(reputation, stringsAsFactors = FALSE)
  return(reputation)
}

# test = getNodeSet(html, "//div[@class = 'user-details']")
# boop = getNodeSet(test[[21]], ".//div[@class = '-flair']/span[@class = 'reputation-score']/text()")
# loop = xmlValue(boop[[1]])
# shoop = getNodeSet(test[[6]], ".//div[@class = '-flair']/span/@title")[1]

 ##################
#VIEWS FOR THE POST#
 ##################
getviews = function(html)
{
  views = getNodeSet(html, "//div[@class = 'statscontainer']")
  for(i in 1:length(views))
  {
    lookers = getNodeSet(views[[i]], ".//div[@class = 'views ']/@title
                     | .//div[@class = 'views warm']/@title
                     | .//div[@class = 'views hot']/@title")[1]
    thechar = as.character(lookers)
    thechar = gsub("([^0-9]*)","",thechar)
    views[[i]] = thechar
   
  }
  views = unlist(views)
  views = as.data.frame(views, stringsAsFactors = FALSE)
  return(views)
}

# test = getNodeSet(lasthtml, "//div[@class = 'statscontainer']")
# 
# look = getNodeSet(test[[1]], ".//div[@class = 'views ']/@title
#                      | .//div[@class = 'views warm']/@title
#                      | .//div[@class = 'views hot']/@title")[1]
# 
# book = as.character(look)

 ####################
#ANSWERS FOR THE POST#
 ####################

getanswers = function(html)
{
  numanswered = getNodeSet(html, "//div[@class = 'status unanswered']/strong/text() |
                           //div[@class = 'status answered']/strong/text() |
                           //div[@class = 'status answered-accepted']/strong/text()")
  numanswered = sapply(numanswered, xmlValue)
  numanswered = as.data.frame(numanswered, stringsAsFactors = FALSE)
  return(numanswered)
}

# boop = getNodeSet(html, "//div[@class = 'status unanswered']/strong/text() |
#                            //div[@class = 'status answered']/strong/text() |
#                            //div[@class = 'status answered-accepted']/strong/text()")


########################
#VOTE SCORE FOR THE POST#
########################

getscore = function(html)
{
  score = getNodeSet(html, "//span[@class = 'vote-count-post ']/strong/text()")
  score = sapply(score, xmlValue)
  score = as.data.frame(score, stringsAsFactors = FALSE)
  return(score)
}

#boop = getNodeSet(html, "//span[@class = 'vote-count-post ']/strong/text()")


#################
#URL FOR THE POST#
#################

getposturl = function(html)
{
  posturl = getNodeSet(html, "//div[@class = 'summary']/h3/a/@href")
  posturl = as.character(posturl)
  posturl = sapply(1:length(posturl), function(i) 
    {
    getRelativeURL(posturl[i], "https://stackoverflow.com/questions", sep = "")
  })
  posturl = as.data.frame(posturl, stringsAsFactors = FALSE)
  return(posturl)
}

# temp = getNodeSet(html, "//div[@class = 'summary']/h3/a/@href")
# temp = as.character(temp)

################
#ID FOR THE POST#
################

getpostID = function(html)
{
  postID = getNodeSet(html, "//div[@class = 'question-summary']/@id")
  postID = as.character(postID)
  postID = sapply(1:length(postID), function(i) {gsub("([^0-9]*)","",postID[i])})
  postID = as.data.frame(postID, stringsAsFactors = FALSE)
  return(postID)
}

# test = getNodeSet(html, "//div[@class = 'question-summary']/@id")


#########
#GET TAGS#
#########

gettags = function(html)
{
  tag = getNodeSet(html, "//div[@class = 'summary']")
  for(i in 1:length(tag)){
      temp = xpathSApply(tag[[i]], ".//div/a[@class = 'post-tag']", xmlValue)
      temp = paste(temp, collapse = '; ')
      tag[i] = temp
  }
  tag = as.character(tag)
  tag = as.data.frame(tag, stringsAsFactors = FALSE)
  return(tag)
}

# alli = getNodeSet(html, "//div[@class = 'summary']")
# 
# xpathSApply(alli[[2]], ".//div/a[@class = 'post-tag']", xmlValue)


#################
#GET TO NEXT PAGE#
#################

nextpage = function(html)
{
  thenext = getNodeSet(html, "//div[@class = 'pager fl']/a[@rel = 'next']/@href")
  if(is.null(thenext))
  {
    #tests for the last page
    return(NULL)
  }
  else
  {
    #puts together the URL
    thenext = as.character(thenext)
    thenext = getRelativeURL(thenext, "https://stackoverflow.com", sep = "")
    return(thenext)
  }
}

# floyd = getNodeSet(html, "//div[@class = 'pager fl']/a[@rel = 'next']/@href")
# alli = getNodeSet(lasthtml, "//div[@class = 'pager fl']/a[@rel = 'next']/@href")
 ###########################
#DATAFRAME BUILDING FUNCTION#
 ###########################

DFcompilation = function(html)
{
  #put them all together finally
  bigDF = data.frame(getpostID(html), whenposted(html), gettags(html), getname(html),
                     getposturl(html), getviews(html), getscore(html), getanswers(html), 
                     getposter(html), getreputation(html))
  return(bigDF)
}


######################
#THE BIG BOSS FUNCTION#
######################

StackOverflow = function(tagged = 'r', pagestart = 1, pagelimit = Inf)
{
  ans = NULL
  url = sprintf("https://stackoverflow.com/questions/tagged/%s?page=%s&sort=newest&pagesize=50", tagged, pagestart)
  pagecount = 0
  while(TRUE & pagecount < pagelimit)
  {
    html = getURLContent(url, binary = TRUE)
    html = rawToChar(html)
    html = htmlParse(html)
    #manage the html stuff
    tempDF = DFcompilation(html)
    #make the DF
    ans = rbind(ans, tempDF)
    #merge the DFs together
    url = nextpage(html)
    #get the next URL and break loop if done
    if(is.null(url))
    {
      break
    }
    pagecount = pagecount + 1
  }
  #rename the collumns for future use
  colnames(ans) = c("ID", "Date", "Tags", "Title", "URL", "Views", "Votes", "Answers", "User", "Reputation")
  return(ans)
}

# https://stackoverflow.com/questions/tagged/r?page=3&sort=newest&pagesize=50
# alli = 'r'
# amy = sprintf("https://stackoverflow.com/questions/tagged/%s?page=%s&sort=newest&pagesize=50",alli)
# lasttest = DFcompilation(lasthtml)
# test = rbind(test, lasttest)
# colnames(test) = c("ID", "Date", "Tags", "Title", "URL", "Views", "Votes", "Answers", "User", "Reputation")

StackDataset = StackOverflow(pagelimit = 1250)



 ######
#PART 2#
 ######

#Q1

answersub = subset(rQAs, rQAs$type == "answer")
answertable = table(answersub$user)
hist(answertable, xlab = "Number of Questions Answered", 
     main = "Number of Questions Answered by Each Unique User")
max(answertable)

#Q2 most common tags
StackDataset$Tags
strsplit(StackDataset$Tags[1], "; ")
taglist = sapply(1: length(StackDataset$Tags), function(i) strsplit(StackDataset$Tags[i], "; "))
taglist = unlist(taglist)
tagtable = table(taglist)
head(sort(tagtable, decreasing = TRUE))

#Q3 number of questions about ggplot

table(taglist[taglist %in% "ggplot2"])
table(taglist[taglist %in% "ggplot"])
table(taglist[taglist %in% c("html", "xml", "web-scraping")])

#Q4 questions with XML HTML Webscraping

# questionsub = subset(rQAs, rQAs$type == "question")
# questionsub$text[1]
#table(taglist[taglist %in% c("html", "xml", "web-scraping")])
#taglistlist[1][taglistlist[1] %in% c("html", "xml", "web-scraping")]

taglistlist = sapply(1: length(StackDataset$Tags), function(i) strsplit(StackDataset$Tags[i], "; "))
#function found online through a link someone put on piazza
#from cookbook for R website
Q4function = function(hugelist)
{
  questioncount = 0
  for(i in 1:length(hugelist))
  {
    for(w in 1:length(hugelist[[i]]))
    {
      if(hugelist[[i]][w] == "xml")
      {
        questioncount = questioncount + 1
        break
      }
      else if (hugelist[[i]][w] == "html")
      {
        questioncount = questioncount + 1
        break
      }
      else if (hugelist[[i]][w] == "web-scraping")
      {
        questioncount = questioncount + 1
        break
      }
    }
  }
  return(questioncount)
}

Q4function(taglistlist)

#Q5
#use made dataset and compare to functionlist
questionsub = subset(rQAs, rQAs$type == "question")

#function from cookbook-r
showPackageContents <- function (packageName) {
  
  # Get a list of things contained in a particular package
  funlist <- objects(packageName)
  
  # Remove things that don't start with a letter
  idx <- grep('^[a-zA-Z][a-zA-Z0-9._]*', funlist)
  funlist <- funlist[idx]
  
  # Remove things that contain arrow <-
  idx <- grep('<-', funlist)
  if (length(idx)!=0)
    funlist <- funlist[-idx]
  
  # Make a data frame to keep track of status
  objectlist <- data.frame(name=funlist,
                           primitive=FALSE,
                           func=FALSE,
                           object=FALSE,
                           constant=FALSE,
                           stringsAsFactors=F)
  
  for (i in 1:nrow(objectlist)) {
    fname <- objectlist$name[i]
    if (exists(fname)) {
      obj <- get(fname)
      if (is.primitive(obj)) {
        objectlist$primitive[i] <- TRUE
      }
      if (is.function(obj)) {
        objectlist$func[i] <- TRUE
      }
      if (is.object(obj)) {
        objectlist$object[i] <- TRUE
      }
      
      # I think these are generally constants
      if (is.vector(obj)) {
        objectlist$constant[i] <- TRUE
      }
      
      
    }  
  }
  
  final = c(objectlist$name[objectlist$primitive],
  objectlist$name[objectlist$func  &  !objectlist$primitive],
  objectlist$name[objectlist$constant],
  objectlist$name[objectlist$object])
  return(final)
}
library(ggplot2)
library(plyr)
functionlist = c(showPackageContents("package:base"), showPackageContents("package:plyr"),
         showPackageContents("package:ggplot2"))
functionlist = as.factor(functionlist)
tablefunction = table(functionlist)
for(x in 1:length(tablefunction))
{tablefunction[[x]] = 0}

test = "a_ply is cool"

Q5function = function(tablelist, datasubset)
{
  for(i in 1:length(datasubset$text))
  {
    textvalue = datasubset$text[i]
    for(x in 1:length(tablelist))
    {
      greptest = grepl(names(tablelist[x]), textvalue)
      if(greptest == TRUE)
      {
        tablelist[[x]] = tablelist[[x]] + 1
      }
    }
  }
  return(tablelist)
}

tester = Q5function(tablefunction, questionsub)
head(sort(tester, decreasing = TRUE), n = 54)

#Q6

thetopones = subset(rQAs, rQAs$type == c("answer", "comment"))
thetopones = subset(thetopones, thetopones$score == "" | thetopones$score > 0)

Q6ans = Q5function(tablefunction, thetopones)
head(sort(Q6ans, decreasing = TRUE), n = 54)










