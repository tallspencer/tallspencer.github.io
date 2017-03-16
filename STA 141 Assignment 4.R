#Spencer Phillips
#998887568
#STA 141 Assignment 4

load(url("http://eeyore.ucdavis.edu/stat141/Data/vehicles.rda"))
install.packages("stringr")
library(stringr)

#Q1
#make function to check for the price
findprice = function(num)
{
  bodyval = vposts$body[num]
  cash = as.vector(str_match(bodyval, "\\$[0-9]+\\,?[0-9][0-9]+"))
  cash = gsub("\\$|,", "", cash)
  return(cash)
}

nrow(vposts)
vposts$bodyprice = findprice(1:nrow(vposts))
pricevs = table(vposts$bodyprice == vposts$price)
pricevs/sum(pricevs)
sum(pricevs)

#Q2
#VINS have 17 alphanumeric digits

findvin = function(num)
{
  bodyval = vposts$body[num]
  vin = as.vector(str_match(bodyval,"[A-Z0-9a-z]{11}[0-9]{6}"))
  return(vin)
}

vposts$vin = findvin(1:nrow(vposts))

blarg = table(vposts$vin)
sum(blarg)
sum(blarg)/nrow(vposts)

#Q3
findphonenumbers = function(num)
{
  bodyval = vposts$body[num]
  numbers = as.vector(str_match(bodyval, "\\(?[2-9][0-9]{2}\\)?-?[:blank:]?[2-9][0-9]{2}-?[:blank:]?[0-9]{4}"))
  return(numbers)
}
vposts$phonenum = findphonenumbers(1:nrow(vposts))
sum(table(vposts$phonenum))
sum(table(vposts$phonenum))/nrow(vposts)

#Q4
findemail = function(num)
{
  bodyval = vposts$body[num]
  emails = as.vector(str_match(bodyval, "[[:alnum:].,!?-_#$&]+@[[:alnum:][:punct]]+\\.[A-Za-z]{3}"))
  return(emails)
}

vposts$emails = findemail(1:nrow(vposts))
table(blarg)
sum(table(blarg))

#Q5
findyear = function(num)
{
  headerval = vposts$header[num]
  years = as.vector(str_match(headerval, "(([1][9][3456789][0-9])|([2][0][01][0-9]))"))
  years = years[1]
  return(years)
}
vposts$bodyyear 
merr = sapply(1:nrow(vposts), function(x) findyear(x))
table(merr)
sum(table(merr))/34677
#compare to true year and get proportion
table(vposts$year == vposts$bodyyear)/sum(table(vposts$bodyyear))

#Q6
#Find the models
tempheader = vposts$header
makers = paste(names(table(vposts$maker)), collapse = "|")
headtype = "[0-9]{4}[[:space:]]+acura|chevy|alfa romeo|amc|aston martin|audi|bentley|bmw|bricklin|bugatti|buick|cadillac|chevrolet|chrysler|daewoo|datsun|desoto|dodge|eagle|ferrari|fiat|ford|freightliner|geo|gmc|harley davidson|honda|hudson|hummer|hyundai|infiniti|international|isuzu|jaguar|jeep|kia|lamborghini|land rover|leaf|lexus|lincoln|mack|maserati|mazda|mercedes|mercury|mg|mini|mitsubishi|nissan|oldsmobile|peterbilt|peugeot|plymouth|pontiac|porsche|rolls royce|saab|saturn|scion|shelby|smart|studebaker|subaru|suzuki|tesla|toyota|triumph|volkswagen|volvo|willys|yerfdog|zap[[:space:]]+([:alnum:]+)"
#gsub(headtype, "\\1", c("2006toyota prius", "chevrolet car", "bentley british", "chevy car"), ignore.case = TRUE)

tempheader = gsub(headtype, "\\1", vposts$header, ignore.case = TRUE)
models = sub("[0-9]{4}[[:space:]]+", "", tempheader, ignore.case = TRUE)
vposts$models = models
head(models)
table(models)
# ModelData <- read.csv("G:/Library/Spencer/Downloads/automotive-model-year-data-master/automotive-model-year-data-master/data.sql", header=FALSE)

sort(table(toupper(models)))
vposts$age = 2015 - vposts$year

camrymodel = lm(price~age + odometer + condition + city, models == "CAMRY", data = vposts)
civicmodel = lm(price~age + odometer + condition + city, models == "CIVIC", data = vposts)








