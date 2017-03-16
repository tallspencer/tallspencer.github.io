value1 = readLines("~/Spencer/School/College Junior/STA 141/nasa/NASA/cloudhigh1.txt")
value1
length(value1)
#Part 1
#set up data so it is in 4 columns for %, Long, Lat, and Time
#draw time first from time area
#next draw long and lat, ignore identifiers
#total for one will be 576, for all 72 is 41472

#split date away from everything else
testdate=substring(value1[5], 25, 35)

#pull long out of function and repeat it as needed to later move to rows
numOfValues=length(value1)-7
testlong = strsplit(value1[6], " +")[[1]][-1] #from piazza
testlong = rep(testlong, each = numOfValues)

#get all data by going across and then putting into set
testlat = sapply(1:numOfValues, function(x) strsplit(value1[8:31], " +")[[x]][2])
testlat = rep(testlat, times = numOfValues)

#extract percentages using a nested loop
printpercent = function(x, y) return(strsplit(value1[8:31], " +") [[x]][y+4])
TestPercentage=apply(expand.grid(c(1:numOfValues), c(1:numOfValues)), 1, function(xval, yval) printpercent(xval[1], xval[2]))
#data goes down the y axis first 

#to make dataframe
testframe = data.frame(TestPercentage, testlong, testlat, testdate)

#make function to create each of these sets
MakeDataFrame = function(filename)
{
  CurrentFile=readLines(filename)
  Date=substring(CurrentFile[5], 25, 35)
  numberOfValues = length((filename)) - 7
  
  numberOfValues=length(CurrentFile)-7
  Longitude = strsplit(CurrentFile[6], " +")[[1]][-1]
  Longitude = rep(Longitude, each = numberOfValues)
  
  Latitude = sapply(1:numberOfValues, function(x) strsplit(CurrentFile[8:31], " +")[[x]][2])
  Latitude = rep(Latitude, times = numberOfValues)
  
  printpercent = function(x, y) return(strsplit(CurrentFile[8:31], " +") [[x]][y+4])
  Percentage=apply(expand.grid(c(1:numberOfValues), c(1:numberOfValues)), 1, function(xval, yval) printpercent(xval[1], xval[2]))
  
  finalframe = data.frame(Percentage, Longitude, Latitude, Date)
  return(finalframe)
}

#for taking files in and making into a set for cloudhigh
cloudhighinput = paste("~/Spencer/School/College Junior/STA 141/nasa/NASA/cloudhigh", 1:72, ".txt", sep = "")
cloudhigh = lapply(cloudhighinput, function(input) MakeDataFrame(input))
class(cloudhigh)
cloudhigh = do.call(rbind,cloudhigh)
library(plyr)
cloudhigh = rename(cloudhigh, c("Percentage" = "Cloudhigh"))

#for cloudlow
cloudlowinput = paste("~/Spencer/School/College Junior/STA 141/nasa/NASA/cloudlow", 1:72, ".txt", sep = "")
cloudlow = lapply(cloudlowinput, function(input) MakeDataFrame(input))
cloudlow = do.call(rbind,cloudlow)
cloudlow = rename(cloudlow, c("Percentage" = "Cloudlow"))

#for cloudmid
cloudmidinput = paste("~/Spencer/School/College Junior/STA 141/nasa/NASA/cloudmid", 1:72, ".txt", sep = "")
cloudmid = lapply(cloudmidinput, function(input) MakeDataFrame(input))
cloudmid = do.call(rbind,cloudmid)
cloudmid = rename(cloudmid, c("Percentage" = "Cloudmid"))

#for ozone
ozoneinput = paste("~/Spencer/School/College Junior/STA 141/nasa/NASA/ozone", 1:72, ".txt", sep = "")
ozone = lapply(ozoneinput, function(input) MakeDataFrame(input))
ozone = do.call(rbind,ozone)
ozone = rename(ozone, c("Percentage" = "Ozone"))

#for pressure
pressureinput = paste("~/Spencer/School/College Junior/STA 141/nasa/NASA/pressure", 1:72, ".txt", sep = "")
pressure = lapply(pressureinput, function(input) MakeDataFrame(input))
pressure = do.call(rbind,pressure)
pressure = rename(pressure, c("Percentage" = "Pressure"))

#for surftemp
surftempinput = paste("~/Spencer/School/College Junior/STA 141/nasa/NASA/surftemp", 1:72, ".txt", sep = "")
surftemp = lapply(surftempinput, function(input) MakeDataFrame(input))
surftemp = do.call(rbind,surftemp )
surftemp = rename(surftemp, c("Percentage" = "Surftemp"))

#for Temperature
temperatureinput = paste("~/Spencer/School/College Junior/STA 141/nasa/NASA/temperature", 1:72, ".txt", sep = "")
temperature = lapply(temperatureinput, function(input) MakeDataFrame(input))
temperature = do.call(rbind,temperature)
temperature = rename(temperature, c("Percentage" = "Temperature"))

#Part 2
#check if the columns are identical for each of the 7 dataframes if all are TRUE then continue
identical(paste(cloudhigh$Longitude, cloudhigh$Latitude, cloudhigh$Date), paste(cloudlow$Longitude, cloudlow$Latitude, cloudlow$Date))
identical(paste(cloudlow$Longitude, cloudlow$Latitude, cloudlow$Date), paste(cloudmid$Longitude, cloudmid$Latitude, cloudmid$Date))
identical(paste(ozone$Longitude, ozone$Latitude, ozone$Date), paste(cloudmid$Longitude, cloudmid$Latitude, cloudmid$Date))
identical(paste(ozone$Longitude, ozone$Latitude, ozone$Date), paste(surftemp$Longitude, surftemp$Latitude, surftemp$Date))
identical(paste(temperature$Longitude, temperature$Latitude, temperature$Date), paste(surftemp$Longitude, surftemp$Latitude, surftemp$Date))

#combine other parts to the dataframe
Complete.df = cbind(cloudlow$Cloudlow, cloudmid$Cloudmid, cloudhigh$Cloudhigh, ozone$Ozone, pressure$Pressure, surftemp$Surftemp, temperature)
Complete.df =  rename(Complete.df, c("cloudlow$Cloudlow"= "Cloudlow" , "cloudmid$Cloudmid" = "Cloudmid", "cloudhigh$Cloudhigh" = "Cloudhigh", "ozone$Ozone" = "Ozone", "pressure$Pressure" = "Pressure", "surftemp$Surftemp" = "Surftemp"))

#Part 3
#check out the .dat file
elevation = read.csv("~/Spencer/School/College Junior/STA 141/nasa/NASA/intlvtn.dat", row.names=1, sep="")
class(elevation)

#make a function to create a nested loop as we did earlier, do this by making a function then applying
#go down collumns first.
GetElevation = function(x, y) return(elevation[x,y])
ElevationList=apply(expand.grid(c(1:nrow(elevation)), c(1:ncol(elevation))), 1, function(xval, yval) GetElevation(xval[1], xval[2]))

#add to Complete.df
Complete.df$Elevation = ElevationList

#Step 4 Part 1, plot temp vs pressure
#set values as numeric first
Complete.df[,1:7]= as.numeric(as.character(unlist(Complete.df[,1:7])))

#use ggplot to make the graph
library(ggplot2)
qplot(data = Complete.df, x = Temperature, y = Pressure, color = Cloudlow, main = "Pressure vs Temperature With Percent of Low Clouds" ) + scale_color_gradient(low = "yellow", high = "black")

#change Date to the date type
Complete.df$Date = as.Date(as.character(unlist(Complete.df$Date)), format = "%d-%b-%Y")

#make long and lat numbers
LongToNumeric = function(long)
{
  west = "W"
  east = "E"
  if(grepl(west, long, fixed = TRUE))
  {
    temp = strsplit(long, west)
    temp = as.numeric(temp)
    temp = -temp
    return(temp)
  }
  
  ifelse(grepl(east, long, fixed = TRUE))
  {
  temp = strsplit(long, east)
  temp = as.numeric(temp)
  return(temp)
  }
}
#then transfer this to a lapply function
#make into character first
Complete.df$Longitude = as.character(Complete.df$Longitude)
Complete.df$Longitude = lapply(Complete.df$Longitude, function(x) LongToNumeric(x))
Complete.df$Longitude = (as.numeric(Complete.df$Longitude))

#Then for latitude it is the same sort of thing
LatToNumeric = function(lat) 
{
  north = "N"
  south = "S"
  
  if(grepl(south, lat, fixed = TRUE))
  {
    temp = strsplit(lat, south)
    temp = as.numeric(temp)
    temp = -temp
    return(temp)
  }
  
  ifelse(grepl(north, lat, fixed = TRUE), yes = TRUE, no = FALSE)
  {
  temp = strsplit(lat, north)
  temp = as.numeric(temp)
  return(temp)
  }
}
Complete.df$Latitude = as.character(Complete.df$Latitude)
Complete.df$Latitude = lapply(Complete.df$Latitude, function(x) LatToNumeric(x))
Complete.df$Latitude = as.numeric(Complete.df$Latitude)

#find the four corners of the long and lat and then graph the temp vs date
topright = subset(Complete.df, Longitude == max(Longitude) & Latitude == max(Latitude))
topleft = subset(Complete.df, Longitude == min(Longitude) & Latitude == max(Latitude))
botright = subset(Complete.df, Longitude == max(Longitude) & Latitude == min(Latitude))
botleft = subset(Complete.df, Longitude == min(Longitude) & Latitude == min(Latitude))

topright.plot = qplot(data = topright, x = Date, y = Temperature, main = "Date vs Temperature for the Top Right Corner of Info" ) + geom_line()
topleft.plot = qplot(data = topleft, x = Date, y = Temperature, main = "Date vs Temperature for the Top Left Corner of Info" ) + geom_line()
botright.plot = qplot(data = botright, x = Date, y = Temperature, main = "Date vs Temperature for the Bottom Right Corner of Info" ) + geom_line()
botleft.plot = qplot(data = botleft, x = Date, y = Temperature, main = "Date vs Temperature for the Bottom Left Corner of Info") + geom_line()
#install.packages('Rmisc', dependencies = TRUE)
#library(Rmisc)
multiplot(topright.plot, topleft.plot, botright.plot, botleft.plot, cols = 2)

#make function that makes lists of the mean and SD of the values
LongLatMeans = function(df)
{
  
  SuperSplit = split(df, list(df$Longitude, df$Latitude))
  SuperSplitNames = names(SuperSplit)
  #starts with long at lowest then to lat at lowest and so on (goes down long first)
  SuperMeanList = sapply(1:length(SuperSplit), function(x) 
    {
      sapply(1:7, function (y) 
      {
        temp = SuperSplit[[x]][,y]
        mean(temp)
      })
    })
  rownames(SuperMeanList) <- paste(c("Cloudlow","Cloudmid", "Cloudhigh", "Ozone", "Pressure", "SurfTemp", "Pressure"))
  colnames(SuperMeanList) <- paste(SuperSplitNames)
  return(SuperMeanList)
}

LongLatSds = function(df)
{
  SuperSplit = split(df, list(df$Longitude, df$Latitude))
  SuperSplitNames = names(SuperSplit)
  SuperSdList = sapply(1:length(SuperSplit), function(x) 
  {
    sapply(1:7, function (y) 
    {
      temp = SuperSplit[[x]][,y]
      sd(temp)
    })
  })
  rownames(SuperSdList) <- paste(c("Cloudlow","Cloudmid", "Cloudhigh", "Ozone", "Pressure", "SurfTemp", "Pressure"))
  colnames(SuperSdList) <- paste(SuperSplitNames)
  return(SuperSdList)
}

AllMeans = LongLatMeans(Complete.df)
AllSds = LongLatSds(Complete.df)

#4.4
LongLatPressure = AllMeans["Pressure",]
LongLatPressure = as.data.frame(LongLatPressure)
LongLatPressure$Means = LongLatPressure$LongLatPressure
LongLatPressure$LongLatPressure = NULL
PressureNames = rownames(LongLatPressure)
#make function to cut at the second decimal so we can get the Longitude and Latitude seperately!
SplitNamesLong = function(list)
{
  #make list of lengths of the first part of the string
  length = lapply(1:length(list), function(y) nchar(unlist(strsplit(list[y], ".", fixed = TRUE))[1]))
  #knowing the second part is just 2 things, (the decimal point and the number) we just add 2 to it
  long = lapply(1:length(list), function(x) substring(list[x], 1, as.numeric(length[x]) + 2))
  return(long)
}

PressureLong = SplitNamesLong(PressureNames)
PressureLong = as.numeric(PressureLong)
LongLatPressure$Longitude = PressureLong

SplitNamesLat = function(list)
  {
  length = lapply(1:length(list), function(y) nchar(unlist(strsplit(list[y], ".", fixed = TRUE))[1]))
  #same as above just substring at different points
  long = lapply(1:length(list), function(x) substring(list[x], as.numeric(length[x]) + 4, nchar(list[x])))
  return(long)
}
#move everything into place, make things numeric yadda yadda
PressureLat = SplitNamesLat(PressureNames)
PressureLat = as.numeric(PressureLat)
LongLatPressure$Latitude = PressureLat

#commented out installs to prevent future issues
library(maps)
#install.packages('mapproj')
#install.packages('ggmap')
library(ggmap)
library(mapproj)
#crazy ggplot stuff that made it look sexy but I got most of the info online
worldmap = get_map(location = c(-115, -23, -55, 38), zoom = 3, source = "google", maptype = "terrain", color = "bw")
mymap = ggmap(worldmap) #prints map
points = mymap + geom_point(data = LongLatPressure, aes(x = Longitude, y = Latitude, color = Means)) + scale_color_gradient(low = "purple", high = "grey") + ggtitle('Mean Pressure at each Coordinate over Time') + labs(x= 'Longitude', y = 'Latitude')
points

#part 4.5
class(Complete.df$Elevation)
#group up elevations, so make it into a FACTOR WOOOOOOO 
Complete.df$Elevation = as.factor(Complete.df$Elevation)
Highaf = split(Complete.df, Complete.df$Elevation)
HighafMeans = lapply((1:length(Highaf)), function(x) mean(Highaf[[x]]$Surftemp))
namesof = names(Highaf)
plot(x = namesof, y = HighafMeans, main = "Average Surface Temperature vs Elevation", ylab = "Average Surface Temperature", xlab = "Elevation", pch = 20)



