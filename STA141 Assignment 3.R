#STA 141 Assignment 3
#Spencer Phillips
#998887568
#11/3/15

digits <- read.csv("~/Spencer/School/College Junior/STA 141/digitsTrain.csv")
library(class)

#check out the data

#following functions are from lecture to view the values
getImage =  function(vals)
{
  matrix(as.integer(vals), 28, 28, byrow = TRUE)
}
draw = function(vals, colors = rgb((255:0)/255, (255:0)/255, (255:0)/255), ...)
{
  if(!is.matrix(vals))
    vals = getImage(vals)
  
  m = t(vals)
  m = m[,nrow(m):1]
  # rotate
  image(m, col = colors, ..., xaxt = "n", yaxt = "n")
} #from lecture

draw(colMeans(digits[["0"]][,-1]))
draw(colMeans(digits[["5"]][,-1]))
draw(digits[1, -1])

dim(digits)

#function to shuffle up the data, use once
shuffle = function(data) 
{
  data[sample(nrow(data)),]
}

#mix up the data
digits = shuffle(digits)
Labels = digits["label"]


#function creates distance matrix for a dataset and selected type
mkDistance = function(data, type)
{
  dist = dist(data, method = type)
  dist = as.matrix(dist)
  return(dist)
}

#create distance matrix for euclidean
distanceE = mkDistance(digits[,-1], "euclidian")

#set up training and test sets

#makes the training set for a set number of folds, also names the values
#sets it up in a list which each list named "training#" by default
mkTrainSets = function(distance, fold, names = "training")
{ 
  TheSplit = nrow(distance)/fold
  ListofDF = lapply(c(1:fold), function(x)
  { #splits up the set
    test = distance[(1+(x-1)*TheSplit) : (x*TheSplit) , -((1+(x-1)*TheSplit) : (x*TheSplit))]
    return(test)
  })
  varnames = sapply(c(1:fold), function(x) {sprintf("%s%s", names, x)})
  names(ListofDF) = varnames #sets up the names of the variables, needs to be split up later
  return(ListofDF)
}

TrainersE = mkTrainSets(distanceE, 5, names = "trainingE")
#list2env(TrainersE, environment()) #splits up the data no longer needed

#makes the test set for a set number of folds, also names the values
#sets it up in a list which each list named "test#" by default
mkTestSets = function(distance, fold, names = "test")
{ #this is the same as above basically, just with a few changes to where the breaks are
  TheSplit = TheSplit = nrow(distance)/fold
  ListofDF = lapply(c(1:fold), function(x)
  {
    test = distance[(1+(x-1)*TheSplit) : (x*TheSplit) , ((1+(x-1)*TheSplit) : (x*TheSplit))]
    return(test)
  })
  varnames = sapply(c(1:fold), function(x) {sprintf("%s%s", names, x)})
  names(ListofDF) = varnames #sets up the names of the variables, needs to be split up later
  return(ListofDF)
}

TestsE = mkTestSets(distanceE, 5, names = "testE")


#sets up training labels, which is a list of the labels for the training set
#for each fold
getTrainingLabels = function(totalLabel, fold)
{ #function that gives labels for training set
  split = nrow(totalLabel)/fold
  ListOfLabels = lapply(c(1:fold), function(x) {totalLabel[-(((x-1)*split + 1) : (x*split)),1]})
  return(ListOfLabels)
}

TrainingLabels = getTrainingLabels(Labels, 5)

#Gives the test labels, makes it easier in the future
getTestLabels = function(totalLabel, fold)
{ 
  split = nrow(totalLabel)/fold
  ListOfLabels = lapply(c(1:fold), function(x) {totalLabel[(((x-1)*split + 1) : (x*split)),1]})
  return(ListOfLabels)
}

TestLabels = getTestLabels(Labels, 5)

#This function takes in the testset and a number k and runs a knn set over it, it gives back the expected values
ktest = function(testset, k, setnumber, labelset)
{
  ord = t(apply(testset[[setnumber]], 1, function(row) {labelset[[setnumber]][order(row)][1:k]}))
  #checks the testset and puts whatever the labesls are on top of it
  if(k==1)
  {
    bigset = sapply(1:length(ord), function(x) names(which.max(table(ord[x]))))
  }
  else
  {
    bigset = apply(ord, 1, function(x) names(which.max(table(x)))) #found online, picks most common value
  }
  return(bigset)
}


testerE1 = ktest(TrainersE, 20, 1, TrainingLabels)

#This function takes in the bellow discribed variables and then 
#gives back a list of the misclassification rates for each value k
bestk = function(set, trainlabel, maxk, setnumber, testlabel)
{ #set is the training set, label is the labels that coresponds
  #maxk is what you are going to overall, setnumber is what set you are checking
  #mainlabel is the overarching Label set
  split = nrow(set[[setnumber]])
  finalset = lapply(c(1:maxk), function(x) { 
    ks = ktest(set, x, setnumber, trainlabel)
    tbls = table(ks == testlabel[[setnumber]])
    value = as.numeric(tbls["FALSE"]/split)
    return(value)
  })
  return(finalset)
}

FinalE = lapply(c(1:5), function(x) as.numeric(bestk(TrainersE, TrainingLabels, 30, x, TestLabels)))
# test1E = as.numeric(bestk(TrainersE, TrainingLabels, 30, 1, TestLabels))
# test2E = as.numeric(bestk(TrainersE, TrainingLabels, 30, 2, TestLabels))
# test3E = as.numeric(bestk(TrainersE, TrainingLabels, 30, 3, TestLabels))
# test4E = as.numeric(bestk(TrainersE, TrainingLabels, 30, 4, TestLabels))
# test5E = as.numeric(bestk(TrainersE, TrainingLabels, 30, 5, TestLabels))
# plot(x = test1E, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Euclidean best KNN Group 1")
# plot(x = test2E, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Euclidean best KNN Group 2")
# plot(x = test3E, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Euclidean best KNN Group 3")
# plot(x = test4E, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Euclidean best KNN Group 4")
# plot(x = test5E, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Euclidean best KNN Group 5")

#distance matrix for Manhattan
distanceM = mkDistance(digits[,-1], "manhattan")

TrainersM = mkTrainSets(distanceM, 5, names = "trainingM")
TestsM = mkTestSets(distanceM, 5, names = "testM")
testerM = ktest(TrainersM, 50, 1, TrainingLabels)

FinalM = lapply(c(1:5), function(x) as.numeric(bestk(TrainersM, TrainingLabels, 30, x, TestLabels)))
# test1M = as.numeric(bestk(TrainersM, TrainingLabels, 30, 1, TestLabels))
# test2M = as.numeric(bestk(TrainersM, TrainingLabels, 30, 2, TestLabels))
# test3M = as.numeric(bestk(TrainersM, TrainingLabels, 30, 3, TestLabels))
# test4M = as.numeric(bestk(TrainersM, TrainingLabels, 30, 4, TestLabels))
# test5M = as.numeric(bestk(TrainersM, TrainingLabels, 30, 5, TestLabels))
# plot(x = test1M, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Manhattan best KNN Group 1")
# plot(x = test2M, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Manhattan best KNN Group 2")
# plot(x = test3M, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Manhattan best KNN Group 3")
# plot(x = test4M, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Manhattan best KNN Group 4")
# plot(x = test5M, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Manhattan best KNN Group 5")
plot(x = FinalM[[1]], y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Groups of all values", pch = 20, asp = 80)
points(FinalM[[2]], col = "blue", pch = 24)
points(FinalM[[3]], col = "red", pch = 15)
points(FinalM[[4]], col = "pink", pch = 17)
points(FinalM[[5]], col = "grey", pch = 10)
points(FinalE[[1]], col = "green", pch = 18)
points(FinalE[[2]], col = "darksalmon", pch = 1)
points(FinalE[[3]], col = "rosybrown3", pch = 3)
points(FinalE[[4]], col = "grey47", pch = 16)
points(FinalE[[5]], col = "palegreen", pch = 2)
legend("bottomright", legend =c("Manhattan #1", "Manhattan #2", "Manhattan #3", "Manhattan #4", "Manhattan #5"), 
       pch = c(20, 24, 15, 17, 10), col = c("black", "blue", "red", "pink", "grey"))
legend("bottom", legend = c("Euclidean #1", "Euclidean #2",  "Euclidean #3",  "Euclidean #4",  "Euclidean #5"), pch = c(18, 1, 3, 16, 2), col = c("green", "darksalmon", "rosybrown3", "grey47", "palegreen"))

meanM = (FinalM[[1]] + FinalM[[2]] + FinalM[[3]] + FinalM[[4]] + FinalM[[5]])/5
meanE = (FinalE[[1]] + FinalE[[2]] + FinalE[[3]] + FinalE[[4]] + FinalE[[5]])/5

plot(meanE, y = NULL, ylab = "Misclassification Rate", xlab = "Value of K", main = "Means of Manhattan and Euclidian Types", pch = 20, asp = 80)
points(meanM, pch = 24)
legend("bottomright", legend = c("Euclidean", "Manhattan"), pch = c(20, 24))
#Set up all above functions first cause it relies on them
#takes in the distance matrix and what you want, then gives the expecations for a value k
Spencerknn = function(distanceMatrix, labels, k, setnumber, fold)
{
  TrainSets = mkTrainSets(distanceMatrix, fold)
  TestSets = mkTestSets(distanceMatrix, fold)
  trainlabels = getTrainingLabels(labels, fold)
  testlabels = getTestLabels(labels, fold)
  expectations = ktest(TrainSets, k, setnumber, trainlabels)
  return(expectations)
}

dat = lapply(c(1:2), function(x) {final = Spencerknn(distanceE, Labels, 3, x, 2)
return(final)})
dat = unlist(dat)
dat = as.matrix((dat))
#make confusion matrix

confusionMatrix(dat, Labels)
Lab = as.matrix(Labels)
Confusion = table(Lab, dat)
Confusion
draw(Confusion["0"])

par(mfrow = c(2,2), mar = rep(0, 4))
rownames(dat) = rownames(Labels)
wrongstuff = (dat == Lab)
wrongstuff = as.data.frame(wrongstuff)
draw(digits["116",-1]) #8 as 0
draw(digits["2204",-1]) #5 mistaken as 3
draw(digits["482",-1]) #5 mistaken as 9
draw(digits["2077",-1]) #7 as 9

#part 2
#splits into 5 parts the data, for training sets
splitdataparts = split(digits, 1:5)

# takes the first piece of the split data, splits by label, then makes into a 10x784 matrix
# setup for function below
trainsplit1 = do.call(rbind, splitdataparts[-1])
trainsplit1 = split(trainsplit1, trainsplit1$label)
trainsplit1 = lapply(1:10, function(x) colMeans(trainsplit1[[x]][-1]))
trainsplit1 = do.call(rbind, trainsplit1)
testsplit1 = splitdataparts[[1]][-1]
distancesetup1 = rbind(testsplit1,trainsplit1)

#Takes in the overall data, the number of folds, and the test set number you are interested in
#returns a 1010x1010 distance matrix
part2getDist = function(data, fold, testset, type)
{
  whole = split(data, 1:fold)
  trainingset = do.call(rbind, whole[-testset])
  trainingset = split(trainingset, trainingset$label)
  trainingset = lapply(c(1:10), function(x) colMeans(trainingset[[x]][-1]))
  trainingset = do.call(rbind, trainingset)
  testingset = whole[[testset]][-1]
  setup = rbind(testingset, trainingset)
  dmatrix = mkDistance(setup, type)
  return(dmatrix)
}
Part2DistE = lapply(1:5, function(x) part2getDist(digits, 5, x, "euclidean"))
Part2DistM = lapply(1:5, function(x) part2getDist(digits, 5, x, "manhattan"))

LabelsPart2 = getTestLabels(Labels, 5)

#gets the closest value to the mean
getclosestvalues = function(distmatrix, TestLabels)
{ #splits apart the training part
  setup = distmatrix[1:1000, 1001:1010]
  colnames(setup) = c(0:9)   #changes the names so it is 0 to 9
  closest = apply(as.array(1:nrow(setup)), 1, function(x) 
  { #loop to check the minimum and then get the collumn name
    val = which.min(setup[x,])
    names = colnames(setup)[val]
    return(names)
  })
  tbls = table(closest == TestLabels)  #takes list of responses and tests against actual data
  value = as.numeric(tbls["FALSE"]/1000) #gives back percentage
  return(value)
}

Part2E = lapply(1:5, function(x) getclosestvalues(Part2DistE[[x]], LabelsPart2[[x]]))
Part2M = lapply(1:5, function(x) getclosestvalues(Part2DistM[[x]], LabelsPart2[[x]]))

Part2meanE = (Part2E[[1]] + Part2E[[2]] + Part2E[[3]] + Part2E[[4]] + Part2E[[5]])/5
Part2meanM = (Part2M[[1]] + Part2M[[2]] + Part2M[[3]] + Part2M[[4]] + Part2M[[5]])/5

barplot(c(Part2meanE,Part2meanM), names.arg = c("Euclidean", "Manhattan"), ylab = "Misclassification Rate", main = "Misclassification Rate of comparing values to average one")

##setup for above
setup = Part2DistE[[1]]
setup = setup[1:1000, 1001:1010]
colnames(setup) = c(0:9)
closest = apply(as.array(1:nrow(setup)), 1, function(x) 
  {
    val = which.min(setup[x,])
    names = colnames(setup)[val]
    return(names)
  })

merr = table(closest == LabelsPart2[[4]])
as.numeric(merr["FALSE"])
