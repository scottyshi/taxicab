options(digits=8) #this will allow for 8 digit variables (which we need)

#modified from "popularpaths.R"
getAllCoords <- function(file) {
  
  data <- read.csv(file, head=TRUE, sep=",")#loading in the data
  polylines <- data$POLYLINE # this will grab all the polylines from the data
  polylines <- levels(polylines) #this will contain all the data w/o suffix summary
  
  coords <- list()
  #matrix(, nrow=length(polylines), ncol=4) #creating the matrix
  # to hold the x/y start/end coordinates
  
  #for loop to grab start/end x/y of polylines
  for (i in 1:length(polylines))
  {
    str <- strsplit(strsplit(polylines[i],"\\[\\[")[[1]][2],"\\]\\]")[[1]][1] #get rid of start and end brackets
    arr <- strsplit(str, "\\],\\[")[[1]] #get all coords
    trip <- list()
    for (j in 1:length(arr))
    {
      x_y_tuple = as.numeric(strsplit(arr[j],",")[[1]])
      trip <- append(trip,list(x_y_tuple))
    }
    coords[[i]] = trip
  }
  return (coords)
}

#taken from "reformatXY.R"
getMaxMins <- function(coords) {
  X <- double()
  Y <- double()
  for (i in 1:length(coords)) {
    for (j in 1:length(coords[[i]])) {
      X <- c(X,as.numeric(coords[[i]][[j]][1]))
      Y <- c(Y,as.numeric(coords[[i]][[j]][2]))
    } 
  }
  return (c(min(X),max(X),min(Y),max(Y)))
}

#taken from "reformatXY.R"
translateAll <- function(coords,fourvector) {
  XFactor <- fourvector[2] - fourvector[1];
  YFactor <- fourvector[4] - fourvector[3];
  for (i in 1:length(coords)) {
    for (j in 1:length(coords[[i]])) {
      coords[[i]][[j]][1] <- (as.numeric(coords[[i]][[j]][1])-fourvector[1])/XFactor
      coords[[i]][[j]][2] <- (as.numeric(coords[[i]][[j]][2])-fourvector[3])/YFactor
    } 
  }
  return (coords)
}

#usage:
#input: filename, num clusters, time start (military), time range (minutes)
#output range of number of customers that are predicted 
#to pop up in each cluster at a given time

#From the training data, I will treat the first time tick of 
#each completed trip as a customer
predictCustomers <- function(file, numclust, start, range=10) {
    coords <- getAllCoords(file)
    data <- read.csv(file, head=TRUE, sep=",")#loading in the data
    fourvector <- getMaxMins(coords)
    coords <- translateAll(coords, fourvector)
    #this is the set of all coordinates, I want to take a random sample from it
    begpoints <- matrix(nrow=length(coords), ncol=2)
    for (i in 1:length(coords)){
        begpoints[i,1] = coords[[i]][[1]][1]
        begpoints[i,2] = coords[[i]][[1]][2]
    }
    keep <- vector() # keeps track of all indicies to keep based on 'start'
    for (i in 1:length(data$TIMESTAMP)) {
      #TODO: some processing to see what military time TIMESTAMP is
      #if TIMESTAMP time >= start and <= start + interval
      keep[i] = i
      #else
      keep[i] = 0 #do not keep the times that don't meet the requirements
    }
    begpoints <- begpoints[keep,] 
    #keep only the ones that have appropriate times
    #this is the entire set of all beginning coordinates
    samplesize <- length(begpoints) * .6 #I can change the sample size to 
                #be a percentage of the entire data
    replace=TRUE
    for (iter in 1:100) { #total number of trials I want to run this
        set.seed(1)
        mysamp <- begpoints[sample(nrow(begpoints),size=samplesize, replace=FALSE),]
        #TODO: save size of each cluster in this iteration
        # save all data gathered from all iterations for the clusters
        # construct a confidence interval telling how many customers you expect from each cluster
    }
    #return avg coordinates of each cluster and number of customers to expect around that location
    return(coords)
}