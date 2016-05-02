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

#distance function between two points
#returns the distance between the two points
pointDistance <- function(p1, p2) { #i.e. pass in coords[[somedriver]][[length(coords[[somedriver]])]] for p1 point
  #pass in coords[[somecustomer]][[1]] for p2 point
  xsq <- (as.numeric(p1[1]) - as.numeric(p2[1]))^2
  ysq <- (as.numeric(p1[2]) - as.numeric(p2[2]))^2
  return (sqrt(xsq + ysq))
}

#get a master list of all the clusters in a 
getCenters <- function(CLUS, trials)
{
  mastercenterx <- vector() #creating a master list of the centers of all centers
  mastercentery <- vector()
  numcenters <- 1
  for (i in 1:trials) #looking at all clusters
  {
    for(j in 1:numclust) #looking at each cluster per iteration
    {
      mastercenterx[numcenters] = CLUS[[i]][j,2]
      mastercentery[numcenters] = CLUS[[i]][j,3]
      numcenters <- numcenters + 1
      
    } 
  }
  len <- length(mastercenterx)
  for(i in 1:len)
  {
    for(j in (i+1):len)
    {
      dist <- (pointDistance(c(mastercenterx[i],mastercentery[i]),c(mastercenterx[j],mastercentery[j])))
      if(!is.na(dist))
      {
        if ( dist <= 0.02) 
        { #the two clusters are close enough to each other so i will count it as the same
          mastercenterx[j] = 0
          mastercentery[j] = 0
        }
      }
    }
  }
  mastercenterx <- mastercenterx[mastercenterx != "0"]
  mastercentery <- mastercentery[mastercentery != "0"]
  mastercenter <- matrix(nrow=length(mastercenterx),ncol=2)
  mastercenter[,1] = mastercenterx
  mastercenter[,2] = mastercentery
  return (mastercenter)
}

#usage:
#input: filename, num clusters, time start (i.e. 1, 2, 10, 13, 16, 24), 
    #start minute(10, 39, 58, etc), range of times allowed (over 10 min interval)
#output range of number of customers that are predicted 
#to pop up in each cluster at a given time

#From the training data, I will treat the first time tick of 
#each completed trip as a customer
predictCustomers <- function(file, numclust, starthour, startmin, range=10) {
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
    for (i in 1:(length(data$TIMESTAMP))) {
      time <- as.POSIXlt(data$TIMESTAMP[i], origin="1970-01-01") #using unix timestamp
      if (time$hour == starthour && time$min >= startmin && time$min <= (startmin + range))
          keep[i] = i
      else
          keep[i] = 0 #do not keep the times that don't meet the requirements
    }
    begpoints <- begpoints[keep,] 
    #keep only the ones that have appropriate times
    #this is the entire set of all beginning coordinates
    samplesize <- as.integer(length(begpoints)/2 * .6) #I can change the sample size to 
                #be a percentage of the entire data
    replace=TRUE
    set.seed(1)
    CLUS <- list() #list of all cluster information
    trials <- 100
    for (iter in 1:trials) { #total number of trials I want to run this
        mysamp <- begpoints[sample(nrow(begpoints),size=samplesize, replace=replace),]
        fit <- kmeans (mysamp, numclust)
        est <- matrix(nrow=numclust, ncol=3) #this will hold the data for one iteration
        est[,1] = fit$size
        est[,2] = fit$centers[,1]
        est[,3] = fit$centers[,2]
        # save all data gathered from all iterations for the clusters
        CLUS[[iter]] <- est
    }
    centers <- getCenters(CLUS, trials)
    occurrences <- matrix(nrow=(length(centers)/2), ncol=trials) # number people who popped up in each cluster @ each trial
    for (i in 1:trials)
    {
        for(j in 1:numclust)
        {
            for(k in 1:(length(centers)/2))
            {
                if(pointDistance(c(CLUS[[i]][j,2], CLUS[[i]][j,3]),c(centers[k,1], centers[k,2])) < 0.02)
                {
                     if(is.na(occurrences[k,i]))
                        occurrences[k,i] <- CLUS[[i]][j,1]
                     else
                        occurrences[k,i] <- occurrences[k,i] + CLUS[[i]][j,1]
                }
            }
        }
    }
    #now occurrences has the information of the number of people who popped up in each cluster in each trial
    #i want to return the confidence interval of # of customers i expect to pop up in each region
    intervals <- matrix(nrow=(length(centers)/2), ncol=4)
    intervals[,1] = centers[,1]
    intervals[,2] = centers[,2]
    for (i in 1:(length(centers)/2))
    {
        mu <- summary(occurrences[i,])[[4]]
        s <- sd(occurrences[i,], na.rm=TRUE)
        if((mu - 2*s) < 0)
          intervals[i,3] = 0 #cannot interpret negative values
        else
          intervals[i,3] = mu - 2*s
        intervals[i,4] = mu + 2*s
    }
    return(intervals)
}