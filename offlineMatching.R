require(igraph)
options(digits=8)
last <- function(x) { tail(x, n = 1) }
#usage: 
#input: test data, timestamp to start reading from
# ** I will consider the time frame ['start', 'start' + 10 minutes]
#output: I will output in a matrix 1 column all "free taxi drivers" and
	#in another column all "waiting customers" --> indicies of data
#Free taxi drivers: those who completed their trips before 'start'
	#there might be duplicate taxi drivers, I'll only accept the latest
	#completed taxi driver
#Waiting Customers: trips that have not started yet within the given time interval


getAvg <- function(filename,ID) {
	data <- read.csv(filename, head=TRUE, sep=",") #reading the csv file
	data <- data[order(data$TIMESTAMP),]
	polylines <- data$POLYLINE # this will grab all the polylines from the data
	polylines <- levels(polylines) #this will contain all the data w/o suffix summary
	coords <- list()
	#matrix(, nrow=length(polylines), ncol=4) #creating the matrix
	# to hold the x/y start/end coordinates
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
	
	return (getAverageMoved(data,coords,ID))


}

getCoords <- function(data) {
	polylines <- data$POLYLINE # this will grab all the polylines from the data
	polylines <- levels(polylines) #this will contain all the data w/o suffix summary
	coords <- list()
	#matrix(, nrow=length(polylines), ncol=4) #creating the matrix
	# to hold the x/y start/end coordinates
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

getSet <- function(filename,num) {
	data <- read.csv(filename, head=TRUE, sep=",") #reading the csv file
	data <- data[order(data$TIMESTAMP),]
	coords <- getCoords(data)
	
	minTime <- data$TIMESTAMP[1]
	maxTime <- last(data$TIMESTAMP) - 10
	set <- c()
	startSet <- sample(minTime:maxTime,num)

	for (time in startSet) {
		print("Looking at the following time:")
		print(time)
		tuple <- findIncrease(data,coords,time,5)
		print("Result is:")
		print(tuple)
		set <- c(set,tuple[1]/tuple[2])
	}
	return (set)
}

findIncrease <- function(data,coords,start,interval=10) {
	###TAKEN FROM PREDICTOFFLINE
	
	
	
	
	partition <- getPartitionGraph(data,coords,start,interval)
	print("Got graph, now weighting edges")
	partition <- weightEdges(data,coords,partition,start)
	print("Now matching")
	match <- maximum.bipartite.matching(partition)
	
	
	return (compare(data,coords,match))
	
}

compare <- function(data,coords,match) {
	avgPredicted <- match$matching_size/match$matching_weight
	print("Now comparing to old")
	predictedDists <- c()
	actualDists <- c()
	for (i in 1:match$matching_size) {
		ID <- toString(match$matching[i])
		index <- as.numeric(match$matching[ID])
		for (j in (index-1):1) {
			foundID <- data$TAXI_ID[index]
			if (data$TAXI_ID[j] == ID) {
				predictedDists <- c(predictedDists,pointDistanceLatLongToKM(coords[[index]][[1]],last(coords[[j]])[[1]]))
				break
			}
		}
		for (k in (index-1):1) {
			foundID <- data$TAXI_ID[index]
			if (data$TAXI_ID[k] == foundID) {
				actualDists <- c(actualDists,pointDistanceLatLongToKM(coords[[index]][[1]],last(coords[[k]])[[1]]))
				break
			}
		}
	}
	return (c(mean(actualDists),mean(predictedDists)))
}

predictOffline <- function(filename,start,interval=10) {
	data <- read.csv(filename, head=TRUE, sep=",") #reading the csv file
	data <- data[order(data$TIMESTAMP),]
	coords <- getCoords(data)
	#sort entire datalist by timestamp
	
	
	partition <- getPartitionGraph(data,coords,start,interval)
	print("Got graph, now weighting edges")
	partition <- weightEdges(data,coords,partition,start)
	print("Now matching")
	matching <- maximum.bipartite.matching(partition)
	return (matching)
}

mapIDtoIndex<- function(data,ID,timestamp) {
	rt <- 0
	for (i in 1:length(data$TAXI_ID)) {
		if (as.numeric(data$TIMESTAMP[i]) > as.numeric(timestamp)) break
		if (data$TAXI_ID[i]==ID) rt <- i
	}
	return (rt)
}
#distance function between two points
#returns the distance between the two points
pointDistance <- function(p1, p2) { #i.e. pass in coords[[somedriver]][[length(coords[[somedriver]])]] for p1 point
                              #pass in coords[[somecustomer]][[1]] for p2 point
    xsq <- (as.numeric(p1[1]) - as.numeric(p2[1]))^2
    ysq <- (as.numeric(p1[2]) - as.numeric(p2[2]))^2
    return (sqrt(xsq + ysq))
}
pointDistanceLatLongToKM <- function(p1, p2) { #i.e. pass in coords[[somedriver]][[length(coords[[somedriver]])]] for p1 point
	phi1 <- p1[2] * pi / 180.0
	phi2 <- p2[2] * pi / 180.0
	deltaphi <- (p2[2]-p1[2]) * pi / 180.0
	deltalambda <- (p2[1]-p1[1])*pi/180.0
	a <- sin(deltaphi/2)^2 + cos(phi1) * cos(phi2) * sin(deltalambda/2)^2
	c <- 2 * atan2(sqrt(a),sqrt(1-a))
    return (6371 * c) #Earth's radius times fraction in KM
}





weightEdges <- function(data,coords, igraph,start) {
	edges <- E(igraph)
	print(length(edges))
	for (i in 1:length(edges)) {
		if (is.na(edges$spec[i]) || !edges$spec[i]) {
			if (i%%500 == 0) {
				percent <- as.double(i) / length(edges) * 100.0
				print(percent)
			}
			tuple <- ends(igraph,edges[i])
			num1 <- as.numeric(tuple[1])
			num2 <- mapIDtoIndex(data,tuple[2],start)
			if (num1 < 1 || num1 > length(coords)) {
				print("Assigning no weight, found an error")
					igraph <- set.edge.attribute(igraph,"weight",i,0)
				}
				else {
				coord1 <- coords[[num1]][[1]]
				coord2 <- last(coords[[num2]])[[1]]
				dist <- pointDistanceLatLongToKM(coord1,coord2)
				igraph <- set.edge.attribute(igraph,"weight",i,1/dist)
			}
		}
		else {
			if (i%%50 == 0) {
				percent <- as.double(i) / length(edges) * 100.0
				print(percent)
			}
			tuple <- ends(igraph,edges[i])
			coord1 <- coords[[as.numeric(tuple[1])]][[1]]
			dist <- getPredictedDistance(data,coords,5,tuple[2],start,coord1)
			igraph <- set.edge.attribute(igraph,"weight",i,1/dist)

		}
	}
	return (igraph)

}

getPartitionGraph <- function(data,coords, start, interval) { #default searches for 10 MINUTE intervals
	drivers <- vector()
	customers <- vector()
	specdrivers <- vector()
	specdriversIDs <- vector()
	#GETTING INDICIES OF FREE DRIVERS
	for (i in 1:length(data$TIMESTAMP)) {
	                                      #I add seconds to it's start timestamp to see when it ends (every tick = 15 seconds)
	    if(data$TIMESTAMP[i] < start) { #we have a free driver
			if ((data$TIMESTAMP[i] + (length(coords[[i]])-1)*15) >= start) {#bleeds into the interval
				specdrivers <- c(specdrivers,i)
				specdriversIDs <- c(specdriversIDs,data$TAXI_ID[i])
			}
		else {
	        	drivers <- c(drivers,data$TAXI_ID[i]) #taxi I
		}
	    }
	    else if(data$TIMESTAMP[i] >= start && data$TIMESTAMP[i] < (start + interval*60)) #this is an available customer in this time frame
	    {
	        customers <- c(customers,i)
	    }
	}
	customers <- unique(customers)
	drivers <- unique(drivers)
	drivers <- setdiff(drivers,specdriversIDs)
	print("Printing what I think the # of edges should be")
	numedges <- length(customers) * length(drivers)
	print(numedges)
	print("Could be as much as:")
	print(numedges + length(specdrivers) * length(customers))
	
	dim <- length(customers) * length(drivers)
	partitions <- matrix(nrow=dim, ncol=2) #col 1 = taxi drivers, col2 = customers
	for(i in 1:length(customers)) {
		for (j in 1:length(drivers)) {
	    		partitions[(i-1)*length(drivers)+j,1] = customers[i]
	    		partitions[(i-1)*length(drivers)+j,2]= drivers[j]
		}	
	}

	igraph <- graph.data.frame(partitions,directed=FALSE)

	igraph <- set.edge.attribute(igraph,"spec",E(igraph),FALSE)
	e <- vector()

	for (driver in specdrivers) {
		if (is.element(toString(data$TAXI_ID[driver]),V(igraph)$name)) {
		}
		else {
			igraph <- igraph + vertex(toString(data$TAXI_ID[driver]))
		}
		for (i in 1:length(customers)) {
			if ((data$TIMESTAMP[driver] + (length(coords[[driver]])-1)*15) <= data$TIMESTAMP[customers[i]]) {
				e <- append(e,customers[i])
				e <- append(e,data$TAXI_ID[driver])
				
			}

		}
	}
	print(length(e))
	for (i in seq(1,length(e),2)) {
		igraph <- igraph + edge(toString(e[i]),toString(e[i+1]),spec=TRUE)
	}
	V(igraph)$type<-bipartite.mapping(igraph)$type
	return (igraph)
	#return all of these indicies, can use it to grab endpoints of all
	#free drivers and customers to create a bipartite matching problem
}

#Takes DATA in sorted format.
getPartitions <- function(data, start, interval) { #default searches for 10 MINUTE intervals
		
	drivers <- vector()
	customers <- vector()
	numdrivers <- 1
	numcust <- 1
	#GETTING INDICIES OF FREE DRIVERS
	for (i in 1:length(data$TIMESTAMP)) {
	                                      #I add seconds to it's start timestamp to see when it ends (every tick = 15 seconds)
	    if(data$TIMESTAMP[i] < start && (data$TIMESTAMP[i] + (length(coords[[i]])-1)*15) <= start) { #we have a free driver
	        drivers[numdrivers] = i #saving the data index
	        numdrivers <- numdrivers + 1
	    }
	    else if(data$TIMESTAMP[i] >= start && data$TIMESTAMP[i] < (start + interval*60)) #this is an available customer in this time frame
	    {
	        customers[numcust] = i
	        numcust <- numcust + 1
	    }
	}
	#now i have a list of drivers that are free WITH duplicates
	#I want to keep unique driver id's and getting rid of duplicates with earlier time stamps
	#I will remove all duplicate taxi IDs in the following way (as well as keeping the most recent duplicate trip)
	remove <- vector()
	remcnt <- 1
	uniqueid <- vector()
	uniqcnt <- 2
	uniqueid[1] = drivers[length(drivers)]
	#for(i in (length(drivers)-1):1) {#look at all drivers
	    #uni <- TRUE
	    #for(j in 1:length(uniqueid)) {#compare against unique list
	       # if(data$TAXI_ID[i] == data$TAXI_ID[uniqueid[j]]) {#duplicate
	       #     remove[remcnt] = i
	      #      remcnt <- remcnt + 1
	     #       uni <- FALSE
	    #    }
	    #}
	    #if(uni) {#if it was unique, append to uniques
	      #  uniqueid[uniqcnt] = i
	     #   uniqcnt <- uniqcnt + 1
	    #}
	#}
	#drivers <- uniqueid
	
	#i also have a list of all customers in this time period
	dim <- max(length(customers), length(drivers))
	partitions <- matrix(nrow=dim, ncol=2) #col 1 = taxi drivers, col2 = customers
	for(i in 1:dim) {
	    partitions[i,1] = drivers[i]
	    partitions[i,2]= customers[i]
	}
	return(partitions)
	#return all of these indicies, can use it to grab endpoints of all
	#free drivers and customers to create a bipartite matching problem
}

getAverageMoved <- function(data,coords,ID) {
	loc <- vector()
	dist <- vector()
	for (i in 1:length(data$TAXI_ID)) {
		if (data$TAXI_ID[i]==ID) {
			if (length(loc) > 0) {
				dist <- c(dist, pointDistance(loc,coords[[i]][[1]]))
				
			}
			loc <- (last(coords[[i]])[[1]])
		}
	}
	dist <- na.omit(dist)
	return (mean(dist))

}


getPredictedDistance <- function(data,coords,usingFirstN,ID,stamp,coord1) {
	i <- mapIDtoIndex(data,ID,stamp)
	matrix <- getSimilarityUpToMatrix(coords,i,usingFirstN,.9999999)
	if (length(matrix) == 0) return (0)
	numClusters <- 5
	mat <- getProbabilities(matrix,numClusters)
	distance <- 0
	for (i in 1:(length(mat)/3)) {
		coord2 <- c(mat[i,2],mat[i,3])
		distance <- distance+ mat[i,1] * pointDistanceLatLongToKM(coord1,coord2)
	}
	return (distance)
}

#### TAKEN FROM KMEANS

getProbabilities <- function(mat, numClusters) {
	fit <- kmeans(mat, numClusters)
	prob <- matrix(nrow=numClusters, ncol=3)
	#initalizing all probabilities by 0
	for (i in 1:numClusters) {
		prob[i,1] = 0
	}
	clusvec <- fit$cluster

	#inputting frequency of each cluster
	for (i in 1:length(clusvec)) {
		prob[clusvec[i],1] = prob[clusvec[i],1] + 1
	}

	#divide by cluster size to get length
	for (i in 1:numClusters) {
		prob[i,1] = prob[i,1] / length(clusvec)
		prob[i,2] = fit$centers[i,1]
		prob[i,3] = fit$centers[i,2]
	}
	return(prob)
}

getSimilarityUpToMatrix <- function(coords, indexOfTrip,untilIndex,threshold=.999) {
	
	len <- length(coords)
	if (length(coords[[indexOfTrip]]) < untilIndex)
		untilIndex <- length(coords[[indexOfTrip]])
	masterList <- c()

	for (i in 1:len)
	{
		if (i == indexOfTrip) next
		for (j in 2:untilIndex) {
			
			if (length(coords[[i]]) < j) break
			comparisonVector <- c(coords[[indexOfTrip]][[1]],coords[[indexOfTrip]][[j]])
			if (cosineSimilarity(c(coords[[i]][[1]], coords[[i]][[j]]),comparisonVector) >= threshold)
			{
				if (j == untilIndex) {		
					masterList <- c(masterList, i)
				}
			} else {
				break
			}
		}
	}

	matrix <- matrix(nrow=length(masterList), ncol=2)
	if (length(masterList)>0) {
		for (i in 1:length(masterList)) {
			matrix[i,1]=coords[[masterList[i]]][[length(coords[[masterList[i]]])]][1]
			matrix[i,2]=coords[[masterList[i]]][[length(coords[[masterList[i]]])]][2]
		}
	}
	return (matrix)
}

cosineSimilarity <- function(vector1,vector2) {
	#if (length(vector1) != length(vector2)) {
	#	return NULL;
	#}
	dotProduct = 0;
	length1 = 0;
	length2 = 0;
	for (i in 1:length(vector1)) {
		dotProduct <- dotProduct + vector1[i] * vector2[i];
		length1 <- length1 + (vector1[i])^2;
		length2 <- length2 + (vector2[i])^2;
	}
	length1 <- sqrt(length1);
	length2 <- sqrt(length2);
	product <- dotProduct/(length1 * length2);
	return (product);
}




