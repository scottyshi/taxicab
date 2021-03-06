
getAllCoords <- function(file) {

	data <- read.csv(file, head=TRUE, sep=",")#loading in the data
	polylines <- data$POLYLINE # this will grab all the polylines from the data
	#polylines <- polylines[-c(1)] #this will delete the first row of data
	polylines <- levels(polylines) #this will contain all the data w/o suffix summary

	options(digits=8) #this will allow for 8 digit variables (which we need)

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


getStartEndCoords <- function(file) {

	data <- read.csv(file, head=TRUE, sep=",")#loading in the data
	polylines <- data$POLYLINE # this will grab all the polylines from the data
	#polylines <- polylines[-c(1)] #this will delete the first row of data
	polylines <- levels(polylines) #this will contain all the data w/o suffix summary

	options(digits=8) #this will allow for 8 digit variables (which we need)

	coords <- matrix(, nrow=length(polylines), ncol=4) #creating the matrix
					# to hold the x/y start/end coordinates

	#for loop to grab start/end x/y of polylines

	for (i in 1:length(polylines))
	{
		str <- polylines[i]
		arr <- as.list(strsplit(str, ",")[[1]]) #get all coords
		#some data only have a starting x/y but not an ending
		#i just leave the entire row N/A in this case
		if(length(arr)!= 2)
		{
			coords[i, 1] <- as.numeric(substr(arr[[1]], 3, 12))
			coords[i, 2] <- as.numeric(strsplit(arr[[2]], "[]]"))
			coords[i, 3] <- as.numeric(substr(arr[[length(arr)-1]], 2, 11))
			coords[i, 4] <- as.numeric(unlist(strsplit(arr[[length(arr)]], "[]]"))[1])
		}
	}
	#now coords holds all the x/y coordinates of start/end of every trip

	return (coords)
}


#usage, cosineSimilarity(matrix[1,],matrix[2,])
#returns the cosine of the angle in-between 
# ~1 means they are very similar
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

#usage: groupSimilars(matrix)
#assuming coords is an (n x 4) matrix
###############################################################################
groupSimilars <- function(coords,similarIndex, offset=4, threshold=0.9999999)
{
	#I want to group together trips that are similar within some threshhold
	#the cosine similarity function returns cos(angle between 2 vectors)
	#i'll only group two paths together as 'similar' if they pass a certain
	#threshold 

	comparisonVector <- c(coords[[similarIndex]][[1]],coords[[similarIndex]][[offset]])
	len <- length(coords)

	masterList <- c()

	for (i in 1:len)
	{
		if (i == similarIndex) next
		if (length(coords[[i]]) < offset) next
		if (cosineSimilarity(c(coords[[i]][[1]], coords[[i]][[offset]]),comparisonVector) >= threshold)
		{				
			masterList <- c(masterList, i); 
		}
	}
	return (masterList);

}
################################################################


# First, find the largest latitude and longitude, and create XY box.
# Second, Data Structures for the points
# Third, Retune the similarity
# Finally, write a function that average out the endpoints




#input: all NORMALIZED paths that seem to have a similar endpoint as the path
	#whose endpoint we are trying to predict
#output: a 2 item vector, "predict"
	#if i see that there is a similar endpoint, I will return
		#predict[1] = predicted xcoord = mean(x), #predict[2] = mean(y)
	#if i don't see there's a similar endpoint, I will return
		#predict[1] = n/a, predict[2] = n/a

################################################################
determineSimilarEnd <- function(coords) {
	xcoords <- vector() #this will keep a track of all the end x coords
	ycoords <- vector() #this will keep track of all the end y coords
	
	#filling in 'xcoords' and 'ycoords'
	for (i in 1:length(coords)) {
		xcoords[i] = coords[[i]][[length(coords[[i]])]][1]
		ycoords[i] = coords[[i]][[length(coords[[i]])]][2]
	}

	#this standard deviation function 'sd' uses Besel's correction
		#it will use denominator "n-1" instead of "n"
	sdx <- sd(xcoords)
	sdy <- sd(ycoords)
	mx <- mean(xcoords)
	my <- mean(ycoords)
	cvx <- sdx / mx
	cvy <- sdy / my
	#cvx and cvy are coefficients of variations, they describe how much %
	#variation is within a dataset based on the mean
	#I will use it in the following way: if the cv for x and y are below
	#a certain 'threshold', i'll determine that I can conclude a ending x,y
	#coordinate from this 

	threshold <- 0.05 #5% variability based on the mean
	predict <- vector()
	if(cvx < threshold && cvy < threshold) { #can conclude, return means
		predict[1] = mx
		predict[2] = my
	}
	else {
		is.na <- predict[1]
		is.na <- predict[2]
	}
	return(predict)
}
################################################################################


#Function in order to guess the endpoints of a function
################################################################################
#args: list, list of coordinates to guess based on


predictEndPoints <- function(coords,similarIndex,offset, isTranslated=FALSE) {
	#First, get list of similar
	similars <- c()
	if (isTranslated) {
		similar <- groupSimilars(coords,similarIndex,offset,.999)
	}
	else {
		similar <- groupSimilars(coords,similarIndex,offset)
	}
	wantedcoords <- list()
	for (i in 1:(length(similar))) {
		wantedcoords[[i]]=coords[[similar[i]]]
	}
	
	return (determineSimilarEnd(wantedcoords))

}









	#cosine similarity -- 2 routes will be similar if there's close points
	#put all points into k-dimensional space
	#use start / end x and y coordinates to create 4 dimensional vectors
	#	for EACH trip --> create a vector for each trip
	#when comparing two trips --> if angle is small between them -- they're similar

	#1. see how geometrically similar two routes are
	#2. see what kind of time difference some geometrically similar routes are

	#PART TWO!!!
	#We have data about where trips start
	#We want to predict where it will end (w/ 

#usage: OLDgroupSimilars(matrix)
#assuming coords is an (n x 4) matrix
###############################################################################
groupSimilarsOld <- function(coords,threshold=0.9999999)
{
	#I want to group together trips that are similar within some threshhold
	#the cosine similarity function returns cos(angle between 2 vectors)
	#i'll only group two paths together as 'similar' if they pass a certain
	#threshold 

	len <- length(coords)/4

	masterList <- list()

	for (i in 1:(len-1))#n^2 function to make all comparisons
	{
		rowSim <- c(i); #this will hold a row of all items 
		for (j in 1:i) 
		{
			if (is.element(i,masterList[[j]])) {
				rowSim <- c(rowSim, j);
			}
				
		}
		for (j in (i+1):len)
		{
			if (cosineSimilarity(coords[i,], coords[j,]) >= threshold)
			{	#I want to group together similar paths
				#I'll append similar paths in one row
				rowSim <- c(rowSim, j); 
			}
		}
		masterList[[i]] = rowSim;
	}
	return (masterList);

}
################################################################