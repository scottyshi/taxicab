options(digits=8)

#usage: 
#input: test data, timestamp to start reading from
# ** I will consider the time frame ['start', 'start' + 10 minutes]
#output: I will output in a matrix 1 column all "free taxi drivers" and
	#in another column all "waiting customers" --> indicies of data
#Free taxi drivers: those who completed their trips before 'start'
	#there might be duplicate taxi drivers, I'll only accept the latest
	#completed taxi driver
#Waiting Customers: trips that have not started yet within the given time interval

getPartitions <- function(file, start, interval=10) { #default searches for 10 MINUTE intervals
	data <- read.csv(file, head=TRUE, sep=",") #reading the csv file
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
	
	#sort entire datalist by timestamp
	data <- data[order(data$TIMESTAMP),]
	
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
	    if(data$TIMESTAMP[i] >= start && data$TIMESTAMP[i] < (start + interval*60)) #this is an available customer in this time frame
	    {
	        customers[numcust] = i
	        numcust <- numcust + 1
	    }
	}
	#now i have a list of drivers that are free WITH duplicates
	#I will remove all duplicate taxi IDs in the following way (as well as keeping the most recent duplicate trip)
	
	#i also have a list of all customers in this time period
	partitions <- matrix(nrow=(max(numcust, numdrivers)), ncol=2) #col 1 = taxi drivers, col2 = customers
	for(i in 1:(max(numcust, numdrivers))) {
	    partitions[i,1] = drivers[i]
	    partitions[i,2]= customers[i]
	}
	return(partitions)
	#return all of these indicies, can use it to grab endpoints of all
	#free drivers and customers to create a bipartite matching problem
}


#getMatching
#match free taxi drivers and waiting customers based on distance
	#closer = better match