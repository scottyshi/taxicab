options(digits=8)

#usage: 
#input: test data, timestamp to start reading from
# ** I will consider the time frame ['start', 'start' + 10 minutes]
#output: I will output in a list 1 column all "free taxi drivers" and
	#in another column all "waiting customers" --> indicies of data
#Free taxi drivers: those who completed their trips before 'start'
	#there might be duplicate taxi drivers, I'll only accept the latest
	#completed taxi driver
#Waiting Customers: trips that have not started yet

getPartitions <- function(file, start, interval=10) {
	data <- read.csv(file, head=TRUE, sep=",") #reading the csv file
	#PSEUDOCODE
	#sort entire datalist by timestamp somehow?
	#binary search through timestamp to find 'start'
	
	#GETTING INDICIES OF FREE DRIVERS
	#search left, create a list of "free taxi drivers" based on following:
		#free if driver's start timestamp + len(polyline)*.25 < 'start'
		#aka if they will complete their trip before 'start'
	#now you will have a list of all indicies of data that denots free drivers
	#remove duplicate of the same driver (accept the latest timestamp one)
	
	#GETTING INDICIES OF CUSTOMERS
	#look @ sorted list of timestamps, search right until 'start'+interval
	#all of those indicies belong in the range of possible customers for
		#the given time frame

	#return all of these indicies, can use it to grab endpoints of all
	#free drivers and customers to create a bipartite matching problem
}


#getMatching
#match free taxi drivers and waiting customers based on distance
	#closer = better match