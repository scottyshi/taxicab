# Taxicab Prediction Directed Research Project (Miodrag Potkojnak)

# Context

The data that was used came in the following format:
- Timestamp
- Unique ID for the trip
- Boolean (for whether or not the trip data was complete)
- Vector of coordinates that denoted the taxi's position every 15 seconds.

This data was collected for 451 different taxi trips over the course of several months.

The good majority of the data had complete taxi trips, while some others only had partial data for the vector of coordinates.

# Taxi Prediction / Customer Popup

Based on the massive amount of given information, we were able to generate several random samples of data within certain time intervals. We performed k-means clustering on each of these samples and made confidence intervals that told us how many taxi cab drivers would end up in a certain region at a certain time. 

In a similar fashion, we could predict where customers would pop up at certain intervals of time as well.

With both of these pieces of information, we saw that it could be used to minimize the distance travelled by taxi cabs.

# Bipartite Matching

Given where we expected customers and taxis to pop up, it's not necessarily optimal for a customer to request a taxi ride in order to have a taxi driver to start driving over, wherever they might be. Instead, if it's possible to predict where customers might pop up, we could allocate some free drivers who are nearby that cluster that we anticipate customers to show up. Additionally, if we anticipate a taxi driver will arrive in a region close to the area where a customer just requested a trip, it's not necessary to assign a currently free taxi driver at a farther location to service this customer.

Since it would be too risky to depend on a single taxi driver to end up in a certain area or a single customer to show up in an area, it's important to be confident in how many drivers/customers one would expect to show up at a certain region at a certain time. This is where the confidence intervals we generated before come in handy. 

With these confidence intervals of how many customers/taxi drivers we expect to see in certain regions at certain times, we can simply perform bipartite matching to create the most optimal, gas efficient solution. 

# Results

To theoretically evaluate our model, we compare how far the taxi drivers would travel if they went to the destinations our model told it to go to against how far these drivers actually went. (We simply look at the beginning point of the next taxi trip that specific driver had). We found that our model reduced this "in-between" travelling distance on average by 4-fold.
