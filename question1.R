
options(digits=10) # set the decimal place to 10

# This function randomly selects a coin from the denomination list
# until the total value of all the coins selected is greater than or
# equal to the cost of the item. It then returns the mean value of
# all the coins selected. This assumes there is an equal number of 
# pennies, nickels, dimes, quarters, and half dollars
stat <- function(cost) {
  denomination <- c(.01, .05, .10, .25, .50) # denomination of change
  paid_amount <- list() # initialize a list to hold the values
  paid_amount$paid <- 0 # set the initial amount paid to 0
  counter <- 0 # initialize a counter
  while(cost >= paid_amount$paid) {
    counter <- counter + 1 # iterate the counter
    paid_amount$selected[counter] <- sample(denomination, 1) # store the selected coin in the list
    paid_amount$paid <- paid_amount$paid + paid_amount$selected[counter] # calculate the total amount paid
  }
  return(mean(paid_amount$selected)) # return the mean value paid
}

# This function runs a simulation of stat function a total number of
# times set in repititions
simulation <- function(cost, repititions) {
  statistic <- vector() # initialize a vector that will store the means of each repitition
  for (i in 1:repititions) {
    statistic[i] <- stat(cost) # run the stat function and store the mean value
  }
  return(statistic)
}

# Find the mean and standard deviation for the cost of 0.25
a <- simulation(0.25, 1000)
mean(a)
sd(a)

# Find the mean and standard deviation for the cost of 1.00
b <- simulation(1.00, 1000)
mean(b)
sd(b)

# Find the mean and standard deviation for the cost of 10.00
c <- simulation(10, 1000)
mean(c)
sd(c)

