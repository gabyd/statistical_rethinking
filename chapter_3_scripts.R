#Chapter 3

p_grid<-seq(from=0, t=1, length.out=1000)
p_grid

prob_p<-rep(1,1000)
prob_data<-dbinom(6,size=9, prob=p_grid)
posterior<-prob_data*prob_p
posterior<-posterior/sum(posterior)

#drawing 10,000 samples from a bucket

samples<-sample(p_grid, prob=posterior, size=1e4, replace=TRUE)

#the sample function here pulls values from a vector which in this case if p_grid the grid fo parameter values
#The probability of each value is the posterior which we computed above. 

plot(samples)

library(rethinking)
dens(samples)


#1. Intervals of defined boundaries. 
#suppose you're asked for the posterior probability that the proportion of water is less than 0.5
#add up posterior probability where p<0.5
sum(posterior[p_grid<0.5])

#so about 17% of the posterior probability is below 0.5

#the method to use without making use of grid approximation is add up all of the samples below 0.5 
#and divide the resulting count by the total number of smaples 
sum(samples<0.5)/1e4

#using the same approximation you can find how muhc posterior probability lies between 0.5 and 0.75
sum(samples>0.5 & samples<0.75)/1e4
#SO about 61 percent of the posterior probability lies between 0.50 and 0.75


# 2. Intervals of defined mass 

#similar to the confidence interal idea. 
#And interval o fposterior probaility is called a credibility interval but we refer to it here as compatibility interval

#These posterior intervals report two parameter values that contain between them a specified amount of posterior probabiltiy, a probability mass. 

quantile(samples,0.8)

quantile(samples, c(0.1,0.9))
#Intervals of this sort which assign equal probability mass to eahch tail are very common in the scientific literature
#We call them percentile intervals (PI). These intervals do a good job of communicating the shape of oa distribution as long as the distribution isn't too asymmetrical


p_grid<-seq(from=0, to=1, length.out=1000)
prior<-rep(1,1000)
likelihood<-dbinom(3,size=3, prob=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)
samples<-sample(p_grid, size=1e4, replace=TRUE, prob=posterior)
plot(posterior)
PI(samples, prob=0.5)

#This interval assigns 25 % of the probability mass above and below the interval. So it provides the central 50% probability
#in contrast the right-hand plot in the figure displays the 50% highest posterior density interval (HPDI)


HPDI(samples, prob=0.5)
#This interval captures the parameters with highest posterior probability as well as bieng noticeably narrower
#0.16 in width rather than 0.23 for the percentile interval

#The HDPI has some advantages over the PI but in some acses the two types of intervals are very similar
#The only look so different in this case because the posterior distribution is highly skewed.

#Tryin the same example from above but for 6 Water in 9 tosses
p_grid<-seq(from=0, to=1, length.out=1000)
prior<-rep(1,1000)
likelihood<-dbinom(6,size=9, prob=p_grid)
posterior<-likelihood*prior
posterior<-posterior/sum(posterior)
samples<-sample(p_grid, size=1e4, replace=TRUE, prob=posterior)
plot(posterior)
#And changing the probability
PI(samples, prob=0.8)
HPDI(samples, prob=0.8)
PI(samples, prob=0.95)
HPDI(samples, prob=0.95)

#When the posterior is bell shaped it hardly matters what type of interval you use

#The HPDI also has some disadvantages. HPDI is more computationally intensive than PI adn suffers from greater simulation varaince 
#, which is a fnacy way of saying that it is senstivie to how many samples you draw from the posterior 

#Advice is generally that if the choice of interval type makes a difference then you shouldn't be using intervals to summarise the posterior 
#Remember the entire posterior distribution si a Bayesian estimate. it summarises the relative plausibility of each possible value of a parameter
#Intervals fo the distribution are just helpful for summarizing it. If choice of *an interval* leads to different inferences,then you'd be better off just plotting the entire distribution

# 3. Point estimates

#The Bayesian paramter estimate is precisely the entire posterior distribution which is not a isngle number but instead a function that maps each unique parameter value onto a plausibility value


