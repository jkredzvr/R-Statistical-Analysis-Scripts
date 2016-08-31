X = "data"  #set of Observed data

#N=(zalpha/E)^2*sd^2
# Confidence Interval length / 2 = Error from Estimation
E=5  #Error is +/- E
alpha=.05
za=qnorm(1-alpha/2)
N=(za*sd(X)^2/E)^2  #N sample size for (1-alpha)% Confidence Interval on the mean with at most CI length of E*2
# or error of E


