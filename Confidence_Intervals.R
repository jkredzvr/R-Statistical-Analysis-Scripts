#Set working Directory
setwd("~/GitHub/R_code") 


####Confidence Interval from Bootstrap Data##########
#Scan Data
xx=scan("hel55.txt")

N = length(xx)

#generate nsim bootstrap samples
nsim = 500

#statistics desired..
meansim = 1:nsim
varsim = 1:nsim
mediansim = 1:nsim

#bootstrap samples to estimate the population mean, var, median
for(i in 1:nsim){
  xsamp = sample(xx, N, replace=TRUE)  #resample with replace
  meansim[i]=mean(xsamp)
  varsim[i]=var(xsamp)
  mediansim[i]=median(xsamp)
}

# Confidence interval on the population mean
# assuming the data is Normally distributed
alpha = 0.05		#confidence level
alpha2 = alpha/2

#get statistic values (quantile value) for alpha2 quantile and 1-alpha2 qunatile 
print(c(quantile(meansim, c(alpha2, 1-alpha2))))
print(c(quantile(varsim, c(alpha2, 1-alpha2))))
print(c(quantile(mediansim, c(alpha2, 1-alpha2))))


########################################################
#Confidence Interval Calculations 

xx=scan("hel55.txt")
N = length(xx)

# Confidence interval on the population mean
# assuming the data is Normally distributed
alpha = .05 # 95% Confidence Interval

print(c(t.test(xx, alternative="two.sided", conf.level=(1-alpha))$conf.int))

#or

alpha=0.05		#confidence level
alpha2=alpha/2

N=length(xx)

### CONFIDENCE Interval ###
XL = mean(xx) - qt(alpha2,N-1)*sd(xx)/sqrt(N)  #qt quantile value from t-dist, #(1-alpha)% Conf. Lower limit
UL = mean(xx) + qt(1-alpha2,N-1)*sd(xx)/sqrt(N) #(1-alpha)% Conf. Int Upper Limit
print(c(XL, UL))

### PREDICTION Interval ####
XL = mean(xx) - qt(alpha2,N-1)*sd(xx)/sqrt(1+1/N) #(1-alpha)% Prediction Interval Lower limit
UL = mean(xx) + qt(1-alpha2,N-1)*sd(xx)/sqrt(1+1/N) #(1-alpha)% Prediction Interval Upper Limit
print(c(XL, UL))

### TOLERANCE Interval ####
#Mean-ks , Mean+ks is tolerance interval, k value found in table: k(CI,N)
k=3.213
XL = mean(xx)-k*sd(xx)   #lower bound interval containing X% of data wiht CI 95% 
UL = mean(xx)+k*sd(xx)  #upper bound interval containing X% with CI 95%


#####  Population Proportion Confidence Interval ########
# 1600 Random Samples, 8 found to have errors
# Find 99% CI on Proportion of the errors
#sample proportion = subsample / n
alpha=.01
sampleproportion = 8/1600
z=qnorm((1-alpha/2))
#standarderror=(sampleproportion*(1-sampleproportion)/n)^.5
standarderror=(sampleproportion*(1-sampleproportion)/1600)^.5
XL = sampleproportion+z*standarderror  #Lower Bound Population Proportion (1-alpha)% Confidence Interval
UL = sampleproportion-z*standarderror  #Upper Bound Population Proportion (1-alpha)% Confidence Interval

##### Population Variance Confidence Interval ####### 
alpha=.05
alpha2=alpha/2
VL = var(xx)*(N-1)/qchisq(1-alpha2,N-1) #Lower Limit
UL = var(xx)*(N-1)/qchisq(alpha2,N-1) #Upper Limit
print(c(XL, UL))




#### Median Confidence Interval ####
xx=sort(xx)
alpha=.05
alpha2=alpha/2
xl=qbinom(alpha2,length(xx),0.5)
xu=qbinom((1-alpha2),length(xx),0.5)
print(c(xx[xl],xx[xu]))


# for larger N
xl = round((N - qnorm(1-alpha2)*sqrt(N))/2)
xu = round((N + qnorm(1-alpha2)*sqrt(N))/2)
print(c(xx[xl],xx[xu]))


###########################################################################################################
#Variance and Correlation Testing


test=scan("hel55.txt")


x=test[1:13]		#with fracturing..
y=test[14:25]		#without fracturing..

# you can change the confidence level and/or the alternate hypothesis
#two sample T-test
#unequal variance
print(c(t.test(x,y,paired=FALSE,var.equal=FALSE)))

#two sample Variance test
#Ho: Ratio of var x & y = 1 , H1: Var x & y =/= 1
print(c(var.test(x,y)))


# To test the correlation between x and y
# rho = correlation(x,y)
# Ho rho = 0 i.e x & y are not correlated; H1: rho not equal to 0

print(c(cor.test(x,y)))








#####################################################
#Wilcox Test Paired
#Test Sample Independence without assuming normal dist
#Null hypothesis Ho: x and y are identical
#default is alpha=.05
test=scan("hel55.txt")


x=test[1:13]		#with fracturing..
y=test[14:25]		#without fracturing..

x[x < 0.05]=0
y[y < 0.05]=0

#paired vs unpaired 
#Ho ?????   H1:  X is greater than Y
print(c(wilcox.test(x,y,paired=FALSE,exact=FALSE,alternative="greater",conf.level=.99)))


test=matrix(scan("hel64.txt"),ncol=2,byrow=T)

x=test[,2]	#Sep
y=test[,1]	#June

print(c(wilcox.test(x,y,alternative="greater",paired=TRUE,exact=FALSE)))


x[x <= 0.01]=0
y[y <= 0.01]=0

print(c(t.test(x,y,alternative="greater",var.equal=FALSE,paired=TRUE)))



