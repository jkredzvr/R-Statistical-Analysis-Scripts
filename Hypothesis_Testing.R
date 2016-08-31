### Hypothesis Testing ###

#1. test data for normality
#2. select null hypothesis
#3. select alpha confidence level

## 1. Data Normality
X=c(11.48,11.45,11.48,11.47,11.48,11.5,11.42,11.49,11.45,11.44,11.45,11.47,11.46,11.47,11.43,11.5,11.49,11.45,11.46,11.47)

#Normal Data = Parametric Testing
#Non Normal Data = Non-Parametric Testing

#QQ Plot should be linear
qqnorm(X)
qqline(X)
n=length(X)

#### Hypothesis Testing of Mean ( Small Sample N < 40)
# Test statistic - T distribution (if small sample)
# Ho: mean=11.5  
# H1: mean=/= 11.5
# alpha=.05
smean=mean(X)
alpha=.05
alpha2=alpha/2
sdev=sd(X)

#Reject Ho if |tstat| > t.05,n-1 || tstat < -t.05,n-1
tstat=(smean-11.5)/(sdev/(n^.5))  #tstat = (smean - mean)/sdev/(n^.5)
#or
nullhypo = 11.5  #Ho mean = nullvalue
tstat = t.test(X, mu=nullhypo, conf.level=(1-alpha))  

lowercrit=qt(alpha2,n-1)
uppercrit=qt(1-alpha2,n-1)

# is p-value < alpha ? or is tstat > upper crit value
pvalue=2*pt(tstat,n-1)  #pvalue - tail area beyond the value of the test statistic, smallest level of significnce (alpha)
pvalue

##### Power of Test: Use Operative Curves Chart VII ########
#Calculate power if true mean is 11.4
d=abs(11.4-smean)/sdev
d
#for alpha .05, d=2.96, n=20 , beta = .. from Appendix Chart VI E
beta=0
power=1-beta
power

##############################################
# Hypothesis Testing Variance ( Small Sample N < 40)
# Test statistic - Chi distribution (if small sample)
# Ho var =11.5  
# H1 var =/= 11.5
# alpha=.05
# find p-value
X="data"
n=length(X)
var = 1  # variance is 1 for chi sq distribution
s=sd(X) #sample standard deviation
alpha=.05
alph2=alpha/2

chisq= (n-1)s^2/var
#or
chisq.test(data)

lowercrit=qchisq(alpha2,n-1)
uppercrit=qchisq(1-alpha2,n-1)

#Reject Ho if chisq < chisq,alpha/2,n-1 || chisq > chiqsq,1-alpha/2,n-1
#or p-value < alpha



#######################
# Hypothesis Testing Independence of two groups
# Test statistic - Chi distribution 
# Ho A is indpendent from B  
# H1 A is not indpendent from B
# alpha=.05
# find p-value

alpha=.05
data=matrix(data=c(25,6,7,13,17,16,15,6,18,5,18,10,10,8,11,20),ncol=4,byrow=TRUE)
total=sum(data[1:4,])
E=matrix(data=rep(0,16),ncol=4,byrow=TRUE)
rows = 1:4
cols = 1:4

for(i in 1:4){
  for(j in 1:4){
    E[i,j]=total*(sum(data[i,])/total)*(sum(data[,j])/total)  
  }
}

chi2=0
for (i in 1:4){
  for(j in 1:4){
    a=(data[i,j]-E[i,j])^2/E[i,j]
    chi2=chi2+a
  }
}
chi2
#reject null if chi2 > chi2aplha,(r-1)(c-1)
qchisq(1-alpha,(4-1)*(4-1)) #21.665
#chi2 is > ch2alpha.. so the null hypothesis is rejected and the variabes are not indepenedent (variables homogeneous)



####### Hypothesis Testing for Comparing two Variances #####
#use F statisitc = s1^2/s2^2
#Ho: var1 = var2 i.e fo ratio = 1
#H1: var1 =/ var2
#alpha=.05
#Reject Ho if fo > falpha/2,n1-1,n2-1 or fo < -f1-alpha/2,n1-1,n2-1 

alpha=.05
ven1=c(96.8,100,100.3,98.5,98.3,98.2,99.6,99.4,99.9,101.1,103.7,97.7,99.7,101.1,97.7,98.6,101.9,101.0,99.4,99.8,99.1,99.6,101.2,98.2
       ,98.6)
ven2=c(106.8,106.8,104.7,104.7,108,102.2,103.2,103.7,106.8,105.1,104,106.2,102.6,100.3,104,107,104.3,105.8,104,106.3,102.2,102.8,104.2,103.4,104.6,103.5,106.3,109.2,107.2,105.4,106.4,106.8,104.1,107.1,107.7)

#test normality
hist(ven1)
hist(ven2)
qqnorm(ven1)
qqnorm(ven2)

#Variance unknown, alpha = , n1+n2=60 > 40 large statistics
n1=length(ven1)
n2=length(ven2)
var1=var(ven1)
var2=var(ven2)
fo=var1/var2

#or
var.test(ven1,ven2, conf.level=(1-alpha))

#from Appendix Table V  f(.025,n1-1,n2-1)=f(.025,24,34) 
fl=2.12 #between 2.2 and 2.07

#f(.0975,24,34)=1/f(.025,24,34)
fu=1/fl
#fo > fl, and fo < fu, so hypotheisis is not rejected.. variances are not different int he two groups.

#or var.test(x,y)$.conf.int to get critical values


######### Hypothesis of Delta between Two Data Sets #####
#Test statistic - T distribution (if small sample)
#Ho: delta0 = 3  
#H1: delta0 > | < | =/= 11.5
#alpha=.05

#test statistic = (Davg - delta0) / ( s/(n)^.5 )

weightbefore=c(165,201,195,198,155,143,150,187)
weightafter=c(161,195,192,193,150,141,146,183)
qqnorm(weightbefore)
qqnorm(weightafter)
#alpha = .05, Ho: delta0 >= 3 , H1: delta0 < 3, onesided t test

n=length(weightbefore)
D=rep(0,n)
for (i in 1:n)
  D[i]=weightbefore[i]-weightafter[i]
Davg=mean(D)
Dsd=sd(D)
delta0=3   #Null hypothesis
teststat=(Davg-delta0)/(Dsd/(n)^.5)
talpha=qt(1-alpha,n-1)
#reject null hypothesis if teststat < -talpha
-talpha
teststat




######### Hypothesis Testing of two Proportions #######
#### use Z statistics  proportion of children in either groups is statisiically different
#Ho p1=p2
#H1 p1 =/= p2 
#alpha = .05
np=201299 # num of placebo group
pop=110 #cases polio observed in placebo
p1=pop/np
nv=200745 #num of vaccine group
pov=33    #cases of polio observed in vaccine group
p2=pov/nv
phat=(pov+pop)/(np+nv)
zteststat=(p1-p2)/(phat*(1-phat)*(1/np+1/nv))^.5
#Reject null p1=p2 if zteststat > z.025=1.96 or zteststat < -z.025=-1.96
zteststat

qnorm(alpha/2) #lower critical value
qnorm(1-alpha/2) #upper critical value


### Hypothesis Test of Median ###
# Test statistics is the observed number of plus differences r+ = (n/2)
#teststat = (R+ - .5n)/(.5*n^.5)
#is the median 10? , alpha =.05, what is the p-value of the test
#Ho median = 10
#H1 median =/= 10

parts=c(10.32,9.68,9.92,10.10,10.2,9.87,10.14,9.74,9.8,10.26)
#alpha=.05 means 5% chance of Ho being incorrect, rejecting null..  The 
plus=sum(parts > 10)
n=length(parts)
#pvalue=2*prob( R+ >= r+ | p=.5)
pvalue=2*sum(dbinom(5:10,10,0.5)) #prob of getting 5,6,7,8,9,10 (+) signs out of 10 
pvalue # prob of getting more than 5 plusses out of 10

#pvalue is > alpha=.05, then we cannot reject null hypothesis that 10 is the median


#teststat=R+ - .5n/(.5*n^.5)
teststat=(plus-.5*n)/(.5*n^.5)
teststat #teststat=0
pvalue=2*(1-pnorm(teststat))

######## Non Parametric Median Hypothesis ####

library(BSDA)
#Wilcoxon  (Wilcoxon rank sum)
#wilcox.test (Wilcoxon sign rank)
#SIGN.test  (Sign test)

### Sign Test ###
#Ho median = nullmedian
#H1 median =/= null median
#Reject Ho if the pvalue corresponding to r+ is less than or equal to alpha

alpha=.05
nullmedian= #null hypothesis median
SIGN.test (data, md= nullmedian, conf.level = (1-alpha))
pvalue=2*sum(dbinom(5:10,10,0.5)) #prob of getting 5,6,7,8,9,10 (+) signs out of 10 
#Reject null hypothesis that the medians are same at (1-p-value)*100% confidence.


#Wilcox Rank Sum
#Ho mean = nullmean
#H1 mean =/= null mean
#alpha = .05
# test statistic w2 = (n1+n2)(n1+n2+1)/2 - w1
# Reject Ho if w <= walpha





