test=matrix(scan("http://civil.colorado.edu/~balajir/CVEN5454/R-sessions/sess5/helsel-prob12-4.txt"), ncol=2, byrow=T)
Y = test[,2]
X = test[,1]

#  Pearson Correlation 
cor(X,Y)
cor.test(X,Y)

#Kendall's Tau Correlation
cor(X,Y,method=c("kendall"))
cor.test(X,Y,method=c("kendall"))

# Spearman's Rank Correlation
cor(X,Y,method=c("spearman"))
cor.test(X,Y,method=c("spearman"))

# fit a Kendall Thiel slope estimate..
# Chapter 10, pages 266-267  of Helsel and Hirsch

library(mblm)
zkt = mblm(Y ~ X)
zlin = lsfit(X, Y)    #Linear Regression Model

plot(X, Y, xlab="% Carbon-14", ylab="Calcium")
abline(zlin, col="red")
abline(zkt, col="blue")
# clearly the blue line based on Kendall-Thiel slope is robust to outlier

# Kendall Slope is tested using the Kendall Tau
# Chapter 8 pages 212-216  of Helsel and Hirsch
cor.test(X,Y,method = c("kendall"))

#Kendall's Rank Correlation Tau
#data:  X and Y 
#z = 6.1658, p-value = 7.014e-10
#alternative hypothesis: true tau is not equal to 0 
#sample estimates:
#  tau 
#0.6026173 

#The Kendall-Thiel slope is statistically significant.

#######################
# You can also use the library Kendall

library(Kendall)        #install library Kendall
help(Kendall)           # read the help on the command Kendall

zz = Kendall(X,Y)       #computes the Kendall tau correlation
zz$sl                   #gives you the p-value of the test. 