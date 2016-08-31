library(MASS)
library(sm)
library(plotrix)
library(car)
library(psych)
library(plotrix)

#########################################################
#### Scatterplots to visually inspect correlation########

TOC_data.dataframe = read.table('http://civil.colorado.edu/~balajir/CVEN5454/r-project-data/NN_TOC_predictors.txt', sep = '', header = T)
TOC_data.dataframe1=TOC_data.dataframe[(c(2:8,1))]
TOC_data.dataframe2=TOC_data.dataframe[(c(9:16,1))]

Y=TOC_data.dataframe$TOC
X=TOC_data.dataframe[,2:16]  #coefficient data
N=length(Y)	# of data points..
NL = log(N)

pairs.panels(TOC_data.dataframe1) #shows the Correlation values for each variable comparison
pairs.panels(TOC_data.dataframe2,smooth=TRUE)



##################### BIC model ##########
fullmod = glm(Y ~ ., data=X) #full model uses all data variables

#BIC
bestmodel1 = stepAIC(fullmod, k=NL)	#BIC , k=log(n)

#glm(formula = Y ~ T30D3M + P15D + NDVI1M + PDSI1M + PDSI2M, data = X)
#Coefficients:
#  (Intercept)       T30D3M         P15D       NDVI1M       PDSI1M       PDSI2M  
#1.411200     0.007978     0.101534     1.077723     0.095993    -0.066525  
#Degrees of Freedom: 406 Total (i.e. Null);  401 Residual
#Null Deviance:      121.8 
#Residual Deviance: 90.3         AIC: 556.2


######### AIC ############

bestmodel2 = stepAIC(fullmod)	

#Call:  glm(formula = Y ~ T7D + T30D + T30D3M + P15D + P30D + NDVI1M + 
#             PDSI1M + PDSI2M, data = X)
#Coefficients:
#  (Intercept)          T7D         T30D       T30D3M         P15D         P30D  
#1.415700     0.007625    -0.010871     0.008289     0.131468    -0.030170  
#NDVI1M       PDSI1M       PDSI2M  
#1.471474     0.110520    -0.082492  
#Degrees of Freedom: 406 Total (i.e. Null);  398 Residual
#Null Deviance:      121.8 
#Residual Deviance: 89.04        AIC: 556.5

############# Use best model from BIC
summary(bestmodel)


##############
#### Error/Residual Diagnostics 
#get the residuals of the model..
x11=residuals(bestmodel1) 


### Model Diagnostics ######
# plot the histogram of the residuals and add the pdf. This is to
# see how good the residuals fit a normal distribution..
# you can do a KS test on this, or a qqplot..)
par(mfrow=c(2,3))
hist(x11,probability=T)  #Residual PDF 
sm.density(x11,add=T)    #Nonparametric Fitted Line

qqnorm(x11) #QQ Plot
qqline(x11) 

#plot the autocorrelation function - to see if the residuals are related
# to each other..  IID assumption
z1=acf(x11)

zz=predict(bestmodel1,se.fit=TRUE)
yest = zz$fit #y value prediction


# plot the Y estimates against the residuals..
# for constant variance or homoskedasticity..
plot(yest,x11,xlab="estiimate of Y", ylab="residuals")
abline(h=0)

yupper = qnorm(0.975,mean=yest,sd=zz$se.fit)
ylower = qnorm(0.025,mean=yest,sd=zz$se.fit)


plot(Y, yest,xlab="observed TOC", ylab="Modell TOC",ylim=range(Y)) #plot the observed vs predicted values (We want a linear relationship)
abline(a=0, b=1)
plotCI(Y,yest,ui=yupper,li=ylower,add=TRUE)  #plot on top the confidence intervals as bars


# if there is a pattern then transformation is requred or iterated least squares..

#plot the residuals against each X variable..
# plot 
nd = ncol(X)
for(i in 1:nd){
  plot(X[,i],x11,xlab=names(X[,i]),ylab="residuals")
}

# 1(d) ##### include lag
N1 = N-1
Ynew = Y[2:N]
Xnew = cbind(X[2:N,],Y[1:N1])

fullmod = glm( Ynew ~ ., data=Xnew)

bestmodel = stepAIC(fullmod,k=log(N1)) ## BIC
summary(bestmodel)

#Call:
#  glm(formula = Ynew ~ T30D2M + P7D + `Y[1:N1]`, data = Xnew)
#Deviance Residuals: 
#  Min        1Q    Median        3Q       Max  
#-1.10205  -0.29384  -0.07172   0.25201   1.38946  
#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 0.825172   0.116941   7.056 7.53e-12 ***
#  T30D2M      0.007555   0.001423   5.310 1.82e-07 ***
#  P7D         0.157294   0.025932   6.066 3.04e-09 ***
#  `Y[1:N1]`   0.488250   0.041640  11.725  < 2e-16 ***
#  ---
#  Signif. codes:  0 â€˜***â€™ 0.001 â€˜**â€™ 0.01 â€˜*â€™ 0.05 â€˜.â€™ 0.1 â€˜ â€™ 1
#(Dispersion parameter for gaussian family taken to be 0.1835463)
#Null deviance: 121.464  on 405  degrees of freedom
#Residual deviance:  73.786  on 402  degrees of freedom
#AIC: 469.87
#Number of Fisher Scoring iterations: 2
########

#### Error/Residual Diagnostics 
#get the residuals of the model..
x11=residuals(bestmodel)

# model diagnostics..
# plot the histogram of the residuals and add the pdf. This is to
# see how good the residuals fit a normal distribution..
# you can do a KS test on this, or a qqplot..)
par(mfrow=c(2,3))
hist(x11,probability=T)
sm.density(x11,add=T)
qqnorm(x11)
qqline(x11) # Create a qq line

#plot the autocorrelation function - to see if the residuals are related
# to each other..  IID assumption
z1=acf(x11)  #plots the correlation values

zz=predict(bestmodel,se.fit=TRUE)
yest = zz$fit

# plot the Y estimates agains the residuals..
# for constant variance or homoskedasticity..
plot(yest,x11,xlab="estiimate of Y", ylab="residuals")
abline(h=0)

yupper = qnorm(0.975,mean=yest,sd=zz$se.fit)
ylower = qnorm(0.025,mean=yest,sd=zz$se.fit)

plot(Ynew, yest,xlab="observed TOC", ylab="Modell TOC",ylim=range(Y))
abline(a=0, b=1)
plotCI(Ynew,yest,ui=yupper,li=ylower,add=TRUE)

############################
### Skill Evaluation ######

N = length(Y)
N1 = N-1
Ynew = Y[2:N]
Xnew = cbind(X[2:N,],Y[1:N1])

fullmod = glm( Ynew ~ ., data=Xnew)

bestmodel = stepAIC(fullmod,k=log(N1)) ## BIC

bm = bestmodel	### bestmodel from BIC 
NN = length(Ynew)
sn = round(NN/10)  # 10% of data
kmax = 250
rsquared = rmse = numeric(kmax)
for (k in 1:kmax) {
  i = sample(1:NN, sn, replace = FALSE)
  #	tm = glm(Y[-i] ~ ., data=X[-i,])
  tm = glm(formula(bm), data = bm$model[-i, ])
  cvpred = predict(tm, newdata = Xnew[i, ], type = "response")
  SSt = sum((bm$model[i, 1] - mean(bm$model[i, 1]))^2)
  SSr = sum((bm$model[i, 1] - cvpred)^2)
  rsquared[k] = cor(cvpred,Ynew[i])^2
  rmse[k] = sqrt(SSr/sn)
}

par(mfrow=c(1,2))
boxplot(rsquared)
points(cor(Ynew,predict(bestmodel))^2, col="red")	#Cor of the best model

boxplot(rmse)
points(sqrt(sum((bm$model[, 1] - predict(bm))^2)/NN),col="Red")	#RMSE of the best model

######### 1(f)   Gamma GLM
par(mfrow=c(1,2))

N = length(Y)
N1 = N-1
Ynew = Y[2:N]
Xnew = cbind(X[2:N,],Y[1:N1])

fullmod = glm( Ynew ~ ., data=Xnew, family="gamma")

bestmodel = stepAIC(fullmod,k=log(N1)) ## BIC


zz=predict(bestmodel,se.fit=TRUE,type="response")
yest = zz$fit

yupper = qnorm(0.975,mean=yest,sd=zz$se.fit)
ylower = qnorm(0.025,mean=yest,sd=zz$se.fit)


plot(Ynew, yest,xlab="observed TOC", ylab="Modell TOC",ylim=range(Y))
abline(a=0, b=1)
plotCI(Ynew,yest,ui=yupper,li=ylower,add=TRUE)


#### 1(f) or fit a model on log of the data

N = length(Y)
N1 = N-1
Ynew = Y[2:N]
Xnew = cbind(X[2:N,],Y[1:N1])

fullmod = glm(log(Ynew) ~ ., data=Xnew)

bestmodel = stepAIC(fullmod,k=log(N1)) ## BIC
yest = exp(predict(bestmodel))


zz=predict(bestmodel,se.fit=TRUE)
yestl = zz$fit
yest = exp(zz$fit)

yupper = exp(qnorm(0.975,mean=yestl,sd=zz$se.fit))
ylower = exp(qnorm(0.025,mean=yestl,sd=zz$se.fit))


plot(Ynew, yest,xlab="observed TOC", ylab="Modell TOC",ylim=range(Y))
abline(a=0, b=1)
plotCI(Ynew,yest,ui=yupper,li=ylower,add=TRUE)

