## Regression Diagnostics
# Assume that we are fitting a multiple linear regression
# on the MTCARS data
rm(list = ls())
library(car)
fit <- lm(mpg~disp+hp+wt+drat, data=mtcars) 
summary(fit)

###Outliers

# Assessing Outliers
outlierTest(fit) # Bonferonni p-value for most extreme obs
qqPlot(fit, main="QQ Plot") #qq plot for studentized resid
leveragePlots(fit) # leverage plots 


###Influential Observations

# Influential Observations
# added variable plots
#av.plots()
avPlots(fit)
# Cook's D plot
# identify D values > 4/(n-k-1)
cutoff <- 4/((nrow(mtcars)-length(fit$coefficients)-2))
plot(fit, which=4, cook.levels=cutoff)
# Influence Plot
# influencePlot(fit, id.method="identify", main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )
# An influence plot shows the outlyingness, leverage, and influence of each case.
# 
# The plot shows the residual on the vertical axis, leverage on the horizontal axis, and the point size is the square root of Cooks D statistic, 
# a measure of the influence of the point.
# 
# Outliers are cases that do not correspond to the model fitted to the bulk of the data. 
# You can identify outliers as those cases with a large residual (usually greater than approximately +/- 2), 
# though not all cases with a large residual are outliers and not all outliers are bad. Some of the most interesting cases may be outliers.
# 
# Leverage is the potential for a case to have an influence on the model. 
# You can identify points with high leverage as those furthest to the right. 
# A point with high leverage may not have much influence on the model if it fits the overall model without that case.
# 
# Influence combines the leverage and residual of a case to measure how the parameter 
# estimates would change if that case were excluded. Points with a large residual and high leverage 
# have the most influence. They can have an adverse effect on (perturb) the model if they are changed or excluded, 
# making the model less robust. Sometimes a small group of influential points can have an 
# unduly large impact on the fit of the model.




###Non-normality

# Normality of Residuals
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")

# distribution of studentized residuals
library(MASS)
sresid <- studres(fit)
hist(sresid, freq=FALSE,
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40)
yfit<-dnorm(xfit)
lines(xfit, yfit) 


###Non-constant Error Variance

# Evaluate homoscedasticity
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values
spreadLevelPlot(fit)



###Multi-collinearity

# Evaluate Collinearity
vif(fit) # variance inflation factors
#A simple approach to identify collinearity among explanatory variables is the use of variance inflation factors (VIF). 
#VIF calculations are straightforward and easily comprehensible; the higher the value, 
#the higher the collinearity. 
#A VIF for a single explanatory variable is obtained using the r-squared value of the regression of 
#that variable against all other explanatory variables:
# Run an OLS regression that has for example X1
# as a dependent variable on the left hand side and all your other independent variables on the right hand side.
# Take the R2
# from the regression in 1 and stick it into this equation: VIF=11???R2i.

sqrt(vif(fit)) > 2 # problem?


###Nonlinearity
# Evaluate Nonlinearity
# component + residual plot
crPlots(fit)
# Ceres plots
ceresPlots(fit)



###Non-independence of Errors
# Test for Autocorrelated Errors
durbinWatsonTest(fit)


# Additional Diagnostic Help
# 
# The gvlma( ) function in the gvlma package, performs a global validation of linear model assumptions as well separate evaluations of skewness, kurtosis, and heteroscedasticity.

# Global test of model assumptions
library(gvlma)
gvmodel <- gvlma(fit)
summary(gvmodel) 


### MSE /RMSE


# The next step is to determine whether the relationship is statistically significant and 
#not just some random occurrence. This is done by investigating the variance of the data points
#about the fitted line. If the data fit well to the line, then the relationship is likely 
#to be a real effect. The goodness of fit can be quantified using the 
#root mean squared error (RMSE) and R-squared metrics. 
#The RMSE represents the variance of the model errors and is an absolute measure of fit 
#which has units identical to the response variable. R-squared is simply 
#the Pearson correlation coefficient squared and represents variance explained in the 
#response variable by the predictor variable.
# 
# The number of data points is also important and influences the p-value of the model. 
#A rule of thumb for OLS linear regression is that at least 20 data points are required 
#for a valid model. The p-value is the probability of there being no relationship 
#(the null hypothesis) between the variables.
# 
# An OLS linear model is now fit to the transformed data.


#Residual sum of squares:
rss <- c(crossprod(fit$residuals))
#Mean squared error:
mse <- rss / length(fit$residuals)
#Root MSE:
rmse <- sqrt(mse)
#Pearson estimated residual variance (as returned by summary.lm):
  
sig2 <- rss / fit$df.residual

# Statistically, MSE is the maximum likelihood estimator of residual variance, 
# but is biased (downward). The Pearson one is the restricted maximum likelihood estimator of 
# residual variance, which is unbiased.


