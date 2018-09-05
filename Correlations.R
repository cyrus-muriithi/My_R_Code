

tbl = matrix(data=c(55, 45, 20, 30), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('B', 'T'), Gender=c('M', 'F'))
chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)
#Here the p value is 0.08 - quite small, but still not enough to reject the hypothesis of independence. So we can say that the "correlation" here is 0.08
sqrt(chi2$statistic / sum(tbl))
#And get 0.14 (the smaller v, the lower the correlation)



tbl = matrix(data=c(51, 49, 24, 26), nrow=2, ncol=2, byrow=T)
dimnames(tbl) = list(City=c('B', 'T'), Gender=c('M', 'F'))

chi2 = chisq.test(tbl, correct=F)
c(chi2$statistic, chi2$p.value)
sqrt(chi2$statistic / sum(tbl))
#The p-value is 0.72 which is far closer to 1, and v is 0.03 - very close to 0


###########################################################################################################
###########################################################################################################

# Categorical vs Numerical Variables
# 
# For this type we typically perform One-way ANOVA test: we calculate in-group variance and 
# intra-group variance and then compare them.
t1 = c(164, 172, 168, 177, 156, 195)
t2 = c(178, 191, 197, 182, 185, 177)
t3 = c(175, 193, 178, 171, 163, 176)
t4 = c(155, 166, 149, 164, 170, 168)

val = c(t1, t2, t3, t4)
fac = gl(n=4, k=6, labels=c('type1', 'type2', 'type3', 'type4'))

aov1 = aov(val ~ fac)
summary(aov1)

#Is there any dependence between the variables? For that we conduct ANOVA test and see that 
#the p-value is just 0.007 - there's no correlation between these variables. 





