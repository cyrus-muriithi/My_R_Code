
rm(list = ls())
library(dplyr)
library("ggplot2")
SIC <- scale_fill_manual(values = c("#d5e1e7","#4495d8","#ff333a","#aeef88","#484b4d","#ffda4a","#2ecd72","#63b5ff","#fdfdfe"))

mytheme <- theme(legend.position = "bottom", legend.direction = "horizontal",
                 legend.title = element_blank(),
                 plot.title = element_text( size = rel(1.5), hjust = 0.5),
                 plot.subtitle = element_text(size = rel(1.4), hjust = 0.5),
                 axis.text.x = element_text(size =rel(1.3),angle = 0),
                 axis.text.y = element_text(size =rel(1.3),angle = 0),
                 axis.title = element_text( size = rel(1.4)))

mydata<- mtcars
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
ggplot(mydata, aes(x = qsec)) + 
  geom_histogram(aes(y=(..count..)/sum(..count..)),
                 breaks = seq(13, 23, by = 0.5), 
                 colour = "#4292C6", 
                 fill = "#4292C6") +
  scale_y_continuous (labels = scales::percent) +
  stat_function(fun = dnorm, args = list(mean = mean(mydata$qsec), sd = sd(mydata$qsec))) +
  geom_vline(aes(xintercept=mean(mydata$qsec, na.rm=T)),   # Ignore NA values for mean
             color="red", linetype="dashed", size=1)+
  annotate("text", x = 20, y = 0.2, 
           label = 'atop(bold("Average value"),"17.84875")',
           colour = "red", parse = TRUE) +
  labs(x = "Number of queries", y = "Percent", title = "Distribution of number of engine queries") + mytheme + SIC + theme_bw()
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

mydata<- mtcars %>%
  mutate(vs= as.factor(vs))%>%
  group_by(vs) %>%
  summarise(mean.val = round(mean(mpg, na.rm = TRUE),2),
            sd = sd(mpg, na.rm = TRUE),
            Count = n()) %>%
  mutate(se = sd / sqrt(Count),
         lower.ci = mean.val - qt(1 - (0.05 / 2), Count - 1) * se,
         upper.ci = mean.val + qt(1 - (0.05 / 2), Count - 1) * se,
         
         cyrus_low = mean.val - (se * 1.96),
         cyrus_high = mean.val + (se * 1.96))

ggplot(mydata, aes(x=vs,y=mean.val)) +
  geom_bar(stat="identity", aes(fill=vs)) +
  geom_errorbar(aes(x=vs, ymin=lower.ci, ymax=upper.ci), color="black", width=0.05) +
  geom_text(aes_string(label = "mean.val"),vjust = -0.25,hjust =0.05, size = 4,position = position_dodge(0.5)) +
  theme_bw() + SIC +mytheme+
  labs(x = "Treatment", y= "Score",title = "Distribution of test scores by treatments", subtitle = "(Psychometric test scores)") +
  theme(legend.position = "none",plot.subtitle=element_text(size=rel(1.0), hjust=0.5, face="italic", color="black"))
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
library(gmodels)
my_data<- mtcars %>%
  mutate(vs= as.factor(vs))%>%
  group_by(vs)%>% 
  summarise(mean = round(ci(mpg)[1],2), 
            lowCI = ci(mpg)[2],
            hiCI = ci(mpg)[3], 
            sd = ci (mpg)[4])

ggplot(my_data, aes(x=vs,y=mean)) +
  geom_bar(stat="identity", aes(fill=vs)) +
  geom_errorbar(aes(x=vs, ymin=lowCI, ymax=hiCI), color="black", width=0.05) +
  geom_text(aes_string(label = "mean"),vjust = -0.25,hjust =0.05, size = 4,position = position_dodge(0.5)) +
  theme_bw() + SIC +mytheme+
  labs(x = "Treatment", y= "Score",title = "Distribution of test scores by treatments", subtitle = "(Psychometric test scores)") +
  theme(legend.position = "none",plot.subtitle=element_text(size=rel(1.0), hjust=0.5, face="italic", color="black"))

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
#rm(list = ls())
# 0. Build linear model 
data("cars", package = "datasets")
model <- lm(dist ~ speed, data = cars)
# 1. Add predictions 
pred.int <- predict(model, interval = "prediction")
mydata <- cbind(cars, pred.int)
# 2. Regression line + confidence intervals

p <- ggplot(mydata, aes(speed, dist)) +
  geom_point() +
  stat_smooth(method = lm)
# 3. Add prediction intervals
p + geom_line(aes(y = lwr), color = "red", linetype = "dashed")+
  geom_line(aes(y = upr), color = "red", linetype = "dashed") + theme_bw()



#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------
data("cars", package = "datasets")
#regress ingroup vs outgroup on Ios scale
model1=lm(dist ~ speed, data = cars)

#summary(model1)

#extracting model coefficients and pvalues

coefficients<-as.data.frame(coef(summary(model1)))

## table of estimates with 95% CI

se <- round(summary(model1)$coefficients[,2],4)

p_value<-round(summary(model1)$coefficients[,4],4)

est<-round(summary(model1)$coefficients[,1],4)

tab <- data.frame(cbind(Est = est, LL = est-(1.96 * se), UL = est + (1.96 *se),p_value))

tab$Variable<-row.names(tab)
row.names(tab)<-NULL
tab<-tab%>%mutate(Variable=gsub("^group","",Variable))%>%
  select(Variable,everything())

#calculate the upper and the lower limits

toplot<-tab%>%
  mutate(Variable=gsub("\\(|\\)","",Variable),
         Variable=gsub("^group","",Variable),
         Variable=stringr::str_to_title(Variable))%>%
  mutate(Variable=ifelse(Variable=="Intercept","In-group",Variable))%>%
  mutate(impact=ifelse(Variable!="In-group",(Est + Est[Variable=="In-group"]),Est),
         LL_impact=ifelse(Variable!="In-group",(LL + LL[Variable=="In-group"]),LL),
         UL_impact=ifelse(Variable!="In-group",(UL + UL[Variable=="In-group"]),UL))


#plot the graph

ggplot(data=toplot,aes(x=Variable, y=impact,fill=Variable))+
  geom_bar(stat = "identity",position = position_dodge())+
  geom_errorbar(aes(x=Variable, ymin=LL_impact, ymax=UL_impact), colour="orange", alpha=0.9,width = 0.25, size=1.0)+
  SIC+
  labs(y="IOS Scale",x="",title="Department Closeness Across Groups")+mytheme+
  theme(legend.position="none") + theme_bw()

#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------

by_cyl <- mtcars %>% group_by(cyl)

# Sample fixed number per group
sample_n(mtcars, 10)
sample_n(mtcars, 50, replace = TRUE)
sample_n(mtcars, 10, weight = mpg)

sample_n(by_cyl, 3)
sample_n(by_cyl, 10, replace = TRUE)
sample_n(by_cyl, 3, weight = mpg / mean(mpg))

# Sample fixed fraction per group
# Default is to sample all data = randomly resample rows
sample_frac(mtcars)

sample_frac(mtcars, 0.1)
sample_frac(mtcars, 1.5, replace = TRUE)
sample_frac(mtcars, 0.1, weight = 1 / mpg)

sample_frac(by_cyl, 0.2)
sample_frac(by_cyl, 1, replace = TRUE)


