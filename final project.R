




#The report should contain four sections: Introduction, Visualizations, Analysis, and Conclusion.  
#It should be a minimum of 500 words long and a maximum of 1000 words. 
#The Introduction section should introduce the dataset to the reader and explain the variables that are used.  

#The Visualizations section should contain at least two ggplot2 plots appropriately labeled.


#The Analysis section should use at least one of the following concepts emphasized in the course; 
#probability simulation, confidence intervals, hypothesis testing, linear models, 
#or machine learning.

#Finally, the Conclusion section should tell us what we can conclude from the visualization and analysis.  


## INTRODUCTION 
#The world Health Organizations(WHO) announced that cardiovascular diseases is the top one killer over the world. Nearly 17.9 million people die from cardiovascular disease every year. Preventing cardiovascular disease could be better than a cure. 
#If there was a way to evaluate the risk of every patient who could have a hear disease.
#This data set is real data that include important features of patients. 

library(data.table)
library(tidyverse)
library(ggthemes)
library(rpart)
library(caTools)


urlfile<- "https://github.com/mgafford/IS-4300-Final-Project/raw/main/heart.csv"
heart <- read_csv(url(urlfile))
#head(heart)
df<-as.data.frame(heart)

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
### Data Dictionary
#age: Age of the patient in years
#sex: (1 = male; 0 = female)
#cp: chest pain type:
#  Value 1: typical angina
#Value 2: atypical angina
#Value 3: non-anginal pain
#Value 4: asymptomatic
#trestbps: Resting blood pressure (in mm Hg on admission to the hospital)
#chol: serum cholestoral in mg/dl
#fbs: fasting blood sugar > 120 mg/dl (1 = true; 0 = false)
#restecg: resting electrocardiographic results
#Value 0: normal
#Value 1: having ST-T wave abnormality (T wave inversions and/or ST elevation or depression of > 0.05 mV)
#Value 2: showing probable or definite left ventricular hypertrophy by Estes
#thalach: maximum heart rate achieved
#exang: exercise induced angina (1 = yes; 0 = no)
#oldpeak: ST depression induced by exercise relative to rest
#slope: the slope of the peak exercise ST segment:
#  Value 1: upsloping
#Value 2: flat
#Value 3: downsloping
#ca: number of major vessels (0-3) colored by flourosopy
#thal:
#  3 = normal
#6 = fixed defect
#7 = reversable defect
#target: diagnosis of heart disease (angiographic disease status)
#Value 0: < 50% diameter narrowing
#Value 1: > 50% diameter narrowing (in any major vessel: attributes 59 through 68 are vessels)






## Visualization
#In the following visualizations we see the density of age to heart disease. With 0 meaning no disease and 1 having heart disease.  

#Variable Age
g1 <- ggplot(df,aes(age,col=as.factor(target),fill=as.factor(target)))+
  geom_density(alpha=0.2)+
  guides(col=F)+
  labs(fill="Heart Disease",x="Age")

g2 <- ggplot(df,aes(as.factor(target),age,fill=as.factor(target)))+
  geom_boxplot(alpha=0.2)+
  labs(y="Age",x="Heart Disease",fill="Heart Disease")

multiplot(g1,g2,cols=1)

# Maximum heart rate

g1 <- ggplot(df,aes(thalach,col=as.factor(target),fill=as.factor(target)))+
  geom_density(alpha=0.2)+
  guides(col=F)+
  labs(fill="Heart Disease",x="Maximum heart rate achieved")

g2 <- ggplot(df,aes(as.factor(target),thalach,fill=as.factor(target)))+
  geom_boxplot(alpha=0.2)+
  labs(y="Maximum Heart Rate Achieved",x="Heart Disease",fill="Heart Disease")

multiplot(g1,g2,cols=1)




#Analysis

# create a decision tree to pick the best variables
D_Tree <- rpart(target~age+sex+cp+thalach+restecg,
                data =df)
plot(D_Tree,margin =.1)
text(D_Tree, cex=.7)


#training & test

nobs=nrow(df)
nosample = round(nobs*0.7)
train_ind <-sample(nobs,size=nosample)
train <-df[train_ind,]
test <- df[-train_ind,]

regression1 <-step(lm(target~., data=train))
summary(regression1)

goodmodel <-"target ~ slope + thalach + exang + chol + oldpeak + thal + 
    ca + cp + sex"

regression2<-lm(goodmodel, data=df)
summary(regression2)


#Logistic Model 
logit1 = glm(goodmodel, data=train, family=binomial(link = logit))
summary(logit1)

yhat<-predict (logit1, type ="response")
data1 <- cbind(df$target,yhat)
head(data1)


#prediction using logit model to new test data(out of sample forecasting )
prob1 <-predict(logit1,newdata=test,type="response")
#decision of heart disease
result1 <-ifelse(prob1 >0.5,1,0)
#true of hear disease
test_disease <-test$target
table(test_disease,result1)



#Conclusion
df1 <- df%>%
  filter(age > 57) %>%
  filter(thalach > 153) %>%
  filter(cp > 1)%>%
  filter(sex == 0)
df1
