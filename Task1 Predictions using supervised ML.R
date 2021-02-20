# The Sparks Foundation GRIP February 2021
# Data Science and Business Analytics Internship
# TASK 1- Prediction using Supervised ML 

#reading the data from the csv file
dataset=read.csv(file="Study hours data.csv",header =T);dataset; 

#extracting the study hours as the independent variable
x=dataset$Hours;x   

#extracting the scores as the dependent variable
y=dataset$Scores;y 

#plotting the data as a scatterplot 
library(ggplot2)
g=ggplot(dataset,aes(x,y)) + geom_point(col="#000099",size=2) + labs(title="Scatterplot of Score vs Study hour",y="Score",x="Study hours") + xlim(c(0,10)) + ylim(c(0,100))+theme(panel.background = element_rect(fill = "#ffff99"),panel.grid = element_line(linetype = 0))
g

# regression of y on x
reg=lm(y~x)
summary(reg)
beta=reg$coefficients

# to predict the score if a student studies 9.25 hours/day
beta[1]+(beta[2]*9.25)

#hence a student will score approximately 93 if he/she studies 9.25 hrs/day

g1=ggplot(dataset,aes(x,y)) + geom_point(col="#000099",size=2) + labs(title="Scatterplot of Score vs Study hour along with the regression line",y="Score",x="Study hours") + xlim(c(0,10)) + ylim(c(0,100)) + geom_smooth(method="lm",col="#009900")+theme(panel.background = element_rect(fill="#ffffcc"),panel.grid = element_line(linetype = 0))
g1

y_pred=reg$fitted.values;
d=data.frame(x,y,y_pred)
#plotting both observed scores and predicted scores to compare
g2=ggplot(d)+geom_point(aes(x,y=y),col="#cc0066",size=2)+geom_point(aes(x,y_pred),col="#009900",size=2)+xlim(c(0,10))+ylim(c(0,100))+labs(x="Study hours",y="score")+theme(panel.background = element_rect(fill = "#cccccc"),panel.grid = element_line(linetype = 0))
g2

# Thank You
