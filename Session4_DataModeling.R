##Header: Session4 Data Modelling
#Date:05/15/19
#Author:Sonsiray Alvarez (sa73772@uga.edu)

##Load Packages
library(tidyverse)
library(magrittr)
library(GGally)
library(ggplot2)
library(purrr)


##Load Data
Data<- read_csv('lymedata_clean.csv')


##Declare Functions 
#ggpairs function to obtain a 4x4 summary plot of precipitation (prcp), average temperature
Data%>%ggpairs(columns =c("prcp","avtemp","pop_size","n_cases"))

#Create two new columns for log10(size) and log10(cases+1) 
Data$log10_size<- log10(Data$pop_size)
Data$log10_cases<- log10(Data$n_cases+1) #we add 1 because log10 of 1 is 0

#substitute these for the original size and cases & recreate the ggpairs plot
Data%>%ggpairs(columns =c("prcp","avtemp","log10_size","log10_cases"))

#Using set.seed(222), create a new data frame to be a random sample (n=100rows) of the full data frame 
set.seed(222);SubSet<-Data %>% sample_n(100)

#plot precipitation (x-axis) vs average temperature (y-axis).
Myplot<- ggplot(data= SubSet, mapping=aes(x=prcp, y=avtemp))+
geom_point()+
labs(x='prcp', y='avtemp')

#Add the best straight line to the plot using geom_smooth.
Myplot2<- ggplot(data= SubSet, mapping=aes(x=prcp, y=avtemp))+
  geom_point()+
  labs(x='prcp', y='avtemp')+
  geom_smooth(aes(x=prcp, y=avtemp),method='lm')

#Create a linear model (lm) object with a call like myModel
Mymodel<- summary(lm(avtemp~prcp,data=SubSet))

#What is the slope of the line you plotted in Task 5, and is the slope significantly different from 0 (p<0.05)?
      # slope is positive correlation, significantly different ->  p-value: 1.06e-05

#Write a single line of code to generate a ggplot of total population size by year
Data%>% group_by(year) %>% summarize(total=sum(size)) %>% ggplot(.,aes(year,total))+geom_point()

#Create a data frame called "by_state" from the main data frame, that groups by state, and inspect it.
by_state<-Data %>%  group_by(state)

#update this new data frame so that it is nested (simply pass it to nest).
by_state<-Data %>%  group_by(state)%>% nest()

#Display the Georgia data in the console window.
by_state$data[[10]]

#Write a function that takes a data frame as its argument and returns a linear model object that predicts size by year.
LM_function <- function(i){
  mymodel<-lm(size~year,data=i)
  return (mymodel)
}

#Add a column to the by_state dataframe, where each row (state) has its own model object.
by_state$LM_object <- purrr::map(by_state$data,LM_function)

#Run purrr::map and inspect "resids". What is the structure of "resids"?
by_state$resids <- purrr::map(by_state$LM_object,residuals)

#Write a function that accepts an object of the type in the resids list, and returns a sum of the absolute values
Sum_function <- function(i){
  sum(abs(i))
}

by_state$resids_sum <- purrr::map(by_state$resids,Sum_function)

#Write a function that accepts a linear model and returns the slope (model M has slope M$coefficients[2]) 
Slope_function<- function(model){
  summary(model)$coefficients[2]
}
  
#use this function to create a new column called slope in the by_state data frame, that is the slope for each state
by_state$slope <- purrr::map(by_state$LM_object,Slope_function)

#Plot the growth rate (slope value) for all states.
slope <- unnest(by_state, slope)
ggplot(data= slope, mapping=aes(x=state, y=slope))+
  geom_point()+
  labs(x='state', y='slope')

#Plot the total resisduals for all states.
resids <- unnest(by_state, resids)
ggplot(data= resids, mapping=aes(x=state, y=resids))+
  geom_point()+
  labs(x='state', y='resids')

#Repeat Tasks 9 and 10 using a different data frame name, by_state2.
by_state2<-Data %>%  group_by(state)
by_state2<-Data %>%  group_by(state)%>% nest()

#Write a function that accepts an element of the by_state2$data list-column and returns the spearman correlation coefficient between Lyme disease cases and precipitation
by_state$Spearman<-spearman_coeff <-function(i){
  mySpearman_coeff <- cor.test(by_state2$data,method="spearman")$estimate
  return (mySpearman_coeff)
}
  