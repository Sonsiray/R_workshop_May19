##Header: Session5 Project Management
#Date:05/15/19
#Author:Sonsiray Alvarez (sa73772@uga.edu)

##Load Packages
library(lubridate)
library(ggplot2)

##Load Data
mers <- read.csv('cases.csv')

##Declare Functions 

  #convert dates into numbers
mers$onset2 <- ymd(mers$onset)
mers$hospitalized2 <- ymd(mers$hospitalized)
class(mers$onset2)

 # na.omit to remove rows with missing data from column onset2 in file mers
 # min finds the min value of your data set (in this case is day0 of outbreak)
day0 <- min(na.omit(mers$onset2))

 #as.numeric translates dates into epidemic days (number)
mers$epi.day <- as.numeric(mers$onset2 - day0)

 #Run ggplot2 --> bar plot (+ signs add functions, without them we would have en empty plot)
ggplot(data=mers) + #gives you an empty plot
  geom_bar(mapping=aes(x=epi.day)) + #introduces bar plot
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv") #labels
