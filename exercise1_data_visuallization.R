##Header: Session1 Data Visualization
#Date:05/12/19
#Author:Sonsiray Alvarez (sa73772@uga.edu)

##Load Data
  #Set working directory and upload your database
setwd("C:/Users/sa73772/Desktop/mers")
mers <- read.csv('cases.csv')

##Install and load packages
install.packages("lubridate")
install.packages("ggplot2")
library(lubridate)
library(ggplot2)

##Functions
  #look at your file
head(mers)

  #fix errors in file mers
mers$hospitalized[890] <- c('2015-02-20')
mers <- mers[-471,]

  #convert into numbers
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

  #Run ggplot2 --> bar plot with colors for different countries
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #modify plot --> every bar is a patient (we look @ time and space)
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country),position="fill") +
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #modify plot 
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  coord_flip()+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #modify plot 
ggplot(data=mers) +
  geom_bar(mapping=aes(x=epi.day, fill=country)) +
  coord_polar()+
  labs(x='Epidemic day', y='Case count', title='Global count of MERS cases by date of symptom onset',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #Calculate raw infectious period (# these data are class "difftime")
mers$infectious.period <- mers$hospitalized2-mers$onset2
class(mers$infectious.period)
mers$infectious.period <- as.numeric(mers$infectious.period, units = "days") # convert to days
ggplot(data=mers) +
geom_histogram(aes(x=infectious.period)) +
  labs(x='Infectious period', y='Frequency', title='Distribution of calculated MERS infectious period',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #Calculate raw infectious period with values 0 or above(not taking into consideration nonsocomial infections)
mers$infectious.period2 <- ifelse(mers$infectious.period<0,0,mers$infectious.period)
ggplot(data=mers) +
  geom_histogram(aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Distribution of calculated MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #Calculate density plot
ggplot(data=mers) +
  geom_density(mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Probability density for MERS infectious period (positive values only)', caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #Calculate area plot
ggplot(data=mers) +
  geom_area(stat='bin', mapping=aes(x=infectious.period2)) +
  labs(x='Infectious period', y='Frequency',
       title='Area plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #other representation using geom_dotplot and geom_bar.
ggplot(data=mers) +
  +     geom_dotplot(stat='bin', mapping=aes(x=infectious.period2)) +
  +     labs(x='Infectious period', y='Frequency',   title='Area plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #Smoothline
ggplot(data=mers) 
  +     geom_bar(stat='bin', mapping=aes(x=infectious.period2)) 
  +     labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)',caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")

  #Smoothline by country
ggplot(data=mers) +
geom_smooth(stat='bin',mapping=aes(x=infectious.period2, color=country)) +
labs(x='Infectious period', y='Frequency', title='Area plot for MERS infectious period (positive values only)')

  #Faceting (using different boxes per country)
ggplot(data=mers, mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_wrap(~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period (positive values only) over time')

  #Faceting (using different boxes per country and gender)
ggplot(data=subset(mers, gender %in% c('M', 'F') &
                     country %in% c('KSA', 'Oman', 'Iran', 'Jordan', 'Qatar', 'South Korea','UAE')),
       mapping=aes(x=epi.day, y=infectious.period2)) +
  geom_point(mapping = aes(color=country)) +
  facet_grid(gender ~ country) +
  scale_y_continuous(limits = c(0, 50)) +
  labs(x='Epidemic day', y='Infectious period',
       title='MERS infectious period by gender and country',
       caption="Data from: https://github.com/rambaut/MERS-Cases/blob/gh-pages/data/cases.csv")
