##Header: Session2 Programing 
  #Date:05/13/19
  #Author:Sonsiray Alvarez (sa73772@uga.edu)

##Load Packages
  library(ggplot2)
  library (dplyr)
  
##Load Data
  WNV <- read.csv('https://raw.githubusercontent.com/jdrakephd/ideas-workshop/master/wnv.csv')
  head(WNV)

##Declare Functions 
  
  #log total number of cases
  logTotal<- log10(WNV$Total)
  logTotal.v2 <- log(10,WNV$Total)
     
  #Total number of cases in each state in each year
  ggplot(data= WNV, mapping=aes(x=Year, y=Total)) +
    geom_point(mapping = aes(color=State)) +
    facet_wrap(~ State) +
    scale_y_continuous(limits = c(0, 4000)) +
    labs(x='Year', y=' log10 Total cases',
         title='Total number of cases in each state in each year')

  #Log values of total number of cases in each state in each year
  ggplot(data= WNV, mapping=aes(x=Year, y=Total)) +
    geom_point(mapping = aes(color=State)) +
    facet_wrap(~ State) +
    scale_y_continuous(limits = c(0, 4)) +
    labs(x='Year', y='log10 Total cases',
         title='Log values of total number of cases in each state in each year')
  
  #Case fatality rate(CFR)
  CFR <- WNV$Fatal/WNV$Total
  
  #CFR in each state in each year
  ggplot(data= WNV, mapping=aes(x=CFR)) +
    geom_histogram()  +
    facet_wrap(~ Year) +
    labs(x='CFR', y='count',title='CFR in each state in each year')
  
  #cases in which the calculated total does not match the given total
  Calculated_total <- WNV$Fever+WNV$EncephMen+WNV$Other
  sum(Calculated_total!=WNV$Total)

  #Modular arithmetic for dozen error
  error <- WNV$Total%%12 #divide by 12 and give me what is left
  error #show error
  Total_error <- sum(error)
  Total_error #show total error
  
  #NDR for 3 states & ggplot
  ndr <- function(state="Colorado", years=1999:2007){ #neuroinvasive name of function, state and years are inputs (generic, put what you want)
   #Colorado and years are deffault
   x <- WNV[WNV$State %in% state & WNV$Year %in% years,]
    #subset states and years we are interested
   y <- data.frame (state=x$State, ndr=x$EncephMen/x$Total)
    #new data frame with 2 variables
   m <- aggregate (y$ndr, by=list (y$state), FUN=mean)
    #use some function that we use to sumarize ndr by state, that function is mean
   se <- aggregate(y$ndr, by=list (y$state), FUN=function(x) sd(x)/sqrt(length(x)))
    #use some function that we use to sumarize ndr by state, that function is sd
   out <-merge (m,se,by= 'Group.1')
   names(out) <- c('state','mean.ndr', 'se.ndr')
   #vector with name state, mean and se
   return (out)
    }
   
  ndr_disease <- ndr (state=c('California', 'Colorado', 'New York'))
    # it uses the deffault years because i didnt give any value

  ggplot(ndr_disease, aes(x=state, y=mean.ndr,fill=state))+
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax=mean.ndr+se.ndr))+
    labs(x='state', y='NDR')
  
  #ndr for all states
  ndr_disease_all <- ndr(state=levels(WNV$State))
  
  ggplot(ndr_disease_all, aes(x=state, y=mean.ndr,fill=state))+
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax=mean.ndr+se.ndr))+
    labs(x='state', y='NDR')
  
  #using pipes to produce the plot
  WNV %>%
    filter ( State %in% c('California', 'Colorado', 'New York'))%>%
    group_by(State) %>%
    summarize(mean.ndr = mean(EncephMen/Total),se.ndr = sd(EncephMen/Total)/sqrt(length(EncephMen/Total)))
    ggplot (aes(x=State, y=mean.ndr, fill=State))+
    geom_bar(stat = "identity")+
    geom_errorbar(aes(ymin=mean.ndr-se.ndr, ymax=mean.ndr+se.ndr))+
    labs(x='state', y='NDR')
  
  