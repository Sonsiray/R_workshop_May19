 ##Header: Session3 Data wrangling
#Date:05/14/19
#Author:Sonsiray Alvarez (sa73772@uga.edu)

##Load Packages
library(tidyverse)
library(magrittr)
library(dplyr)
library(stringr)
library(GGally)
library(maptools)
library(ggmap)
library(maps)

##Load Data
Id <- read_csv('lyme.csv')
pop<- read_csv('pop.csv')
prism <- read_csv('climate.csv')

##Declare Functions 
#to convert the population data to tidy data format.
pop %<>% select(fips,starts_with("pop2")) #only selects values of fips and pop starting with "pop2"
pop %<>% gather(starts_with("pop2"),key="str_year",value="size") %>% na.omit # gather takes multiple columns, and gathers them into key-value pairs
pop %<>% mutate(year=str_replace_all(str_year,"pop","")) #mutate adds new variables and preserves existing ones
pop %<>% mutate(year=as.integer(year))# turns value into integer
pop %<>% mutate(fips=str_replace_all(fips,"^0",""))#stir_replace_All replaces characters (text) with other characters
pop %<>% mutate(fips=as.integer(fips))# turns value into integer

#to convert the Lyme disease data to tidy data format.
  #1.Add appropriate number of zeros in front of the county value
Id$n_county <- ifelse(Id$CTYCODE < 10, paste0('00',Id$CTYCODE),ifelse(Id$CTYCODE < 100, paste0('0',Id$CTYCODE),paste0(Id$CTYCODE)))
    #ifelse is asking if Id$CITYCODE < 10 is true then paste00 without space, if false go to next function that is another ifelse
  #2.Paste statecode to new variable tath we created with edited county codes
Id$fips <- paste0(Id$STCODE,Id$n_county)
  #3.Convert the population data to tidy data format
Id %<>% select(fips,starts_with("Cases2"))
Id %<>% gather(starts_with("Cases2"),key="str_year",value="size")
Id %<>% mutate(year=str_replace_all(str_year,"Cases",""))
Id %<>% mutate(year=as.integer(year))
Id %<>% mutate(fips=str_replace_all(fips,"^0",""))
Id %<>% mutate(fips=as.integer(fips))

#Rename columns
Id %<>% 
  rename(
    n_cases =size
    )

pop %<>% 
  rename(
    pop_size =size
  )

#merge database Id and prism with dyplr
Id_prism<- inner_join(Id, prism, by = c('fips','year'))

#merge database Id_prism & pop with dyplr
Id_prism %<>% mutate(year=as.integer(year)) #convert years in integer
pop %<>% mutate(year=as.integer(year))  #convert years in integer
Id_prism_pop<-left_join(Id_prism, pop, by = c('fips','year') )
Id_prism_pop %<>% select(-starts_with("str_")) #removes columns "str_..." that we dont need anymore

#(1) to determine how many cases of Lyme
cases_per_year<-Id_prism_pop %>% group_by(year)%>% summarize (avCases=mean(n_cases))%>%arrange(desc(avCases))

#(2) the average number of cases in each state
Id_original <- read_csv('lyme.csv')
Id_original %<>% gather(starts_with("Cases2"),key="str_year",value="size")
Id_original %<>% mutate(year=str_replace_all(str_year,"Cases",""))
Id_original %<>% mutate(year=as.integer(year))
Id_original %<>% select(- 'CTYNAME')
Id_original %<>% select(- 'STCODE')
Id_original %<>% select(- 'CTYCODE')
Id_original %<>% select(- 'str_year')
cases_state<-Id_original %>% group_by(STNAME)%>% summarize (avCases=mean(size))%>%arrange(desc(avCases))

#save data
save(Id_prism_pop, file='Id_prism_pop.Rda')

#create a csv files
write.csv(cases_per_year,"cases_per_year.csv")
write.csv(Id_prism_pop,"Id_prism_pop.csv")
write.csv(cases_state,"cases_state.csv")

#visualization
  #preparing the data
Lyme <- read_csv('lyme.csv')
Lyme %<>% gather(starts_with("Cases2"),key="str_year",value="size")
Lyme %<>% mutate(year=str_replace_all(str_year,"Cases",""))
Lyme %<>% mutate(year=as.integer(year))
Lyme$n_county <- ifelse(Lyme$CTYCODE < 10, paste0('00',Lyme$CTYCODE),ifelse(Lyme$CTYCODE < 100, paste0('0',Lyme$CTYCODE),paste0(Lyme$CTYCODE)))
Lyme$fips <- paste0(Lyme$STCODE,Lyme$n_county)
Lyme %<>% mutate(fips=as.integer(fips))
Lyme_visualization<-left_join(Lyme,Id_prism_pop, by =c('fips','year') )
  #visualizing the data
county_map <- map_data("county")
state_map <- map_data("state")
ag.fips <- group_by(Lyme_visualization,fips)
ld.16y<-summarize(ag.fips,all.cases=sum(n_cases))
ld.16y<-left_join(select(Lyme_visualization,c('STNAME','CTYNAME','fips')),ld.16y)
ld.16y<-distinct(ld.16y)
ld.16y %<>% rename(region=STNAME,subregion=CTYNAME)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," County","")
ld.16y$region<-tolower(ld.16y$region)
ld.16y$subregion<-tolower(ld.16y$subregion)
ld.16y$subregion<-str_replace_all(ld.16y$subregion," parish","")
ld.16y %<>% mutate(log10cases=log10(1+all.cases))
map.ld.16y<-left_join(county_map,ld.16y)
ggplot(map.ld.16y)+geom_polygon(aes(long,lat,group=group,fill=log10cases),color="gray",lwd=0.2) +
  scale_fill_gradientn(colours=rev(heat.colors(10)))

