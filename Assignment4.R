#######################################################################################################################
#AYUSH KUMAR
#ayk19003
#######################################################################################################################

#Question 1: 
#For the lower 48 States, , write a code that
#dynamically updates MAPS for New Reported Confirmed Cases per day 


#Set the word directory using setwd()
setwd("C:/Users/kumar/Downloads/Courses/project/R/Assignment4")

#Create a work directory wd (object) Assign a path
wd <- "C:/Users/kumar/Downloads/Courses/project/R/Assignment4"


#Install a package "readr"to read data from github
#install.packages("readr")
#Load the package using library() function
library(readr)

#to read the fie from Github assign the UR to an object, url.file
url.file <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/archived_data/archived_time_series/time_series_19-covid-Confirmed_archived_0325.csv"

#Read the file using read_csv(url()) function and assign it to a variable confirmed.cases
confirmed.cases <- read_csv(url(url.file))

#Lets put the data frame into another data frame named mydata
mydata <- confirmed.cases


#Download the US shape fie and keep it under the directory
#Load the shape file using the package rgdal
library(rgdal)


#Open the shape file using the function readOGR("path","folder_name") and assign it to a variable USA
USA <- readOGR("C:/Users/kumar/Downloads/Courses/project/R/Assignment4/tl_2019_us_state",
               "tl_2019_us_state")

#Download List of States
#Subset States of Interest 

lower48 <- "https://raw.githubusercontent.com/jasonong/List-of-US-States/master/states.csv"
lower48 <- read_csv(url(lower48))#read the file from URL using read_csv(url())

#Drop AK, HI , DC

lower48 <- lower48[lower48$Abbreviation!="HI",] #removes "Hawaii"
lower48 <- lower48[lower48$Abbreviation!="AK",] #removes "Alaska"
lower48 <- lower48[lower48$Abbreviation!="DC",] #removes "District of Columbia"


#Merge Shape file and .csv file

USA <- merge(x= USA, y=lower48,
             by.x = "NAME",
             by.y ="State")

#Remove NA after merge from USA
USA <- USA[!is.na(USA@data$Abbreviation),]

#plot to see the map of lower48 states and label it as "My First Map"
plot(USA, col = "skyblue",
     main = "My First Map")

#subset the US data only from mydata and assign it to covid.virus
covid.data <- mydata[mydata$`Country/Region`=="US",]



#Merge covid.data with the lower48 data frame 

covid.data <- merge(x=lower48, 
                    y=covid.data,
                    by.x="State",
                    by.y="Province/State",
                    all.x= T)#all the vlues in lower48 is true




COVID <- covid.data


#Creating new columns for Increased number of cases per day and adding the columns
a<-1
b<-6
for (a in 1:60) {
  covid.data[67+a] = covid.data[b+1] -covid.data[b]
  a<- a+1
  b<- b+1
  
}


#Creating new columns for the percentage increase of cases from the preceding day and add new columns

a<-1
b<-6
c<-1
for (a in 1:60) {
  for(c in 1:48){
    #d <- COVID[b]
    if(COVID[c,b]== 0)
    {
       COVID[c,67+a]= 0
    }
    else
    {
       COVID[c,67+a] = ((COVID[c,b+1] - COVID[c,b])/COVID[c,b])*100
    }
       
    c <- c+1
       
   }
  a <- a+1
  b <- b+1
  
}
  


#rm(COVID)

#rm(covid.data)

#Merge covid.data dataframe onto USA shape file

USA1 <- USA

covid.data <- covid.data[,-c(6:67)]


#Change the names of the newly created columns assign the dates to the names
i<-1
j<- 6
for (i in 1:60) {
  names(covid.data)[j] = names(confirmed.cases)[j]
  
  j <- j+1
  i <- i+1
  
}

#new names
colnames(covid.data)

#Subsetting COVID data
COVID <- COVID[,-c(6:67)]

#Change the names of the newly created columns assign the dates to the names
i<-1
j<- 6
for (i in 1:60) {
  names(COVID)[j] = names(confirmed.cases)[j]
  
  j <- j+1
  i <- i+1
  
}

#rm(COVID)



#Merge covid.data onto shapefile USA



USA <- merge(x=USA, 
             y=covid.data,
             by.x= "NAME",
             by.y= "State",
             all.x= T)



#plot the shape file for the number of cases in a day to check whether its working
plot(USA, col= USA@data$`3/22/20`)#it uses default colors

#To get nice plots use packages ggplot2 and tmap
library(ggplot2)
library(tmap)

#View the data of USA
USA@data
#view the column headings
names(USA)




#First create a directory confirmed.cases.maps
dir.create("confirmed.cases.maps")

#install.packages("rgeos")
library(rgeos)

#Run a for loop
a<-20
for(a in 20:ncol(USA)){ #Dates start from column number 20 
  map <- tm_shape(USA)+ #tm_shape loads the spatial object
    tm_fill(names(USA)[a], 
            breaks = c(0, 10, 25, 50, 100 ,200, 300, 400, 500, 1000, 2000, 5000, Inf),
            style = "fixed",
            colorNA = "white", 
            palette = "Reds")+
    tm_borders()+
    tm_layout("Increased Confirmed Cases Per day")
  
  tmap_save(map, 
            #width=1541, #default width
            #height=2860, #default height, big plot though...
            paste0(wd,"/confirmed.cases.maps/",gsub("/","_",names(USA)[a]),".png"))
}





USA1@data
names(USA1)

#Subset COVID

#COVID <- COVID[,-c(6:67)]
#rm(COVID)

#Merge COVID onto USA1 Shape file

USA1 <- merge(x=USA1, 
             y=COVID,
             by.x= "NAME",
             by.y= "State",
             all.x= T)



USA1@data
names(USA1@data)



#First create a directory confirmed.cases.maps
dir.create("Percent.confirmed.cases.maps")

#Run a for loop
a<- 6
for(a in 20:ncol(USA1)){ #Dates start from column number 20 
  map1 <- tm_shape(USA1)+ #tm_shape loads the spatial object
    tm_fill(names(USA1)[a], 
            breaks = c(0, 10, 25, 50, 100 ,200, 300, 400, 500, 1000, 2000, 5000, Inf),
            style = "fixed",
            colorNA = "white", 
            palette = "Reds")+
    tm_borders()+
    tm_layout("Percent increase in Confirmed Cases per day")
  
  tmap_save(map1, 
            #width=1541, #default width
            #height=2860, #default height, big plot though...
            paste0(wd,"/Percent.confirmed.cases.maps/",gsub("/","_",names(USA1)[a]),".png"))
}








