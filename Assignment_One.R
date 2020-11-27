## Programming assignment one
## DATA
# specdata (https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip)
datase_url<- "https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2Fspecdata.zip"
download.file(datase_url, "specdata.zip")
unzip("specdata.zip", exdir = "specdata")

## contains 332 csv files, each file contains 3 variables;
# Date: the date of the observation in YYYY-MM-DD format (year-month-day)
# sulfate: the level of sulfate PM in the air on that date 
# (measured in micrograms per cubic meter)
# nitrate: the level of nitrate PM in the air on that date 
# (measured in micrograms per cubic meter)


##  PART ONE ##
# POLLUTANTMEAN Function
# pollutantmean() calculates the mean of a pollutant (sulfate, nitrate) across
# a specified list of monitors ignoring missing values
# the function takes in 3 arguments, directory, pollutant and id
# the provided directory has the dataset
# the function returns a numeric vector
pollutantmean<- function(directory,pollutant,id= 1:332){
    # create a list of all files
    # list.files() returns a list of all files in the provided directory/folder
    # and append directory to the beginning of the file name
    myfiles<-list.files(directory, full.names = TRUE)
    unlist(myfiles) # converts the list to a vector
    # create an empty data frame to hold all the files (because of rbind)
    mydata<- data.frame()
    # create an empty vector to hold the means
    means<- numeric() 
    for( i in id){
        # loops through the files, rbinding them together
        mydata<-rbind(mydata, read.csv(myfiles[i]))
        if (pollutant=="sulfate"){
            means<-mean(mydata[,2],na.rm = TRUE)
        } else {
            means<- mean(mydata[,3], na.rm = TRUE)
        }
    }
    
    return(means)
}
pollutantmean("specdata", "sulfate", 1:10) #1
pollutantmean("specdata", "nitrate", 70:72) #2
pollutantmean("specdata", "sulfate", 34) #3
pollutantmean("specdata", "nitrate")  #4



## PART TWO ##
## COMPLETE Function

# the function reads a directory full of files and reports the number of 
# completely observed cases in each data file
# the function returns a data frame where the first column is the name of 
# the file and the second column is the number of complete cases
complete<- function(directory, id = 1:332){
    myfiles<- list.files(directory, full.names = TRUE)
    unlist(myfiles)
    nobs<-numeric()
    mycomplete<- data.frame()
    for(i in id){
        mydata <-  read.csv(myfiles[i])
        # complete.cases () returns a logical vector of length k 
        # where missing values are treated as false
        # sum (complete.cases()) returns the number of complete cases since
        # TRUE = 1 and FALSE =0
        nobs<- sum(complete.cases(mydata))
        cases<- data.frame(id= i, nobs=nobs)
        
        mycomplete<- rbind(mycomplete, cases)
        
    }
    
    return(mycomplete)
}

#5
cc<-complete("specdata", c(6,10,20,34,100,200,310))
print(cc$nobs)
#6
cc<-complete("specdata", 54)
print(cc$nobs)
#7
# RNGversion("3.5.1")  
set.seed(42)
cc <- complete("specdata", 332:1)
use <- sample(332, 10)
print(cc[use, "nobs"])


## PART THREE ##
## CORR Function

# the function takes a directory of data files and a threshold for complete 
# cases and calculates the correlation between sulfate and nitrate for monitor
# locations where the number of completely observed cases (on all variables) is 
# greater than the threshold.
# the function returns a vector of correlations for the monitors that meet the 
# threshold requirement.
corr<- function(directory, threshold = 0){
    myfiles<- list.files(directory, full.names = TRUE)
    unlist(myfiles)
    corrV<- numeric()
    corrValues<-c()
    for(i in 1:332){
        mydata<- read.csv(myfiles[i])
        nobs<- sum(complete.cases(mydata))
        if (nobs > threshold){
            corrV<- cor(mydata[,2], mydata[,3],use = "complete.obs")
            corrValues<- c(corrValues, corrV)
        }
        # corrValues<- c(corrValues, corrV)
    }
    return(corrValues)
}
#8
cr<-corr("specdata")
cr<- sort(cr)
# RNGversion("3.5.1")
set.seed(868)
out<- round(cr[sample(length(cr), 5)], 4)
print(out)

#9
cr <- corr("specdata", 129)                
cr <- sort(cr)                
n <- length(cr)    
RNGversion("3.5.1")
set.seed(197)                
out <- c(n, round(cr[sample(n, 5)], 4))
print(out)

#10
cr <- corr("specdata", 2000)                
n <- length(cr)                
cr <- corr("specdata", 1000)                
cr <- sort(cr)
print(c(n, round(cr, 4)))

###  END    ###


