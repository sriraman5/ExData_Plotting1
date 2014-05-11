plot3 <- function(datevec) {
    
    ## Submetering by minute for designated dates
    ## uses data from the UC Irvine Machine Learning Repository
    
    ## datevec <- vector of dates (character OK)
    ## dislike having same function below in multiple files :(
    
    getDATA(datevec)    
    
    with(HPC, plot(datetime,Sub_metering_1,type="l",xlab="",ylab="Energy sub metering"))
    with(HPC,lines(datetime,Sub_metering_2,type="l",xlab="",ylab="",col="red"))
    with(HPC,lines(datetime,Sub_metering_3,type="l",xlab="",ylab="",col="blue"))
    
    legend("topright",lty=1,col=c("black","red","blue"),names(HPC)[7:9])
    
    
}

getDATA <- function(datevec) {
    
    ## creates HPC if it doesn't exist or the dates have changed
    
    datevec <- as.Date(datevec)
    if((!exists("HPC"))|(!exists("dateVEC"))) {
        gethpc(datevec)
    }
    else if (!identical(datevec,dateVEC)) {
        gethpc(datevec)
    }
    
    invisible()
}

gethpc <- function(datevec) {
    
    # read raw file data from "household_power_consumption.txt"
    
    # datevec = list of dates
    
    # creates globals HPC and dateVEC
    
    ## file below is copied from University of Irvine database -- last modified 10/12/2012
    ## https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
    
    fn <- "household_power_consumption.txt"
    datevec <- as.Date(datevec)
    
    # create global hpc for later use
    
    classes <- rep(c("character","numeric"),c(2,7))
    d <- read.csv(fn,sep=";",na.strings="?",colClasses=classes)
    
    # select desired dates
    
    fd <- as.Date(d$Date,"%d/%m/%Y")
    d <- d[fd %in% datevec,]
    
    d$Time<- strptime(paste(d$Date, d$Time), format='%d/%m/%Y %H:%M:%S') ## format character time data
    d$Date <- as.Date(d$Date,"%d/%m/%Y")   # probably better than using fd which is usually much longer
    
    n <- names(d)
    n[2] <- "datetime" ## futzing around with names to match desired output (I liked Time better)
    names(d) <- n
    
    ## creating globals, 
    
    HPC <<- d
    dateVEC <<- datevec
    
    print("HPC cached. dateVEC created.")
    
    invisible()
    
}