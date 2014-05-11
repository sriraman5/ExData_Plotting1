plot4 <- function(datevec) {
    
    getDATA(datevec)
    
    par(mfrow=c(2,2),mar=c(4,4,2,1))
    
    with(HPC,plot(datetime,Global_active_power,type="l",xlab="",ylab="Global Active Power"))
    
    with(HPC,plot(datetime,Voltage,type="l"))
    
    with(HPC, plot(datetime,Sub_metering_1,type="l",xlab="",ylab="Energy sub metering"))
    with(HPC,lines(datetime,Sub_metering_2,type="l",xlab="",ylab="",col="red"))
    with(HPC,lines(datetime,Sub_metering_3,type="l",xlab="",ylab="",col="blue"))
    
    legend("topright",lty=1,cex=0.7,col=c("black","red","blue"),names(HPC)[7:9])
    
    with(HPC,plot(datetime,Global_reactive_power,type="l",ylab="Global Reactive Power"))
    
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