#Function to create Plot2 as defined in Exploratory Data Analysis - Week 1 project
#Uses a dataset which must be downloaded and extracted from the following location
#https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#Data must be downloaded and unzipped in working directory prior to executing
plot2 <- function() {
      
      #read file
      power.data <- read.csv("household_power_consumption.txt",sep=";",header=TRUE)
      
      #subset data to 2/1/2007 and 2/2/2007
      power.data.sub <- subset(power.data,Date=="1/2/2007" | Date=="2/2/2007",
                               select=c(Global_active_power,Time,Date))
      
      #convert to numeric.  It's a factor so must be converted to character first
      #expect warning "NAs introduced by coercion" due to "?" in the data set
      #could be handled more elegantly, but sufficient for this exercise
      power.data.sub$Global_active_power <- as.numeric(as.character(power.data.sub$Global_active_power))
      
      #convert to POSIXlt date
      power.data.sub$Date <- as.POSIXlt(as.character(power.data.sub$Date),format="%d/%m/%Y")
      
      #convert to POSIXct time
      power.data.sub$Time <- as.POSIXlt(as.character(power.data.sub$Time),format = "%H:%M:%S")
      
      #add time to the date in a new variable called date.time
      power.data.sub$DateTime <- ISOdatetime(
              year(power.data.sub$Date)
            , month(power.data.sub$Date)
            , day(power.data.sub$Date)
            , power.data.sub$Time$hour
            ,power.data.sub$Time$min
            ,0)
      
      
      
      #open PNG device and create working file
      png(file="plot2.png",width=480,height=480)
      
      #create line graph
      plot(power.data.sub$DateTime
           ,power.data.sub$Global_active_power
           #set type to line
           ,type="l"
           #set Y-axis label
           ,ylab="Global Active Power (kilowatts)"
           #turn off X-axis label
           ,xlab=NA)
      
      #close PNG device
      dev.off()
}