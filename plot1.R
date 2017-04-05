#Function to create Plot1 as defined in Exploratory Data Analysis - Week 1 project
#Uses a dataset which must be downloaded and extracted from the following location
#https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip
#Data must be downloaded and unzipped in working directory prior to executing
plot1 <- function() {
      
      #read file
      power.data <- read.csv("household_power_consumption.txt",sep=";",header=TRUE)
      
      #subset data to 2/1/2007 and 2/2/2007
      power.data.sub <- subset(power.data,Date=="1/2/2007" | Date=="2/2/2007",
                               select=Global_active_power)
      
      #convert to numeric.  It's a factor so must be converted to character first
      #expect warning "NAs introduced by coercion" due to "?" in the data set
      #could be handled more elegantly, but sufficient for this exercise
      power.data.sub <- as.numeric(as.character(power.data.sub$Global_active_power))
      
      #open PNG device and create working file
      png(file="plot1.png",width=480,height=480)
      
      #create histogram
      hist(power.data.sub
           #set the title
           ,main="Global Active Power"
           #set the bar color to red
           ,col="red"
           #set the x-axis label
           ,xlab="Global Active Power (kilowatts)")
      
      #close PNG device
      dev.off()
}