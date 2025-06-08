install.packages("tidyverse")
library(tidyverse)
library(dplyr)

#This is the destination of the website URL
fileUrl <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2Fhousehold_power_consumption.zip"

#I downloaded the zip file
download.file(fileUrl, destfile = "exdata_data_household_power_consumption.zip")
# I set directory to Project-5 and inside the file is the zip file
directory <- "C:/Users/Danie/Desktop/John Hopkins University Data Science Certification/Project-5/
exdata_data_household_power_consumption.zip"
# I unziped the zip file I am going to be using
unzip(zipfile = "exdata_data_household_power_consumption.zip",
      exdir = "./Project-5", overwrite = TRUE)
#gives me a list of of files in the directory
list.dirs(recursive = TRUE)
# will give me a list of Files that are in my Project-5 folder
list.files("./Project-5")
# It will check if the household folder is in my Project-5 folder
file.exists("./Project-5/household_power_consumption.txt")
#Load the dataset from the text file using read_delim from readr package
#Set column names to TRUE, delimiter to ";" and NA values to "?"
Dataset <- read_delim("./Project-5/household_power_consumption.txt",
                      col_names  = TRUE, delim = ";", na = "?")
#Check the structure of the date column 
str(Dataset$Date)
#Convert the date column to date format
Dataset$Date <- as.Date(Dataset$Date, format = "%d/%m/%Y")
# Filter the dataset to include only dates from February 1 and 2, 2007.
FilteredData <- filter(Dataset, Date == as.Date("2007-02-01")
                       | Date == as.Date("2007-02-02"))
# Combines the date and time into one datetime column 
FilteredData <- mutate(FilteredData, Date_Time_Combined
                    = as.POSIXct(paste(Date, Time), format = "%Y-%m-%d %H:%M:%S"))
# View the structure and summary of the new datetime column
str(FilteredData$Date_Time_Combined)


# Create a  Histogram of Global Active power 
Plot1 <- hist(FilteredData$Global_active_power, main = "Global Active Power",
              xlab = "Global Active Power(kilowatts)", ylab = "Frequency",
              xlim = c(0,6), ylim = c(0,1200), col = "red")
# Line Plot of Global Active Power over time,
# x-axis is the combined  datetime, y-axis is the power usage.
Plot2 <- plot(x = FilteredData$Date_Time_Combined,
              y = FilteredData$Global_active_power,
             type = "l", xlab = "Date/Time", ylab = "Global Active Power(kilowatts)")


# I created the plot looking at Date Time combined in the x-axis
# and Sub metering 1 in the y axis and labeled the color black.
Plot3 <- plot(FilteredData$Date_Time_Combined,
              FilteredData$Sub_metering_1, 
              type = "l",
              xlab = "Date/Time",
              ylab = "Energy sub metering")
              #I created a second plot and used the color red to represent it 
              lines(FilteredData$Date_Time_Combined,
              FilteredData$Sub_metering_2, col = "red")
              #I created a third plot and used the color blue to represent it
              lines(FilteredData$Date_Time_Combined,
              FilteredData$Sub_metering_3, col = "blue")
              #Position topright corner you can see Sub metering 1 - 3 
              #corresponding colors
              legend("topright", 
              legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
              col = c("black","red","blue"),
              lty = 1)
            
              
# Create a 2x2 grid for plot 4 combining Plot4, Plot5, Plot6,
# and Plot 7 together.     
Plot4 <- par(mfrow = c(2,2))
         plot(FilteredData$Date_Time_Combined,
              FilteredData$Global_active_power,
              type = "l",
              xlab = "Date/Time",
              ylab = "Global active power",
              ylim = c(0,6))
         
          
Plot5 <- par(mfrow = c(2,2))
         plot(FilteredData$Date_Time_Combined,
              FilteredData$Voltage,
              type = "l",
              xlab = "Date/Time",
              ylab = "Voltage",
              ylim = c(234, 246))
         
         
Plot6 <-par(mfrow = c(2,2))
          plot(FilteredData$Date_Time_Combined,
              FilteredData$Sub_metering_1, 
              type = "l",
              xlab = "Date/Time",
              ylab = "Energy sub metering")
#I created a second plot and used the color red to represent it 
lines(FilteredData$Date_Time_Combined,
      FilteredData$Sub_metering_2, col = "red")
#I created a third plot and used the color blue to represent it
lines(FilteredData$Date_Time_Combined,
      FilteredData$Sub_metering_3, col = "blue")
#Position topright corner you can see Sub metering 1 - 3 
#corresponding colors
legend("topright", 
       legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
       col = c("black","red","blue"),
       lty = 1, 
       cex = 0.75)         
 

 Plot7 <- par(mfrow = c(2,2))
          plot(FilteredData$Date_Time_Combined,
               FilteredData$Global_reactive_power,
               type = "l",
               xlab = "Date/Time",
               ylab = "Global reactive power",
               ylim = c(0,0.5))
          
  # Close the PNG device to write the file
  dev.off()                  
          