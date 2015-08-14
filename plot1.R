

### Download data
### If "summarySCC_PM25.rds" and "Source_Classification_Code.rds" are already downloaded to the working directory
### this step can be skiped

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipfile="FNEI_data.zip"  
download.file(fileURL, destfile=zipfile)
unzip(zipfile)



### Read data into data frames
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

### ____________ PLOT 1 ________________________________________________________________________
### Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
### Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
### for each of the years 1999, 2002, 2005, and 2008.

### Group the registers by year and sum to calculate yearly totals
yearly_total <- sapply(split(NEI$Emissions,NEI$year),sum)

### Plot the graph "Total early Emissions"
plot (x = as.numeric(names(yearly_total)), y = yearly_total/10^6,
      ylab = "million tons" ,xlab="",main="Total Yearly Emissions",pch=20)
lines(x = as.numeric(names(yearly_total)), y = yearly_total/10^6, col="red" )

### Copy the plot from the screen to a png file
dev.copy(png,file="plot1.png",width = 480, height = 480, units = "px", res=72, bg="transparent")
dev.off()

