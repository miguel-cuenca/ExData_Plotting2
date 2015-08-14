


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


### ____________ PLOT 2 ________________________________________________________________________
### Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
### Use the base plotting system to make a plot answering this question.

### Subset the NEI data frame to include only registers whose fips = Baltimore
baltimore<-subset(NEI,NEI$fips == "24510")

### Group the Baltimore registers by year and sum to calculate yearly totals
yearly_total_baltimore<-sapply(split(baltimore$Emissions,baltimore$year),sum)

### Plot the graph "Total early Emissions in Baltimore"
plot (x = as.numeric(names(yearly_total_baltimore)), y = yearly_total_baltimore/10^3,
      ,main="Total Yearly Emissions in Baltimore",ylab = "(thousand tons)" ,xlab="",pch=20)
lines(x = as.numeric(names(yearly_total_baltimore)), y = yearly_total_baltimore/10^3, col="red" )

### Copy the plot from the screen to a png file
dev.copy(png,file="plot2.png",width = 480, height = 480, units = "px", res=72, bg="transparent")
dev.off()

