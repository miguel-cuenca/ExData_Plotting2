


### Download data
### If "summarySCC_PM25.rds" and "Source_Classification_Code.rds" are already downloaded to the working directory
### this step can be skipped

fileURL <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
zipfile="FNEI_data.zip"  
download.file(fileURL, destfile=zipfile)
unzip(zipfile)



### Read data into data frames
## This first line will likely take a few seconds. Be patient!
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")



### ____________ PLOT 4 ________________________________________________________________________
### Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

### Subset the SCC data frame with only the fields containing "Coal" in the EI.Sector field
coalSCC<-as.character(SCC[grep(pattern = "Coal",SCC$EI.Sector),1])

### Subset the NEI file using the SCC codes calculated in the previous step
coalNEI<- NEI [NEI$SCC %in% coalSCC,]

### Calculate the yaerly totals from Coal sources
yearly_total_coal<-sapply(split(coalNEI$Emissions,coalNEI$year),sum)

### Plot the graph "Total Yearly Emissions due to Coal combustion"
plot (x = as.numeric(names(yearly_total_coal)), y = yearly_total_coal/10^3,
      ylab = "thousand tons" ,xlab="",main="Total Yearly Emissions due to Coal combustion",pch=20)
lines(x = as.numeric(names(yearly_total_coal)), y = yearly_total_coal/10^3, col="red" )

### Copy the plot from the screen to a png file
dev.copy(png,file="plot4.png",width = 480, height = 480, units = "px", res=72, bg="transparent")
dev.off()

