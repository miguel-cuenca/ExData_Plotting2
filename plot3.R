

library (ggplot2)


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



### ____________ PLOT 3 ________________________________________________________________________
### Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
### which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
### Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
### answer this question.

### Subset the NEI data frame to the registers whose fips=Baltimore
baltimore<-subset(NEI,NEI$fips == "24510")

### Convert type into a factor
baltimore$type<-as.factor(baltimore$type)

### Convert year to numeric
baltimore$year <-as.numeric(baltimore$year)

### Group the registers by year and type and calculate the totals
tmp <- sapply(split(baltimore$Emissions,list(baltimore$year,baltimore$type)),sum)

### Turn the resulting vector into a data frame "cumulative" to be used with ggplot
splitnames <- strsplit(names(tmp),"\\.") 
cumulative <- data.frame(total=tmp,row.names = NULL)
type<-character(0)
year<-numeric(0)

for (i in 1:length(tmp)){
        year[i] <- as.numeric (splitnames[[i]][1])
        type[i] <- splitnames[[i]][2]
}

cumulative$type<-type
cumulative$year<-year


### Create the plot "Baltimore total emissions by source"
g<-ggplot(data = cumulative, aes(year,total))
g+geom_point(aes(color=type),size=4)+geom_smooth(method="lm", se=FALSE,linetype=3,size=1)+
facet_grid(type~.,scales="free_y")+geom_line(aes(color=type),size=1)+
labs(y="Total Emissions (tons)",title="Baltimore total emissions by source")

### Copy the plot from the screen to a png file
dev.copy(png,file="plot3.png",width = 480, height = 480, units = "px", res=72, bg="transparent")
dev.off()

