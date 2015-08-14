

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

### ____________ PLOT 1 ________________________________________________________________________
### Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
### Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
### for each of the years 1999, 2002, 2005, and 2008.

### Group the registers by year and sum to calculate yearly totals
yearly_total<-sapply(split(NEI$Emissions,NEI$year),sum)

### Plot the graph "Total early Emissions"
plot (x = as.numeric(names(yearly_total)), y = yearly_total/10^6,
      ylab = "million tons" ,xlab="",main="Total Yearly Emissions",pch=20)
lines(x = as.numeric(names(yearly_total)), y = yearly_total/10^6, col="red" )

### Copy the plot from the screen to a png file
dev.copy(png,file="plot1.png",width = 480, height = 480, units = "px", res=72, bg="transparent")
dev.off()

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

### ____________ PLOT 3 ________________________________________________________________________
### Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, 
### which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
### Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make a plot 
### answer this question.

baltimore<-subset(NEI,NEI$fips == "24510")

baltimore$type<-as.factor(baltimore$type)
#baltimore$year <- year(strptime(paste(baltimore$year,"0101"),"%Y%m%d"))
baltimore$year <-as.numeric(baltimore$year)

tmp <- sapply(split(baltimore$Emissions,list(baltimore$year,baltimore$type)),sum)
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



g<-ggplot(data = cumulative, aes(year,total))
g+geom_point(aes(color=type),size=4)+geom_smooth(method="lm", se=FALSE,linetype=3,size=1)+
facet_grid(type~.,scales="free_y")+geom_line(aes(color=type),size=1)+
labs(y="Total Emissions (tons)",title="Baltimore total emissions by source")

### Copy the plot from the screen to a png file
dev.copy(png,file="plot3.png",width = 480, height = 480, units = "px", res=72, bg="transparent")
dev.off()



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

### ____________ PLOT 5 ________________________________________________________________________
### How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?

baltimore<-subset(NEI,NEI$fips == "24510")


### Select [1] Mobile - On-Road Gasoline Light Duty Vehicles [2] Mobile - On-Road Gasoline Heavy Duty Vehicles
###        [3] Mobile - On-Road Diesel Light Duty Vehicles   [3] Mobile - On-Road Diesel Heavy Duty Vehicles
### from all EI.Sector unique values to subset SCC
motor_vehicles<-as.character(unique(SCC$EI.Sector)[21:24])

### Subset SSC codes that belong to 'Mobile - On road' cathegories
motor_veh_SCC <- as.character(subset(SCC$SCC, (SCC$EI.Sector %in% motor_vehicles)))

### Find registers in Baltimore NEI with above calculated SCCs                                     
bal_motor_veh <- baltimore[baltimore$SCC %in% motor_veh_SCC,]

### Calculate yearly totals
yearly_total_mob<-sapply(split(bal_motor_veh$Emissions,bal_motor_veh$year),sum)

### Plot the graph
plot (x = as.numeric(names(yearly_total_mob)), y = yearly_total_mob,
      ylab = "tons" ,xlab="",main="Total Yearly Emissions due to 'Mob. On Road' Sources, Baltimore",pch=20,)
lines(x = as.numeric(names(yearly_total_mob)), y = yearly_total_mob, col="red" )

### Copy the plot from the screen to a png file
dev.copy(png,file="plot5.png",width = 480, height = 480, units = "px", res=72, bg="transparent")
dev.off()


### ____________ PLOT 6 ________________________________________________________________________
### Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources 
### in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in 
### motor vehicle emissions?

bal_cal<-subset(NEI,NEI$fips == "24510" | NEI$fips =="06037" )


### Select [1] Mobile - On-Road Gasoline Light Duty Vehicles [2] Mobile - On-Road Gasoline Heavy Duty Vehicles
###        [3] Mobile - On-Road Diesel Light Duty Vehicles   [3] Mobile - On-Road Diesel Heavy Duty Vehicles
### from all EI.Sector unique values to subset SCC
motor_vehicles<-as.character(unique(SCC$EI.Sector)[21:24])

### Subset SSC codes that belong to 'Mobile - On road' cathegories
motor_veh_SCC <- as.character(subset(SCC$SCC, (SCC$EI.Sector %in% motor_vehicles)))
                                     
### Find registers in Baltimore & California NEI with above calculated SCCs                                     
bal_cal_motor_veh <- bal_cal [bal_cal$SCC %in% motor_veh_SCC,]
                                     
### Calculate yearly totals
yearly_total_bal_cal<- aggregate(Emissions~year+fips,bal_cal_motor_veh,sum)
   
### Plot the graph
g<-ggplot(data = yearly_total_bal_cal, aes(year,Emissions))
g+geom_point(aes(color=fips),size=4)+geom_smooth(method="lm", se=FALSE,linetype=3,size=1)+
facet_grid(fips~.,scales="free_y")+geom_line(aes(color=fips),size=1)+
labs(y="Total Emissions (tons)",title="California/Baltimore total motor vehicle emissions")
             
### Copy the plot from the screen to a png file
dev.copy(png,file="plot6.png",width = 480, height = 480, units = "px", res=72, bg="transparent")
dev.off()
                                     
                                     


