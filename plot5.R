

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



### ____________ PLOT 5 ________________________________________________________________________
### How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?


### Subset the NEI data frame to the registers whose fips=Baltimore
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
      ylab = "tons" ,xlab="",main="Total Yearly Emissions due to 'Mob. On Road' Sources, Baltimore",pch=1)
lines(x = as.numeric(names(yearly_total_mob)), y = yearly_total_mob, col="red" )

### Copy the plot from the screen to a png file
dev.copy(png,file="plot5.png",width = 480, height = 480, units = "px", res=50, bg="transparent")
dev.off()

