

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
                                     
                                     


