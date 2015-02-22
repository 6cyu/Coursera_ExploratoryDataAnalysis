## gathering data
rm (list = ls())
if (!file.exists("Source_Classification_Code.rds") | 
            !file.exists("summarySCC_PM25.rds")) {
        download.file("https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip",
                      "exdata-data-NEI_data.zip")
        unzip("exdata-data-NEI_data.zip", overwrite = T)
}

## loading data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

## subsetting NEI data for Baltimore city, fips = 24510
NEI_balt <- subset(NEI,fips == "24510")
## subsetting the Baltimore data based on the type of pollutants
NEI_balt_point <- subset(NEI_balt,type == "POINT")
NEI_balt_nonpoint <- subset(NEI_balt,type == "NONPOINT")
NEI_balt_onroad <- subset(NEI_balt,type == "ON-ROAD")
NEI_balt_nonroad <- subset(NEI_balt,type == "NON-ROAD")

## summing the emissions from all types of pollutants in the Baltimore city
sum_balt_point <- with(NEI_balt_point,tapply(Emissions,year,sum))
sum_balt_nonpoint <- with(NEI_balt_nonpoint,tapply(Emissions,year,sum))
sum_balt_onroad <- with(NEI_balt_onroad,tapply(Emissions,year,sum))
sum_balt_nonroad <-with(NEI_balt_nonroad,tapply(Emissions,year,sum))

## creating data matrix
data <- rbind(sum_balt_point,sum_balt_nonpoint,sum_balt_onroad,sum_balt_nonroad)

## plotting bar-chart
clrs <- c("red", "green", "blue", "yellow")
barplot(as.matrix(data), main="Total Emissions of PM2.5 in the Baltimore City",
        xlab = "Years", 
        ylab = "Emissions (tons)", 
        beside=TRUE, 
        cex.axis=1.1, cex.names=1.1,
        font.lab = 2,
        col=clrs)
legend("topright",
       legend = c("Point","Non-Point","On-Road","Non-Road"), 
       text.width = 3,
       cex = 0.65, 
       bty = "n",
       fill=clrs)

## creating plot2.png
dev.copy(png, file="plot2.png", height=650, width=650)
dev.off()