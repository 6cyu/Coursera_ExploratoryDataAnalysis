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

## subsetting NEI data based on the type of pollutants
NEI_point <- subset(NEI,type == "POINT")
NEI_nonpoint <- subset(NEI,type == "NONPOINT")
NEI_onroad <- subset(NEI,type == "ON-ROAD")
NEI_nonroad <- subset(NEI,type == "NON-ROAD")

## summing the emissions from all types of pollutants
sum_point <- with(NEI_point,tapply(Emissions,year,sum))
sum_nonpoint <- with(NEI_nonpoint,tapply(Emissions,year,sum))
sum_onroad <- with(NEI_onroad,tapply(Emissions,year,sum))
sum_nonroad <-with(NEI_nonroad,tapply(Emissions,year,sum))

## creating data matrix
data <- rbind(sum_point,sum_nonpoint,sum_onroad,sum_nonroad)

## plotting bar-chart
clrs <- c("red", "green", "blue", "yellow")
barplot(as.matrix(data), main="Total Emissions of PM2.5", xlab = "Years", 
        ylab = "Emissions (tons)", 
        beside=TRUE, 
        cex.axis=1.1, cex.names=1.1,
        font.lab = 2,
        col=clrs)
legend("topright",
       legend = c("Point","Non-Point","On-Road","Non-Road"), 
       text.width = 3,
       cex = 0.55, 
       bty = "n",
       fill=clrs)

## creating plot1.png
dev.copy(png, file="plot1.png", height=650, width=650)
dev.off()