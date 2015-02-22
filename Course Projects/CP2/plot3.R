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
year <- c("1999", "2002", "2005", "2008")
emType <- c(rep("POINT",4), rep("NON-POINT",4), rep("ONROAD",4), rep("NON-ROAD",4))
emVal <- c(sum_balt_point,sum_balt_nonpoint,sum_balt_onroad,sum_balt_nonroad)
data_coal <- data.frame(cbind(sum_balt_point,sum_balt_nonpoint,sum_balt_onroad,sum_balt_nonroad))
data_coal <- data.frame(emVal,year,emType)
rownames(data_coal) <- NULL

## plotting ggplot2
library(ggplot2)
g <- ggplot(data_coal,aes(x = year, y = emVal))
p <- g + geom_bar(stat="identity") + facet_grid(.~emType) +
        labs(title="Total Emissions of PM2.5 in the Baltimore City")+ 
        labs(x="Year", y="Emissions (tons)") + 
        theme(text = element_text(size = 20),
              axis.text.x = element_text(colour="gray20", angle=90, face="plain"),
              axis.text.y = element_text(colour="gray20", face="plain"))
print(p)

## creating plot3.png
dev.copy(png, file="plot3.png", height=650, width=1000)
dev.off()

