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

## subsetting the SCC data for coal combustion-related sources.
# The data is filtered according to SCC$EI.Sector and
# the sectors that use coal become the data subset
# Those sectors are:
# 1. Fuel Comb - Electric Generation - Coal (FCEG)
# 2. Fuel Comb - Industrial Boilers, ICEs - Coal (FCIB)
# 3. Fuel Comb - Comm/Institutional - Coal (FCCI)
SCC_FCEG <- subset(SCC, EI.Sector=="Fuel Comb - Electric Generation - Coal")
SCC_FCIB <- subset(SCC, EI.Sector=="Fuel Comb - Industrial Boilers, ICEs - Coal")
SCC_FCCI <- subset(SCC, EI.Sector=="Fuel Comb - Comm/Institutional - Coal")

## joining SCC_FCEG, SCC_FCIB, and SCC_FCCI to get the unique SCC of coal
## combustion-related sources. Luckily, the SCC in those 3 datasets are unique
## so they can be directly joined by using rbind
SCC_coal <- rbind(SCC_FCEG,SCC_FCIB,SCC_FCCI)

## subsetting the NEI data for coal-related sources across the US
NEI_coal <- subset(NEI,SCC %in% SCC_coal$SCC)

## subsetting the NEI_coal data based on the type of pollutants
NEI_coal_point <- subset(NEI_coal,type == "POINT")
NEI_coal_nonpoint <- subset(NEI_coal,type == "NONPOINT")
NEI_coal_onroad <- subset(NEI_coal,type == "ON-ROAD")
NEI_coal_nonroad <- subset(NEI_coal,type == "NON-ROAD")


## summing the emissions from all types of pollutants in the Baltimore city
sum_coal_point <- with(NEI_coal_point,tapply(Emissions,year,sum))
sum_coal_nonpoint <- with(NEI_coal_nonpoint,tapply(Emissions,year,sum))
sum_coal_onroad <- with(NEI_coal_onroad,tapply(Emissions,year,sum))
sum_coal_nonroad <-with(NEI_coal_nonroad,tapply(Emissions,year,sum))
# total pollutants' emissions
sum_coal <-with(NEI_coal,tapply(Emissions,year,sum))

## creating data matrix
year <- c("1999", "2002", "2005", "2008")
emType <- c(rep("POINT",4), rep("NON-POINT",4),rep("TOTAL",4))
emVal <- c(sum_coal_point,sum_coal_nonpoint,sum_coal)
data_coal <- data.frame(cbind(sum_coal_point,sum_coal_nonpoint,sum_coal))
data_coal <- data.frame(emVal,year,emType)
rownames(data_coal) <- NULL

## plotting ggplot2
library(ggplot2)
g <- ggplot(data_coal,aes(x = year, y = emVal))
p <- g + geom_bar(stat="identity") + facet_grid(.~emType) +
        labs(title="Total Emissions of Coal Combustion-Related Sources in US")+ 
        labs(x="Year", y="Emissions (tons)") + 
        theme(text = element_text(size = 20),
              axis.text.x = element_text(colour="gray20", angle=90, face="plain"),
              axis.text.y = element_text(colour="gray20", face="plain"))
print(p)

## creating plot4.png
dev.copy(png, file="plot4.png", height=650, width=1000)
dev.off()