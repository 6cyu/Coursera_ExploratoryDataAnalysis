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
# 1. Mobile - On-Road Gasoline Light Duty Vehicles (RGLV)
# 2. Mobile - On-Road Gasoline Heavy Duty Vehicles (RGHV)
# 3. Mobile - On-Road Diesel Light Duty Vehicles (RDLV)
# 4. Mobile - On-Road Diesel Heavy Duty Vehicles (RDHV)
# 5. Mobile - Non-Road Equipment - Gasoline (NG) 
# 6. Mobile - Non-Road Equipment - Other (NO)
# 7. Mobile - Non-Road Equipment - Diesel (ND)           
# 8. Mobile - Aircraft (Air)          
# 9. Mobile - Commercial Marine Vessels (Mar)                
# 10. Mobile - Locomotives (Loc)

SCC_RGLV <- subset(SCC, EI.Sector=="Mobile - On-Road Gasoline Light Duty Vehicles")
SCC_RGHV <- subset(SCC, EI.Sector=="Mobile - On-Road Gasoline Heavy Duty Vehicles")
SCC_RDLV <- subset(SCC, EI.Sector=="Mobile - On-Road Diesel Light Duty Vehicles")
SCC_RDHV <- subset(SCC, EI.Sector=="Mobile - On-Road Diesel Heavy Duty Vehicles")
SCC_NG <- subset(SCC, EI.Sector=="Mobile - Non-Road Equipment - Gasoline")
SCC_NO <- subset(SCC, EI.Sector=="Mobile - Non-Road Equipment - Other")
SCC_ND <- subset(SCC, EI.Sector=="Mobile - Non-Road Equipment - Diesel")
SCC_Air <- subset(SCC, EI.Sector=="Mobile - Aircraft")
SCC_Mar <- subset(SCC, EI.Sector=="Mobile - Commercial Marine Vessels")
SCC_Loc <- subset(SCC, EI.Sector=="Mobile - Locomotives")


## joining all SCC_ to get the unique SCC of vehicles.
## Luckily, the SCC in those datasets are unique
## so they can be directly joined by using rbind
SCC_veh <- rbind(SCC_RGLV,SCC_RGHV,SCC_RDLV,SCC_RDHV,SCC_NG,
                 SCC_NO,SCC_ND,SCC_Air,SCC_Mar,SCC_Loc)

## subsetting the NEI Baltimore data for coal-related sources across the US
# subsetting the NEI data for Baltimore city, fips = 24510
NEI_BaltVeh <- subset(NEI,SCC %in% SCC_veh$SCC & fips=="24510")

## plotting ggplot2
library(ggplot2)
g <- ggplot(NEI_BaltVeh,aes(x = year, y = Emissions))
p <- g + geom_point(aes(color=type), size = 3)+
         labs(title="Emissions from Motor Vehicle Sources in the Baltimore City")+
         labs(x="Year", y="Emissions (tons)") +
         geom_smooth(method ="lm", se=FALSE, col="steelblue", size=1) +
         theme_bw(base_family = "Times", base_size = 20) +
         theme(text = element_text(size = 20),
              axis.text.x = element_text(colour="gray20"),
              axis.text.y = element_text(colour="gray20"))
print(p)

## creating plot5.png
dev.copy(png, file="plot5.png", width = 800)
dev.off()