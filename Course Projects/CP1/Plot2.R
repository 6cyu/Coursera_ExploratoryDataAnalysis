# loading data
elData <- read.table("household_power_consumption.txt",sep = ";",header = T,na.strings = "?")

# removing NA values
good <- complete.cases(elData)
elDataGood <- elData[good,]

# getting data on 1/2/2007 and 2/2/2007
elDataFeb <- subset(elDataGood,Date==c("1/2/2007","2/2/2007"))

# converting date
elDataFeb$Date <- strptime(paste(elDataFeb$Date,elDataFeb$Time), "%d/%m/%Y %H:%M:%S")

# plotting and exporting Global Active Power in days
png(file="Plot2.png")
plot(elDataFeb$Date,elDataFeb$Global_active_power,xlab = "",ylab = "Global Active Power (kilowatts)",
     type = "l")
dev.off()
