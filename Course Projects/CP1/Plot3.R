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
x = elDataFeb$Date
sub1 = elDataFeb$Sub_metering_1
sub2 = elDataFeb$Sub_metering_2
sub3 = elDataFeb$Sub_metering_3

png(file="Plot3.png")
plot(x,sub1,xlab = "",ylab = "Energy sub metering",
     type = "l")
lines(x,sub2,col = "red")
lines(x,sub3,col = "blue")
legend("topright",legend = c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),
       lty = c(1,1), lwd = c(2.5,2.5), col = c("black","red","blue"))
dev.off()