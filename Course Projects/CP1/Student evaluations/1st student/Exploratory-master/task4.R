household_power_consumption <- read.csv("C:/DB/household_power_consumption.txt", sep=";")

# We will only be using data from the dates 2007-02-01 and 2007-02-02.

subs01 <- 
  household_power_consumption[as.Date(household_power_consumption$Date,"%d/%m/%Y") == "2007-02-01",]

subs02 <- 
  household_power_consumption[as.Date(household_power_consumption$Date,"%d/%m/%Y") == "2007-02-02", ]

subs <- rbind(subs01, subs02)
rm(household_power_consumption, subs01, subs02)

subs$Date <- as.Date(subs$Date, format="%d/%m/%Y")
subs$Global_active_power <- as.numeric(as.character(subs$Global_active_power))


par(mfrow = c(2,2), mar = c(4,4,2,1))

with(subs,plot(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S"),
               Global_active_power,type='l', xlab="",ylab = 'Global Active Power (kilowatts)'))

with(subs,plot(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S"),
               Voltage,type='l', xlab="datetime",ylab = 'Voltage'))

with(subs,plot(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S"), Sub_metering_1,col='black',
               type='l', xlab="",ylab = 'Energy sub metering'))
with(subs,lines(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S"),Sub_metering_2,col='red'))
with(subs,lines(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S"),Sub_metering_3,col='blue'))
legend("topright",legend = c('Sub_metering_1','Sub_metering_2','Sub_metering_3'),col=c('black','red','blue'),
       lwd = 3, text.font = 1)


with(subs,plot(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S"),
               Global_reactive_power,type='l', xlab="datetime",ylab = 'Global_reactive_power'))


setwd("C:/DB/")

dev.copy(png, file= "task4.png",width = 480, height = 480, units = "px", pointsize = 12  )
dev.off()



