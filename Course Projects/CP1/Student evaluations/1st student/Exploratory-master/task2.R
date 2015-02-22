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


with(subs,plot(strptime(paste(Date,Time),"%Y-%m-%d %H:%M:%S"),Global_active_power,type='l',xlab="",ylab = 'Global Active Power (kilowatts)'))

axis(side=1, at=seq(0, 6, by=2))
axis(side=2, at=seq(0, 1200, by=200))
setwd("C:/DB/")

dev.copy(png, file= "task2.png",width = 480, height = 480, units = "px", pointsize = 12  )
dev.off()


