household_power_consumption <- read.csv("C:/DB/household_power_consumption.txt", sep=";")
Sys.setlocale("LC_TIME", "English")

# We will only be using data from the dates 2007-02-01 and 2007-02-02.

subs01 <- 
  household_power_consumption[as.Date(household_power_consumption$Date,"%d/%m/%Y") == "2007-02-01",]

subs02 <- 
  household_power_consumption[as.Date(household_power_consumption$Date,"%d/%m/%Y") == "2007-02-02", ]

subs <- rbind(subs01, subs02)
rm(household_power_consumption, subs01, subs02)

hist(as.numeric(subs$Global_active_power)/500,
     breaks=seq(0,12,by=0.5), col = "red", axes=FALSE, 
     ylab="Frequency", xlab = "Global Active Power (kilowatts)", 
     main = "Global Active Power" )

axis(side=1, at=seq(0, 6, by=2))
axis(side=2, at=seq(0, 1200, by=200))
setwd("C:/DB/")

dev.copy(png, file= "task1.png",width = 480, height = 480, units = "px", pointsize = 12  )
dev.off()

