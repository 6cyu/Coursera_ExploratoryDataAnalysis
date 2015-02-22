
f=file("household_power_consumption.txt",open="r")
# read lines that match on dates
nolines <- 100
greped<-c()
repeat {
    lines=readLines(f,n=nolines) #read lines
    idx <- grep("^[12]/2/2007", lines) #find those that match
    greped<-c(greped, lines[idx]) #add the found lines
    if(nolines!=length(lines)) {
        break #are we at the end of the file?
    }
}
close(f)


# now we create a text connection and load data
tc<-textConnection(greped,"rt")
df<-read.table(tc,sep=";", col.names = colnames(read.table(
    "household_power_consumption.txt",
    nrow = 1, header = TRUE, sep=";")), na.strings = "?")
# convert Date and Time variables to Date/Time classes
df$Date <- as.Date(df$Date , "%d/%m/%Y")
df$Time <- paste(df$Date, df$Time, sep=" ")
df$Time <- strptime(df$Time, "%Y-%m-%d %H:%M:%S")

png("plot4.png", width = 480, height = 480)
par(mfrow = c(2, 2))
with(df, {
    plot(Time, Global_active_power, type = "l", xlab = "", ylab = "Global Active Power")
    plot(Time, Voltage, xlab = "datetime", type = "l", ylab = "Voltage")
    ylimits = range(c(df$Sub_metering_1, df$Sub_metering_2, df$Sub_metering_3))
    plot(Time, Sub_metering_1, xlab = "", ylab = "Energy sub metering", type = "l", ylim = ylimits, col = "black")
    par(new = TRUE)
    plot(Time, Sub_metering_2, xlab = "", axes = FALSE, ylab = "", type = "l", ylim = ylimits, col = "red")
    par(new = TRUE)
    plot(Time, Sub_metering_3, xlab = "", axes = FALSE, ylab = "", type = "l", ylim = ylimits, col = "blue")
    legend("topright",
           legend = c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"),
           bty = "n",
           lty = c(1,1,1),
           col = c("black", "red", "blue")
    )
    plot(Time, Global_reactive_power, type = "l", xlab = "datetime", ylab = "Global Reactive Power")
})
dev.off()