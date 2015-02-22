
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

png("plot1.png", width = 480, height = 480)
hist(df$Global_active_power, main = "Global Active power", col = "red", xlab = "Global Active Power (kilowatts)", )

dev.off()