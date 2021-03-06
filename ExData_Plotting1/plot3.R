## open png plotting device
png("plot3.png")
## read input data
ip <- read.table("household_power_consumption.txt",sep=";",header=TRUE,na.strings="?")
## filter out only required data
ip_filt <- subset(ip,Date == '1/2/2007' | Date == '2/2/2007')
## free up memory used by ip
ip <- NULL
## concat date and time columns
ip_filt$DtTm <- paste(ip_filt$Date,ip_filt$Time,sep=" ")
## cast as datetime type
ip_filt$DtTm <- strptime(ip_filt$DtTm, format="%d/%m/%Y %H:%M:%S")
## create plot
plot(ip_filt$DtTm,ip_filt$Sub_metering_1, type = "l",col="black", ylab="Energy sub metering", xlab = "", main="")
points(ip_filt$DtTm,ip_filt$Sub_metering_2, type = "l",col="red")
points(ip_filt$DtTm,ip_filt$Sub_metering_3, type = "l",col="blue")
legend("topright",c("Sub_metering_1","Sub_metering_2","Sub_metering_3"),lty = c(1, 1, 1),col=c("black","red","blue"))
## close device
dev.off()
