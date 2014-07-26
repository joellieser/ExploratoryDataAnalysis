## open png plotting device
png("plot2.png")
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
with(ip_filt,plot(ip_filt$DtTm,ip_filt$Global_active_power, type = "l", ylab="Global Active Power (kilowatts)", xlab = "", main=""))
## close device
dev.off()
