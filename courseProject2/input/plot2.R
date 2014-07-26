## Create barplot of total emissions by year for Baltimore to asnwer the question:
## Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? Use the base plotting system to make a plot answering this question.

## Download and unzip the assignment data
if(!file.exists("./courseProject2")) {dir.create("./courseProject2")}
if(!file.exists("./courseProject2/input")) {dir.create("./courseProject2/input")}
if(!file.exists("./courseProject2/output")) {dir.create("./courseProject2/output")}
# COMMENT : download and untar the files 
if(!file.exists("./courseProject2/input/download.zip")){
  url <- 'https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip'
  download.file(url, destfile="./courseProject2/input/download.zip",method="curl")
  untar("./courseProject2/input/download.zip", compressed = 'gzip', exdir = "./courseProject2/input")
}

## load required libraries
library(sqldf)
library(data.table)

## read pm25 data
pm25 <- readRDS("./courseProject2/input/summarySCC_PM25.rds")

## keep only relevant columns: SCC, Emissions, fips
pm25 <- pm25[,c(1,4,6)]

##convert fips to character
pm25$fips <- as.character(pm25$fips)

## convert pm25 to data table
pm25 <- as.data.table(pm25)

## set pm25 key for grouping
setkey(pm25,year)

## sum(Emissions) by year for Baltimore
byYear <- sqldf("SELECT SUM(Emissions) as totalEmissions, year FROM pm25 WHERE fips = '24510' GROUP BY year")

## open png plotting device
png("./courseProject2/output/plot2.png")

## set options and margins for plotting
opt <- options("scipen" = 20)
par(las=1, mar=c(4,7,4,0))
par(mgp = c(0, 1, 0))

## create plot
barplot(byYear$totalEmissions,names.arg=byYear$year,main="Total Baltimore (fips 24510) Emissions by Year")
par(las=0)
mtext("Total Emissions (tons)", side=2, line=5)
mtext("Year", side=1, line=2)

## close png plotting device
dev.off()
