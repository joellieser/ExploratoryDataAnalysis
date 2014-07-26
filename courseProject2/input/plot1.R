## Create barplot of total emissions by year for United States to answer the question:
## Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

## Load required libraries
library(sqldf)
library(data.table)

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

## Read pm25 input data
pm25 <- readRDS("./courseProject2/input/summarySCC_PM25.rds")

## Keep only relevant columns: Emissions, year
pm25 <- pm25[,c(4,6)]

## Convert pm25 to data table
pm25 <- as.data.table(pm25)

## Set key to year for grouping
setkey(pm25,year)

## Sum(Emissions) by year
byYear <- sqldf("SELECT SUM(Emissions) as totalEmissions, year FROM pm25 GROUP BY year")

## Open png plotting device
png("./courseProject2/output/plot1.png")

## Set opptions and margins
opt <- options("scipen" = 20)
par(las=1, mar=c(4,7,4,0))
par(mgp = c(0, 1, 0))

## Create Plot
barplot(byYear$totalEmissions,names.arg=byYear$year,main="Total US Emissions by Year")
par(las=0)
mtext("Total Emissions (tons)", side=2, line=5)
mtext("Year", side=1, line=2)

## Close plotting Device
dev.off()
