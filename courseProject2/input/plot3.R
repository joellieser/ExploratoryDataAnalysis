## Create line graph with ggplot of total emissions by year and type for Baltimore to answer the question:
## Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.

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

## Load required libraries
library(sqldf)
library(ggplot2)
library(data.table)

## Read pm25 data
pm25 <- readRDS("./courseProject2/input/summarySCC_PM25.rds")

## Keep only necessary columns: SCC, Emissions, year, type
pm25 <- pm25[,c(1,4,5,6)]

## Convert fips to character
pm25$fips <- as.character(pm25$fips)

## Convert pm25 to data table
pm25 <- as.data.table(pm25)

## keep only Baltimore rows
pm25 <- subset(pm25, fips == "24510")

## Sum(Emissions) by year and type
byYear <- sqldf("SELECT SUM(Emissions) as totalEmissions, year, type FROM pm25 GROUP BY year, type")

## Set type to factor
byYear$type <- as.factor(byYear$type)

## Set year to factor
byYear$factYear <- as.factor(byYear$year)

## open png plotting device
png("./courseProject2/output/plot3.png")

## Create plot
print(ggplot(byYear, aes(byYear$year, byYear$totalEmissions,col=byYear$type)) +
  geom_point() +
  geom_line() + 
  scale_x_continuous(breaks = byYear$year) +
  labs(x = "Year",
       y = "Emissions (tons) by Type",
       title = "Baltimore (fips 24510) Emissions by Type",color="Types"))

## Close plotting device
dev.off()
