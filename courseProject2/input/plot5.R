## Create series of ggpplots of emissions by year for United States to answer the question:
## How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

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
library(ggplot2)

## read pm25 data and source classification codes
pm25 <- readRDS("./courseProject2/input/summarySCC_PM25.rds")
codes <- readRDS("./courseProject2/input/Source_Classification_Code.rds")

## create multiplot function
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

## keep only relevant columns: fips, SCC, Emissions, Type, Year
pm25 <- pm25[,c(1,2,4,5,6)]

## convert to data table
pm25 <- as.data.table(pm25)

## filter out Baltimore only data (fips = 24510)
pm25<-sample(pm25[pm25$fips==24510])

## Keep only data with type of ON-ROAD
highwayPm25<-sample(pm25[pm25$type=='ON-ROAD'])

## keep only relevant columns: SCC,  Short.Name, EI.Sector
codes <- codes[,c(1,3,4)]

## keep only off highway related data
highwayCodes<-codes[grep("Off-highway.*Recreational",codes$Short.Name),]

## convert to data table
codes <- as.data.table(codes)

# set keys 
setkey(as.data.table(highwayCodes),SCC)
setkey(highwayPm25,SCC)

## Join codes and pm25
full <- sqldf("
SELECT
   t.fips
   , t.emissions
   , t.year
   , t.category
FROM
(SELECT 
   a.fips      
   , a.Emissions  AS emissions
   , a.year       
   , a.type       AS category
FROM
   highwayPm25 a
   ,codes b
WHERE 
   a.SCC = b.SCC
UNION
SELECT 
   a.fips      
   , a.Emissions  AS emissions
   , a.year       
   , b.Short_Name AS category
FROM
   pm25 a
   ,highwayCodes b
WHERE 
   a.SCC = b.SCC
) t   
")

## create summarized data sets
summed <- sqldf("SELECT SUM(emissions) AS totalEmissions, year FROM full GROUP BY year")
summedCat <- sqldf("SELECT SUM(emissions) AS totalEmissions, category, year FROM full GROUP BY year, category")

## set joined columns to factors
summed$factYear <- as.factor(summed$year)
full$factYear <- as.factor(full$year)

## remove outlier from full
fullNoOutliers<-sqldf("SELECT * FROM full WHERE emissions < 30")

## adjust margins
par(las=1, mar=c(2,7,1,1))
par(mgp = c(0, 1, 0))

## open png plotting device
png("./courseProject2/output/plot5.png", width=1200, height=960)

## create plots
p1 <- ggplot(summedCat, aes(summedCat$year, summedCat$totalEmissions, col=summedCat$category)) +
             geom_point() +
             geom_line() + 
             scale_x_continuous(breaks = summedCat$year) +
             labs(x = "Year",
                  y = "Emissions (tons) by Motor Vehicle Source",
                  title = "Baltimore Motor Vehicle Source Emissions",color="Category")

p3 <- ggplot(summed, aes(summed$year, summed$totalEmissions)) +
             geom_point() +
             geom_smooth(method=lm,  se=FALSE) +  # Linear Regression line without shaded confidence region
             labs(x = "Year",
                  y = "Raw Value Emissions (tons) by Motor Vehicle Source category",
                  title = "Baltimore Motor Vehicle Source Emissions",color="EI_Sector")

p4 <- ggplot(summedCat, aes(as.factor(summedCat$category), log2(summedCat$totalEmissions), fill=summedCat$category)) + 
             geom_boxplot() +
             labs(x = "Sector",
                  y = "LOG2 Emissions (tons) by Motor Vehicle Source category",
                  title = "LOG2 Raw Value Baltimore Motor Vehicle Source Emissions",color="category")

p5 <- ggplot(full, aes(full$year, full$emissions)) +
             geom_point() +
             geom_smooth(method=lm,  se=FALSE) +  # Linear Regression line without shaded confidence region
             scale_x_continuous(breaks = full$year) +
             labs(x = "Year",
                  y = "Total Emissions (tons) of Motor Vehicle Source Total",
                  title = "Total Baltimore Motor Vehicle Source Emissions")

p6 <- ggplot(full, aes(full$year, full$emissions)) +
              geom_bar(stat="identity") +
              scale_x_continuous(breaks = full$year) +
              labs(x = "Year",
                   y = "Total Emissions (tons) of Motor Vehicle Source category",
                   title = "Total Baltimore Motor Vehicle Source Emissions")

p2 <- ggplot(fullNoOutliers, aes(fullNoOutliers$year, fullNoOutliers$emissions)) +
             geom_point() +
             geom_smooth(method=lm,  se=FALSE) +  # Linear Regression line without shaded confidence region
             scale_x_continuous(breaks = fullNoOutliers$year) +
             labs(x = "Year",
                  y = "Total Emissions (tons) of Motor Vehicle Source Total",
                  title = "Total Baltimore Motor Vehicle Source Emissions Minus Outliers")

## print plots into two columns
print(multiplot(p1, p2, p3, p4, p5, p6, cols=2))

## close plotting device
dev.off()
