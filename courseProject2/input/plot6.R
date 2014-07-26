## Create series of ggpplots of emissions by year comparing LA to Baltimore to answer the question:
## Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (fips == "06037"). Which city has seen greater changes over time in motor vehicle emissions?

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
pm25 <- subset(pm25, subset = fips %in% c('24510','06037'))

pm25$city <- sapply(pm25$fips, switch, 
                  '24510' = 'Baltimore', 
                  '06037' = 'Los Angeles')

## convert to data table
pm25  <- as.data.table(pm25)

## Keep only data with type of ON-ROAD
highwayPm25<-sample(pm25[pm25$type=='ON-ROAD'])

## keep only relevant columns: SCC,  Short.Name, EI.Sector
codes <- codes[,c(1,3,4)]

## keep only off highway related data
highwayCodes<-codes[grep("Off-highway.*Recreational",codes$Short.Name),]

## convert to data table
highwayCodes <- as.data.table(highwayCodes)

# set keys 
setkey(highwayCodes,SCC)
setkey(highwayPm25,SCC)

## Join codes and pm25
full1 <- sqldf("
SELECT 
   a.fips      
   , a.Emissions  AS emissions
   , a.year       
   , a.type       AS category
   , a.city
FROM
   highwayPm25 a
   ,codes b
WHERE 
   a.SCC = b.SCC")
full2 <- sqldf("
SELECT 
   a.fips      
   , a.Emissions  AS emissions
   , a.year       
   , b.Short_Name AS category
   , a.city
FROM
   pm25 a
   ,highwayCodes b
WHERE 
   a.SCC = b.SCC")

full  <- as.data.table(rbind(full1,full2))

## create summarized data sets

summed <- sqldf("SELECT SUM(emissions) AS totalEmissions, year, city FROM full GROUP BY year, city")
summedCat  <- sqldf("SELECT SUM(emissions) AS totalEmissions, category, year, city  FROM full  GROUP BY category, year, city")

## set joined columns to factors
summed$factYear <- as.factor(summed$year)

## Split By City
summedLA   <-subset(summed,city=='Los Angeles')
summedBAL  <-subset(summed,city=='Baltimore')
summedCatLA   <-subset(summedCat,city=='Los Angeles')
summedCatBAL  <-subset(summedCat,city=='Baltimore')

minMaxLA <- subset(summedLA, subset = year %in% c(1999,2008))
minMaxBAL <- subset(summedBAL, subset = year %in% c(1999,2008))

## Generate Change Over Time
currLA <- summedLA$totalEmissions[-1]
prevLA <- summedLA$totalEmissions[1:(length(summedLA$totalEmissions)-1)]
LAChange <- 100 * round( (currLA-prevLA) / prevLA, 2 )
LAChange <- c(0,LAChange)
currBAL <- summedBAL$totalEmissions[-1]
prevBAL <- summedBAL$totalEmissions[1:(length(summedBAL$totalEmissions)-1)]
BALChange <- 100 * round( (currBAL-prevBAL) / prevBAL, 2 )
BALChange <- c(0,BALChange)

## combine change data with summaries
summedLACombined <- cbind(summedLA,LAChange)
names(summedLACombined) <- c('totalEmissions','year','city','factYear','change')
summedBALCombined <- cbind(summedBAL,BALChange)
names(summedBALCombined) <- c('totalEmissions','year','city','factYear','change')
summedCombined <-rbind(summedLACombined,summedBALCombined)

## create offsets
full$offsetYear <- - ifelse(full$city %in% 'Baltimore', full$year+.1, full$year)

## adjust margins
par(las=1, mar=c(2,7,1,1))
par(mgp = c(0, 1, 0))

## open png plotting device
png("./courseProject2/output/plot6.png", width=1200, height=960)


## create plots

p1 <- ggplot(summedCombined, aes(x=summedCombined$year, summedCombined$change, col=summedCombined$city)) +  
             geom_point() +
             geom_line() + 
             geom_line(y=0,col="black",size=1) +
             scale_x_continuous(breaks = summedCombined$year) +
             labs(x = "Year",
                  y = "Percentage Change in Emissions\n (tons) by Motor Vehicle Source",
                  title = "LA vs BAL Motor Vehicle Source Emissions\n Percentage Change From Previous Year",color="city")

p2 <- ggplot(full, aes(full$year, full$emissions, col=full$city)) +
             geom_point(aes(col=factor(full$city)), size = 2, show_guide=TRUE,alpha = 2/3,position=position_jitter(width=.75,height=.2)) +
             scale_x_continuous(breaks = full$year) +
             labs(x = "Year",
                  y = "Emissions (tons) by Motor Vehicle Source",
                  title = "Baltimore and Los Angeles Motor Vehicle Source Emissions",color="City")

p3 <- ggplot(summed, aes(summed$year, summed$totalEmissions,col=summed$city)) +
             geom_point() +
             geom_line() +
             geom_smooth(method=lm,  se=FALSE) +  # Linear Regression line without shaded confidence region
             labs(x = "Year",
                  y = "Raw Value Emissions (tons) by Motor Vehicle Source all categories",
                  title = "Baltimore Motor Vehicle Source Emissions",color="city")

p4 <- ggplot(summedCat, aes(summedCat$year, y=log10(summedCat$totalEmissions),col=paste(summedCat$city,summedCat$category,sep="\n"))) +
             geom_line() +
             scale_x_continuous(breaks = summedCat$year) +
             labs(x = "Year",
                  y = "LOG10 Emissions (tons) by Motor \n Vehicle Source category",
                  title = "LOG10 Raw Value Baltimore + Los Angeles\nMotor Vehicle Source Emissions",color="City and Category") 

p5 <- ggplot(summedCatBAL, aes(as.factor(summedCatBAL$category), y=log2(summedCatBAL$totalEmissions), fill=summedCatBAL$category)) + 
             geom_boxplot() +
             theme(axis.ticks = element_blank(), axis.text.x = element_blank()) +
             labs(x = "Category",
                  y = "LOG2 Emissions (tons) by Motor Vehicle Source category",
                  title = "LOG2 Raw Value Baltimore\nMotor Vehicle Source Emissions") 

p6 <- ggplot(summed, aes(summed$year, summed$totalEmissions,col=summed$city,fill=summed$city)) +
             geom_bar(stat="identity") +
             geom_smooth(method=lm,  se=FALSE,color="black") +
             scale_x_continuous(breaks = summed$year) +
             labs(x = "Year",
                  y = "Total Emissions (tons) of Motor Vehicle Source Total",
                  title = "Total Baltimore vs Los Angeles\n Motor Vehicle Source Emissions")

## print plots into two columns
print(multiplot(p1, p2, p3, p4, p5, p6, cols=2))

## close plotting device
dev.off()
