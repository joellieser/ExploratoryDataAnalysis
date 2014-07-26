## Create series of ggplots of emissions by year for United States to answer the question:
## Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

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

## keep only relevant columns: SCC, Emissions, Year
pm25 <- pm25[,c(2,4,6)]

## convert to data table
pm25 <- as.data.table(pm25)

## keep only relevant columns: SCC, EI.Sector
codes <- codes[,c(1,4)]

## convert to data table
codes <- as.data.table(codes)

## keep only coal related EI.Sectors
coal<-codes[grep("coal|Coal",codes$EI.Sector),]

# set keys 
setkey(coal,SCC)
setkey(pm25,SCC)

## Join codes and pm25
nosum <- sqldf("select a.Emissions as totalEmissions, a.year, b.EI_Sector FROM pm25 a, coal b WHERE a.SCC = b.SCC")

## Join codes and pm25, summarize with sectors
joined <- sqldf("select SUM(a.Emissions) as totalEmissions, a.year, b.EI_Sector FROM pm25 a, coal b WHERE a.SCC = b.SCC GROUP BY a.year, b.EI_Sector")

## Join codes and pm25, summarize without sectors
total <- sqldf("select SUM(a.Emissions) as totalEmissions, a.year FROM pm25 a, coal b WHERE a.SCC = b.SCC GROUP BY a.year")


## set joined columns to factors
joined$EI_Sector <- as.factor(joined$EI_Sector)
joined$factYear <- as.factor(joined$year)

## adjust margins
par(las=1, mar=c(2,7,1,1))
par(mgp = c(0, 1, 0))

## open png plotting device
png("./courseProject2/output/plot4.png", width=1200, height=960)

## create plots
p1 <- ggplot(joined, aes(joined$year, joined$totalEmissions, col=joined$EI_Sector)) +
             geom_point() +
             geom_line() + 
             scale_x_continuous(breaks = joined$year) +
             labs(x = "Year",
                  y = "Emissions (tons) by Coal Combustion Sectors",
                  title = "United States Coal Combustion Related Emissions",color="EI_Sector")

p2 <-  ggplot(joined, aes(joined$year, log10(joined$totalEmissions),fill=joined$EI_Sector)) +
              geom_bar(stat="identity") +
              scale_x_continuous(breaks = joined$year) +
              labs(x = "Year",
                   y = "log10 Emissions (tons) by Coal Combustion Sectors",
                   title = "LOG10 United States Coal Combustion Related Emissions",color="EI_Sector")

p3 <- ggplot(nosum, aes(nosum$year, nosum$totalEmissions, col=nosum$EI_Sector)) +
             geom_point() +
             geom_smooth(method=lm,  se=FALSE) +  # Linear Regression line without shaded confidence region
             labs(x = "Year",
                  y = "Raw Value Emissions (tons) by Coal Combustion Sectors",
                  title = "United States Coal Combustion Related Emissions",color="EI_Sector")

p4 <- ggplot(nosum, aes(as.factor(nosum$EI_Sector), log10(nosum$totalEmissions), fill=nosum$EI_Sector)) + 
             geom_boxplot() +
             #scale_x_continuous(breaks = nosum$year) +
             labs(x = "Sector",
                  y = "LOG10 Emissions (tons) by Coal Combustion Sectors",
                  title = "LOG10 Raw Value United States Coal Combustion Related Emissions",color="EI_Sector")

p5 <- ggplot(total, aes(total$year, total$totalEmissions)) +
             geom_point() +
             geom_line() + 
             scale_x_continuous(breaks = total$year) +
             labs(x = "Year",
                  y = "Total Emissions (tons) of Coal Combustion Total",
                  title = "Total United States Coal Combustion Related Emissions")

p6 <- ggplot(total, aes(total$year, total$totalEmissions)) +
              geom_bar(stat="identity") +
              scale_x_continuous(breaks = total$year) +
              labs(x = "Year",
                   y = "Total Emissions (tons) of Coal Combustion Sectors",
                   title = "Total United States Coal Combustion Related Emissions")


## print plots into two columns
print(multiplot(p1, p2, p3, p4, p5, p6, cols=2))

## close plotting device
dev.off()
