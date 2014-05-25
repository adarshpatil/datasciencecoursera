# Setting workng directory
setwd("/home/adarsh/Rclass/datasciencecoursera/exploratory-data-analysis/course-project2")

# Reading input
nei <- readRDS("summarySCC_PM25.rds")
scc <- readRDS("Source_Classification_Code.rds")

# Merge NEI and SCC dataframes by SCC
m_nei <- merge( nei, scc, by = "SCC", all.x = TRUE)

# Subset SCC of coal combustion-related sources 
subset_scc <- subset(m_nei,
                     subset = ( 
                         ( EI.Sector == "Mobile - On-Road Diesel Heavy Duty Vehicles" | 
                             EI.Sector == "Mobile - On-Road Diesel Light Duty Vehicles" |
                             EI.Sector == "Mobile - On-Road Gasoline Heavy Duty Vehicles" |
                             EI.Sector == "Mobile - On-Road Gasoline Light Duty Vehicles" )
                         & fips=="24510"
                     ) , 
                     select = c(SCC, EI.Sector, Emissions, year))

# Use aggregate to sum Emissions by year and EI.Sector
res <- aggregate(subset_scc$Emissions, 
                 by = list(subset_scc$year, subset_scc$EI.Sector), 
                 FUN = sum, na.rm=TRUE)


png("plot5.png", height = 450, width = 1000)

# Plot using base ggplot2 system
library(ggplot2)

qplot(Group.1, x, facets = . ~ Group.2, geom = c("point", "smooth"),
      method = "lm", data = res, xlim = c(1998, 2008), 
      xlab = "Years", ylab = "PM2.5 Emissions") +
    ggtitle("Emissions from motor vehicle sources in Baltimore City\n")

dev.off()