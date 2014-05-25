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
                         & ( fips=="24510" | fips == "06037" )
                     ) , 
                     select = c(SCC, EI.Sector, Emissions, year, fips))

# Use aggregate to sum Emissions by year and EI.Sector
res <- setNames( aggregate(subset_scc$Emissions, 
                 by = list(subset_scc$year, subset_scc$EI.Sector, subset_scc$fips), 
                 FUN = sum, na.rm=TRUE),
                c( "Years", "Sector", "County_Code", "Emissions") )


png("plot6.png", height = 450, width = 1000)

# Plot using base ggplot2 system
library(ggplot2)

g <- ggplot(res, aes(Years, Emissions))

g + geom_point( aes(color = County_Code)) + 
    facet_grid( . ~ Sector) + 
    geom_smooth(aes(color = County_Code), method = "lm") +
    labs( title = "Comparison of Emissions from motor vehicle sources between Baltimore and Los Angeles\n") +
    labs( y = "PM2.5 Emissions") +
    scale_color_discrete( name="County Code; City",
                         breaks=c("06037", "24510"),
                         labels=c("06037; LA County", "24510; Baltimore"))

dev.off()