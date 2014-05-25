# Setting workng directory
setwd("/home/adarsh/Rclass/datasciencecoursera/exploratory-data-analysis/course-project2")

# Reading input
nei <- readRDS("summarySCC_PM25.rds")

# Subset baltimore Emissions
nei_baltimore <- subset(nei, fips=="24510")

# Use aggregate to group by year and sum Emissions by year
res <- aggregate(nei_baltimore$Emissions, 
                 by = list(nei_baltimore$year, nei_baltimore$type), 
                 FUN = sum, na.rm = TRUE)

png("plot3.png", height = 450, width = 800)

# Plot using base ggplot2 system
library(ggplot2)

qplot(Group.1, x, facets = . ~ Group.2, geom = c("point", "smooth"),
      method = "lm", data = res, xlim = c(1998, 2008), 
      xlab = "Years", ylab = "PM2.5 Emissions") +
    ggtitle("Variation of types of pollutants over years\n")

dev.off()