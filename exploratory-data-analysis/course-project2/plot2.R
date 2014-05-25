# Setting workng directory
setwd("/home/adarsh/Rclass/datasciencecoursera/exploratory-data-analysis/course-project2")

# Reading input
nei <- readRDS("summarySCC_PM25.rds")

# Subset baltimore Emissions
nei_baltimore <- subset(nei, fips=="24510")

# Use aggregate to group by year and sum Emissions by year
res <- aggregate(nei_baltimore$Emissions, by = list(nei_baltimore$year), 
                 FUN = sum, na.rm = TRUE)

# Fit linear model to see trend
linear_model <- lm(res$x ~ res$Group.1)

png("plot2.png", height = 480, width = 480)

# Plot using base plotting system
plot(res$Group.1, res$x, xlab = "Years", ylab = "PM2.5 Emissions", pch = 2, 
     xlim = c(1998,2008))
lines(res$Group.1, res$x)

abline(linear_model, lty = "dashed", col = "red")
legend("topright", legend = c("linear model to fit data", "data progression"), 
       lty = c(2,1), col = c("red", "black"))

title("PM2.5 Emission Levels in Baltimore City over years")

dev.off()