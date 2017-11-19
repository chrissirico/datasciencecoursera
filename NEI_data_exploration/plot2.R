library(dplyr)

# Have total emissions from PM2.5 decreased in the Baltimore City, Maryland
# (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base plotting system to make a plot answering this

NEI <- readRDS("summarySCC_PM25.rds") # This has 6M rows, tonnage of pollutant by source & year
SCC <- readRDS("Source_Classification_Code.rds") # 11k rows, maps SCC codes to classification names

# We'll group by year, type and other factors a lot and then sum Emissions.
# Let's make a function to do that last part.
sum_emissions <- function(df){
  result <- summarise(df, Emissions = sum(Emissions));
  return(result)
}

# Select locale, group by year & sum
bcpm <- NEI %>% filter(fips == "24510") %>% 
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))

png(file = "plot2.png") # Open png graphics device

# graph the years 99, 02, 05, 08
barplot(bcpm$year, height = as.matrix(tidyr::spread(bcpm, year, Emissions)))
title(main = expression('Total Baltimore City PM'[2.5]*' yearly emissions (tons)'))

dev.off() # Close & save png device.

# Yes, emissions decreased in Baltimore.