library(dplyr)

# Have total emissions from PM2.5 decreased in the United States from 1999 to 2008?
# Using the base plotting system, make a plot showing the total PM2.5 emission
# from all sources for each of the years 1999, 2002, 2005, and 2008.

NEI <- readRDS("summarySCC_PM25.rds") # This has 6M rows, tonnage of pollutant by source & year
SCC <- readRDS("Source_Classification_Code.rds") # 11k rows, maps SCC codes to classification names
bigass_df <- left_join(NEI, SCC) %>% # Join the tables
  arrange(desc(Emissions)) # Sorting by emissions shows that forest fires are the largest contributors to emissions. Incorporating this finding in the first graph.

# We'll group by year, type and other factors a lot and then sum Emissions.
# Let's make a function to do that last part.
sum_emissions <- function(df){
  result <- summarise(df, Emissions = sum(Emissions));
  return(result)
}

# Add "is fire" column
bigass_df <- bigass_df %>%
  mutate(
    Is.Fire = stringr::str_detect(EI.Sector, "Fires - Wildfires")
  )

# create df summing emissions by year
uspm <- NEI %>% group_by(year) %>%
  sum_emissions

# make columns for years and rows for fire and other sources
fire <- bigass_df %>% group_by(year, Is.Fire) %>%
  sum_emissions() %>% 
  tidyr::spread(year, Emissions) %>% select(-Is.Fire)

png("plot1.png") # open png device

# Base R stacked bar plot
barplot(height = as.matrix(fire),
        col = c(1, 2),
        legend.text = c("All others", "Wildfire emissions"),
        args.legend = list(x = "topright"));
title(main = expression('Total US PM'[2.5]*' yearly emissions (tons)'))

dev.off() # close png device

# US emissions went down.