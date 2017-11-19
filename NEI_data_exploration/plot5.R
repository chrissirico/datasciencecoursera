library(dplyr)
library(ggplot2)

# How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?

NEI <- readRDS("summarySCC_PM25.rds") # This has 6M rows, tonnage of pollutant by source & year
SCC <- readRDS("Source_Classification_Code.rds") # 11k rows, maps SCC codes to classification names

# We'll group by year, type and other factors a lot and then sum Emissions.
# Let's make a function to do that last part.
sum_emissions <- function(df){
  result <- summarise(df, Emissions = sum(Emissions));
  return(result)
}

bc_cars <- NEI %>% filter(fips == "24510" & type == "ON-ROAD") %>%
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))

png("plot5.png") # open png device

# And plot findings
ggplot(bc_cars, aes(year, Emissions)) +
  geom_col() +
  labs(
    title = expression('Baltimore vehicular PM'[2.5]*' emissions decreased dramatically \'99 - \'02'),
    subtitle = "Reductions may be driven by vehicular emissions improvements in response to Clean Air Act",
    x = "Year",
    y = "Vehicular emissions (tons)"
  ) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) +
  # scale_x_discrete(limits = rev(levels(all_coal_sum$year))) +
  # scale_x_date(date_labels = "%y") +
  geom_text(aes(label=plyr::round_any(Emissions, 1), vjust = 2),
            position = position_dodge(width=1), color = "white") +
  guides(fill=FALSE)

dev.off() # close png device