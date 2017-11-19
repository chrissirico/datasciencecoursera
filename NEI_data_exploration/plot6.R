library(dplyr)
library(ggplot2)

# Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
# vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽").
# Which city has seen greater changes over time in motor vehicle emissions?

# L.A. has about 10x the population of Baltimore.
# Let's look at changes in motor vehicle emissions by year in terms of
# proportion of total as well as delta from beginning to end.

NEI <- readRDS("summarySCC_PM25.rds") # This has 6M rows, tonnage of pollutant by source & year
SCC <- readRDS("Source_Classification_Code.rds") # 11k rows, maps SCC codes to classification names
bigass_df <- left_join(NEI, SCC) %>%
  arrange(desc(Emissions))

# We'll group by year, type and other factors a lot and then sum Emissions.
# Let's make a function to do that last part.
sum_emissions <- function(df){
  result <- dplyr::summarise(df, Emissions = sum(Emissions));
  return(result)
}

onroad <- bigass_df %>%
  filter(type == "ON-ROAD") # All "on-road" sources

onroad_la <- onroad %>%
  dplyr::filter(fips == "06037")

onroad_baltimore <- onroad %>%
  dplyr::filter(fips == "24510")

la_sum <- group_by(onroad_la, year) %>%
  sum_emissions %>% tibble::as.tibble() %>% 
  dplyr::rename(LosAngeles = Emissions)

balt_sum <- group_by(onroad_baltimore, year) %>%
  sum_emissions %>% tibble::as.tibble() %>% 
  dplyr::rename(Baltimore = Emissions)

la_v_balt <- left_join(la_sum, balt_sum, key = "year") # put la and baltimore summaries into one object

# let's convert years to density
la_total <- summarise(la_sum,
                      LosAngeles = sum(LosAngeles)
)[[1,1]]
balt_total <- summarise(balt_sum,
                        Baltimore = sum(Baltimore)
)[[1,1]]

# gather cities into one column for plotting
la_v_balt_density <- la_v_balt %>%
  group_by(year) %>%
  summarise(LosAngeles = LosAngeles / la_total,
            Baltimore = Baltimore / balt_total) %>%
  gather(City, Emissions, -year) %>%
  group_by(City)

# percent change by city for labeling
la_percent_change <- (la_v_balt_density[[4, "Emissions"]] - la_v_balt_density[[1, "Emissions"]]) 
  / la_v_balt_density[[1, "Emissions"]]

balt_percent_change <- (la_v_balt_density[[8, "Emissions"]] - la_v_balt_density[[5, "Emissions"]])
  / la_v_balt_density[[5, "Emissions"]]

png("plot6.png") # open png device

# And plot findings
ggplot(la_v_balt_density) +
  geom_col(aes(x = year, y = Emissions, group = City, fill = City), position="dodge", width = 2.8) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) + 
  geom_segment(
               x = (1999 - .7),
               y = la_v_balt_density$Emissions[5],
               xend = (2008 - .7),
               yend = la_v_balt_density$Emissions[8],
               color = "pink",
               lineend = "round"
               ) +
  geom_segment(
               x = (1999 + .7),
               y = la_v_balt_density$Emissions[1],
               xend = (2008 + .7),
               yend = la_v_balt_density$Emissions[4],
               color = "turquoise",
               lineend = "round") +
  labs(
    title = expression('Baltimore vehicular PM'[2.5]*' emissions decreased while LA\'s rose'),
    x = "Year",
    y = "Vehicular emissions (tons)"
  ) +
  
  annotate("text", x = (2002.5), y = (.42),
           label = str_c((percent(balt_percent_change)), "\nchange\n1999-2008"),
           color = "salmon", size = 2.5) +
  annotate("text", x = (2001), y = (.3),
           label = str_c((percent(la_percent_change)),"\nchange\n1999-2008"),
           color = "#129496", size = 2.5)

dev.off() # close png device