library(dplyr)
library(stringr)
library(ggplot2)

# Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?

NEI <- readRDS("summarySCC_PM25.rds") # This has 6M rows, tonnage of pollutant by source & year
SCC <- readRDS("Source_Classification_Code.rds") # 11k rows, maps SCC codes to classification names
bigass_df <- left_join(NEI, SCC) %>%
  arrange(desc(Emissions))

# We'll group by year, type and other factors a lot and then sum Emissions.
# Let's make a function to do that last part.
sum_emissions <- function(df){
  result <- summarise(df, Emissions = sum(Emissions));
  return(result)
}

# Look for mentions of coal, then filter out non-combustion categories.
all_coal <- bigass_df %>%
  filter( # Look for "coal" in text columns
    str_detect(Short.Name, "[Cc]oal") |
      str_detect(EI.Sector, "[Cc]oal") |
      str_detect(SCC.Level.One, "[Cc]oal") |
      str_detect(SCC.Level.Two, "[Cc]oal") |
      str_detect(SCC.Level.Three, "[Cc]oal") |
      str_detect(SCC.Level.Four, "[Cc]oal")
  ) %>%
  filter( # getting rid of some large buckets of irrelevant results
    !(str_detect(EI.Sector, "Mining")) &
      !(str_detect(EI.Sector, "Storage and Transfer")) 
    &
      !(str_detect(Short.Name, "Charcoal"))
  )%>%
  arrange(desc(Emissions)) # Arrange by emissions to filter out high-impact irrelevant results first

# Let's focus in on non-"fuel combustion" sources to separate out any other irrelevant results
coal_non_fuel_comb <- all_coal %>% 
  filter(!str_detect(EI.Sector, "Fuel Comb"))
# Keep anything "Kiln", "Fuel," "Coal-fired"
coal_non_fuel_comb_include <- coal_non_fuel_comb %>%
  filter(str_detect(Short.Name, "(Kiln)|(Fuel)|(Coal\\-fired)"))

# Coal grafitization, preheaters (coke manufacture), and other types of handling are the anomalies that remain.
# These aren't relevant to our exploration, so we'll filter them out.
coal_anomalies <- anti_join(coal_non_fuel_comb, coal_non_fuel_comb_include)
all_coal <- anti_join(all_coal, coal_anomalies)
all_coal <- filter(all_coal,
                   !str_detect(Short.Name, "Grafitization") &
                     !str_detect(Short.Name, "Handling") &
                     !str_detect(Short.Name, "Crushing") &
                     !str_detect(Short.Name, "Preheater") &
                     !str_detect(Short.Name, "Screening")
)

# Finally(!), let's sum emissions.
all_coal_sum <- all_coal %>% group_by(year) %>%
  sum_emissions() 

png("plot4.png") # open png device

# And plot findings
ggplot(all_coal_sum, aes(year, Emissions)) +
  geom_col() +
  labs(
    title = expression('Nationwide coal-burning PM'[2.5]*' emissions decreased \'99 - \'08'),
    x = "Year",
    y = "Coal burning emissions (tons)"
  ) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) +
  # scale_x_date(date_labels = "%y") +
  geom_text(aes(label=plyr::round_any(Emissions, 1), vjust = 2),
            position = position_dodge(width=1), color = "white") +
  guides(fill=FALSE)

dev.off() # close png device

# Coal-burning emissions have decreased.