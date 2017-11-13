# Project 2 - Exploratory Data Science Coursera
Chris Sirico  
11/7/2017  



1. **Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.**

Emissions did, in fact, decrease steadily from '99 to '08. (Wildfires were the single greatest contributor in years '99-'05, but didn't significantly effect the trend.)

```r
#create df summing emissions by year
uspm <- NEI %>% group_by(year) %>%
  summarise(Emissions = sum(Emissions))

bigass_df <- bigass_df %>% 
  mutate(
  Is.Fire = str_detect(EI.Sector, "Fires - Wildfires")
)

fire <- bigass_df %>% group_by(year, Is.Fire) %>%
  sum_emissions() %>% 
  spread(year, Emissions) %>% select(-Is.Fire)

# Base R stacked bar plot
barplot(height = as.matrix(fire),
        col = c(1, 2),
        legend.text = c("All others", "Wildfire emissions"),
        args.legend = list(x = "topright"));
title(main = expression('Total US PM'[2.5]*' yearly emissions (tons)')) # use cmd+shift+enter to run whole chunk and this title displays properly
```

![](project-2_files/figure-html/total-US, fig_caption: true-1.png)<!-- -->

```r
# graph the years 99, 02, 05, 08 (without fires)
# barplot(uspm$year, height = as.matrix(tidyr::spread(uspm, year, Emissions)))
# title(main = expression('Total US PM'[2.5]*' yearly emissions (tons)')) # use cmd+shift+enter to run whole chunk and this title displays properly
# analyze per capita and by GDP.
```

There were some changes in the way data were collected for the 2008 version of the NEI report (some categories were flat-lined) that could lead to lower-than-expected values compared to previous years. Click the first link on this page for more information: [Air Pollutant Emissions Trends Data](https://www.epa.gov/air-emissions-inventories/air-pollutant-emissions-trends-data)


2. **Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (𝚏𝚒𝚙𝚜 == "𝟸𝟺𝟻𝟷𝟶") from 1999 to 2008? Use the base plotting system to make a plot answering this question.**


```r
bcpm <- NEI %>% filter(fips == "24510") %>% 
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))

# graph the years 99, 02, 05, 08
barplot(bcpm$year, height = as.matrix(tidyr::spread(bcpm, year, Emissions)))
title(main = expression('Total Baltimore City PM'[2.5]*' yearly emissions (tons)')) # use cmd+shift+enter to run whole chunk and this title displays properly
```

![](project-2_files/figure-html/baltimore-total-1.png)<!-- -->


3. **Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable, which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City? Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make a plot answer this question.**


```r
# percent change from 99 to 08 by type in Baltimore
bc_type <- NEI %>% filter(fips == "24510", year == 1999 | year == 2008) %>% 
  group_by(year, type) %>%
  summarise(Emissions = sum(Emissions)) %>% ungroup() %>%
  group_by(type) %>%
  spread(year, Emissions) %>% 
  summarise(PercentChange = (`2008`-`1999`) / `1999`, # add a column showing % change from beginning to end of period
            positive = PercentChange >= 0) # create positive / negative column for bar colors

ggplot(bc_type, aes(type, PercentChange)) +
  geom_col(aes(fill = positive)) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    title = expression('Baltimore PM'[2.5]*' emissions mostly decreased \'99 - \'08'),
    subtitle = "With exception of point sources, this follows national trend",
    x = "Source type",
    y = "Change from 1999 to 2008"
  ) +
  geom_text(aes(label=percent(PercentChange),
            hjust=ifelse(sign(PercentChange)>0, 1.5, -.5)),
            position = position_dodge(width=1)) +
  coord_flip() +
  guides(fill=FALSE)
```

![](project-2_files/figure-html/unnamed-chunk-1-1.png)<!-- -->

A note on **source types**:
"POINT" refers to a fixed geographic source. "NONPOINT" refers to distributed activity, such as use of solvents or burning at multiple locations. "ON-ROAD" refers to vehicular sources (fueling, idling, and driving on roads & parking lots). "NON-ROAD" refers to emissions from use of gasoline, diesel or similar fuels by machines and vehicles not operating on roadways.

POINT emissions seem to have been at an unusual low in 1999, resulting in the positive trend shown above. 2008 has the lowest POINT emissions of all years observed except for 1999.

See the below graph for a sense of proportion amongst emission types in Baltimore during the years observed.


```r
bc_type_sum <- NEI %>% filter(fips == "24510") %>% 
  group_by(type) %>%
  summarise(Emissions = sum(Emissions))

ggplot(data = bc_type_sum, aes(x = type, y = Emissions)) +
  geom_col() +
  labs(title = expression('NONPOINT sources account for most PM'[2.5]*' emissions in Baltimore \'99 - \'08'),
    y = "Emissions (tons)"
  )
```

![](project-2_files/figure-html/baltimore-type-scale-1.png)<!-- -->


4. **Across the United States, how have emissions from coal combustion-related sources changed from 1999–2008?**


```r
library(stringr)

# Look for mentions of coal, then filter out non-combustion categories.
# This was the most time-consuming part of this project
bigass_df <- left_join(NEI, SCC)
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
# all_coal$year <- factor(all_coal$year) # re-class year as factor for plotting

# Finally(!), let's sum emissions.
all_coal_sum <- all_coal %>% group_by(year) %>%
  summarise(Emissions = sum(Emissions)) 

# And plot findings
ggplot(all_coal_sum, aes(year, Emissions)) +
  geom_col() +
  labs(
    title = expression('Nationwide coal-burning PM'[2.5]*' emissions decreased \'99 - \'08'),
    x = "Year",
    y = "Coal burning emissions (tons)"
  ) +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008), trans = "reverse") +
  # scale_x_date(date_labels = "%y") +
  geom_text(aes(label=plyr::round_any(Emissions, 1),
            hjust=1.2),
            position = position_dodge(width=1), color = "white") +
  coord_flip() +
  guides(fill=FALSE)
```

![](project-2_files/figure-html/us-coal-1.png)<!-- -->


5. **How have emissions from motor vehicle sources changed from 1999–2008 in Baltimore City?**


```r
bc_cars <- NEI %>% filter(fips == "24510" & type == "ON-ROAD") %>%
  group_by(year) %>%
  summarise(Emissions = sum(Emissions))

bc_cars$year <- factor(bc_cars$year)

ggplot(bc_cars, aes(year, Emissions)) +
  geom_col() +
  labs(
    title = expression('Baltimore vehicular PM'[2.5]*' emissions decreased dramatically \'99 - \'02'),
    subtitle = "Reductions may be driven by vehicular emissions improvements in response to Clean Air Act legislation",
    x = "Year",
    y = "Vehicular emissions (tons)"
  ) +
  scale_x_discrete(limits = rev(levels(all_coal_sum$year))) +
  # scale_x_date(date_labels = "%y") +
  geom_text(aes(label=plyr::round_any(Emissions, 1),
            hjust=2),
            position = position_dodge(width=1), color = "white") +
  coord_flip() +
  guides(fill=FALSE)
```

![](project-2_files/figure-html/baltimore-cars-1.png)<!-- -->

See more about the [Clean Air Act](https://www.epa.gov/clean-air-act-overview/progress-cleaning-air-and-improving-peoples-health#pollution).


6. **Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in Los Angeles County, California (𝚏𝚒𝚙𝚜 == "𝟶𝟼𝟶𝟹𝟽"). Which city has seen greater changes over time in motor vehicle emissions?**

Population Histories:
[Baltimore](https://www.biggestuscities.com/city/baltimore-maryland) (Slight population decrease in period)
[Los Angeles](https://www.biggestuscities.com/city/los-angeles-california) (Slight population increase in period.)

L.A. has about 10x the population of Baltimore. Let's look at changes in motor vehicle emissions by year in terms of proportion of total as well as delta from beginning to end.


```r
# After in-depth analysis (see below appendix), on-road seems to be the right bucket. Let's drill down by those counties and by year.

onroad <- bigass_df %>% filter(type == "ON-ROAD") # All "on-road" sources

onroad_la <- onroad %>%
  filter(fips == "06037")

onroad_baltimore <- onroad %>%
  filter(fips == "24510")

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
la_v_balt_density <- la_v_balt %>% group_by(year) %>%
  summarise(LosAngeles = LosAngeles / la_total,
            Baltimore = Baltimore / balt_total) %>%
  gather(City, Emissions, -year) %>% group_by(City)

# percent change by city for labeling
la_percent_change <- (la_v_balt_density[[4, "Emissions"]] - la_v_balt_density[[1, "Emissions"]]) / la_v_balt_density[[1, "Emissions"]]
balt_percent_change <- (la_v_balt_density[[8, "Emissions"]] - la_v_balt_density[[5, "Emissions"]]) / la_v_balt_density[[5, "Emissions"]]

ggplot(la_v_balt_density) +
  geom_col(aes(x = year, y = Emissions, group = City, fill = City), position="dodge", width = 2.8) +
    scale_y_continuous(labels = scales::percent) +
    scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) + 
    geom_segment(x = (1999 - .7), y = la_v_balt_density$Emissions[5], xend = (2008 - .7), yend = la_v_balt_density$Emissions[8], color = "pink", lineend = "round") +
    geom_segment(x = (1999 + .7), y = la_v_balt_density$Emissions[1], xend = (2008 + .7), yend = la_v_balt_density$Emissions[4], color = "turquoise", lineend = "round") +
    geom_text(aes(x = year, y = Emissions, label=percent(Emissions),
                    hjust=ifelse(City=="Baltimore", 1.1, -.2),
                    vjust=2),
                  position = position_dodge(width=1)) +
   annotate("text", x = (2008 - .7), y = (.126 + .05), label = str_c((percent(balt_percent_change)), "\nchange")) +
   annotate("text", x = (2008 + .7), y = (.243 + .05), label = str_c((percent(la_percent_change)), "\nchange"))
```

![](project-2_files/figure-html/la-v-balt-1.png)<!-- -->

```r
# absolute number plots, one per city (not as useful):
# ggplot(la_v_balt) +
#   geom_col(aes(year, LosAngeles))
# 
# ggplot(la_v_balt)+
#   geom_col(aes(year, Baltimore))
```

Baltimore has seen the bigger shift in on-road emissions, with a dramatic reduction between 1999 and 2002. The magnitude of the shift suggests a change in legislation, transit infrastructure or data collection methodology. Further investigation could help illuminate the cause of the shift.


## Appendix - categories exploration

This section contains insights about the data uncovered during analysis of whether "on-road" is an accurate bucket for vehicular sources. Not incredibly compelling presentation, but the process was helpful in getting a handle on what was going on in the data. Interesting insights include the fact that non-road diesel equipment is responsible for more particulate than all on-road sources.


```r
# onroad contains all "on-road" sources. Here are some other possible categories:
veh <- bigass_df %>% filter(str_detect(Short.Name, "Veh")) # Everything that contains "veh" (vehicle)
gas <- bigass_df %>% filter(str_detect(Short.Name, "Gasoline")) # Everything containing Gasoline fuel mention
diesel <- bigass_df %>% filter(str_detect(Short.Name, "Diesel")) # Everything containing Diesel fuel mention
# what do veh, gas and diesel have that on-road doesn't?
# looks like "Off-highway Diesel" equipment is a substantial category
# as is "Iron Production"
# Diesel as a category seems to dominate high-quantity particulate emissions

# Large majority of NEI sources contain either "Gasoline" or "Diesel"; each makes up a little less than half of the list
onroad %>% sum_emissions() # 580k tons
```

```
##   Emissions
## 1  579793.2
```

```r
veh %>% sum_emissions() #620 k tons
```

```
##   Emissions
## 1  618328.5
```

```r
gas %>% sum_emissions() #430 k tons
```

```
##   Emissions
## 1  430435.1
```

```r
diesel %>% sum_emissions() # 1,225 k tons - lots extra here compared to on-road
```

```
##   Emissions
## 1   1225612
```

```r
# Vehicles sources not including on-road. Small category by emissions (38k tons); Iron Production, Lime Production, Coal Mining, other industrial types of sources on the high emission end
veh_unique <- anti_join(veh, onroad)
veh_unique %>% sum_emissions()
```

```
##   Emissions
## 1  38535.29
```

```r
# Gasoline sources not including on-road. Small (23k tons); Lots of Pleasure Craft (private boats) and other small personal vehicles / equipment on the high end
gas_unique <- anti_join(gas, onroad)
gas_unique %>% sum_emissions()
```

```
##   Emissions
## 1    231984
```

```r
# Diesel sources not on-road. Big category - 884 k tons - more than on-road!
# Lots of diesel marine vessels on the high end; (port emissions seem to be higher than underway emissions--interesting)
# Lesser quantities from stationary & nonroad diesel equipment, but more sources
diesel_unique <- anti_join(diesel, onroad)
diesel_unique %>% sum_emissions()
```

```
##   Emissions
## 1    844270
```

```r
# Let's look at diesel marine vessels: 115 k tons -- does not account for the majority of unique diesel emissions
# Non-road equipment looks like another big one once you get down past the very largest emission sources
diesel_marine <- diesel_unique %>%
  filter(str_detect(Short.Name, "Marine Vessels"))
diesel_marine %>% sum_emissions()
```

```
##   Emissions
## 1  115286.2
```

```r
# Bingo. Non-road diesel equipment does in fact emit more fine particulate than all on-road sources combined. 600 k tons!
nr_equipment <- diesel_unique %>%
  filter(str_detect(EI.Sector, "Non-Road Equipment"))
nr_equipment %>% sum_emissions() 
```

```
##   Emissions
## 1  606203.4
```


