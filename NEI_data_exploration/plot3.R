library(dplyr)
library(ggplot2)

# Of the four types of sources indicated by the 𝚝𝚢𝚙𝚎 (point, nonpoint, onroad, nonroad) variable,
# which of these four sources have seen decreases in emissions from 1999–2008 for Baltimore City?
# Which have seen increases in emissions from 1999–2008? Use the ggplot2 plotting system to make
# a plot answer this question

NEI <- readRDS("summarySCC_PM25.rds") # This has 6M rows, tonnage of pollutant by source & year
SCC <- readRDS("Source_Classification_Code.rds") # 11k rows, maps SCC codes to classification names

# We'll group by year, type and other factors a lot and then sum Emissions.
# Let's make a function to do that last part.
sum_emissions <- function(df){
  result <- summarise(df, Emissions = sum(Emissions));
  return(result)
}

# define multiplot (not my code)
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

# Select and munge relevant data:
# percent change from 99 to 08 by type in Baltimore
bc_type <- NEI %>% filter(fips == "24510", year == 1999 | year == 2008) %>% 
  group_by(year, type) %>%
  sum_emissions %>% # ungroup() %>% # not sure we need this
  group_by(type) %>%
  tidyr::spread(year, Emissions) %>% 
  summarise(PercentChange = (`2008`-`1999`) / `1999`, # add a column showing % change from beginning to end of period
            positive = PercentChange >= 0) # create positive / negative column for bar colors

# create data for emissions over time by plots (one per type)
bc_years <- NEI %>% filter(fips == "24510") %>% 
  group_by(year, type) %>%
  summarise(Emissions = sum(Emissions)) %>%
  ungroup() %>% 
  group_by(type)


png("plot3.png", height = 960) # open png device

# bar chart showing % change
plot1 <- ggplot(bc_type, aes(type, PercentChange)) +
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

# facet plot: emissions vs. time by type
plot2 <- ggplot(bc_years, aes(year, Emissions)) +
  geom_col() +
  labs(title = expression('Baltimore PM'[2.5]*' emissions mostly decreased over time')) +
  xlab("Source type") +
  ylab("Emissions (tons)") +
  scale_x_continuous(breaks = c(1999, 2002, 2005, 2008)) +
  facet_wrap(~type, nrow = 2)

multiplot(plot1, plot2)

dev.off() # close png device

# POINT was the only category to increase. The rest saw decrease.
