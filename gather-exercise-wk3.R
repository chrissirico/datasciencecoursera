if(!require(devtools)) install.packages('devtools')
devtools::install_git("http://stash.redventures.net/scm/dsc/rvdata.git", quiet = TRUE)

# get connection
# username: REDVENTURES\<username>  e.g. REDVENTURES\kwilliams
atlas_conn <- rvdata::getAtlasDatabaseConnection(company = 'dsc')

# run query
data <- rvdata::pullDbData(queryString = 'SELECT TOP 10 * FROM ETL.ETL_Common.c_Company', dbType = 'MSSQL', conn = atlas_conn)

# view data
dplyr::glimpse(data)

# link to getting RODBC to work with MAC
# https://confluence.redventures.net/display/DSCI/How+to+install+RODBC+on+a+Mac

# query
query_string <- "select C.CallID, C.CallDate as TheDate, MP.ProgramName as MarketingProgram, 1 as Calls, COUNT(DISTINCT iif(O.Completed = 'y', O.OrderID, NULL)) as Orders, O.HomeOwner, C.CallDirection, iif(O.GeoUnits > 0, 1, 0) as GreenAttachments, C.CallerID_Number, C.CallLength, C.DNIS
FROM
ODS.EnergySavings.Calls C
left join ODS.EnergySavings.Orders O on O.CallID = C.CallID
left join ODS.common.Corporate_MarketingCodes MC on MC.MarketingCodeID = C.MarketingCodeID
left join ODS.common.Corporate_MarketingPrograms MP on MP.MarketingProgramID = MC.MarketingProgramID
LEFT JOIN ODS.EnergySavings.Markets_Price_Codes PC on PC.PriceCodeID = O.PriceCodeID
left join ODS.EnergySavings.Products P on P.ProductID = PC.ProductID
WHERE
C.CallDate between '2017-09-01' and '2017-09-30'

group by C.CallID, C.CallDate, O.GeoUnits, MP.ProgramName, O.HomeOwner, C.CallDirection, C.DNIS, C.CallLength, C.CallerID_Number"

#calls <- rvdata::pullDbData(queryString = query_string, dbType = 'MSSQL', conn = atlas_conn)
calls <- readRDS("calls.RDS")
glimpse(calls)


# Create a graph that shows how many calls, orders, and green attachments were made each day using ggplot2
# hint: use dplyr and tidyr to clean your data set before graphing, you will also need to do some date manipulations

calls %>% separate(TheDate, c("Year", "Month", "Day")) %>% 
  group_by(Day) %>%
  summarize(Calls = sum(Calls),
            Orders = sum(Orders),
            GreenAttachments = sum(GreenAttachments)) %>% 
  gather("Calls", "Orders", "GreenAttachments", key = Product, value = DayTotal) %>%

  ggplot() +
   geom_col(aes(Product, DayTotal, fill=Product))






# EXAMPLE STUFF

  library(dplyr)
  # From http://stackoverflow.com/questions/1181060
  stocks <- tibble(
    time = as.Date('2009-01-01') + 0:9,
    X = rnorm(10, 0, 1),
    Y = rnorm(10, 0, 2),
    Z = rnorm(10, 0, 4)
  )
  
  gather(stocks, stock, price, -time)
  stocks %>% gather(stock, price, -time)
  
  # get first observation for each Species in iris data -- base R
  mini_iris <- iris[c(1, 51, 101), ]
  # gather Sepal.Length, Sepal.Width, Petal.Length, Petal.Width
  gather(mini_iris, key = flower_att, value = measurement,
         Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
  # same result but less verbose
  gather(mini_iris, key = flower_att, value = measurement, -Species)
  
  # repeat iris example using dplyr and the pipe operator
  library(dplyr)
  mini_iris <-
    iris %>%
    group_by(Species) %>%
    slice(1)
  mini_iris %>% gather(key = flower_att, value = measurement, -Species)
  
  
  
  
# geom_bar is designed to make it easy to create bar charts that show
# counts (or sums of weights)
g <- ggplot(mpg, aes(class))
# Number of cars in each class:
g + geom_bar()
# Total engine displacement of each class
g + geom_bar(aes(weight = displ))

# To show (e.g.) means, you need geom_col()
# And, even more succinctly with geom_col()
df <- data.frame(trt = c("a", "b", "c"), outcome = c(2.3, 1.9, 3.2))
ggplot(df, aes(trt, outcome)) +
  geom_col()
# But geom_point() displays exactly the same information and doesn't
# require the y-axis to touch zero.
ggplot(df, aes(trt, outcome)) +
  geom_point()

# You can also use geom_bar() with continuous data, in which case
# it will show counts at unique locations
df <- data.frame(x = rep(c(2.9, 3.1, 4.5), c(5, 10, 4)))
ggplot(df, aes(x)) + geom_bar()
# cf. a histogram of the same data
ggplot(df, aes(x)) + geom_histogram(binwidth = 0.5)


# Bar charts are automatically stacked when multiple bars are placed
# at the same location
g + geom_bar(aes(fill = drv))

# You can instead dodge, or fill them
g + geom_bar(aes(fill = drv), position = "dodge")
g + geom_bar(aes(fill = drv), position = "fill")

# To change plot order of bars, change levels in underlying factor
reorder_size <- function(x) {
  factor(x, levels = names(sort(table(x))))
}
ggplot(mpg, aes(reorder_size(class))) + geom_bar()
