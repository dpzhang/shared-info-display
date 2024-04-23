library(dplyr)
library(magrittr)
library(ggplot2)
library(ggpubr)
library(tibble)
library(rlist)
library(chron)
library(tmap)
library(sf)
library(spdep)
library(sp)
library(purrr)
library(parallel)
library(knitr)
library(urca)
source("./utils/model-data.R")
# Chicago community shape file
chicagoCA <- sf::st_read("./data/chicagoCA-shape/chicagoCA.shp") %>%
  select(area_numbe, community, perimeter, shape_len, geometry) %>%
  mutate(area_numbe = as.numeric(area_numbe)) %>%
  arrange(area_numbe) %>%
  mutate(
    centroid_lat = geometry %>% map_dbl(~ st_centroid(.x)[[2]]),
    centroid_lon = geometry %>% map_dbl(~ st_centroid(.x)[[1]])
  )

# Load processed taxi trips data:
# Must download from: https://data.cityofchicago.org/Transportation/Taxi-Trips/wrvz-psew
alltripsDF <- read.csv("./data/taxiTrips/rawTrips.csv") %>%
  # cleaning and processing: convert time stamp
  mutate(
    starttimestamp = chron::times(starttimestamp),
    endtimestamp = chron::times(endtimestamp)
  )
nrow(alltripsDF)

# Weather data is collected from https://weatherstack.com/
# The data records historical hourly weather data of the Loop region
weatherDF <- read.csv("./data/weather/weather9AM.csv") %>%
  mutate(year = tripdate %>% strsplit("-") %>% lapply(function(x) {
    x %>%
      extract2(1) %>%
      as.numeric()
  })) %>%
  select(tripdate, year, feelslike, precip, wind_speed, totalsnow) %>%
  mutate(
    tripdate = as.character(tripdate),
    year = as.numeric(year),
    feelslike = as.numeric(feelslike),
    precip = as.numeric(precip),
    totalsnow = as.numeric(totalsnow)
  )

## Weather Stratification
# The weather data contains a list of meteorological variables. The variables we would use
# to stratify tripdates include:
# 1. body temperature (feelslike) in celcius
# 2. precip (precip) in mm
# 3. total fall amount (totalsnow) in cm
# 4. wind speed (wspd) in km/h

# 1. Apparent Temperature
# By inspecting the distributions, we made a judgment call to remove days with
# apparent temperatures below -10 °C (14 °F) and above 23 °C (73 °F).
# The thresholds are visualized in red vertical lines in the histogram.
tempFeelsDF <-
  weatherDF %>%
  select(year, feelslike) %>%
  mutate(year = factor(year))
tempBoxPlot <-
  tempFeelsDF %>%
  ggplot(aes(x = year, y = feelslike, color = year, fill = year, group = year)) +
  geom_boxplot(alpha = 0.5) +
  scale_color_manual(values = c("#003f5c", "#ffa600")) +
  scale_fill_manual(values = c("#003f5c", "#ffa600")) +
  labs(x = NULL, y = "Apparent Temp. (\u00B0C)") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
tempHistgram <-
  tempFeelsDF %>%
  ggplot(aes(x = feelslike, color = year, fill = year)) +
  geom_histogram(position = "identity", bins = 25, alpha = 0.5) +
  geom_vline(xintercept = -10, color = "#bc5090") +
  geom_vline(xintercept = 23, color = "#bc5090") +
  scale_color_manual(values = c("#003f5c", "#ffa600")) +
  scale_fill_manual(values = c("#003f5c", "#ffa600")) +
  labs(x = "Apparent Temp. (\u00B0C)", y = "Freq.") +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )
ggarrange(tempBoxPlot, tempHistgram, common.legend = T, legend = "bottom")

# 2. Percipiatation measures rainfall intensity.
# American Meteorological Society defines light rain when the precipitation rate is < 2.5 mm (0.098 in) per hour.
# Therefore, we include all days that have light rains.
weatherDF %>%
  ggplot(aes(x = precip)) +
  geom_histogram(bins = 30, color = "#ffa600", fill = "#003f5c") +
  geom_vline(xintercept = 2.5, color = "#bc5090") +
  labs(x = "9AM  Percipitation (mm)", y = "Freq.") +
  theme_minimal()

# 3. Snow depth
# From the distribution, we can see in 730 days (365 * 2), 594 (81.4%) of days has no snow.
# To impose control, we decide to remove all snow days from the analysis.
weatherDF %>%
  ggplot(aes(x = totalsnow)) +
  geom_histogram(bins = 20, color = "#ffa600", fill = "#003f5c") +
  labs(x = "Snow Depth (mm)", y = "Freq.") +
  theme_minimal()

# 4. Wind speed
# We use Beaufort Wind Scale, developed in 1805 by Sir Francis Beaufort, U.K. Royal NavyNational Weather Service,
# to classify wind intensity. <https://www.spc.noaa.gov/faq/tornado/beaufort.html>
# We decide to remove days when wind is greater than gentle breeze, which can be described as "Leaves and small twigs constantly moving, light flags extended"
weatherDF %>%
  # convert unit from km/h to knot
  mutate(wspdKnots = wind_speed / 1.852) %>% # 1 knot = 1.852 km/h
  ggplot(aes(x = wspdKnots)) +
  geom_histogram(bins = 15, color = "#ffa600", fill = "#003f5c") +
  geom_vline(xintercept = 12, size = 2, color = "#bc5090") +
  labs(x = "Wind Speed (Knot)", y = "Freq.") +
  theme_minimal()

# Define range of analysis datas
analysisDates <- weatherDF %>%
  select(tripdate, feelslike, precip, wind_speed, totalsnow) %>%
  filter(
    between(feelslike, -10, 23) &
      precip < 2.5 &
      totalsnow == 0 &
      (wind_speed / 1.852) < 12
  ) %>%
  pull(tripdate)

# Manually identified outlier
outlierDF <- read.csv("./data/outlierDates/outlierDates.csv")

# Define trips data
tripsDF <- alltripsDF %>%
  filter(tripdate %in% analysisDates) %>%
  filter(!tripdate %in% outlierDF$tripdate)

# In this section, we want to do some data sanity check and exploratory tasks:
# specifically, how many total pickups occurred on a daily basis between AM Peak
# in our target communities.
dailyPickupsDF <- tripsDF %>%
  filter(pickupcommunity %in% c(8, 28, 32, 33)) %>%
  group_by(tripdate) %>%
  summarise(
    numTrips = n(),
    .groups = "drop"
  ) %>%
  mutate(tripdate = as.Date(tripdate, "%Y-%m-%d"))

# Produce some descriptives
summary(dailyPickupsDF$numTrips)

# The histogram on the right seems to be bell shaped and is a bit left-skewed.
# From the boxplot, it seems there are 2 outliers.
g <- ggarrange(
  dailyPickupsDF %>% filter(!numTrips %in% boxplot.stats(numTrips)$out) %>%
    ggplot(aes(x = "", y = numTrips)) +
    geom_boxplot(color = "#003f5c") +
    labs(x = "", y = "Pickups", title = "(A). Boxplot") +
    theme_minimal(),
  dailyPickupsDF %>% filter(!numTrips %in% boxplot.stats(numTrips)$out) %>%
    ggplot(aes(x = numTrips)) +
    geom_histogram(bins = 15, color = "#ffa600", fill = "#003f5c") +
    labs(x = "Pickups", y = "Freq.", title = "(B). Distribution") +
    theme_minimal(),
  ncol = 2
) %>%
  ggarrange(dailyPickupsTS, ncol = 1, nrow = 2)
annotate_figure(g, top = text_grob("Distribution of Total AM Peak Pickups from 3 Target CAs",
  face = "bold", size = 14
))

# Show the time series:
dailyPickupsTS <- dailyPickupsDF %>%
  filter(!numTrips %in% boxplot.stats(numTrips)$out) %>%
  ggplot(aes(x = tripdate, y = numTrips, group = 1)) +
  geom_point(color = "#ffa600") +
  geom_line(color = "#003f5c") +
  scale_x_date(
    date_breaks = "4 month",
    labels = scales::date_format("%Y-%m"),
    limits = as.Date(c("2014-01-01", "2015-12-30"))
  ) +
  labs(
    x = "Dates",
    y = "Pickups",
    title = "(C). Time Series"
  ) +
  theme_minimal()

# It seems there are 4 outliers.
outlierQuantities <- boxplot.stats(dailyPickupsDF$numTrips)$out
outlierDates <- dailyPickupsDF %>%
  filter(numTrips %in% outlierQuantities) %>%
  pull(tripdate) %>%
  as.character()

# Remove and finalize data
finalTripsDF <- tripsDF %>%
  filter(!tripdate %in% outlierDates)

write.csv(finalTripsDF, "./data/taxiTrips/trips.csv", row.names = F)
