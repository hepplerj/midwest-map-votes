library(readr)
library(tidyverse)
library(ggplot2)
library(acs)
library(choroplethr)
library(choroplethrMaps)

# Data
# ----------------------------------------------------------
county_data <- read_csv("Desktop/github/county_elections/2016_US_County_Level_Presidential_Results.csv")
county_fips <- read_csv("Desktop/github/county_elections/county-fips-master.csv")

county_merged <- merge(county_data, county_fips, by.x = "combined_fips", by.y = "fips")

# Census data - total population
# ----------------------------------------------------------
api.key.install(key="ff63df5edfd7248c0842206724dc13b6acc45565")
get_county_demographics = function(endyear=2013, span=5)
{  
  county_geo = geo.make(state = "*", county = "*")
  race.data = acs::acs.fetch(geography    = county_geo, 
                             table.number = "B03002", 
                             col.names    = "pretty", 
                             endyear      = endyear, 
                             span         = span)
  
  race.data@geography$fips = paste(as.character(race.data@geography$state), 
                                   race.data@geography$county, 
                                   sep = "")
  
  # choroplethr requires county fips to be numeric (i.e. no leading 0)
  race.data@geography$fips = as.numeric(race.data@geography$fips)
  
  # convert to a data.frame 
  df_race = data.frame(region                   = race.data@geography$fips,
                       total_population         = as.numeric(acs::estimate(race.data[,1])),
                       white_alone_not_hispanic = as.numeric(acs::estimate(race.data[,3])),
                       black_alone_not_hispanic = as.numeric(acs::estimate(race.data[,4])),
                       asian_alone_not_hispanic = as.numeric(acs::estimate(race.data[,6])),
                       hispanic_all_races       = as.numeric(acs::estimate(race.data[,12])))
  
  df_race$percent_white    = round(df_race$white_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_black    = round(df_race$black_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_asian    = round(df_race$asian_alone_not_hispanic / df_race$total_population * 100)
  df_race$percent_hispanic = round(df_race$hispanic_all_races       / df_race$total_population * 100)
  
  df_race = df_race[, c("region", "total_population", "percent_white", "percent_black", "percent_asian", "percent_hispanic")]
  
  # per capita income 
  df_income = choroplethr::get_acs_data("B19301", "county", endyear=endyear, span=span)[[1]]  
  colnames(df_income)[[2]] = "per_capita_income"
  
  # median rent
  df_rent = get_acs_data("B25058", "county", endyear=endyear, span=span)[[1]]  
  colnames(df_rent)[[2]] = "median_rent"
  
  # median age
  df_age = get_acs_data("B01002", "county", endyear=endyear, span=span, column_idx=1)[[1]]  
  colnames(df_age)[[2]] = "median_age"
  
  df_demographics = merge(df_race        , df_income, all.x=TRUE)
  df_demographics = merge(df_demographics, df_rent  , all.x=TRUE)  
  df_demographics = merge(df_demographics, df_age   , all.x=TRUE)
  
  # remove the regions (such as counties in Puerto Rico) that are not on my map.
  data(county.regions, package="choroplethrMaps", envir=environment())
  df_demographics = df_demographics[df_demographics$region %in% county.regions$region, ]
  
  df_demographics
}

df_demographics <- get_county_demographics(endyear = 2013, span = 5)

# Merge county Census data with elections results
county_final <- merge(county_merged, df_demographics, by.x = "combined_fips", by.y = "region")
county_final$percentage_vote <- county_final$total_votes / county_final$total_population

# Remove Alaska; we're getting strange results in vote percentages
county_final_exlude_AK <- county_final[!county_final$state.abbr == "AK", ]

# Determine which party carried the county
county_final_exlude_AK$party <- ifelse(county_final_exlude_AK$votes_dem > county_final_exlude_AK$votes_gop, "D", "R")

# Plot
# ----------------------------------------------------------
plain <- function(x,...) {
  format(x, ..., scientific = FALSE, trim = TRUE, big.mark=",")
}

ggplot(county_final_exlude_AK, aes(x = total_population, y = percentage_vote, color = party)) +
  geom_point(na.rm = TRUE, alpha = 0.3) +
  labs(x = "County Population", y = "Percentage Vote") +
  scale_x_log10(labels = plain) +
  scale_y_continuous(labels = scales::percent)
