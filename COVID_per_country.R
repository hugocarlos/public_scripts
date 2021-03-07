## COVID-19
# Exploring the dataset of COVID-19 cases and vaccinations
# Dataset: <https://github.com/hugocarlos/covid-19-data/blob/master/public/data/owid-covid-data.csv>  
# Explanation on headers: <https://github.com/hugocarlos/covid-19-data/blob/master/public/data/owid-covid-codebook.csv>  

# Dataset from WorldBank with the total population per country in 2019
download.file("https://github.com/hugocarlos/public_scripts/blob/master/WorldPopulation2019.tsv?raw=true",
              destfile = "WorldPopulation2019.tsv")
WorldPopulation <- read.csv("WorldPopulation2019.tsv", sep = "\t", header = TRUE)
  
# Function that generates a vector with x-days rolling average for a given country and a given variable
# USAGE: generate_rolling_avg(subcovid, "France", "new_cases", 7)
# one_country = "France"; one_variable = "new_cases"; days = 7
generate_rolling_avg <- function(subcovid, one_country, one_variable, days = 7){
  range_days_in_one_country <- range(subcovid$date[which(subcovid$location == one_country)])
  # Identifying the dates present in subcovid
  dates_included <- seq(range_days_in_one_country[1], range_days_in_one_country[2],
                        by="days")
  # Calculating 7-day rolling mean
  variable_means <- sapply(dates_included[-(1:6)], function(end_of_the_week){
    # end_of_the_week <- dates_included[7]
    x_days_cases <- sapply(-6:0, function(y){
      # y <- -6
      subcovid[which(subcovid$location == one_country & subcovid$date == (end_of_the_week + y)),
               one_variable]
    })
    mean(x_days_cases)
  })
  variable_means_df <- data.frame(Dates = dates_included[-(1:6)],
                                  new_variable_avg = variable_means)
}

# 
download.file("https://github.com/owid/covid-19-data/blob/master/public/data/owid-covid-data.csv?raw=true",
              "owid-covid-data.txt")
covid <- read.csv("owid-covid-data.txt")
# Selecting some columns
subcovid <- covid %>%
  select(iso_code, location, date, new_cases, new_deaths, new_cases_per_million,
         total_cases_per_million, new_vaccinations, people_fully_vaccinated)

# To date format
subcovid$date <- as.Date(subcovid$date)

# Setting one country
one_country <- "France"

# Calculating the 7-days window average for new cases of COVID-19
cases_means_df <- generate_rolling_avg(subcovid, one_country, "new_cases", 7)

# Merging cases_means_df to subcovid
subcovid$new_cases_avg <- NA
for(i in 1:nrow(cases_means_df)){
  # i <- 1
  subcovid$new_cases_avg[which(subcovid$location == one_country &
                               subcovid$date == cases_means_df$Dates[i])] <-
    cases_means_df$new_variable_avg[i]
}

# Plot
ggplot() +
  geom_bar(stat = "identity",
           aes(x = subcovid$date[which(subcovid$location == one_country)],
               y = subcovid$new_deaths[which(subcovid$location == one_country)],
               colour = "New deaths")) +
  geom_point(aes(x = subcovid$date[which(subcovid$location == one_country)],
                 y = subcovid$new_cases[which(subcovid$location == one_country)],
                 colour = "New cases"), size = 0.7) +
#  geom_line(aes(x = cases_means_df$Dates,
#                y = cases_means_df$avg_mean,
  geom_line(aes(x = subcovid$date[which(subcovid$location == one_country)],
                y = subcovid$new_cases_avg[which(subcovid$location == one_country)],
                colour = "7-day rolling average")) +
  ylim(0, summary(cases_means_df$new_variable_avg)[6]) +
  labs(x = "Date", y = "Cases") +
  ggtitle(paste0("COVID-19 Cases and Deaths in ", one_country)) +
  theme(legend.position = c(0.2, 0.8),
        legend.title = element_blank(),
        legend.background = element_blank(),
        legend.key = element_rect(fill = NULL, color = NULL))

# The vaccination data can be included with the number of cases, using a 7-day rolling average:
# Finding all the days from 2021, as they probably contain vaccination data
# Since data takes time to appear, I skip today
dates_in_2021 <- seq(as.Date("2021-01-01"), (Sys.Date() - 1), by = "days")

one_country <- "Israel"
# Re-calculating the vector with 7-day rolling average of new COVID-19 cases
cases_means_df <- generate_rolling_avg(subcovid, one_country, "new_cases", 7)
# Calculating the percentage of the population fully vaccinated
one_country_tpop <- WorldPopulation$X2019[grep(one_country, WorldPopulation$Country.Name)]
# Checking that exactly one country was matched
if(length(one_country_tpop) != 1){
  print("More than one country retrieved")
  print(WorldPopulation$Country.Name[grep(one_country, WorldPopulation$Country.Name)])
}
# The following currently affects all countries, should not be used for a country other than one_country
subcovid$share_fully_vaccinated <- subcovid$people_fully_vaccinated * 100 / one_country_tpop
# subcovid$share_new_vaccinations <- subcovid$new_vaccinations * 100 / one_country_tpop

# Merging cases_means_df to subcovid
subcovid$new_cases_avg <- NA
for(i in 1:nrow(cases_means_df)){
  # i <- 1
  subcovid$new_cases_avg[which(subcovid$location == one_country &
                               subcovid$date == cases_means_df$Dates[i])] <-
    cases_means_df$new_variable_avg[i]
}

subcovid %>%
  filter(location == one_country) %>%
  filter(date >= dates_in_2021[1] & date < dates_in_2021[length(dates_in_2021)]) %>%
  ggplot() +
  geom_point(aes(x = date, y = share_fully_vaccinated * 200, colour = "Share of people fully vaccinated")) +
  #geom_point(aes(x = date, y = share_new_vaccinations * 200, colour = "Share of total vaccinations")) +
  geom_point(aes(x = date, y = new_cases_avg, colour = "New COVID-19 cases (7-day avg)")) +
  scale_y_continuous(name = "Number of cases",
                     sec.axis = sec_axis(~./200, name = "% of total population vaccinated",
                                         labels = function(b){
                                           paste0(b, "%")
                                          })) +
  xlab("Date") +
  ggtitle(paste0("COVID-19 Cases and vaccinations in ", one_country)) +
  theme(axis.title.y = element_text(color = "tomato"),
        axis.title.y.right = element_text(color = "cyan4"),
        legend.position = "bottom")


covid_onecountry <- subcovid[which(subcovid$location == "Israel" &
                                     subcovid$date >= as.Date("2021-02-10")), ]
# Calculating the Correlation Coefficient
cor(covid_onecountry$new_cases_avg,
    covid_onecountry$share_fully_vaccinated,
    use = "complete.obs")
lm_Israel <- lm(new_cases_avg ~ share_fully_vaccinated, covid_onecountry)
plot(x = covid_onecountry$share_fully_vaccinated, y = covid_onecountry$new_cases_avg,
     xlab = "New COVID-19 cases (7-day avg)",
     ylab = "% of population fully vaccinated")
abline(lm_Israel, col = "red")

# The number of new deaths (in a 7-day rolling average) is expressed as a function of the new vaccinations
# as reported from February 2021:
# Selecting values from Febrary to the most recent
covid_recent <- subcovid %>%
  filter(date >= as.Date("2021-02-01") - 7) # Taking 7 days before the value where we expect no NAs
results <- data.frame(Country = sort(unique(covid_recent$location)), lm_b0 = NA,
                      lm_b1 = NA, Adj_rsquared = NA)
# Adding the new column
covid_recent$new_deaths_avg <- NA
# loop for each country
for(one_country in results$Country){
  # one_country <- results$Country[3]
  newdeaths_7day_avg <- generate_rolling_avg(covid_recent, one_country, "new_deaths", 7)
  # attaching the 7-day rolling average for one country
  for(i in 1:nrow(newdeaths_7day_avg)){
    # i <- 1
    covid_recent$new_deaths_avg[which(covid_recent$location == one_country &
                                        covid_recent$date == newdeaths_7day_avg$Dates[i])] <-
      newdeaths_7day_avg$new_variable_avg[i]
  }
  one_lm <- try(lm(covid_recent$new_deaths_avg[which(covid_recent$location == one_country)] ~
                     covid_recent$new_vaccinations[which(covid_recent$location == one_country)]),
                silent = TRUE)
  if(class(one_lm) != "try-error"){
    results$lm_b0[which(results$Country == one_country)] <- one_lm$coefficients[1]
    results$lm_b1[which(results$Country == one_country)] <- one_lm$coefficients[2]
    results$Adj_rsquared[which(results$Country == one_country)] <-
      summary(one_lm)$adj.r.squared
  }
}

results %>%
  filter(!is.na(Adj_rsquared)) %>%
  arrange(desc(Adj_rsquared)) %>%
  top_n(10)

# Since South Africa has the highest Adjusted R-square, it is plotted:
SouthAfrica <- subcovid[which(subcovid$location == "South Africa"), ]
newdeaths_7day_avg <- generate_rolling_avg(SouthAfrica, "South Africa", "new_deaths", 7)
# attaching the 7-day rolling average for this country
SouthAfrica$new_deaths_avg <- NA
for(i in 1:nrow(newdeaths_7day_avg)){
  # i <- 1
  SouthAfrica$new_deaths_avg[which(SouthAfrica$date == newdeaths_7day_avg$Dates[i])] <-
    newdeaths_7day_avg$new_variable_avg[i]
}
SouthAfrica_fromJan <- SouthAfrica[which(SouthAfrica$date >= as.Date("2021-01-01")), ]
SouthAfrica_fromFeb <- SouthAfrica[which(SouthAfrica$date >= as.Date("2021-02-01")), ]
one_lm <- lm(SouthAfrica_fromFeb$new_deaths_avg ~
               SouthAfrica_fromFeb$new_vaccinations)

ggscatter(x = "new_deaths_avg", y = "people_fully_vaccinated",
          SouthAfrica_fromFeb[which(!is.na(SouthAfrica_fromFeb$new_vaccinations)), ],
          add = "reg.line", add.params = list(color = "blue", fill = "lightgray"),
          conf.int = TRUE) +
  labs(x = "New deaths (7-day avg)", y = "People fully vaccinated", colour = "") +
  stat_cor(method = "pearson") +
  ggtitle(paste0("South Africa, data from February 2021"))

