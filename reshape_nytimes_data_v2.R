
# read in the data
library("tidycensus")

nyt_url <- paste0("https://raw.githubusercontent.com/nytimes/",
                  "covid-19-data/master/us-counties.csv")
dat <- read.csv(nyt_url,as.is=TRUE)
dat$fips <- sprintf("%05d",dat$fips)

# main idea: create a matrix that has one row 
# for each county, one column for each dat

load("stco_maps.RData") # county_pop and state_map
ncounties <- nrow(county_pop)

first_date <- as.Date( dat$date[1], format = "%Y-%m-%d" )
last_date <- as.Date( dat$date[ length(dat$date) ], format = "%Y-%m-%d")
all_dates_num <- first_date:last_date
ndays <- length(all_dates_num)
all_dates <- first_date + (0:(ndays-1))

cases <- matrix(0, ncounties, ndays )
deaths <- matrix(0, ncounties, ndays )

# loop over all rows in nytimes dataset
for (ro in 1:nrow(dat)) {
    
  # isolate the state, county, case count and death count from NYT dataset, dat
  day <- dat$date[ro]
  case_ct <- dat$cases[ro]
  death_ct <- dat$deaths[ro]
  j2 <- which( all_dates == day )
  
  if(dat$county[ro] == "New York City"){ # distrubute among counties in NYC
      fips <- c(36005,36047,36061,36081,36085)
      inds <- which( county_pop$GEOID %in% fips )
      total_pop <- sum( county_pop$estimate[inds] )
      for(j in 1:length(fips)){
          j1 <- which( county_pop$GEOID == fips[j] )
          cases[j1,j2] <- case_ct*county_pop$estimate[j1]/total_pop
          deaths[j1,j2] <- death_ct*county_pop$estimate[j1]/total_pop
      }
  } else {     
    fips <- dat$fips[ro]
    # which row of "cases" this county is
    j1 <- which( county_pop$GEOID == fips )
    # which column of "cases" this day is
    ## then put the value in cases and deaths
    cases[j1,j2] <- case_ct
    deaths[j1,j2] <- death_ct
  }
}
# for each row, figure out which row and column
# we should put the data in 'cases' and 'deaths'

# put the data in 'cases' and 'deaths'

# modify the matrices 

# do some spot checking for accuracy
  # texas bexar 2020-02-14 cases: 2, deaths: 0
    ## cases[2502,25]
    ## deaths[2502,25]

  # wyoming park 2020-04-01 cases: 1, deaths: 0
    ## cases[3064,72]
    ## deaths[3064,72]

  # suffolk massachusetts 2020-03-24 cases:234 deaths:2
    ## cases[1194,64] 
    ## deaths[1194,64]

  # suffolk massachusetts 2020-03-25 cases: 342 deaths: 2
    ## cases[1194,65] 
    ## deaths[1194,65]

  # suffolk new york 2020-03-24 cases:1880 deaths:13
    ## cases[1844,64] 
    ## deaths[1844,64]

  # suffolk new york 2020-03-25 cases:2260 deaths:20
    ## cases[1844,65]
    ## deaths[1844,65]


# save as RData file

save( all_dates, cases, deaths, file = "reshaped_nytimes_data_v2.RData")

