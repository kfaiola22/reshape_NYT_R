
# read in the data
dat <- read.csv("us-counties.csv",as.is=TRUE)

# main idea: create a matrix that has one row 
# for each county, one column for each dat


county_map <- ggplot2::map_data("county")

state_county <- unique(paste(county_map$region, county_map$subregion, sep = "@" ))
ncounties <- length(state_county)

st_co_list <- strsplit( state_county, "@", fixed = TRUE )

state <- rep(NA,ncounties)
county <- rep(NA,ncounties)
for(j in 1:ncounties){
    state[j] <- st_co_list[[j]][1]
    county[j] <- st_co_list[[j]][2]
}

st_co <- data.frame( state = state, county = county )

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
  st <- tolower( dat$state[ro] )
  co <- tolower( dat$county[ro] )
  day <- dat$date[ro]
  
  case_ct <- dat$cases[ro]
  death_ct <- dat$deaths[ro]
  
  # which row of "cases" this county is
  j1 <- which( st_co$state == st & st_co$county == co )
  
  # which column of "cases" this day is
  j2 <- which( all_dates == day )
      
  ## then put the value in cases and deaths
  cases[j1,j2] <- case_ct
  deaths[j1,j2] <- death_ct
}
# for each row, figure out which row and column
# we should put the data in 'cases' and 'deaths'

# put the data in 'cases' and 'deaths'

# modify the matrices 

# do some spot checking for accuracy



# save as csv or .RData file
# write.csv

save( st_co, all_dates, cases, deaths, file = "reshaped_nytimes_data.RData")

