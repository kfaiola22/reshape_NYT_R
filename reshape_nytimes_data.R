
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

##
## try not to use "row" as a variable because "row" is also a function
for (row in 1:nrow(dat)) {
  # isolate the state, county, case count and death count from NYT dataset, dat
  st <- tolower( dat$state[row] )
  # error: st only contains one state... 
  co <- tolower( dat$county[row] )
  day <- dat$date[row]
  
  case_ct <- dat$cases[row]
  death_ct <- dat$deaths[row]
  
  ## this is good up until here (I've moved the day assignment up)
  
  ## next step is to figure out which row of "cases" this county is
  j1 <- which( st_co$state == st & st_co$county == co )
  
  ## you fill in the blank here
  ## to figure out which column of "cases" this day is
  j2 <- ???
      
  ## then put the value in cases and deaths
  cases[j1,j2] <- case_ct
  deaths[j1,j2] <- death_ct
  
  # vector of what a reshaped row would contain 
  #st_co_cases_deaths <- c(st,co,case_ct,death_ct)
  
  # want to insert case and death counts in consecutive columns of same row - iterate thru days
  #for (i in (1:day)) {
  #  cases[i,] <- st_co_cases_deaths[3]
  #  deaths[i,] <- st_co_cases_deaths[4]
  #}
}
# for each row, figure out which row and column
# we should put the data in 'cases' and 'deaths'

# put the data in 'cases' and 'deaths'

# modify the matrices 
# cases[,] <- case_ct; cases
# deaths[,] <- death_ct; deaths

# do some spot checking for accuracy



# save as csv or .RData file
# write.csv

save( st_co, all_dates, cases, deaths, file = "reshaped_nytimes_data.RData")

