
# census_api_key(<your key here>, install = TRUE)

# create table of county demographics
library("tidycensus")

county_pop <- get_acs(geography = "county",
          variables = "B01003_001" )

raw_names <- county_pop$NAME
st_co_list <- strsplit( raw_names, " County, | Parish, " )
county <- sapply( st_co_list, function(x) tolower(x[1]) )
state <- sapply( st_co_list, function(x) tolower(x[2]) )
county <- stringr::str_remove( county, "\\." )



# fix spellings
county[ 1803 ] <- "dona ana"
county[ county == "dekalb" ] <- "de kalb"
county[ county == "lasalle" ] <- "la salle"
county[ state == "florida" & county == "desoto" ] <- "de soto"
county[ state == "illinois" & county == "dupage" ] <- "du page"
county[ state == "indiana" & county == "laporte" ] <- "la porte"
county[ state == "iowa" & county == "o'brien" ] <- "obrien"
county[ state == "maryland" & county == "prince george's" ] <- "prince georges"
county[ state == "maryland" & county == "queen anne's" ] <- "queen annes"
county[ state == "maryland" & county == "st mary's" ] <- "st marys"
county[ state == "mississippi" & county == "desoto" ] <- "de soto"
county[ state == "north dakoa" & county == "lamoure"] <- "lamoure"
#county[ state == "missouri" ]


county_demog <- data.frame(
    state = state,
    county = county,
    GEOID = county_pop$GEOID,
    pop = county_pop$estimate,
    stringsAsFactors = FALSE
)

save( county_demog, file = "county_demog.RData" )


