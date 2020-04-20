
tidycensus::census_api_key("34ee793ce93ee686718193c8f108cb8de08dfc91")

# create table of county demographics
options(tigris_use_cache = TRUE)

#county_map <- tigris::counties(class = "sf", cb = TRUE, resolution = "20m")


county_pop <- tidycensus::get_acs(
    geography = "county",
    variables = "B01003_001", 
    geometry = TRUE,
    resolution = "5m"
)
#county_pop <- sf::st_transform(county_pop, crs = 2163 )
state_map <- tigris::states( cb = TRUE, resolution = "5m", class = "sf" )

save( county_pop, state_map, file = "stco_maps.RData" )

