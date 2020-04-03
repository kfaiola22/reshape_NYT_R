
# plot the nytimes data
source("reshape_nytimes_data.R")

# read in the matrix of cases and deaths
load("reshaped_nytimes_data.RData")

county_map <- ggplot2::map_data("county")

library("ggplot2")

county_map$cases <- NA
county_map$deaths <- NA
for( j in 1:nrow(st_co) ){
    i1 <- county_map$region == st_co$state[j]
    i2 <- county_map$subregion == st_co$county[j]
    inds <- i1 & i2
    county_map$cases[inds] <- cases[j,ncol(cases)]
    county_map$deaths[inds] <- deaths[j,ncol(deaths)]
}
    
    
# play around with colors, projections,
# transformations (log, etc.)
# write a loop to make a bunch of plots, one for each day
for(j in 1:4){
    p1 <- ggplot() 
    p2 <- geom_polygon( 
        data = county_map, 
        aes( x = long, y = lat, group = group, fill = sqrt(cases) ), 
        color = NA
    )
    p3 <- coord_map(projection = "mollweide") 
    p4 <- theme_void()
    fname <- paste0( "testplot", j, ".png" )
    png(fname,width=800,height=600)
    print(p1 + p2 + p3 + p4)
    dev.off()
}

