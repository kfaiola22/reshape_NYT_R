
# plot the nytimes data
source("reshape_nytimes_data.R")

# read in the matrix of cases and deaths
load("reshaped_nytimes_data.RData")

county_map <- ggplot2::map_data("county")
state_map <- ggplot2::map_data("state")
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

# trial aesthetics:
# library("hrbrthemes")
for(j in 1:4){
    p1 <- ggplot() 
    p2 <- geom_polygon( 
        data = county_map,
        aes( x = long, y = lat, group = group, fill = log(cases)), 
        color = NA
    )
    p5 <- scale_fill_gradient(low = "yellow", high = "red")
    p3 <- coord_map(projection = "mercator") 
    p4 <- theme_void()
    p7 <- ggtitle("Confirmed Coronavirus Cases by County")
    p8 <- labs(caption = "Data Source:The New York Times, based on reports from state and local health agencies")
    p6 <- geom_polygon(
        data = state_map,
        aes( x = long, y = lat, group = group),
        color = "black"
    )
    fname <- paste0( "testplot", j, ".png" )
    png(fname,width=800,height=600)
    print(p1 + p6 + p2 + p5 + p3 + p4 + p7 + p8)
    dev.off()
}

# from website on election visual:
# ggplot()+
# geom_polygon(county_map, aes(long, lat, group = group)) +
# geom_polygon(aes(fill = log(cases)),colour = alpha("white", 1/2), size = 0.05)  +
# geom_polygon(data = state_df, colour = "white", fill = NA) +
#     ggtitle("2012 US Election") +
#     scale_fill_gradientn(colours=c(blue,"white", red))  +
#     theme_void()