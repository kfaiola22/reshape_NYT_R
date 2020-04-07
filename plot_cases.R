
args <- commandArgs(trailingOnly = TRUE)
region <- args[1]
days_back <- args[2]

# plot the nytimes data
source("reshape_nytimes_data.R")

# read in the matrix of cases and deaths
load("reshaped_nytimes_data.RData")

library("ggplot2")
county_map <- map_data("county")
state_map <- map_data("state")

# county_map$cases <- NA
# county_map$deaths <- NA
# for( j in 1:nrow(st_co) ){
#     i1 <- county_map$region == st_co$state[j]
#     i2 <- county_map$subregion == st_co$county[j]
#     inds <- i1 & i2
#     county_map$cases[inds] <- cases[j,ncol(cases)]
#     county_map$deaths[inds] <- deaths[j,ncol(deaths)]
# }
    
    
# play around with colors, projections,
# transformations (log, etc.)
# write a loop to make a bunch of plots, one for each day

for(j in ((ncol(cases) - days_back):ncol(cases))){
    county_map$cases <- NA
    county_map$deaths <- NA
    for( k in 1:nrow(st_co) ){
        i1 <- county_map$region == st_co$state[k]
        i2 <- county_map$subregion == st_co$county[k]
        inds <- i1 & i2
        county_map$cases[inds] <- cases[k,j]
        county_map$deaths[inds] <- deaths[k,j]
    }

    p1 <- ggplot() 
    p2 <- geom_polygon( 
        data = county_map,
        aes( x = long, y = lat, group = group, fill = cases), 
        color = NA,
    )
    p5 <- scale_fill_gradient(low = "yellow", high = "red", trans = "log10", limits = c(0, 15,000))
    p3 <- coord_map(projection = "lambert", parameters=c(25,50)) 
    p4 <- theme_void()
    p7 <- ggtitle("Confirmed Coronavirus Cases by County")
    p8 <- labs(caption = "Data Source:The New York Times, based on reports from state and local health agencies")
    p6 <- geom_polygon(
        data = state_map,
        aes(x = long, y = lat, group = group),
        color = "black",
        fill = "white",
        alpha = 0
    )
    fname <- paste0( "testplot", j, ".png" )
    png(fname,width=800,height=600)
    print(p1 + p2 + p5 + p6 + p3 + p4 + p7 + p8)
    dev.off()
}
